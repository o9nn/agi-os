#include "common.h"
#include "array.h"
#include "ioloop.h"
#include "hash.h"
#include "llist.h"
#include "str.h"
#include "safe-mkstemp.h"
#include "time-util.h"
#include "sleep.h"
#include "master-client.h"
#include "service.h"
#include "service-process.h"
#include "service-process-notify.h"
#include "service-anvil.h"
#include "service-log.h"
#include "service-monitor.h"
#include <unistd.h>
#include <sys/wait.h>
#include <syslog.h>
#include <signal.h>
#define SERVICE_DROP_WARN_INTERVAL_SECS 1
#define SERVICE_DROP_TIMEOUT_MSECS (10*1000)
#define SERVICE_LOG_DROP_WARNING_DELAY_MSECS 500
#define MAX_DIE_WAIT_MSECS 5000
#define SERVICE_MAX_EXIT_FAILURES_IN_SEC 10
#define SERVICE_MIN_SUCCESSFUL_AGE_SECS 10
#define SERVICE_PREFORK_MAX_AT_ONCE 10
static void service_monitor_start_extra_avail(struct service *service);
static void service_status_more(struct service_process *process,
const struct master_status *status);
static void service_monitor_listen_start_force(struct service *service);
static void service_process_idle_kill_timeout(struct service_process *process)
{
struct master_status status;
e_error(process->service->event, "Process %s is ignoring idle SIGINT",
dec2str(process->pid));
i_zero(&status);
service_status_more(process, &status);
process->available_count = 0;
}
static void service_kill_idle(struct service *service)
{
unsigned int processes_to_kill =
service->process_idling_lowwater_since_kills;
service->process_idling_lowwater_since_kills = service->process_idling;
i_assert(processes_to_kill <= service->process_avail);
if (processes_to_kill <= service->set->process_min_avail) {
if (service->process_idling == 0)
timeout_remove(&service->to_idle);
return;
}
processes_to_kill -= service->set->process_min_avail;
for (; processes_to_kill > 0; processes_to_kill--) {
struct service_process *process = service->idle_processes_head;
i_assert(process != NULL);
if (process->to_idle_kill != NULL) {
break;
}
i_assert(process->available_count == service->client_limit);
if (kill(process->pid, SIGINT) < 0 && errno != ESRCH) {
e_error(service->event, "kill(%s, SIGINT) failed: %m",
dec2str(process->pid));
}
process->last_kill_sent = ioloop_time;
process->to_idle_kill =
timeout_add(service->idle_kill * 1000,
service_process_idle_kill_timeout, process);
DLLIST2_REMOVE(&service->idle_processes_head,
&service->idle_processes_tail, process);
DLLIST2_APPEND(&service->idle_processes_head,
&service->idle_processes_tail, process);
}
}
static void service_status_more(struct service_process *process,
const struct master_status *status)
{
struct service *service = process->service;
if (process->idle_start != 0) {
DLLIST2_REMOVE(&service->idle_processes_head,
&service->idle_processes_tail, process);
DLLIST_PREPEND(&service->busy_processes, process);
process->idle_start = 0;
i_assert(service->process_idling > 0);
service->process_idling--;
service->process_idling_lowwater_since_kills =
I_MIN(service->process_idling_lowwater_since_kills,
service->process_idling);
}
process->total_count +=
process->available_count - status->available_count;
if (status->available_count != 0)
return;
i_assert(service->process_avail > 0);
service->process_avail--;
if (service->type == SERVICE_TYPE_LOGIN &&
service->process_avail == 0 &&
service->process_count == service->process_limit)
service_login_notify(service, TRUE);
service_monitor_start_extra_avail(service);
service_monitor_listen_start(service);
}
static void service_check_idle(struct service_process *process)
{
struct service *service = process->service;
if (process->available_count != service->client_limit)
return;
if (process->idle_start == 0) {
DLLIST_REMOVE(&service->busy_processes, process);
service->process_idling++;
} else {
DLLIST2_REMOVE(&service->idle_processes_head,
&service->idle_processes_tail, process);
}
DLLIST2_APPEND(&service->idle_processes_head,
&service->idle_processes_tail, process);
process->idle_start = ioloop_time;
if (service->process_avail > service->set->process_min_avail &&
service->to_idle == NULL &&
service->idle_kill != UINT_MAX) {
service->to_idle =
timeout_add(service->idle_kill * 1000,
service_kill_idle, service);
}
}
static void service_status_less(struct service_process *process)
{
struct service *service = process->service;
timeout_remove(&service->to_drop_warning);
if (process->available_count == 0) {
if (service->process_avail++ == 0)
service_monitor_listen_stop(service);
i_assert(service->process_avail <= service->process_count);
}
if (service->type == SERVICE_TYPE_LOGIN)
service_login_notify(service, FALSE);
}
static void
service_status_input_one(struct service *service,
const struct master_status *status)
{
struct service_process *process;
process = hash_table_lookup(service_pids, POINTER_CAST(status->pid));
if (process == NULL) {
return;
}
if (process->uid != status->uid || process->service != service) {
e_error(service->event, "Ignoring invalid update from child %s "
"(UID=%u)", dec2str(status->pid), status->uid);
return;
}
process->last_status_update = ioloop_time;
timeout_remove(&process->to_idle_kill);
timeout_remove(&process->to_status);
if (process->available_count != status->available_count) {
if (process->available_count > status->available_count) {
service_status_more(process, status);
} else {
service_status_less(process);
}
process->available_count = status->available_count;
}
service_check_idle(process);
}
static void service_status_input(struct service *service)
{
struct master_status status[1024/sizeof(struct master_status)];
unsigned int i, count;
ssize_t ret;
ret = read(service->status_fd[0], &status, sizeof(status));
if (ret <= 0) {
if (ret == 0)
e_error(service->event, "read(status) failed: EOF");
else if (errno != EAGAIN)
e_error(service->event, "read(status) failed: %m");
else
return;
service_monitor_stop(service);
return;
}
if ((ret % sizeof(struct master_status)) != 0) {
e_error(service->event, "service sent partial status update "
"(%d bytes)", (int)ret);
return;
}
count = ret / sizeof(struct master_status);
for (i = 0; i < count; i++)
service_status_input_one(service, &status[i]);
}
static void service_log_drop_warning(struct service *service)
{
const char *limit_name;
unsigned int limit;
if (service->last_drop_warning +
SERVICE_DROP_WARN_INTERVAL_SECS <= ioloop_time) {
service->last_drop_warning = ioloop_time;
if (service->process_limit > 1) {
limit_name = "process_limit";
limit = service->process_limit;
} else if (service->set->service_count == 1) {
i_assert(service->client_limit == 1);
limit_name = "client_limit/service_count";
limit = 1;
} else {
limit_name = "client_limit";
limit = service->client_limit;
}
e_warning(service->event,
"%s (%u) reached, client connections are being dropped",
limit_name, limit);
}
}
static void service_monitor_throttle(struct service *service)
{
if (service->to_throttle != NULL || service->list->destroying)
return;
i_assert(service->throttle_msecs > 0);
e_error(service->event,
"command startup failed, throttling for %u.%03u secs",
service->throttle_msecs / 1000,
service->throttle_msecs % 1000);
service_throttle(service, service->throttle_msecs);
service->throttle_msecs *= 2;
if (service->throttle_msecs >
SERVICE_STARTUP_FAILURE_THROTTLE_MAX_MSECS) {
service->throttle_msecs =
SERVICE_STARTUP_FAILURE_THROTTLE_MAX_MSECS;
}
}
static void service_drop_timeout(struct service *service)
{
struct service_listener *lp;
int fd;
i_assert(service->process_avail == 0);
array_foreach_elem(&service->listeners, lp) {
while ((fd = net_accept(lp->fd, NULL, NULL)) > 0)
net_disconnect(fd);
}
service_monitor_listen_start_force(service);
service->listen_pending = TRUE;
}
static void service_monitor_listen_pending(struct service *service)
{
i_assert(service->process_avail == 0);
service_monitor_listen_stop(service);
service->listen_pending = TRUE;
service->to_drop = timeout_add(SERVICE_DROP_TIMEOUT_MSECS,
service_drop_timeout, service);
}
static void service_drop_connections(struct service_listener *l)
{
struct service *service = l->service;
int fd;
if (service->type != SERVICE_TYPE_WORKER)
service_log_drop_warning(service);
if (service->type == SERVICE_TYPE_LOGIN) {
service_login_notify(service, TRUE);
service_monitor_listen_pending(service);
} else if (!service->listen_pending) {
service_monitor_listen_pending(service);
if (service->to_drop_warning == NULL &&
service->type == SERVICE_TYPE_WORKER) {
service->to_drop_warning =
timeout_add_short(SERVICE_LOG_DROP_WARNING_DELAY_MSECS,
service_log_drop_warning, service);
}
} else {
fd = net_accept(l->fd, NULL, NULL);
if (fd > 0)
net_disconnect(fd);
}
}
static void service_accept(struct service_listener *l)
{
struct service *service = l->service;
i_assert(service->process_avail == 0);
if (service->process_count == service->process_limit) {
service_drop_connections(l);
return;
}
if (service_process_create(service) == NULL)
service_monitor_throttle(service);
else
service_monitor_listen_stop(service);
}
static bool
service_monitor_start_count(struct service *service, unsigned int limit)
{
unsigned int i, count;
i_assert(service->set->process_min_avail >= service->process_avail);
count = service->set->process_min_avail - service->process_avail;
if (service->process_count + count > service->process_limit)
count = service->process_limit - service->process_count;
if (count > limit)
count = limit;
for (i = 0; i < count; i++) {
if (service_process_create(service) == NULL) {
service_monitor_throttle(service);
break;
}
}
if (i > 0) {
service_monitor_listen_stop(service);
}
return i >= limit;
}
static void service_monitor_prefork_timeout(struct service *service)
{
if (service->list->fork_counter != service->prefork_counter) {
service->prefork_counter = service->list->fork_counter;
return;
}
if (service->process_avail < service->set->process_min_avail) {
if (service_monitor_start_count(service, SERVICE_PREFORK_MAX_AT_ONCE) &&
service->process_avail < service->set->process_min_avail) {
return;
}
}
timeout_remove(&service->to_prefork);
}
static void service_monitor_start_extra_avail(struct service *service)
{
if (service->process_avail >= service->set->process_min_avail ||
service->process_count >= service->process_limit ||
service->list->destroying)
return;
if (service->process_avail == 0) {
if (!service_monitor_start_count(service, 1))
return;
if (service->process_avail >= service->set->process_min_avail)
return;
}
if (service->to_prefork == NULL) {
service->to_prefork =
timeout_add_short(0, service_monitor_prefork_timeout, service);
}
}
static void service_monitor_listen_start_force(struct service *service)
{
struct service_listener *l;
service->listening = TRUE;
service->listen_pending = FALSE;
timeout_remove(&service->to_drop);
timeout_remove(&service->to_drop_warning);
array_foreach_elem(&service->listeners, l) {
if (l->io == NULL && l->fd != -1)
l->io = io_add(l->fd, IO_READ, service_accept, l);
}
}
void service_monitor_listen_start(struct service *service)
{
if (service->process_avail > 0 || service->to_throttle != NULL ||
(service->process_count == service->process_limit &&
service->listen_pending))
return;
service_monitor_listen_start_force(service);
}
void service_monitor_listen_stop(struct service *service)
{
struct service_listener *l;
array_foreach_elem(&service->listeners, l)
io_remove(&l->io);
service->listening = FALSE;
service->listen_pending = FALSE;
timeout_remove(&service->to_drop);
timeout_remove(&service->to_drop_warning);
}
static int service_login_create_notify_fd(struct service *service)
{
int fd, ret;
if (service->login_notify_fd != -1)
return 0;
T_BEGIN {
string_t *prefix = t_str_new(128);
const char *path;
str_append(prefix, service->list->set->base_dir);
str_append(prefix, "/login-master-notify");
fd = safe_mkstemp(prefix, 0600, (uid_t)-1, (gid_t)-1);
path = str_c(prefix);
if (fd == -1) {
e_error(service->event, "safe_mkstemp(%s) failed: %m",
path);
} else if (unlink(path) < 0) {
e_error(service->event, "unlink(%s) failed: %m", path);
} else {
fd_close_on_exec(fd, TRUE);
service->login_notify_fd = fd;
}
} T_END;
ret = fd == -1 ? -1 : 0;
if (fd != service->login_notify_fd)
i_close_fd(&fd);
return ret;
}
void services_monitor_start(struct service_list *service_list)
{
ARRAY(struct service *) listener_services;
struct service *service;
if (services_log_init(service_list) < 0)
return;
service_anvil_monitor_start(service_list);
if (service_list->io_master == NULL &&
service_list->master_fd != -1) {
service_list->io_master =
io_add(service_list->master_fd, IO_READ,
master_client_connected, service_list);
}
t_array_init(&listener_services, array_count(&service_list->services));
array_foreach_elem(&service_list->services, service) {
if (service->type == SERVICE_TYPE_LOGIN) {
if (service_login_create_notify_fd(service) < 0)
continue;
}
if (service->master_dead_pipe_fd[0] == -1) {
if (pipe(service->master_dead_pipe_fd) < 0) {
e_error(service->event, "pipe() failed: %m");
continue;
}
fd_close_on_exec(service->master_dead_pipe_fd[0], TRUE);
fd_close_on_exec(service->master_dead_pipe_fd[1], TRUE);
}
if (service->status_fd[0] == -1) {
if (pipe(service->status_fd) < 0) {
e_error(service->event, "pipe() failed: %m");
continue;
}
net_set_nonblock(service->status_fd[0], TRUE);
fd_close_on_exec(service->status_fd[0], TRUE);
net_set_nonblock(service->status_fd[1], TRUE);
fd_close_on_exec(service->status_fd[1], TRUE);
}
if (service->io_status == NULL) {
service->io_status =
io_add(service->status_fd[0], IO_READ,
service_status_input, service);
}
service_monitor_listen_start(service);
array_push_back(&listener_services, &service);
}
array_foreach_elem(&listener_services, service)
service_monitor_start_extra_avail(service);
if (service_list->log->status_fd[0] != -1) {
if (service_process_create(service_list->log) != NULL)
service_monitor_listen_stop(service_list->log);
}
array_foreach_elem(&service_list->services, service) {
if (service->type == SERVICE_TYPE_STARTUP &&
service->status_fd[0] != -1) {
if (service_process_create(service) != NULL)
service_monitor_listen_stop(service);
}
}
}
static void service_monitor_close_dead_pipe(struct service *service)
{
if (service->master_dead_pipe_fd[0] != -1) {
i_close_fd(&service->master_dead_pipe_fd[0]);
i_close_fd(&service->master_dead_pipe_fd[1]);
}
}
void service_monitor_stop(struct service *service)
{
int i;
io_remove(&service->io_status);
if (service->status_fd[0] != -1 &&
service->type != SERVICE_TYPE_ANVIL) {
for (i = 0; i < 2; i++) {
if (close(service->status_fd[i]) < 0) {
e_error(service->event,
"close(status fd) failed: %m");
}
service->status_fd[i] = -1;
}
}
service_monitor_close_dead_pipe(service);
if (service->login_notify_fd != -1) {
if (close(service->login_notify_fd) < 0) {
e_error(service->event,
"close(login notify fd) failed: %m");
}
service->login_notify_fd = -1;
}
timeout_remove(&service->to_login_notify);
service_monitor_listen_stop(service);
timeout_remove(&service->to_throttle);
timeout_remove(&service->to_prefork);
timeout_remove(&service->to_idle);
}
void service_monitor_stop_close(struct service *service)
{
struct service_listener *l;
service_monitor_stop(service);
array_foreach_elem(&service->listeners, l)
i_close_fd(&l->fd);
}
static void services_monitor_wait(struct service_list *service_list)
{
struct service *service;
struct timeval tv_start;
bool finished;
io_loop_time_refresh();
tv_start = ioloop_timeval;
for (;;) {
finished = TRUE;
services_monitor_reap_children();
array_foreach_elem(&service_list->services, service) {
if (service->status_fd[0] != -1)
service_status_input(service);
if (service->process_avail > 0)
finished = FALSE;
}
io_loop_time_refresh();
if (finished ||
timeval_diff_msecs(&ioloop_timeval, &tv_start) > MAX_DIE_WAIT_MSECS)
break;
i_sleep_msecs(10);
}
}
static bool
service_processes_list_close_listeners(struct service *service,
struct service_process *processes)
{
struct service_process *process = processes;
bool ret = FALSE;
for (; process != NULL; process = process->next) {
if (kill(process->pid, SIGQUIT) == 0)
ret = TRUE;
else if (errno != ESRCH) {
e_error(service->event, "kill(%s, SIGQUIT) failed: %m",
dec2str(process->pid));
}
}
return ret;
}
static bool service_processes_close_listeners(struct service *service)
{
bool ret = FALSE;
if (service_processes_list_close_listeners(service,
service->busy_processes))
ret = TRUE;
if (service_processes_list_close_listeners(service,
service->idle_processes_head))
ret = TRUE;
return ret;
}
static bool
service_list_processes_close_listeners(struct service_list *service_list)
{
struct service *service;
bool ret = FALSE;
array_foreach_elem(&service_list->services, service) {
if (service_processes_close_listeners(service)) {
if (service->type != SERVICE_TYPE_LOG)
ret = TRUE;
else {
}
}
}
return ret;
}
static void services_monitor_wait_and_kill(struct service_list *service_list)
{
services_monitor_wait(service_list);
if (service_list_processes_close_listeners(service_list)) {
i_sleep_msecs(1000);
}
}
void services_monitor_stop(struct service_list *service_list, bool wait)
{
struct service *service;
array_foreach_elem(&service_list->services, service)
service_monitor_close_dead_pipe(service);
if (wait)
services_monitor_wait_and_kill(service_list);
io_remove(&service_list->io_master);
array_foreach_elem(&service_list->services, service)
service_monitor_stop(service);
services_log_deinit(service_list);
}
static bool
service_has_successful_processes_in_list(struct service *service,
struct service_process *processes)
{
struct service_process *process = processes;
for (; process != NULL; process = process->next) {
time_t age_secs = ioloop_time - process->create_time;
if (age_secs >= SERVICE_MIN_SUCCESSFUL_AGE_SECS &&
process->to_status == NULL) {
service->have_successful_exits = TRUE;
return TRUE;
}
}
return FALSE;
}
static bool service_has_successful_processes(struct service *service)
{
if (service->have_successful_exits)
return TRUE;
return service_has_successful_processes_in_list(service,
service->busy_processes) ||
service_has_successful_processes_in_list(service,
service->idle_processes_head);
}
static bool
service_process_failure(struct service_process *process, int status)
{
struct service *service = process->service;
bool throttle;
service_process_log_status_error(process, status);
throttle = process->to_status != NULL;
if (!throttle && !service_has_successful_processes(service)) {
if (service->exit_failure_last != ioloop_time) {
service->exit_failure_last = ioloop_time;
service->exit_failures_in_sec = 0;
}
if (++service->exit_failures_in_sec > SERVICE_MAX_EXIT_FAILURES_IN_SEC)
throttle = TRUE;
}
service_process_notify_add(service_anvil_global->kills, process);
return throttle;
}
void services_monitor_reap_children(void)
{
struct service_process *process;
struct service *service;
pid_t pid;
int status;
bool service_stopped, throttle;
while ((pid = waitpid(-1, &status, WNOHANG)) > 0) {
process = hash_table_lookup(service_pids, POINTER_CAST(pid));
if (process == NULL) {
i_error("waitpid() returned unknown PID %s",
dec2str(pid));
continue;
}
service = process->service;
if (status == 0) {
service->have_successful_exits = TRUE;
service->exit_failures_in_sec = 0;
service->throttle_msecs =
SERVICE_STARTUP_FAILURE_THROTTLE_MIN_MSECS;
throttle = FALSE;
} else {
throttle = service_process_failure(process, status);
}
if (service->type == SERVICE_TYPE_ANVIL)
service_anvil_process_destroyed(process);
service_list_ref(service->list);
service_process_destroy(process);
if (throttle)
service_monitor_throttle(service);
service_stopped = service->status_fd[0] == -1;
if (!service_stopped && !service->list->destroying) {
service_monitor_start_extra_avail(service);
if (service->to_throttle != NULL) {
} else if (service == service->list->log &&
service->process_count == 0) {
if (service_process_create(service) == NULL)
service_monitor_throttle(service);
} else {
service_monitor_listen_start(service);
}
}
service_list_unref(service->list);
}
}