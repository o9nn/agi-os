#ifndef SERVICE_H
#define SERVICE_H
#include "net.h"
#include "master-settings.h"
#define SERVICE_FIRST_STATUS_TIMEOUT_SECS 30
#define SERVICE_STARTUP_FAILURE_THROTTLE_MIN_MSECS (2*1000)
#define SERVICE_STARTUP_FAILURE_THROTTLE_MAX_MSECS (60*1000)
enum service_listener_type {
SERVICE_LISTENER_UNIX,
SERVICE_LISTENER_FIFO,
SERVICE_LISTENER_INET
};
struct service_listener {
struct service *service;
enum service_listener_type type;
int fd;
struct io *io;
const char *name;
const char *inet_address;
union {
struct {
const struct file_listener_settings *set;
uid_t uid;
gid_t gid;
} fileset;
struct {
const struct inet_listener_settings *set;
struct ip_addr ip;
} inetset;
} set;
bool reuse_port;
};
struct service {
struct service_list *list;
struct event *event;
enum service_type type;
const struct service_settings *set;
const char *config_file_path;
const char *executable;
uid_t uid;
gid_t gid;
gid_t privileged_gid;
const char *extra_gids;
ARRAY(struct service_listener *) listeners;
ARRAY(struct service_listener *) unix_pid_listeners;
struct service_process *busy_processes;
struct service_process *idle_processes_head, *idle_processes_tail;
unsigned int process_count;
unsigned int process_avail;
unsigned int process_idling;
unsigned int process_idling_lowwater_since_kills;
unsigned int process_limit;
uint64_t process_count_total;
unsigned int client_limit;
unsigned int idle_kill;
uoff_t vsz_limit;
int log_fd[2];
int log_process_internal_fd;
int status_fd[2];
struct io *io_status;
int master_dead_pipe_fd[2];
unsigned int throttle_msecs;
time_t exit_failure_last;
unsigned int exit_failures_in_sec;
int login_notify_fd;
time_t last_login_notify_time;
struct timeout *to_login_notify;
struct timeout *to_throttle;
struct timeout *to_drop;
struct timeout *to_drop_warning;
struct timeout *to_idle;
struct timeout *to_prefork;
unsigned int prefork_counter;
time_t last_drop_warning;
bool listen_pending:1;
bool listening:1;
bool have_inet_listeners:1;
bool last_login_full_notify:1;
bool have_successful_exits:1;
bool doveadm_stop:1;
};
struct service_list {
pool_t pool;
int refcount;
struct timeout *to_kill;
unsigned int fork_counter;
struct event *event;
const struct master_settings *set;
struct service *config;
struct service *log;
struct service *anvil;
struct file_listener_settings master_listener_set;
struct io *io_master;
int master_fd;
int master_log_fd[2];
struct service_process_notify *log_byes;
ARRAY(struct service *) services;
bool destroying:1;
bool destroyed:1;
bool sigterm_sent:1;
bool sigterm_sent_to_log:1;
};
HASH_TABLE_DEFINE_TYPE(pid_process, void *, struct service_process *);
extern HASH_TABLE_TYPE(pid_process) service_pids;
int services_create(const struct master_settings *set,
struct service_list **services_r, const char **error_r);
void services_destroy(struct service_list *service_list, bool wait);
void service_list_ref(struct service_list *service_list);
void service_list_unref(struct service_list *service_list);
const char *services_get_config_socket_path(struct service_list *service_list);
unsigned int service_signal(struct service *service, int signo,
unsigned int *uninitialized_count_r);
void service_login_notify(struct service *service, bool all_processes_full);
void service_throttle(struct service *service, unsigned int msecs);
void services_throttle_time_sensitives(struct service_list *list,
unsigned int msecs);
struct service *
service_lookup(struct service_list *service_list, const char *name);
struct service *
service_lookup_type(struct service_list *service_list, enum service_type type);
void service_pids_init(void);
void service_pids_deinit(void);
#endif