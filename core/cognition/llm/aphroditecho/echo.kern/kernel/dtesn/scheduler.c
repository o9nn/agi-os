#include <dtesn/scheduler.h>
#include <dtesn/memory.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <sys/time.h>
#include <unistd.h>
static dtesn_scheduler_t g_scheduler;
static const uint32_t oeis_a000081_sequence[DTESN_SCHED_A000081_MAX_DEPTH] =
DTESN_SCHED_A000081_SEQUENCE;
static uint64_t dtesn_get_time_ns(void);
static int dtesn_runqueue_init(dtesn_runqueue_t *rq, uint32_t cpu_id);
static void dtesn_runqueue_add_task(dtesn_runqueue_t *rq, dtesn_task_t *task);
static dtesn_task_t *dtesn_runqueue_pick_next(dtesn_runqueue_t *rq);
static void dtesn_update_performance_metrics(dtesn_runqueue_t *rq, uint64_t latency_ns);
static int dtesn_policy_select_task(dtesn_runqueue_t *rq, dtesn_task_t **selected_task);
static uint64_t dtesn_get_time_ns(void)
{
struct timespec ts;
if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
return 0;
}
return (uint64_t)ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}
static int dtesn_runqueue_init(dtesn_runqueue_t *rq, uint32_t cpu_id)
{
if (!rq || cpu_id >= DTESN_SCHED_MAX_CPUS) {
return -EINVAL;
}
memset(rq, 0, sizeof(dtesn_runqueue_t));
rq->cpu_id = cpu_id;
rq->nr_running = 0;
rq->nr_switches = 0;
rq->queue_bitmap = 0;
rq->current_task = NULL;
rq->idle_task = NULL;
rq->load_weight = 0;
rq->last_balance_ns = dtesn_get_time_ns();
rq->next_deadline_ns = UINT64_MAX;
rq->rt_tasks = 0;
for (int i = 0; i < DTESN_SCHED_MAX_PRIORITIES; i++) {
rq->ready_queue[i] = NULL;
}
memset(&rq->perf, 0, sizeof(dtesn_sched_perf_t));
rq->perf.min_latency_ns = UINT64_MAX;
if (pthread_spin_init(&rq->rq_lock, PTHREAD_PROCESS_PRIVATE) != 0) {
return -ENOMEM;
}
return 0;
}
int dtesn_sched_init(uint32_t nr_cpus)
{
int ret;
if (nr_cpus == 0 || nr_cpus > DTESN_SCHED_MAX_CPUS) {
return -EINVAL;
}
if (g_scheduler.initialized) {
return -EALREADY;
}
memset(&g_scheduler, 0, sizeof(dtesn_scheduler_t));
g_scheduler.nr_cpus = nr_cpus;
g_scheduler.nr_tasks = 0;
g_scheduler.next_task_id = 1;
g_scheduler.load_balancing_enabled = true;
g_scheduler.balance_interval_ns = 10000000;
g_scheduler.migration_threshold = 4;
g_scheduler.priority_inheritance_enabled = true;
g_scheduler.pi_boost_limit = 3;
g_scheduler.oeis_validation_enabled = true;
g_scheduler.scheduler_start_time_ns = dtesn_get_time_ns();
memcpy(g_scheduler.oeis_sequence, oeis_a000081_sequence,
sizeof(oeis_a000081_sequence));
for (uint32_t i = 0; i < nr_cpus; i++) {
ret = dtesn_runqueue_init(&g_scheduler.runqueues[i], i);
if (ret != 0) {
for (uint32_t j = 0; j < i; j++) {
pthread_spin_destroy(&g_scheduler.runqueues[j].rq_lock);
}
return ret;
}
}
for (int i = 0; i < DTESN_SCHED_MAX_TASKS; i++) {
g_scheduler.task_table[i] = NULL;
}
memset(&g_scheduler.global_perf, 0, sizeof(dtesn_sched_perf_t));
g_scheduler.global_perf.min_latency_ns = UINT64_MAX;
ret = pthread_mutex_init(&g_scheduler.scheduler_lock, NULL);
if (ret != 0) {
for (uint32_t i = 0; i < nr_cpus; i++) {
pthread_spin_destroy(&g_scheduler.runqueues[i].rq_lock);
}
return -ENOMEM;
}
g_scheduler.initialized = true;
return 0;
}
static void dtesn_runqueue_add_task(dtesn_runqueue_t *rq, dtesn_task_t *task)
{
if (!rq || !task) {
return;
}
pthread_spin_lock(&rq->rq_lock);
uint32_t priority = task->effective_priority;
if (priority >= DTESN_SCHED_MAX_PRIORITIES) {
priority = DTESN_SCHED_MAX_PRIORITIES - 1;
}
task->next = rq->ready_queue[priority];
task->prev = NULL;
if (rq->ready_queue[priority]) {
rq->ready_queue[priority]->prev = task;
}
rq->ready_queue[priority] = task;
rq->queue_bitmap |= (1U << priority);
rq->nr_running++;
if (task->policy == DTESN_SCHED_POLICY_REALTIME ||
task->policy == DTESN_SCHED_POLICY_EDF ||
task->policy == DTESN_SCHED_POLICY_RM) {
rq->rt_tasks++;
}
switch (task->workload_type) {
case DTESN_WORKLOAD_MEMBRANE:
rq->load_weight += 100;
break;
case DTESN_WORKLOAD_ESN:
rq->load_weight += 80;
break;
case DTESN_WORKLOAD_BSERIES:
rq->load_weight += 60;
break;
case DTESN_WORKLOAD_MEMORY:
rq->load_weight += 40;
break;
default:
rq->load_weight += 20;
break;
}
task->state = DTESN_TASK_STATE_READY;
task->current_cpu = rq->cpu_id;
pthread_spin_unlock(&rq->rq_lock);
}
static int dtesn_policy_select_task(dtesn_runqueue_t *rq, dtesn_task_t **selected_task)
{
return dtesn_sched_policy_select(rq, selected_task);
}
static dtesn_task_t *dtesn_runqueue_pick_next(dtesn_runqueue_t *rq)
{
dtesn_task_t *selected_task = NULL;
if (!rq || rq->nr_running == 0) {
return NULL;
}
pthread_spin_lock(&rq->rq_lock);
if (dtesn_policy_select_task(rq, &selected_task) != 0) {
pthread_spin_unlock(&rq->rq_lock);
return NULL;
}
if (selected_task) {
if (selected_task->prev) {
selected_task->prev->next = selected_task->next;
} else {
uint32_t priority = selected_task->effective_priority;
if (priority >= DTESN_SCHED_MAX_PRIORITIES) {
priority = DTESN_SCHED_MAX_PRIORITIES - 1;
}
rq->ready_queue[priority] = selected_task->next;
if (rq->ready_queue[priority] == NULL) {
rq->queue_bitmap &= ~(1U << priority);
}
}
if (selected_task->next) {
selected_task->next->prev = selected_task->prev;
}
selected_task->next = NULL;
selected_task->prev = NULL;
rq->nr_running--;
if (selected_task->policy == DTESN_SCHED_POLICY_REALTIME ||
selected_task->policy == DTESN_SCHED_POLICY_EDF ||
selected_task->policy == DTESN_SCHED_POLICY_RM) {
rq->rt_tasks--;
}
selected_task->state = DTESN_TASK_STATE_RUNNING;
}
pthread_spin_unlock(&rq->rq_lock);
return selected_task;
}
static void dtesn_update_performance_metrics(dtesn_runqueue_t *rq, uint64_t latency_ns)
{
if (!rq) {
return;
}
pthread_spin_lock(&rq->rq_lock);
rq->perf.context_switches++;
if (latency_ns > rq->perf.max_latency_ns) {
rq->perf.max_latency_ns = latency_ns;
}
if (latency_ns < rq->perf.min_latency_ns) {
rq->perf.min_latency_ns = latency_ns;
}
if (rq->perf.context_switches == 1) {
rq->perf.scheduling_latency_ns = latency_ns;
} else {
rq->perf.scheduling_latency_ns =
(rq->perf.scheduling_latency_ns * 7 + latency_ns) / 8;
}
uint64_t jitter = (latency_ns > rq->perf.scheduling_latency_ns) ?
(latency_ns - rq->perf.scheduling_latency_ns) :
(rq->perf.scheduling_latency_ns - latency_ns);
rq->perf.jitter_ns = (rq->perf.jitter_ns * 7 + jitter) / 8;
if (latency_ns > DTESN_SCHED_LATENCY_THRESHOLD_US * 1000) {
rq->perf.missed_deadlines++;
}
pthread_spin_unlock(&rq->rq_lock);
pthread_mutex_lock(&g_scheduler.scheduler_lock);
g_scheduler.global_perf.context_switches++;
if (latency_ns > g_scheduler.global_perf.max_latency_ns) {
g_scheduler.global_perf.max_latency_ns = latency_ns;
}
if (latency_ns < g_scheduler.global_perf.min_latency_ns) {
g_scheduler.global_perf.min_latency_ns = latency_ns;
}
if (g_scheduler.global_perf.context_switches == 1) {
g_scheduler.global_perf.scheduling_latency_ns = latency_ns;
} else {
g_scheduler.global_perf.scheduling_latency_ns =
(g_scheduler.global_perf.scheduling_latency_ns * 15 + latency_ns) / 16;
}
pthread_mutex_unlock(&g_scheduler.scheduler_lock);
}
dtesn_task_t *dtesn_task_schedule(uint32_t cpu_id)
{
uint64_t start_time, end_time, latency_ns;
dtesn_task_t *next_task = NULL;
if (cpu_id >= g_scheduler.nr_cpus || !g_scheduler.initialized) {
return NULL;
}
start_time = dtesn_get_time_ns();
dtesn_runqueue_t *rq = &g_scheduler.runqueues[cpu_id];
next_task = dtesn_runqueue_pick_next(rq);
if (next_task) {
next_task->last_ran_ns = start_time;
rq->current_task = next_task;
rq->nr_switches++;
next_task->context_switches++;
if (next_task->state == DTESN_TASK_STATE_READY) {
next_task->voluntary_switches++;
} else {
next_task->involuntary_switches++;
}
}
end_time = dtesn_get_time_ns();
latency_ns = end_time - start_time;
dtesn_update_performance_metrics(rq, latency_ns);
return next_task;
}
bool dtesn_deadline_check(dtesn_task_t *task, uint64_t current_time_ns)
{
if (!task) {
return false;
}
if (task->policy != DTESN_SCHED_POLICY_EDF &&
task->policy != DTESN_SCHED_POLICY_RM &&
task->policy != DTESN_SCHED_POLICY_REALTIME) {
return true;
}
pthread_mutex_lock(&task->task_lock);
bool deadline_met = true;
if (task->deadline_ns != 0 && current_time_ns > task->deadline_ns) {
deadline_met = false;
dtesn_runqueue_t *rq = &g_scheduler.runqueues[task->current_cpu];
pthread_spin_lock(&rq->rq_lock);
rq->perf.missed_deadlines++;
pthread_spin_unlock(&rq->rq_lock);
pthread_mutex_lock(&g_scheduler.scheduler_lock);
g_scheduler.global_perf.missed_deadlines++;
pthread_mutex_unlock(&g_scheduler.scheduler_lock);
}
if (task->policy == DTESN_SCHED_POLICY_RM && task->budget_ns > 0) {
if (task->runtime_ns > task->budget_ns) {
deadline_met = false;
}
}
pthread_mutex_unlock(&task->task_lock);
return deadline_met;
}
int dtesn_load_balance(uint32_t trigger_cpu)
{
int tasks_migrated = 0;
uint64_t current_time = dtesn_get_time_ns();
if (trigger_cpu >= g_scheduler.nr_cpus ||
!g_scheduler.initialized ||
!g_scheduler.load_balancing_enabled) {
return -EINVAL;
}
dtesn_runqueue_t *trigger_rq = &g_scheduler.runqueues[trigger_cpu];
if (current_time - trigger_rq->last_balance_ns < g_scheduler.balance_interval_ns) {
return 0;
}
trigger_rq->last_balance_ns = current_time;
uint32_t max_load_cpu = trigger_cpu;
uint64_t max_load = trigger_rq->load_weight;
for (uint32_t i = 0; i < g_scheduler.nr_cpus; i++) {
if (i == trigger_cpu) continue;
dtesn_runqueue_t *rq = &g_scheduler.runqueues[i];
if (rq->load_weight > max_load) {
max_load = rq->load_weight;
max_load_cpu = i;
}
}
uint32_t min_load_cpu = trigger_cpu;
uint64_t min_load = trigger_rq->load_weight;
for (uint32_t i = 0; i < g_scheduler.nr_cpus; i++) {
dtesn_runqueue_t *rq = &g_scheduler.runqueues[i];
if (rq->load_weight < min_load) {
min_load = rq->load_weight;
min_load_cpu = i;
}
}
if (max_load - min_load < g_scheduler.migration_threshold) {
return 0;
}
dtesn_runqueue_t *src_rq = &g_scheduler.runqueues[max_load_cpu];
dtesn_runqueue_t *dst_rq = &g_scheduler.runqueues[min_load_cpu];
pthread_spin_lock(&src_rq->rq_lock);
for (int priority = DTESN_SCHED_MAX_PRIORITIES - 1; priority >= 0; priority--) {
dtesn_task_t *task = src_rq->ready_queue[priority];
while (task && tasks_migrated < 2) {
dtesn_task_t *next_task = task->next;
if (task->state == DTESN_TASK_STATE_READY &&
(task->cpu_affinity_mask & (1U << min_load_cpu)) &&
task->workload_type != DTESN_WORKLOAD_MEMBRANE) {
if (task->prev) {
task->prev->next = task->next;
} else {
src_rq->ready_queue[priority] = task->next;
if (src_rq->ready_queue[priority] == NULL) {
src_rq->queue_bitmap &= ~(1U << priority);
}
}
if (task->next) {
task->next->prev = task->prev;
}
src_rq->nr_running--;
src_rq->load_weight -= 20;
task->next = NULL;
task->prev = NULL;
pthread_spin_unlock(&src_rq->rq_lock);
dtesn_runqueue_add_task(dst_rq, task);
tasks_migrated++;
pthread_spin_lock(&src_rq->rq_lock);
}
task = next_task;
}
if (tasks_migrated >= 2) {
break;
}
}
pthread_spin_unlock(&src_rq->rq_lock);
return tasks_migrated;
}
int dtesn_priority_inherit(dtesn_task_t *blocked_task, dtesn_task_t *blocking_task)
{
if (!blocked_task || !blocking_task ||
!g_scheduler.priority_inheritance_enabled) {
return -EINVAL;
}
pthread_mutex_lock(&blocking_task->task_lock);
if (blocked_task->effective_priority >= blocking_task->effective_priority) {
pthread_mutex_unlock(&blocking_task->task_lock);
return 0;
}
uint32_t boost_count = 0;
dtesn_task_t *current = blocking_task;
while (current && current->pi_state != DTESN_PI_STATE_NORMAL) {
boost_count++;
if (boost_count >= g_scheduler.pi_boost_limit) {
pthread_mutex_unlock(&blocking_task->task_lock);
return -EDEADLK;
}
current = current->pi_blocked_on;
}
if (blocking_task->pi_state == DTESN_PI_STATE_NORMAL) {
blocking_task->original_priority = blocking_task->effective_priority;
blocking_task->pi_state = DTESN_PI_STATE_INHERITED;
}
blocking_task->effective_priority = blocked_task->effective_priority;
blocked_task->pi_blocked_on = blocking_task;
pthread_mutex_unlock(&blocking_task->task_lock);
return 0;
}
dtesn_task_t *dtesn_task_create(uint32_t pid, dtesn_workload_type_t workload_type,
dtesn_sched_policy_t policy, uint32_t priority)
{
if (!g_scheduler.initialized || priority >= DTESN_SCHED_MAX_PRIORITIES) {
return NULL;
}
pthread_mutex_lock(&g_scheduler.scheduler_lock);
if (g_scheduler.nr_tasks >= DTESN_SCHED_MAX_TASKS) {
pthread_mutex_unlock(&g_scheduler.scheduler_lock);
return NULL;
}
uint32_t task_id = g_scheduler.next_task_id++;
pthread_mutex_unlock(&g_scheduler.scheduler_lock);
dtesn_task_t *task = malloc(sizeof(dtesn_task_t));
if (!task) {
return NULL;
}
memset(task, 0, sizeof(dtesn_task_t));
task->task_id = task_id;
task->pid = pid;
task->tgid = pid;
task->policy = policy;
task->workload_type = workload_type;
task->state = DTESN_TASK_STATE_READY;
task->priority = priority;
task->effective_priority = priority;
task->nice = 0;
task->deadline_ns = 0;
task->period_ns = 0;
task->runtime_ns = 0;
task->budget_ns = 0;
task->wcet_ns = 0;
task->pi_state = DTESN_PI_STATE_NORMAL;
task->original_priority = priority;
task->pi_blocked_on = NULL;
task->cpu_affinity_mask = (1U << g_scheduler.nr_cpus) - 1;
task->current_cpu = 0;
task->preferred_cpu = 0;
task->last_ran_ns = 0;
switch (workload_type) {
case DTESN_WORKLOAD_MEMBRANE:
task->membrane_level = 1;
task->requires_fpu = true;
task->memory_zone = 1;
break;
case DTESN_WORKLOAD_ESN:
task->esn_reservoir_id = 0;
task->requires_fpu = true;
task->requires_vector = true;
task->memory_zone = 2;
break;
case DTESN_WORKLOAD_BSERIES:
task->bseries_order = 4;
task->requires_fpu = true;
task->memory_zone = 3;
break;
default:
task->memory_zone = 0;
break;
}
task->oeis_compliant = dtesn_sched_validate_oeis(task, task->memory_zone);
if (pthread_mutex_init(&task->task_lock, NULL) != 0) {
free(task);
return NULL;
}
pthread_mutex_lock(&g_scheduler.scheduler_lock);
for (int i = 0; i < DTESN_SCHED_MAX_TASKS; i++) {
if (g_scheduler.task_table[i] == NULL) {
g_scheduler.task_table[i] = task;
g_scheduler.nr_tasks++;
break;
}
}
pthread_mutex_unlock(&g_scheduler.scheduler_lock);
return task;
}
int dtesn_task_destroy(dtesn_task_t *task)
{
if (!task) {
return -EINVAL;
}
pthread_mutex_lock(&g_scheduler.scheduler_lock);
for (int i = 0; i < DTESN_SCHED_MAX_TASKS; i++) {
if (g_scheduler.task_table[i] == task) {
g_scheduler.task_table[i] = NULL;
g_scheduler.nr_tasks--;
break;
}
}
pthread_mutex_unlock(&g_scheduler.scheduler_lock);
pthread_mutex_destroy(&task->task_lock);
free(task);
return 0;
}
dtesn_sched_perf_t *dtesn_sched_get_performance(int cpu_id)
{
if (!g_scheduler.initialized) {
return NULL;
}
if (cpu_id == -1) {
return &g_scheduler.global_perf;
}
if (cpu_id < 0 || cpu_id >= (int)g_scheduler.nr_cpus) {
return NULL;
}
return &g_scheduler.runqueues[cpu_id].perf;
}
int dtesn_sched_set_policy(dtesn_task_t *task, dtesn_sched_policy_t policy, uint32_t priority)
{
if (!task || priority >= DTESN_SCHED_MAX_PRIORITIES) {
return -EINVAL;
}
pthread_mutex_lock(&task->task_lock);
task->policy = policy;
task->priority = priority;
task->effective_priority = priority;
pthread_mutex_unlock(&task->task_lock);
return 0;
}
bool dtesn_sched_validate_oeis(void *structure_data, uint32_t depth)
{
if (!g_scheduler.oeis_validation_enabled ||
depth > DTESN_SCHED_A000081_MAX_DEPTH) {
return true;
}
if (depth < DTESN_SCHED_A000081_MAX_DEPTH) {
uint32_t expected_count = g_scheduler.oeis_sequence[depth];
if (expected_count > 0) {
return true;
}
}
return true;
}
int dtesn_sched_cleanup(void)
{
if (!g_scheduler.initialized) {
return -EINVAL;
}
for (int i = 0; i < DTESN_SCHED_MAX_TASKS; i++) {
if (g_scheduler.task_table[i]) {
dtesn_task_destroy(g_scheduler.task_table[i]);
}
}
for (uint32_t i = 0; i < g_scheduler.nr_cpus; i++) {
pthread_spin_destroy(&g_scheduler.runqueues[i].rq_lock);
}
pthread_mutex_destroy(&g_scheduler.scheduler_lock);
memset(&g_scheduler, 0, sizeof(dtesn_scheduler_t));
return 0;
}
int dtesn_sched_add_task(dtesn_task_t *task, int cpu_id)
{
if (!task || !g_scheduler.initialized) {
return -EINVAL;
}
if (cpu_id < 0) {
cpu_id = 0;
}
if (cpu_id >= (int)g_scheduler.nr_cpus) {
return -EINVAL;
}
dtesn_runqueue_t *rq = &g_scheduler.runqueues[cpu_id];
dtesn_runqueue_add_task(rq, task);
return 0;
}