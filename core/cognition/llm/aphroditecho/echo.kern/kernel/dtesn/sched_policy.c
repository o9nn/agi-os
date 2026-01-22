#include <dtesn/scheduler.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <sys/time.h>
#include <errno.h>
static int dtesn_policy_realtime_schedule(dtesn_runqueue_t *rq, dtesn_task_t **selected);
static int dtesn_policy_edf_schedule(dtesn_runqueue_t *rq, dtesn_task_t **selected);
static int dtesn_policy_rm_schedule(dtesn_runqueue_t *rq, dtesn_task_t **selected);
static int dtesn_policy_cfs_schedule(dtesn_runqueue_t *rq, dtesn_task_t **selected);
static uint64_t dtesn_get_time_ns(void);
static uint32_t dtesn_calculate_workload_priority(dtesn_workload_type_t workload_type);
static uint64_t dtesn_get_time_ns(void)
{
struct timespec ts;
if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
return 0;
}
return (uint64_t)ts.tv_sec * 1000000000ULL + ts.tv_nsec;
}
static uint32_t dtesn_calculate_workload_priority(dtesn_workload_type_t workload_type)
{
switch (workload_type) {
case DTESN_WORKLOAD_MEMBRANE:
return 0;
case DTESN_WORKLOAD_ESN:
return 10;
case DTESN_WORKLOAD_BSERIES:
return 20;
case DTESN_WORKLOAD_MEMORY:
return 30;
case DTESN_WORKLOAD_IO:
return 40;
case DTESN_WORKLOAD_GENERAL:
default:
return 50;
}
}
static int dtesn_policy_realtime_schedule(dtesn_runqueue_t *rq, dtesn_task_t **selected)
{
if (!rq || !selected) {
return -EINVAL;
}
*selected = NULL;
dtesn_task_t *best_task = NULL;
uint32_t best_priority = UINT32_MAX;
uint64_t current_time = dtesn_get_time_ns();
for (int priority = 0; priority < DTESN_SCHED_MAX_PRIORITIES; priority++) {
if (!(rq->queue_bitmap & (1U << priority))) {
continue;
}
dtesn_task_t *task = rq->ready_queue[priority];
while (task) {
if (task->policy == DTESN_SCHED_POLICY_REALTIME) {
uint32_t workload_boost = dtesn_calculate_workload_priority(task->workload_type);
uint32_t effective_prio = task->effective_priority + workload_boost;
if (task->workload_type == DTESN_WORKLOAD_MEMBRANE) {
if (task->membrane_level <= 3) {
effective_prio = (effective_prio > 5) ? effective_prio - 5 : 0;
}
}
if (task->deadline_ns > 0 && task->deadline_ns < current_time + 1000000) {
effective_prio = (effective_prio > 10) ? effective_prio - 10 : 0;
}
if (task->workload_type == DTESN_WORKLOAD_ESN && task->requires_vector) {
effective_prio = (effective_prio > 3) ? effective_prio - 3 : 0;
}
if (effective_prio < best_priority) {
best_priority = effective_prio;
best_task = task;
}
}
task = task->next;
}
}
if (best_task) {
*selected = best_task;
return 0;
}
return -ENOENT;
}
static int dtesn_policy_edf_schedule(dtesn_runqueue_t *rq, dtesn_task_t **selected)
{
if (!rq || !selected) {
return -EINVAL;
}
*selected = NULL;
dtesn_task_t *earliest_task = NULL;
uint64_t earliest_deadline = UINT64_MAX;
uint64_t current_time = dtesn_get_time_ns();
for (int priority = 0; priority < DTESN_SCHED_MAX_PRIORITIES; priority++) {
if (!(rq->queue_bitmap & (1U << priority))) {
continue;
}
dtesn_task_t *task = rq->ready_queue[priority];
while (task) {
if (task->policy == DTESN_SCHED_POLICY_EDF && task->deadline_ns > 0) {
if (task->deadline_ns < current_time) {
earliest_deadline = 0;
earliest_task = task;
break;
}
if (task->deadline_ns < earliest_deadline) {
earliest_deadline = task->deadline_ns;
earliest_task = task;
}
}
task = task->next;
}
if (earliest_deadline == 0) {
break;
}
}
if (earliest_task) {
*selected = earliest_task;
return 0;
}
return -ENOENT;
}
static int dtesn_policy_rm_schedule(dtesn_runqueue_t *rq, dtesn_task_t **selected)
{
if (!rq || !selected) {
return -EINVAL;
}
*selected = NULL;
dtesn_task_t *shortest_period_task = NULL;
uint64_t shortest_period = UINT64_MAX;
uint64_t current_time = dtesn_get_time_ns();
for (int priority = 0; priority < DTESN_SCHED_MAX_PRIORITIES; priority++) {
if (!(rq->queue_bitmap & (1U << priority))) {
continue;
}
dtesn_task_t *task = rq->ready_queue[priority];
while (task) {
if (task->policy == DTESN_SCHED_POLICY_RM && task->period_ns > 0) {
if (task->budget_ns > 0 && task->runtime_ns >= task->budget_ns) {
task = task->next;
continue;
}
uint64_t period_start = (current_time / task->period_ns) * task->period_ns;
uint64_t time_in_period = current_time - period_start;
if (time_in_period < task->period_ns) {
if (task->period_ns < shortest_period) {
shortest_period = task->period_ns;
shortest_period_task = task;
}
}
}
task = task->next;
}
}
if (shortest_period_task) {
*selected = shortest_period_task;
return 0;
}
return -ENOENT;
}
static int dtesn_policy_cfs_schedule(dtesn_runqueue_t *rq, dtesn_task_t **selected)
{
if (!rq || !selected) {
return -EINVAL;
}
*selected = NULL;
dtesn_task_t *fairest_task = NULL;
uint64_t min_runtime = UINT64_MAX;
for (int priority = 0; priority < DTESN_SCHED_MAX_PRIORITIES; priority++) {
if (!(rq->queue_bitmap & (1U << priority))) {
continue;
}
dtesn_task_t *task = rq->ready_queue[priority];
while (task) {
if (task->policy == DTESN_SCHED_POLICY_CFS) {
uint64_t weighted_runtime = task->total_runtime_ns;
if (task->nice > 0) {
weighted_runtime = weighted_runtime * (100 + task->nice * 5) / 100;
} else if (task->nice < 0) {
weighted_runtime = weighted_runtime * 100 / (100 + (-task->nice) * 5);
}
if (weighted_runtime < min_runtime) {
min_runtime = weighted_runtime;
fairest_task = task;
}
}
task = task->next;
}
}
if (fairest_task) {
*selected = fairest_task;
return 0;
}
return -ENOENT;
}
int dtesn_sched_policy_select(dtesn_runqueue_t *rq, dtesn_task_t **selected_task)
{
if (!rq || !selected_task) {
return -EINVAL;
}
*selected_task = NULL;
dtesn_task_t *candidate_task = NULL;
int result;
result = dtesn_policy_realtime_schedule(rq, &candidate_task);
if (result == 0 && candidate_task) {
*selected_task = candidate_task;
return 0;
}
result = dtesn_policy_edf_schedule(rq, &candidate_task);
if (result == 0 && candidate_task) {
*selected_task = candidate_task;
return 0;
}
result = dtesn_policy_rm_schedule(rq, &candidate_task);
if (result == 0 && candidate_task) {
*selected_task = candidate_task;
return 0;
}
result = dtesn_policy_cfs_schedule(rq, &candidate_task);
if (result == 0 && candidate_task) {
*selected_task = candidate_task;
return 0;
}
for (int priority = 0; priority < DTESN_SCHED_MAX_PRIORITIES; priority++) {
if (!(rq->queue_bitmap & (1U << priority))) {
continue;
}
dtesn_task_t *task = rq->ready_queue[priority];
if (task) {
*selected_task = task;
return 0;
}
}
return -ENOENT;
}
int dtesn_sched_policy_update_runtime(dtesn_task_t *task, uint64_t runtime_ns)
{
if (!task) {
return -EINVAL;
}
pthread_mutex_lock(&task->task_lock);
task->total_runtime_ns += runtime_ns;
if (task->policy == DTESN_SCHED_POLICY_RM && task->period_ns > 0) {
uint64_t current_time = dtesn_get_time_ns();
uint64_t period_start = (current_time / task->period_ns) * task->period_ns;
if (task->last_ran_ns < period_start) {
task->runtime_ns = 0;
}
task->runtime_ns += runtime_ns;
}
pthread_mutex_unlock(&task->task_lock);
return 0;
}
bool dtesn_sched_policy_check_preemption(dtesn_task_t *current_task, dtesn_task_t *new_task)
{
if (!current_task || !new_task) {
return false;
}
uint64_t current_time = dtesn_get_time_ns();
if (new_task->policy == DTESN_SCHED_POLICY_REALTIME ||
new_task->policy == DTESN_SCHED_POLICY_EDF ||
new_task->policy == DTESN_SCHED_POLICY_RM) {
if (current_task->policy != DTESN_SCHED_POLICY_REALTIME &&
current_task->policy != DTESN_SCHED_POLICY_EDF &&
current_task->policy != DTESN_SCHED_POLICY_RM) {
return true;
}
if (new_task->policy == DTESN_SCHED_POLICY_EDF &&
current_task->policy == DTESN_SCHED_POLICY_EDF) {
return (new_task->deadline_ns < current_task->deadline_ns);
}
if (new_task->policy == DTESN_SCHED_POLICY_RM &&
current_task->policy == DTESN_SCHED_POLICY_RM) {
return (new_task->period_ns < current_task->period_ns);
}
if (new_task->policy == DTESN_SCHED_POLICY_REALTIME) {
uint32_t new_workload_boost = dtesn_calculate_workload_priority(new_task->workload_type);
uint32_t current_workload_boost = dtesn_calculate_workload_priority(current_task->workload_type);
uint32_t new_effective = new_task->effective_priority + new_workload_boost;
uint32_t current_effective = current_task->effective_priority + current_workload_boost;
return (new_effective < current_effective);
}
return (new_task->effective_priority < current_task->effective_priority);
}
if (current_task->policy == DTESN_SCHED_POLICY_CFS ||
current_task->policy == DTESN_SCHED_POLICY_RR) {
uint64_t time_slice = current_time - current_task->last_ran_ns;
if (time_slice >= DTESN_SCHED_QUANTUM_NS) {
return true;
}
}
return false;
}
int dtesn_sched_policy_set_deadline(dtesn_task_t *task, uint64_t deadline_ns)
{
if (!task) {
return -EINVAL;
}
pthread_mutex_lock(&task->task_lock);
task->deadline_ns = deadline_ns;
if (deadline_ns > 0 && task->policy != DTESN_SCHED_POLICY_EDF) {
task->policy = DTESN_SCHED_POLICY_EDF;
}
pthread_mutex_unlock(&task->task_lock);
return 0;
}
int dtesn_sched_policy_set_period(dtesn_task_t *task, uint64_t period_ns, uint64_t budget_ns)
{
if (!task || period_ns == 0 || budget_ns > period_ns) {
return -EINVAL;
}
pthread_mutex_lock(&task->task_lock);
task->period_ns = period_ns;
task->budget_ns = budget_ns;
task->runtime_ns = 0;
uint64_t current_time = dtesn_get_time_ns();
uint64_t next_period = ((current_time / period_ns) + 1) * period_ns;
task->deadline_ns = next_period;
task->policy = DTESN_SCHED_POLICY_RM;
pthread_mutex_unlock(&task->task_lock);
return 0;
}
uint64_t dtesn_sched_policy_get_time_slice(dtesn_task_t *task)
{
if (!task) {
return DTESN_SCHED_QUANTUM_NS;
}
switch (task->policy) {
case DTESN_SCHED_POLICY_REALTIME:
if (task->workload_type == DTESN_WORKLOAD_MEMBRANE) {
return DTESN_SCHED_QUANTUM_NS * 2;
} else if (task->workload_type == DTESN_WORKLOAD_ESN) {
return DTESN_SCHED_QUANTUM_NS * 1.5;
}
return DTESN_SCHED_QUANTUM_NS;
case DTESN_SCHED_POLICY_EDF:
if (task->deadline_ns > 0) {
uint64_t current_time = dtesn_get_time_ns();
uint64_t time_to_deadline = (task->deadline_ns > current_time) ?
(task->deadline_ns - current_time) : 0;
if (time_to_deadline < DTESN_SCHED_QUANTUM_NS) {
return time_to_deadline;
}
}
return DTESN_SCHED_QUANTUM_NS;
case DTESN_SCHED_POLICY_RM:
if (task->budget_ns > 0 && task->runtime_ns < task->budget_ns) {
uint64_t remaining_budget = task->budget_ns - task->runtime_ns;
return (remaining_budget < DTESN_SCHED_QUANTUM_NS) ?
remaining_budget : DTESN_SCHED_QUANTUM_NS;
}
return DTESN_SCHED_QUANTUM_NS;
case DTESN_SCHED_POLICY_CFS:
if (task->nice > 0) {
return DTESN_SCHED_QUANTUM_NS / (1 + task->nice / 5);
} else if (task->nice < 0) {
return DTESN_SCHED_QUANTUM_NS * (1 + (-task->nice) / 5);
}
return DTESN_SCHED_QUANTUM_NS;
default:
return DTESN_SCHED_QUANTUM_NS;
}
}