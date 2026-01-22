#ifndef DTESN_SCHEDULER_H
#define DTESN_SCHEDULER_H
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <pthread.h>
#include <time.h>
#ifdef __cplusplus
extern "C" {
#endif
#define DTESN_SCHED_MAX_TASKS 4096
#define DTESN_SCHED_MAX_CPUS 64
#define DTESN_SCHED_MAX_PRIORITIES 256
#define DTESN_SCHED_MAX_RUNQUEUES 64
#define DTESN_SCHED_QUANTUM_NS 1000000
#define DTESN_SCHED_CONTEXT_SWITCH_THRESHOLD_US 5
#define DTESN_SCHED_LATENCY_THRESHOLD_US 10
#define DTESN_SCHED_JITTER_THRESHOLD_US 1
#define DTESN_SCHED_CPU_OVERHEAD_THRESHOLD_PCT 5
#define DTESN_SCHED_A000081_MAX_DEPTH 10
#define DTESN_SCHED_A000081_SEQUENCE \
{ 1, 1, 2, 4, 9, 20, 48, 115, 286, 719 }
typedef enum {
DTESN_SCHED_POLICY_REALTIME = 0,
DTESN_SCHED_POLICY_EDF = 1,
DTESN_SCHED_POLICY_RM = 2,
DTESN_SCHED_POLICY_CFS = 3,
DTESN_SCHED_POLICY_FIFO = 4,
DTESN_SCHED_POLICY_RR = 5
} dtesn_sched_policy_t;
typedef enum {
DTESN_WORKLOAD_MEMBRANE = 0,
DTESN_WORKLOAD_ESN = 1,
DTESN_WORKLOAD_BSERIES = 2,
DTESN_WORKLOAD_MEMORY = 3,
DTESN_WORKLOAD_IO = 4,
DTESN_WORKLOAD_GENERAL = 5
} dtesn_workload_type_t;
typedef enum {
DTESN_TASK_STATE_READY = 0,
DTESN_TASK_STATE_RUNNING = 1,
DTESN_TASK_STATE_WAITING = 2,
DTESN_TASK_STATE_BLOCKED = 3,
DTESN_TASK_STATE_TERMINATED = 4
} dtesn_task_state_t;
typedef enum {
DTESN_PI_STATE_NORMAL = 0,
DTESN_PI_STATE_INHERITED = 1,
DTESN_PI_STATE_BOOSTED = 2
} dtesn_pi_state_t;
typedef struct dtesn_sched_perf {
uint64_t context_switches;
uint64_t scheduling_latency_ns;
uint64_t jitter_ns;
uint64_t cpu_overhead_ns;
uint64_t missed_deadlines;
uint64_t last_context_switch_ns;
uint64_t max_latency_ns;
uint64_t min_latency_ns;
double cpu_utilization;
} dtesn_sched_perf_t;
typedef struct dtesn_task {
uint32_t task_id;
uint32_t pid;
uint32_t tgid;
dtesn_sched_policy_t policy;
dtesn_workload_type_t workload_type;
dtesn_task_state_t state;
uint32_t priority;
int32_t nice;
uint64_t deadline_ns;
uint64_t period_ns;
uint64_t runtime_ns;
uint64_t budget_ns;
uint64_t wcet_ns;
dtesn_pi_state_t pi_state;
uint32_t original_priority;
uint32_t effective_priority;
struct dtesn_task *pi_blocked_on;
uint32_t cpu_affinity_mask;
uint32_t current_cpu;
uint32_t preferred_cpu;
uint64_t last_ran_ns;
uint64_t total_runtime_ns;
uint64_t context_switches;
uint64_t voluntary_switches;
uint64_t involuntary_switches;
uint32_t membrane_level;
uint32_t esn_reservoir_id;
uint32_t bseries_order;
bool requires_fpu;
bool requires_vector;
uint32_t memory_zone;
bool oeis_compliant;
pthread_mutex_t task_lock;
struct dtesn_task *next;
struct dtesn_task *prev;
} dtesn_task_t;
typedef struct dtesn_runqueue {
uint32_t cpu_id;
uint32_t nr_running;
uint32_t nr_switches;
dtesn_task_t *ready_queue[DTESN_SCHED_MAX_PRIORITIES];
uint32_t queue_bitmap;
dtesn_task_t *current_task;
dtesn_task_t *idle_task;
uint64_t load_weight;
uint64_t last_balance_ns;
uint64_t next_deadline_ns;
uint32_t rt_tasks;
dtesn_sched_perf_t perf;
pthread_spinlock_t rq_lock;
} dtesn_runqueue_t;
typedef struct dtesn_scheduler {
bool initialized;
uint32_t nr_cpus;
uint32_t nr_tasks;
dtesn_runqueue_t runqueues[DTESN_SCHED_MAX_CPUS];
dtesn_task_t *task_table[DTESN_SCHED_MAX_TASKS];
uint32_t next_task_id;
bool load_balancing_enabled;
uint64_t balance_interval_ns;
uint32_t migration_threshold;
bool priority_inheritance_enabled;
uint32_t pi_boost_limit;
dtesn_sched_perf_t global_perf;
uint64_t scheduler_start_time_ns;
uint32_t oeis_sequence[DTESN_SCHED_A000081_MAX_DEPTH];
bool oeis_validation_enabled;
pthread_mutex_t scheduler_lock;
} dtesn_scheduler_t;
int dtesn_sched_init(uint32_t nr_cpus);
dtesn_task_t *dtesn_task_schedule(uint32_t cpu_id);
bool dtesn_deadline_check(dtesn_task_t *task, uint64_t current_time_ns);
int dtesn_load_balance(uint32_t trigger_cpu);
int dtesn_priority_inherit(dtesn_task_t *blocked_task, dtesn_task_t *blocking_task);
dtesn_task_t *dtesn_task_create(uint32_t pid, dtesn_workload_type_t workload_type,
dtesn_sched_policy_t policy, uint32_t priority);
int dtesn_task_destroy(dtesn_task_t *task);
dtesn_sched_perf_t *dtesn_sched_get_performance(int cpu_id);
int dtesn_sched_set_policy(dtesn_task_t *task, dtesn_sched_policy_t policy, uint32_t priority);
bool dtesn_sched_validate_oeis(void *structure_data, uint32_t depth);
int dtesn_sched_cleanup(void);
int dtesn_sched_policy_select(dtesn_runqueue_t *rq, dtesn_task_t **selected_task);
int dtesn_sched_add_task(dtesn_task_t *task, int cpu_id);
#ifdef __cplusplus
}
#endif
#endif