#ifndef _MACH_THREAD_INFO_H_
#define _MACH_THREAD_INFO_H_
#include <mach/boolean.h>
#include <mach/policy.h>
#include <mach/time_value.h>
typedef integer_t *thread_info_t;
#define THREAD_INFO_MAX (1024)
typedef integer_t thread_info_data_t[THREAD_INFO_MAX];
#define THREAD_BASIC_INFO 1
struct thread_basic_info {
rpc_time_value_t user_time;
rpc_time_value_t system_time;
integer_t cpu_usage;
integer_t base_priority;
integer_t cur_priority;
integer_t run_state;
integer_t flags;
integer_t suspend_count;
integer_t sleep_time;
rpc_time_value_t creation_time;
time_value64_t user_time64;
time_value64_t system_time64;
time_value64_t creation_time64;
};
typedef struct thread_basic_info thread_basic_info_data_t;
typedef struct thread_basic_info *thread_basic_info_t;
#define THREAD_BASIC_INFO_COUNT \
(sizeof(thread_basic_info_data_t) / sizeof(natural_t))
#define TH_USAGE_SCALE 1000
#define TH_STATE_RUNNING 1
#define TH_STATE_STOPPED 2
#define TH_STATE_WAITING 3
#define TH_STATE_UNINTERRUPTIBLE 4
#define TH_STATE_HALTED 5
#define TH_FLAGS_SWAPPED 0x1
#define TH_FLAGS_IDLE 0x2
#define THREAD_SCHED_INFO 2
struct thread_sched_info {
integer_t policy;
integer_t data;
integer_t base_priority;
integer_t max_priority;
integer_t cur_priority;
integer_t depressed;
integer_t depress_priority;
integer_t last_processor;
};
typedef struct thread_sched_info thread_sched_info_data_t;
typedef struct thread_sched_info *thread_sched_info_t;
#define THREAD_SCHED_INFO_COUNT \
(sizeof(thread_sched_info_data_t) / sizeof(natural_t))
#endif