#ifndef _MACH_TASK_INFO_H_
#define _MACH_TASK_INFO_H_
#include <mach/machine/vm_types.h>
#include <mach/time_value.h>
typedef integer_t *task_info_t;
#define TASK_INFO_MAX (1024)
typedef integer_t task_info_data_t[TASK_INFO_MAX];
#define TASK_BASIC_INFO 1
struct task_basic_info {
integer_t suspend_count;
integer_t base_priority;
rpc_vm_size_t virtual_size;
rpc_vm_size_t resident_size;
rpc_time_value_t user_time;
rpc_time_value_t system_time;
rpc_time_value_t creation_time;
time_value64_t user_time64;
time_value64_t system_time64;
time_value64_t creation_time64;
};
typedef struct task_basic_info task_basic_info_data_t;
typedef struct task_basic_info *task_basic_info_t;
#define TASK_BASIC_INFO_COUNT \
(sizeof(task_basic_info_data_t) / sizeof(integer_t))
#define TASK_EVENTS_INFO 2
struct task_events_info {
rpc_long_natural_t faults;
rpc_long_natural_t zero_fills;
rpc_long_natural_t reactivations;
rpc_long_natural_t pageins;
rpc_long_natural_t cow_faults;
rpc_long_natural_t messages_sent;
rpc_long_natural_t messages_received;
};
typedef struct task_events_info task_events_info_data_t;
typedef struct task_events_info *task_events_info_t;
#define TASK_EVENTS_INFO_COUNT \
(sizeof(task_events_info_data_t) / sizeof(integer_t))
#define TASK_THREAD_TIMES_INFO 3
struct task_thread_times_info {
rpc_time_value_t user_time;
rpc_time_value_t system_time;
time_value64_t user_time64;
time_value64_t system_time64;
};
typedef struct task_thread_times_info task_thread_times_info_data_t;
typedef struct task_thread_times_info *task_thread_times_info_t;
#define TASK_THREAD_TIMES_INFO_COUNT \
(sizeof(task_thread_times_info_data_t) / sizeof(integer_t))
#define TASK_RAS_CONTROL_PURGE_ALL 0
#define TASK_RAS_CONTROL_PURGE_ONE 1
#define TASK_RAS_CONTROL_PURGE_ALL_AND_INSTALL_ONE 2
#define TASK_RAS_CONTROL_INSTALL_ONE 3
#endif