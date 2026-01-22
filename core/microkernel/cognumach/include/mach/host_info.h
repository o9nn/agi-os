#ifndef _MACH_HOST_INFO_H_
#define _MACH_HOST_INFO_H_
#include <mach/machine.h>
#include <mach/machine/vm_types.h>
typedef integer_t *host_info_t;
#define HOST_INFO_MAX (1024)
typedef integer_t host_info_data_t[HOST_INFO_MAX];
#define KERNEL_VERSION_MAX (512)
typedef char kernel_version_t[KERNEL_VERSION_MAX];
#define HOST_BASIC_INFO 1
#define HOST_PROCESSOR_SLOTS 2
#define HOST_SCHED_INFO 3
#define HOST_LOAD_INFO 4
struct host_basic_info {
integer_t max_cpus;
integer_t avail_cpus;
rpc_vm_size_t memory_size;
cpu_type_t cpu_type;
cpu_subtype_t cpu_subtype;
};
typedef struct host_basic_info host_basic_info_data_t;
typedef struct host_basic_info *host_basic_info_t;
#define HOST_BASIC_INFO_COUNT \
(sizeof(host_basic_info_data_t)/sizeof(integer_t))
struct host_sched_info {
integer_t min_timeout;
integer_t min_quantum;
};
typedef struct host_sched_info host_sched_info_data_t;
typedef struct host_sched_info *host_sched_info_t;
#define HOST_SCHED_INFO_COUNT \
(sizeof(host_sched_info_data_t)/sizeof(integer_t))
struct host_load_info {
integer_t avenrun[3];
integer_t mach_factor[3];
};
typedef struct host_load_info host_load_info_data_t;
typedef struct host_load_info *host_load_info_t;
#define HOST_LOAD_INFO_COUNT \
(sizeof(host_load_info_data_t)/sizeof(integer_t))
#endif