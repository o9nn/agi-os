#ifndef	_MACH_PROCESSOR_INFO_H_
#define _MACH_PROCESSOR_INFO_H_
#include <mach/machine.h>
typedef integer_t	*processor_info_t;
#define PROCESSOR_INFO_MAX	(1024)
typedef integer_t	processor_info_data_t[PROCESSOR_INFO_MAX];
typedef integer_t	*processor_set_info_t;
#define PROCESSOR_SET_INFO_MAX	(1024)
typedef integer_t	processor_set_info_data_t[PROCESSOR_SET_INFO_MAX];
#define	PROCESSOR_BASIC_INFO	1
struct processor_basic_info {
cpu_type_t	cpu_type;
cpu_subtype_t	cpu_subtype;
integer_t	running;
integer_t	slot_num;
integer_t	is_master;
};
typedef	struct processor_basic_info	processor_basic_info_data_t;
typedef struct processor_basic_info	*processor_basic_info_t;
#define PROCESSOR_BASIC_INFO_COUNT \
(sizeof(processor_basic_info_data_t)/sizeof(integer_t))
#define	PROCESSOR_SET_BASIC_INFO	1
struct processor_set_basic_info {
integer_t	processor_count;
integer_t	task_count;
integer_t	thread_count;
integer_t	load_average;
integer_t	mach_factor;
};
#define	LOAD_SCALE	1000
typedef	struct processor_set_basic_info	processor_set_basic_info_data_t;
typedef struct processor_set_basic_info	*processor_set_basic_info_t;
#define PROCESSOR_SET_BASIC_INFO_COUNT \
(sizeof(processor_set_basic_info_data_t)/sizeof(integer_t))
#define PROCESSOR_SET_SCHED_INFO	2
struct processor_set_sched_info {
integer_t	policies;
integer_t	max_priority;
};
typedef	struct processor_set_sched_info	processor_set_sched_info_data_t;
typedef struct processor_set_sched_info	*processor_set_sched_info_t;
#define PROCESSOR_SET_SCHED_INFO_COUNT \
(sizeof(processor_set_sched_info_data_t)/sizeof(integer_t))
#endif