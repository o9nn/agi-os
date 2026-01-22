#ifndef	_MACH_MACH_TYPES_H_
#define _MACH_MACH_TYPES_H_
#include <mach/host_info.h>
#include <mach/machine.h>
#include <mach/machine/vm_types.h>
#include <mach/memory_object.h>
#include <mach/pc_sample.h>
#include <mach/port.h>
#include <mach/processor_info.h>
#include <mach/task_info.h>
#include <mach/task_special_ports.h>
#include <mach/thread_info.h>
#include <mach/thread_special_ports.h>
#include <mach/thread_status.h>
#include <mach/time_value.h>
#include <mach/vm_attributes.h>
#include <mach/vm_inherit.h>
#include <mach/vm_prot.h>
#include <mach/vm_statistics.h>
#include <mach/vm_cache_statistics.h>
#include <mach/vm_wire.h>
#include <mach/vm_sync.h>
#ifdef	MACH_KERNEL
typedef struct task		*task_t;
typedef struct thread		*thread_t;
typedef struct processor	*processor_t;
typedef struct processor_set	*processor_set_t;
#else
typedef	mach_port_t	task_t;
typedef task_t		*task_array_t;
typedef	task_t		vm_task_t;
typedef task_t		ipc_space_t;
typedef	mach_port_t	thread_t;
typedef	thread_t	*thread_array_t;
typedef mach_port_t	host_t;
typedef mach_port_t	host_priv_t;
typedef mach_port_t	processor_t;
typedef mach_port_t	*processor_array_t;
typedef mach_port_t	processor_set_t;
typedef mach_port_t	processor_set_name_t;
typedef mach_port_t	*processor_set_array_t;
typedef mach_port_t	*processor_set_name_array_t;
typedef vm_offset_t	*emulation_vector_t;
#endif
#include <mach/std_types.h>
#endif