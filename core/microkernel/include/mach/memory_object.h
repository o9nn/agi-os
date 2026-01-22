#ifndef	_MACH_MEMORY_OBJECT_H_
#define _MACH_MEMORY_OBJECT_H_
#include <mach/port.h>
#ifdef	MACH_KERNEL
#include <ipc/ipc_types.h>
typedef	ipc_port_t	memory_object_t;
#else
typedef	mach_port_t	memory_object_t;
#endif
typedef	memory_object_t *memory_object_array_t;
typedef	mach_port_t	memory_object_control_t;
typedef	mach_port_t	memory_object_name_t;
typedef	int		memory_object_copy_strategy_t;
#define		MEMORY_OBJECT_COPY_NONE		0
#define		MEMORY_OBJECT_COPY_CALL		1
#define		MEMORY_OBJECT_COPY_DELAY 	2
#define		MEMORY_OBJECT_COPY_TEMPORARY 	3
typedef	int		memory_object_return_t;
#define		MEMORY_OBJECT_RETURN_NONE	0
#define		MEMORY_OBJECT_RETURN_DIRTY	1
#define		MEMORY_OBJECT_RETURN_ALL	2
#define		MEMORY_OBJECT_NULL	MACH_PORT_NULL
#endif