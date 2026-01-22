#ifndef	_MACH_DEBUG_MACH_DEBUG_TYPES_H_
#define _MACH_DEBUG_MACH_DEBUG_TYPES_H_
#include <mach_debug/vm_info.h>
#include <mach_debug/slab_info.h>
#include <mach_debug/hash_info.h>
typedef	char	symtab_name_t[32];
typedef	const char	*const_symtab_name_t;
#define KERNEL_DEBUG_NAME_MAX (64)
typedef char	kernel_debug_name_t[KERNEL_DEBUG_NAME_MAX];
typedef const char	*const_kernel_debug_name_t;
#endif