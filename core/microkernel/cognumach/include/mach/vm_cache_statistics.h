#ifndef _MACH_VM_CACHE_STATISTICS_H_
#define _MACH_VM_CACHE_STATISTICS_H_
#include <mach/machine/vm_types.h>
struct vm_cache_statistics {
integer_t	cache_object_count;
integer_t	cache_count;
integer_t	active_tmp_count;
integer_t	inactive_tmp_count;
integer_t	active_perm_count;
integer_t	inactive_perm_count;
integer_t	dirty_count;
integer_t	laundry_count;
integer_t	writeback_count;
integer_t	slab_count;
integer_t	slab_reclaim_count;
};
typedef struct vm_cache_statistics	*vm_cache_statistics_t;
typedef struct vm_cache_statistics	vm_cache_statistics_data_t;
#endif