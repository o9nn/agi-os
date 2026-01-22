#ifndef _MACH_DEBUG_SLAB_INFO_H_
#define _MACH_DEBUG_SLAB_INFO_H_
#include <sys/types.h>
#define CACHE_NAME_MAX_LEN 32
typedef struct cache_info {
int flags;
rpc_vm_size_t cpu_pool_size;
rpc_vm_size_t obj_size;
rpc_vm_size_t align;
rpc_vm_size_t buf_size;
rpc_vm_size_t slab_size;
rpc_long_natural_t bufs_per_slab;
rpc_long_natural_t nr_objs;
rpc_long_natural_t nr_bufs;
rpc_long_natural_t nr_slabs;
rpc_long_natural_t nr_free_slabs;
char name[CACHE_NAME_MAX_LEN];
} cache_info_t;
typedef cache_info_t *cache_info_array_t;
#endif