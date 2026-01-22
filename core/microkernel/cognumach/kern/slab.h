#ifndef _KERN_SLAB_H
#define _KERN_SLAB_H
#include <cache.h>
#include <kern/cpu_number.h>
#include <kern/lock.h>
#include <kern/list.h>
#include <kern/rbtree.h>
#include <mach/machine/vm_types.h>
#include <sys/types.h>
#include <vm/vm_types.h>
struct kmem_cache;
#if SLAB_USE_CPU_POOLS
struct kmem_cpu_pool {
simple_lock_data_t lock;
int flags;
int size;
int transfer_size;
int nr_objs;
void **array;
} __attribute__((aligned(CPU_L1_SIZE)));
struct kmem_cpu_pool_type {
size_t buf_size;
int array_size;
size_t array_align;
struct kmem_cache *array_cache;
};
#endif
union kmem_bufctl {
union kmem_bufctl *next;
unsigned long redzone;
};
struct kmem_buftag {
unsigned long state;
};
struct kmem_slab {
struct kmem_cache *cache;
struct list list_node;
struct rbtree_node tree_node;
unsigned long nr_refs;
union kmem_bufctl *first_free;
void *addr;
};
typedef void (*kmem_cache_ctor_t)(void *obj);
#define KMEM_CACHE_NAME_SIZE 24
struct kmem_cache {
#if SLAB_USE_CPU_POOLS
struct kmem_cpu_pool cpu_pools[NCPUS];
struct kmem_cpu_pool_type *cpu_pool_type;
#endif
simple_lock_data_t lock;
struct list node;
struct list partial_slabs;
struct list free_slabs;
struct rbtree active_slabs;
int flags;
size_t bufctl_dist;
size_t slab_size;
long_natural_t bufs_per_slab;
long_natural_t nr_objs;
long_natural_t nr_free_slabs;
kmem_cache_ctor_t ctor;
size_t obj_size;
size_t align;
size_t buf_size;
size_t color;
size_t color_max;
long_natural_t nr_bufs;
long_natural_t nr_slabs;
char name[KMEM_CACHE_NAME_SIZE];
size_t buftag_dist;
size_t redzone_pad;
} __cacheline_aligned;
typedef struct kmem_cache *kmem_cache_t;
#define KMEM_CACHE_NULL ((kmem_cache_t) 0)
#define KMEM_CACHE_NOOFFSLAB    0x1
#define KMEM_CACHE_PHYSMEM      0x2
#define KMEM_CACHE_VERIFY       0x4
void kmem_cache_init(struct kmem_cache *cache, const char *name,
size_t obj_size, size_t align,
kmem_cache_ctor_t ctor, int flags);
vm_offset_t kmem_cache_alloc(struct kmem_cache *cache);
void kmem_cache_free(struct kmem_cache *cache, vm_offset_t obj);
void slab_bootstrap(void);
void slab_init(void);
void slab_collect(void);
void slab_info(void);
#if MACH_KDB
void db_show_slab_info(void);
void db_whatis_slab(vm_offset_t addr);
#endif
#endif