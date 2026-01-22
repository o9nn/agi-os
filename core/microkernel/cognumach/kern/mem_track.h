#ifndef _KERN_MEM_TRACK_H_
#define _KERN_MEM_TRACK_H_
#include <mach/machine/vm_types.h>
#include <vm/vm_types.h>
#include <sys/types.h>
#include <kern/lock.h>
typedef int kern_return_t;
typedef enum {
MEM_TYPE_GENERAL = 0,
MEM_TYPE_VM_OBJECTS,
MEM_TYPE_IPC,
MEM_TYPE_THREADS,
MEM_TYPE_TASKS,
MEM_TYPE_DEVICE,
MEM_TYPE_NETWORK,
MEM_TYPE_MAX
} mem_type_t;
struct mem_stats {
uint64_t alloc_count;
uint64_t free_count;
uint64_t alloc_bytes;
uint64_t free_bytes;
uint64_t current_bytes;
uint64_t peak_bytes;
uint64_t failed_allocs;
uint32_t large_allocs;
};
struct mem_tracker {
simple_lock_data_t lock;
struct mem_stats stats[MEM_TYPE_MAX];
struct mem_stats total_stats;
uint32_t low_memory_warnings;
uint32_t out_of_memory_events;
vm_size_t memory_threshold_low;
vm_size_t memory_threshold_critical;
uint32_t slab_cache_hits;
uint32_t slab_cache_misses;
uint32_t page_alloc_slow;
uint32_t page_alloc_failed;
};
extern void mem_track_init(void);
extern void mem_track_alloc(mem_type_t type, vm_size_t size);
extern void mem_track_free(mem_type_t type, vm_size_t size);
extern void mem_track_alloc_failed(mem_type_t type, vm_size_t size);
extern void mem_track_update_cache_stats(int hits, int misses);
extern void mem_track_page_alloc_slow(void);
extern void mem_track_page_alloc_failed(void);
extern boolean_t mem_track_check_pressure(void);
extern void mem_track_memory_warning(void);
extern void mem_track_vm_object_inconsistency(void);
extern void mem_track_out_of_memory(void);
extern void mem_track_report_usage(void);
extern void mem_track_report_detailed(void);
extern kern_return_t mem_track_get_stats(mem_type_t type, struct mem_stats *stats);
extern void mem_track_set_thresholds(vm_size_t low_threshold, vm_size_t critical_threshold);
#ifdef MACH_DEBUG
extern void mem_track_verify_stats(void);
extern void mem_track_dump_state(void);
#endif
#endif