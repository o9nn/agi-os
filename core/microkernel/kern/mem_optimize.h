#ifndef _KERN_MEM_OPTIMIZE_H_
#define _KERN_MEM_OPTIMIZE_H_
#include <mach/machine/vm_types.h>
#include <vm/vm_types.h>
#include <sys/types.h>
#include <mach/boolean.h>
typedef int kern_return_t;
typedef enum {
MEM_OPT_CONSERVATIVE = 0,
MEM_OPT_AGGRESSIVE,
MEM_OPT_BALANCED
} mem_opt_policy_t;
struct mem_opt_stats {
uint32_t compaction_runs;
uint32_t pages_moved;
uint32_t fragments_merged;
uint32_t allocation_failures;
uint32_t allocation_successes;
uint64_t bytes_reclaimed;
uint32_t optimization_time_ms;
};
struct mem_fragmentation_info {
uint32_t total_free_blocks;
uint32_t largest_free_block;
uint32_t smallest_free_block;
uint32_t avg_free_block_size;
uint32_t fragmentation_ratio;
boolean_t needs_compaction;
};
extern void mem_opt_init(void);
extern void mem_opt_set_policy(mem_opt_policy_t policy);
extern mem_opt_policy_t mem_opt_get_policy(void);
extern kern_return_t mem_opt_compact_memory(void);
extern kern_return_t mem_opt_defragment_slabs(void);
extern boolean_t mem_opt_should_compact(void);
extern vm_offset_t mem_opt_allocate_best_fit(vm_size_t size);
extern void mem_opt_optimize_allocation_order(void);
extern void mem_opt_merge_adjacent_free_blocks(void);
extern kern_return_t mem_opt_analyze_fragmentation(struct mem_fragmentation_info *info);
extern uint32_t mem_opt_calculate_fragmentation_ratio(void);
extern boolean_t mem_opt_is_heavily_fragmented(void);
extern void mem_opt_handle_memory_pressure(void);
extern void mem_opt_emergency_reclaim(void);
extern void mem_opt_preemptive_cleanup(void);
extern void mem_opt_get_stats(struct mem_opt_stats *stats);
extern void mem_opt_report_optimization(void);
extern void mem_opt_reset_stats(void);
extern void mem_opt_background_optimize(void);
extern void mem_opt_adaptive_threshold_adjustment(void);
extern boolean_t mem_opt_predict_allocation_failure(vm_size_t size);
extern void mem_opt_proactive_management(void);
extern boolean_t mem_opt_predict_allocation_failure_enhanced(vm_size_t size);
#endif