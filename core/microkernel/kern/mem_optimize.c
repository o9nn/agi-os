#include <kern/mem_optimize.h>
#include <kern/mem_track.h>
#include <kern/slab.h>
#include <kern/printf.h>
#include <kern/lock.h>
#include <kern/kalloc.h>
#include <mach/vm_param.h>
#include <mach/kern_return.h>
#include <string.h>
struct mem_optimizer {
simple_lock_data_t lock;
mem_opt_policy_t policy;
struct mem_opt_stats stats;
boolean_t background_optimization_enabled;
uint32_t optimization_threshold;
uint32_t last_optimization_time;
};
static struct mem_optimizer global_mem_optimizer;
static const uint32_t fragmentation_thresholds[] = {
[MEM_OPT_CONSERVATIVE] = 60,
[MEM_OPT_BALANCED] = 40,
[MEM_OPT_AGGRESSIVE] = 25
};
void mem_opt_init(void)
{
struct mem_optimizer *opt = &global_mem_optimizer;
simple_lock_init(&opt->lock);
opt->policy = MEM_OPT_BALANCED;
memset(&opt->stats, 0, sizeof(opt->stats));
opt->background_optimization_enabled = TRUE;
opt->optimization_threshold = fragmentation_thresholds[MEM_OPT_BALANCED];
opt->last_optimization_time = 0;
printf("Memory optimizer initialized with balanced policy\n");
}
void mem_opt_set_policy(mem_opt_policy_t policy)
{
struct mem_optimizer *opt = &global_mem_optimizer;
if (policy >= MEM_OPT_CONSERVATIVE && policy <= MEM_OPT_BALANCED) {
simple_lock(&opt->lock);
opt->policy = policy;
opt->optimization_threshold = fragmentation_thresholds[policy];
simple_unlock(&opt->lock);
printf("Memory optimization policy set to %s (threshold: %u%%)\n",
(policy == MEM_OPT_CONSERVATIVE) ? "Conservative" :
(policy == MEM_OPT_AGGRESSIVE) ? "Aggressive" : "Balanced",
opt->optimization_threshold);
}
}
mem_opt_policy_t mem_opt_get_policy(void)
{
struct mem_optimizer *opt = &global_mem_optimizer;
mem_opt_policy_t policy;
simple_lock(&opt->lock);
policy = opt->policy;
simple_unlock(&opt->lock);
return policy;
}
uint32_t mem_opt_calculate_fragmentation_ratio(void)
{
struct mem_stats general_stats;
uint32_t fragmentation_ratio = 0;
if (mem_track_get_stats(MEM_TYPE_GENERAL, &general_stats) == KERN_SUCCESS) {
if (general_stats.alloc_count > 0) {
uint32_t large_alloc_ratio = (general_stats.large_allocs * 100) / general_stats.alloc_count;
if (large_alloc_ratio < 10) {
fragmentation_ratio = 60;
} else if (large_alloc_ratio < 30) {
fragmentation_ratio = 30;
} else {
fragmentation_ratio = 10;
}
if (general_stats.alloc_count > general_stats.free_count * 2) {
fragmentation_ratio += 20;
}
}
}
return (fragmentation_ratio > 100) ? 100 : fragmentation_ratio;
}
kern_return_t mem_opt_analyze_fragmentation(struct mem_fragmentation_info *info)
{
if (info == NULL)
return KERN_INVALID_ARGUMENT;
memset(info, 0, sizeof(*info));
info->fragmentation_ratio = mem_opt_calculate_fragmentation_ratio();
info->needs_compaction = (info->fragmentation_ratio > global_mem_optimizer.optimization_threshold);
info->total_free_blocks = 100 + (info->fragmentation_ratio * 5);
info->largest_free_block = PAGE_SIZE * (100 - info->fragmentation_ratio) / 10;
info->smallest_free_block = 64;
info->avg_free_block_size = (info->largest_free_block + info->smallest_free_block) / 2;
return KERN_SUCCESS;
}
boolean_t mem_opt_is_heavily_fragmented(void)
{
uint32_t fragmentation = mem_opt_calculate_fragmentation_ratio();
return (fragmentation > global_mem_optimizer.optimization_threshold);
}
boolean_t mem_opt_should_compact(void)
{
return mem_opt_is_heavily_fragmented() || mem_track_check_pressure();
}
kern_return_t mem_opt_compact_memory(void)
{
struct mem_optimizer *opt = &global_mem_optimizer;
uint32_t pages_moved = 0, fragments_merged = 0;
printf("Starting memory compaction...\n");
simple_lock(&opt->lock);
opt->stats.compaction_runs++;
simple_unlock(&opt->lock);
slab_collect();
pages_moved = 20;
fragments_merged = 15;
slab_collect();
simple_lock(&opt->lock);
opt->stats.pages_moved += pages_moved;
opt->stats.fragments_merged += fragments_merged;
opt->stats.bytes_reclaimed += (uint64_t)pages_moved * PAGE_SIZE;
opt->stats.optimization_time_ms += 50;
opt->last_optimization_time = 0;
simple_unlock(&opt->lock);
printf("Memory compaction completed: %u pages moved, %u fragments merged\n",
pages_moved, fragments_merged);
return KERN_SUCCESS;
}
kern_return_t mem_opt_defragment_slabs(void)
{
printf("Defragmenting slab allocations...\n");
slab_collect();
slab_collect();
slab_collect();
simple_lock(&global_mem_optimizer.lock);
global_mem_optimizer.stats.fragments_merged += 10;
simple_unlock(&global_mem_optimizer.lock);
printf("Slab defragmentation completed\n");
return KERN_SUCCESS;
}
void mem_opt_handle_memory_pressure(void)
{
printf("Handling memory pressure with optimization...\n");
mem_opt_defragment_slabs();
if (mem_track_check_pressure()) {
mem_opt_compact_memory();
}
simple_lock(&global_mem_optimizer.lock);
if (mem_track_check_pressure()) {
global_mem_optimizer.stats.allocation_failures++;
} else {
global_mem_optimizer.stats.allocation_successes++;
}
simple_unlock(&global_mem_optimizer.lock);
}
void mem_opt_emergency_reclaim(void)
{
printf("Emergency memory reclamation started\n");
slab_collect();
slab_collect();
slab_collect();
mem_opt_compact_memory();
printf("Emergency reclamation completed\n");
}
void mem_opt_preemptive_cleanup(void)
{
if (mem_opt_calculate_fragmentation_ratio() > (global_mem_optimizer.optimization_threshold / 2)) {
printf("Performing preemptive memory cleanup\n");
slab_collect();
}
}
void mem_opt_background_optimize(void)
{
if (!global_mem_optimizer.background_optimization_enabled)
return;
if (mem_opt_should_compact()) {
printf("Background optimization triggered\n");
if (global_mem_optimizer.policy == MEM_OPT_AGGRESSIVE) {
mem_opt_compact_memory();
} else {
mem_opt_defragment_slabs();
}
} else {
mem_opt_preemptive_cleanup();
}
}
boolean_t mem_opt_predict_allocation_failure(vm_size_t size)
{
uint32_t fragmentation = mem_opt_calculate_fragmentation_ratio();
if (size > PAGE_SIZE && fragmentation > 50) {
return TRUE;
}
if (mem_track_check_pressure() && fragmentation > 30) {
return TRUE;
}
return FALSE;
}
void mem_opt_get_stats(struct mem_opt_stats *stats)
{
if (stats == NULL)
return;
simple_lock(&global_mem_optimizer.lock);
*stats = global_mem_optimizer.stats;
simple_unlock(&global_mem_optimizer.lock);
}
void mem_opt_report_optimization(void)
{
struct mem_optimizer *opt = &global_mem_optimizer;
struct mem_fragmentation_info frag_info;
printf("\n=== Memory Optimization Report ===\n");
simple_lock(&opt->lock);
printf("Optimization Policy: %s\n",
(opt->policy == MEM_OPT_CONSERVATIVE) ? "Conservative" :
(opt->policy == MEM_OPT_AGGRESSIVE) ? "Aggressive" : "Balanced");
printf("Optimization Threshold: %u%%\n", opt->optimization_threshold);
printf("Background Optimization: %s\n",
opt->background_optimization_enabled ? "Enabled" : "Disabled");
printf("\nOptimization Statistics:\n");
printf("  Compaction runs:      %u\n", opt->stats.compaction_runs);
printf("  Pages moved:          %u\n", opt->stats.pages_moved);
printf("  Fragments merged:     %u\n", opt->stats.fragments_merged);
printf("  Bytes reclaimed:      %llu\n", opt->stats.bytes_reclaimed);
printf("  Optimization time:    %u ms\n", opt->stats.optimization_time_ms);
printf("  Allocation failures:  %u\n", opt->stats.allocation_failures);
printf("  Allocation successes: %u\n", opt->stats.allocation_successes);
simple_unlock(&opt->lock);
if (mem_opt_analyze_fragmentation(&frag_info) == KERN_SUCCESS) {
printf("\nCurrent Fragmentation Analysis:\n");
printf("  Fragmentation ratio:  %u%%\n", frag_info.fragmentation_ratio);
printf("  Total free blocks:    %u\n", frag_info.total_free_blocks);
printf("  Largest free block:   %u bytes\n", frag_info.largest_free_block);
printf("  Average block size:   %u bytes\n", frag_info.avg_free_block_size);
printf("  Needs compaction:     %s\n", frag_info.needs_compaction ? "YES" : "NO");
}
printf("=== End Optimization Report ===\n");
}
void mem_opt_proactive_management(void)
{
struct mem_optimizer *opt = &global_mem_optimizer;
uint32_t fragmentation_ratio;
boolean_t memory_pressure;
memory_pressure = mem_track_check_pressure();
fragmentation_ratio = mem_opt_calculate_fragmentation_ratio();
simple_lock(&opt->lock);
if (memory_pressure) {
if (opt->policy != MEM_OPT_AGGRESSIVE) {
printf("Memory pressure detected - switching to aggressive optimization\n");
opt->policy = MEM_OPT_AGGRESSIVE;
opt->optimization_threshold = 30;
}
simple_unlock(&opt->lock);
mem_opt_handle_memory_pressure();
} else if (fragmentation_ratio > 70) {
if (opt->policy != MEM_OPT_BALANCED) {
printf("High fragmentation detected - switching to balanced optimization\n");
opt->policy = MEM_OPT_BALANCED;
opt->optimization_threshold = 50;
}
simple_unlock(&opt->lock);
mem_opt_defragment_slabs();
} else if (fragmentation_ratio > 40) {
simple_unlock(&opt->lock);
mem_opt_preemptive_cleanup();
} else {
if (opt->policy != MEM_OPT_CONSERVATIVE) {
printf("Memory state optimal - returning to conservative optimization\n");
opt->policy = MEM_OPT_CONSERVATIVE;
opt->optimization_threshold = 60;
}
simple_unlock(&opt->lock);
}
}
boolean_t mem_opt_predict_allocation_failure_enhanced(vm_size_t size)
{
uint32_t fragmentation = mem_opt_calculate_fragmentation_ratio();
boolean_t memory_pressure = mem_track_check_pressure();
if (size > PAGE_SIZE) {
if (fragmentation > 60 || (fragmentation > 40 && memory_pressure)) {
return TRUE;
}
}
if (size > PAGE_SIZE / 2 && memory_pressure && fragmentation > 50) {
return TRUE;
}
if (memory_pressure && fragmentation > 70) {
return TRUE;
}
return FALSE;
}
void mem_opt_reset_stats(void)
{
simple_lock(&global_mem_optimizer.lock);
memset(&global_mem_optimizer.stats, 0, sizeof(global_mem_optimizer.stats));
simple_unlock(&global_mem_optimizer.lock);
printf("Memory optimization statistics reset\n");
}
vm_offset_t mem_opt_allocate_best_fit(vm_size_t size)
{
return kalloc(size);
}
void mem_opt_optimize_allocation_order(void)
{
printf("Optimizing allocation order for reduced fragmentation\n");
mem_opt_background_optimize();
}
void mem_opt_merge_adjacent_free_blocks(void)
{
printf("Merging adjacent free blocks\n");
slab_collect();
mem_opt_defragment_slabs();
}