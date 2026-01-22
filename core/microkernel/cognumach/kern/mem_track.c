#include <kern/mem_track.h>
#include <kern/assert.h>
#include <kern/printf.h>
#include <kern/lock.h>
#include <kern/atomic.h>
#include <mach/vm_param.h>
#include <mach/kern_return.h>
#include <string.h>
extern uint32_t mem_opt_calculate_fragmentation_ratio(void);
static struct mem_tracker global_mem_tracker;
static const char *mem_type_names[MEM_TYPE_MAX] = {
"General",
"VM Objects",
"IPC",
"Threads",
"Tasks",
"Device",
"Network"
};
void mem_track_init(void)
{
struct mem_tracker *tracker = &global_mem_tracker;
simple_lock_init(&tracker->lock);
memset(&tracker->stats, 0, sizeof(tracker->stats));
memset(&tracker->total_stats, 0, sizeof(tracker->total_stats));
tracker->low_memory_warnings = 0;
tracker->out_of_memory_events = 0;
tracker->slab_cache_hits = 0;
tracker->slab_cache_misses = 0;
tracker->page_alloc_slow = 0;
tracker->page_alloc_failed = 0;
tracker->memory_threshold_low = 8 * 1024 * 1024;
tracker->memory_threshold_critical = 4 * 1024 * 1024;
printf("Memory tracking system initialized\n");
}
void mem_track_alloc(mem_type_t type, vm_size_t size)
{
struct mem_tracker *tracker = &global_mem_tracker;
struct mem_stats *stats;
if (type >= MEM_TYPE_MAX)
type = MEM_TYPE_GENERAL;
simple_lock(&tracker->lock);
stats = &tracker->stats[type];
stats->alloc_count++;
stats->alloc_bytes += size;
stats->current_bytes += size;
if (stats->current_bytes > stats->peak_bytes)
stats->peak_bytes = stats->current_bytes;
if (size > PAGE_SIZE)
stats->large_allocs++;
tracker->total_stats.alloc_count++;
tracker->total_stats.alloc_bytes += size;
tracker->total_stats.current_bytes += size;
if (tracker->total_stats.current_bytes > tracker->total_stats.peak_bytes)
tracker->total_stats.peak_bytes = tracker->total_stats.current_bytes;
if (size > PAGE_SIZE)
tracker->total_stats.large_allocs++;
simple_unlock(&tracker->lock);
if (tracker->total_stats.current_bytes > tracker->memory_threshold_low) {
mem_track_memory_warning();
}
}
void mem_track_free(mem_type_t type, vm_size_t size)
{
struct mem_tracker *tracker = &global_mem_tracker;
struct mem_stats *stats;
if (type >= MEM_TYPE_MAX)
type = MEM_TYPE_GENERAL;
simple_lock(&tracker->lock);
stats = &tracker->stats[type];
stats->free_count++;
stats->free_bytes += size;
if (stats->current_bytes >= size)
stats->current_bytes -= size;
else
stats->current_bytes = 0;
tracker->total_stats.free_count++;
tracker->total_stats.free_bytes += size;
if (tracker->total_stats.current_bytes >= size)
tracker->total_stats.current_bytes -= size;
else
tracker->total_stats.current_bytes = 0;
simple_unlock(&tracker->lock);
}
void mem_track_alloc_failed(mem_type_t type, vm_size_t size)
{
struct mem_tracker *tracker = &global_mem_tracker;
struct mem_stats *stats;
if (type >= MEM_TYPE_MAX)
type = MEM_TYPE_GENERAL;
simple_lock(&tracker->lock);
stats = &tracker->stats[type];
stats->failed_allocs++;
tracker->total_stats.failed_allocs++;
simple_unlock(&tracker->lock);
if (size > PAGE_SIZE || tracker->total_stats.failed_allocs > 10) {
mem_track_out_of_memory();
}
}
void mem_track_update_cache_stats(int hits, int misses)
{
struct mem_tracker *tracker = &global_mem_tracker;
simple_lock(&tracker->lock);
if (hits > 0)
tracker->slab_cache_hits += (uint32_t)hits;
if (misses > 0)
tracker->slab_cache_misses += (uint32_t)misses;
simple_unlock(&tracker->lock);
}
void mem_track_page_alloc_slow(void)
{
struct mem_tracker *tracker = &global_mem_tracker;
simple_lock(&tracker->lock);
tracker->page_alloc_slow++;
simple_unlock(&tracker->lock);
}
void mem_track_page_alloc_failed(void)
{
struct mem_tracker *tracker = &global_mem_tracker;
simple_lock(&tracker->lock);
tracker->page_alloc_failed++;
simple_unlock(&tracker->lock);
}
boolean_t mem_track_check_pressure(void)
{
struct mem_tracker *tracker = &global_mem_tracker;
uint64_t current_usage;
uint32_t recent_failures;
uint32_t fragmentation_ratio;
boolean_t high_usage, high_failures, high_fragmentation;
simple_lock(&tracker->lock);
current_usage = tracker->total_stats.current_bytes;
recent_failures = tracker->total_stats.failed_allocs;
simple_unlock(&tracker->lock);
high_usage = (current_usage > (uint64_t)tracker->memory_threshold_low);
high_failures = (recent_failures > 10 && tracker->total_stats.alloc_count > 0 &&
(recent_failures * 100 / tracker->total_stats.alloc_count) > 5);
fragmentation_ratio = mem_opt_calculate_fragmentation_ratio();
high_fragmentation = (fragmentation_ratio > 60);
return (current_usage > (uint64_t)tracker->memory_threshold_critical) ||
((high_usage ? 1 : 0) + (high_failures ? 1 : 0) + (high_fragmentation ? 1 : 0) >= 2);
}
void mem_track_memory_warning(void)
{
struct mem_tracker *tracker = &global_mem_tracker;
simple_lock(&tracker->lock);
tracker->low_memory_warnings++;
simple_unlock(&tracker->lock);
if ((tracker->low_memory_warnings % 100) == 1) {
printf("Memory warning: low available memory (%luk bytes in use)\n",
(unsigned long)(tracker->total_stats.current_bytes >> 10));
}
}
void mem_track_vm_object_inconsistency(void)
{
struct mem_tracker *tracker = &global_mem_tracker;
simple_lock(&tracker->lock);
tracker->low_memory_warnings++;
simple_unlock(&tracker->lock);
printf("VM object memory inconsistency detected - potential memory corruption\n");
printf("Consider running comprehensive memory verification\n");
}
void mem_track_out_of_memory(void)
{
struct mem_tracker *tracker = &global_mem_tracker;
simple_lock(&tracker->lock);
tracker->out_of_memory_events++;
simple_unlock(&tracker->lock);
printf("Critical: Out of memory condition detected (%luk bytes in use, %llu failed allocs)\n",
(unsigned long)(tracker->total_stats.current_bytes >> 10),
tracker->total_stats.failed_allocs);
}
void mem_track_report_usage(void)
{
struct mem_tracker *tracker = &global_mem_tracker;
uint64_t total_kb, peak_kb;
simple_lock(&tracker->lock);
total_kb = tracker->total_stats.current_bytes >> 10;
peak_kb = tracker->total_stats.peak_bytes >> 10;
simple_unlock(&tracker->lock);
printf("Memory Usage Summary:\n");
printf("  Current usage: %luk bytes\n", (unsigned long)total_kb);
printf("  Peak usage:    %luk bytes\n", (unsigned long)peak_kb);
printf("  Total allocs:  %llu\n", tracker->total_stats.alloc_count);
printf("  Total frees:   %llu\n", tracker->total_stats.free_count);
printf("  Failed allocs: %llu\n", tracker->total_stats.failed_allocs);
printf("  Large allocs:  %u\n", tracker->total_stats.large_allocs);
printf("  Low mem warnings: %u\n", tracker->low_memory_warnings);
printf("  OOM events:    %u\n", tracker->out_of_memory_events);
}
void mem_track_report_detailed(void)
{
struct mem_tracker *tracker = &global_mem_tracker;
int i;
printf("Detailed Memory Usage by Type:\n");
printf("Type            Allocs      Frees    Current(k)  Peak(k)  Failed  Large\n");
printf("----------------------------------------------------------------------\n");
simple_lock(&tracker->lock);
for (i = 0; i < MEM_TYPE_MAX; i++) {
struct mem_stats *stats = &tracker->stats[i];
if (stats->alloc_count > 0) {
printf("%-12s %9llu %9llu %10luk %7luk %7llu %6u\n",
mem_type_names[i],
stats->alloc_count,
stats->free_count,
(unsigned long)(stats->current_bytes >> 10),
(unsigned long)(stats->peak_bytes >> 10),
stats->failed_allocs,
stats->large_allocs);
}
}
printf("----------------------------------------------------------------------\n");
printf("TOTAL        %9llu %9llu %10luk %7luk %7llu %6u\n",
tracker->total_stats.alloc_count,
tracker->total_stats.free_count,
(unsigned long)(tracker->total_stats.current_bytes >> 10),
(unsigned long)(tracker->total_stats.peak_bytes >> 10),
tracker->total_stats.failed_allocs,
tracker->total_stats.large_allocs);
printf("\nCache Statistics:\n");
printf("  Slab cache hits:   %u\n", tracker->slab_cache_hits);
printf("  Slab cache misses: %u\n", tracker->slab_cache_misses);
printf("  Page alloc slow:   %u\n", tracker->page_alloc_slow);
printf("  Page alloc failed: %u\n", tracker->page_alloc_failed);
simple_unlock(&tracker->lock);
}
kern_return_t mem_track_get_stats(mem_type_t type, struct mem_stats *stats)
{
struct mem_tracker *tracker = &global_mem_tracker;
if (type >= MEM_TYPE_MAX || stats == NULL)
return KERN_INVALID_ARGUMENT;
simple_lock(&tracker->lock);
*stats = tracker->stats[type];
simple_unlock(&tracker->lock);
return KERN_SUCCESS;
}
void mem_track_set_thresholds(vm_size_t low_threshold, vm_size_t critical_threshold)
{
struct mem_tracker *tracker = &global_mem_tracker;
simple_lock(&tracker->lock);
tracker->memory_threshold_low = low_threshold;
tracker->memory_threshold_critical = critical_threshold;
simple_unlock(&tracker->lock);
printf("Memory thresholds set: low=%luk, critical=%luk\n",
(unsigned long)(low_threshold >> 10), (unsigned long)(critical_threshold >> 10));
}
#ifdef MACH_DEBUG
void mem_track_verify_stats(void)
{
struct mem_tracker *tracker = &global_mem_tracker;
uint64_t total_current = 0;
uint64_t total_allocs = 0;
uint64_t total_frees = 0;
int i;
simple_lock(&tracker->lock);
for (i = 0; i < MEM_TYPE_MAX; i++) {
struct mem_stats *stats = &tracker->stats[i];
total_current += stats->current_bytes;
total_allocs += stats->alloc_count;
total_frees += stats->free_count;
assert(stats->alloc_count >= stats->free_count);
assert(stats->current_bytes <= stats->peak_bytes);
assert(stats->alloc_bytes >= stats->free_bytes);
}
assert(total_current == tracker->total_stats.current_bytes);
assert(total_allocs == tracker->total_stats.alloc_count);
assert(total_frees == tracker->total_stats.free_count);
simple_unlock(&tracker->lock);
printf("Memory statistics verification passed\n");
}
void mem_track_dump_state(void)
{
printf("=== Memory Tracker State Dump ===\n");
mem_track_report_detailed();
mem_track_verify_stats();
printf("=== End Memory Tracker Dump ===\n");
}
#endif