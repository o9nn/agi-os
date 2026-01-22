#include <kern/kalloc_enhanced.h>
#include <kern/kalloc.h>
#include <kern/mem_track.h>
#include <kern/mem_optimize.h>
#include <kern/slab.h>
#include <kern/printf.h>
#include <mach/vm_param.h>
#include <mach/kern_return.h>
static vm_offset_t kalloc_typed(vm_size_t size, mem_type_t type)
{
vm_offset_t addr;
if (mem_opt_predict_allocation_failure_enhanced(size)) {
printf("Predicted allocation failure for size %u, running proactive management...\n", (unsigned)size);
mem_opt_proactive_management();
}
addr = kalloc(size);
if (addr != 0) {
mem_track_free(MEM_TYPE_GENERAL, size);
mem_track_alloc(type, size);
} else {
mem_track_alloc_failed(type, size);
printf("Allocation failed for size %u, attempting emergency optimization\n", (unsigned)size);
mem_opt_handle_memory_pressure();
addr = kalloc(size);
if (addr != 0) {
mem_track_free(MEM_TYPE_GENERAL, size);
mem_track_alloc(type, size);
printf("Allocation succeeded after optimization\n");
}
}
return addr;
}
static void kfree_typed(vm_offset_t data, vm_size_t size, mem_type_t type)
{
if ((data == 0) || (size == 0))
return;
mem_track_free(type, size);
mem_track_alloc(MEM_TYPE_GENERAL, size);
kfree(data, size);
}
vm_offset_t kalloc_vm(vm_size_t size)
{
return kalloc_typed(size, MEM_TYPE_VM_OBJECTS);
}
void kfree_vm(vm_offset_t data, vm_size_t size)
{
kfree_typed(data, size, MEM_TYPE_VM_OBJECTS);
}
vm_offset_t kalloc_ipc(vm_size_t size)
{
return kalloc_typed(size, MEM_TYPE_IPC);
}
void kfree_ipc(vm_offset_t data, vm_size_t size)
{
kfree_typed(data, size, MEM_TYPE_IPC);
}
vm_offset_t kalloc_thread(vm_size_t size)
{
return kalloc_typed(size, MEM_TYPE_THREADS);
}
void kfree_thread(vm_offset_t data, vm_size_t size)
{
kfree_typed(data, size, MEM_TYPE_THREADS);
}
vm_offset_t kalloc_task(vm_size_t size)
{
return kalloc_typed(size, MEM_TYPE_TASKS);
}
void kfree_task(vm_offset_t data, vm_size_t size)
{
kfree_typed(data, size, MEM_TYPE_TASKS);
}
vm_offset_t kalloc_device(vm_size_t size)
{
return kalloc_typed(size, MEM_TYPE_DEVICE);
}
void kfree_device(vm_offset_t data, vm_size_t size)
{
kfree_typed(data, size, MEM_TYPE_DEVICE);
}
vm_offset_t kalloc_network(vm_size_t size)
{
return kalloc_typed(size, MEM_TYPE_NETWORK);
}
void kfree_network(vm_offset_t data, vm_size_t size)
{
kfree_typed(data, size, MEM_TYPE_NETWORK);
}
void kalloc_optimize_pools(void)
{
printf("Optimizing memory pools...\n");
if (mem_opt_should_compact()) {
mem_opt_compact_memory();
} else {
slab_collect();
mem_opt_defragment_slabs();
}
if (mem_track_check_pressure()) {
printf("Memory pressure detected during pool optimization\n");
mem_track_report_usage();
mem_opt_report_optimization();
}
printf("Memory pool optimization complete\n");
}
void kalloc_reclaim_memory(void)
{
printf("Reclaiming unused memory...\n");
mem_opt_emergency_reclaim();
mem_track_report_usage();
mem_opt_report_optimization();
printf("Memory reclamation complete\n");
}
boolean_t kalloc_check_fragmentation(void)
{
return mem_opt_is_heavily_fragmented();
}
void kalloc_report_usage(void)
{
printf("\n=== Enhanced Kalloc Memory Report ===\n");
mem_track_report_detailed();
printf("\nSlab Allocator Statistics:\n");
slab_info();
printf("\n=== End Kalloc Memory Report ===\n");
}
void kalloc_report_fragmentation(void)
{
boolean_t fragmented = kalloc_check_fragmentation();
printf("\nMemory Fragmentation Analysis:\n");
printf("  Fragmentation detected: %s\n", fragmented ? "YES" : "NO");
if (fragmented) {
printf("  Recommendation: Consider running kalloc_reclaim_memory()\n");
}
struct mem_stats stats;
if (mem_track_get_stats(MEM_TYPE_GENERAL, &stats) == KERN_SUCCESS) {
uint32_t large_pct = 0;
if (stats.alloc_count > 0) {
large_pct = (stats.large_allocs * 100) / stats.alloc_count;
}
printf("  Large allocation ratio: %u%% (%u large / %llu total)\n",
large_pct, stats.large_allocs, stats.alloc_count);
if (stats.peak_bytes > 0) {
uint32_t usage_pct = (uint32_t)((stats.current_bytes * 100) / stats.peak_bytes);
printf("  Current vs peak usage: %u%% (%luk / %luk)\n",
usage_pct,
(unsigned long)(stats.current_bytes >> 10),
(unsigned long)(stats.peak_bytes >> 10));
}
}
}