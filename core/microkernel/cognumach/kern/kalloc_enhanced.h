#ifndef _KERN_KALLOC_ENHANCED_H_
#define _KERN_KALLOC_ENHANCED_H_
#include <mach/machine/vm_types.h>
#include <vm/vm_types.h>
#include <kern/mem_track.h>
extern vm_offset_t kalloc_vm(vm_size_t size);
extern void kfree_vm(vm_offset_t data, vm_size_t size);
extern vm_offset_t kalloc_ipc(vm_size_t size);
extern void kfree_ipc(vm_offset_t data, vm_size_t size);
extern vm_offset_t kalloc_thread(vm_size_t size);
extern void kfree_thread(vm_offset_t data, vm_size_t size);
extern vm_offset_t kalloc_task(vm_size_t size);
extern void kfree_task(vm_offset_t data, vm_size_t size);
extern vm_offset_t kalloc_device(vm_size_t size);
extern void kfree_device(vm_offset_t data, vm_size_t size);
extern vm_offset_t kalloc_network(vm_size_t size);
extern void kfree_network(vm_offset_t data, vm_size_t size);
extern void kalloc_optimize_pools(void);
extern void kalloc_reclaim_memory(void);
extern boolean_t kalloc_check_fragmentation(void);
extern void kalloc_report_usage(void);
extern void kalloc_report_fragmentation(void);
#endif