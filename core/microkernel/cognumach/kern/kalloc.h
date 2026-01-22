#ifndef	_KERN_KALLOC_H_
#define _KERN_KALLOC_H_
#include <mach/machine/vm_types.h>
#include <vm/vm_types.h>
extern vm_offset_t kalloc (vm_size_t size);
extern void kfree (vm_offset_t data, vm_size_t size);
extern void kalloc_init (void);
#endif