#ifndef _READ_FAULT_H_
#define _READ_FAULT_H_
#include <mach/std_types.h>
extern kern_return_t intel_read_fault(
vm_map_t map,
vm_offset_t vaddr);
#endif