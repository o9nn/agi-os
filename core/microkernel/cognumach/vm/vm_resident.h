#ifndef _VM_RESIDENT_H_
#define _VM_RESIDENT_H_
#include <mach/std_types.h>
extern void vm_page_replace (
vm_page_t mem,
vm_object_t object,
vm_offset_t offset);
extern boolean_t vm_page_readahead_enabled;
extern int vm_page_readahead_max;
extern int vm_page_readahead_min;
#endif