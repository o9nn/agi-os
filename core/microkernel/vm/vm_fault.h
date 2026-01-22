#ifndef	_VM_VM_FAULT_H_
#define _VM_VM_FAULT_H_
#include <mach/kern_return.h>
#include <mach/vm_prot.h>
#include <vm/vm_map.h>
#include <vm/vm_types.h>
typedef	kern_return_t	vm_fault_return_t;
#define VM_FAULT_SUCCESS		0
#define VM_FAULT_RETRY			1
#define VM_FAULT_INTERRUPTED		2
#define VM_FAULT_MEMORY_SHORTAGE 	3
#define VM_FAULT_FICTITIOUS_SHORTAGE 	4
#define VM_FAULT_MEMORY_ERROR		5
typedef void (*vm_fault_continuation_t)(kern_return_t);
#define vm_fault_no_continuation ((vm_fault_continuation_t)0)
extern void vm_fault_init(void);
extern vm_fault_return_t vm_fault_page(vm_object_t, vm_offset_t, vm_prot_t,
boolean_t, boolean_t, vm_prot_t *,
vm_page_t *, vm_page_t *, boolean_t,
continuation_t);
extern void		vm_fault_cleanup(vm_object_t, vm_page_t);
extern kern_return_t	vm_fault(vm_map_t, vm_offset_t, vm_prot_t, boolean_t,
boolean_t, vm_fault_continuation_t);
extern void		vm_fault_wire(vm_map_t, vm_map_entry_t);
extern void		vm_fault_unwire(vm_map_t, vm_map_entry_t);
extern kern_return_t	vm_fault_copy(vm_object_t, vm_offset_t, vm_size_t *,
vm_object_t, vm_offset_t, vm_map_t,
vm_map_version_t *, boolean_t);
kern_return_t vm_fault_wire_fast(
vm_map_t	map,
vm_offset_t	va,
vm_map_entry_t	entry);
#endif