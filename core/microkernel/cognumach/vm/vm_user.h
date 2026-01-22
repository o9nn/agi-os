#ifndef	_VM_VM_USER_H_
#define _VM_VM_USER_H_
#include <mach/kern_return.h>
#include <mach/std_types.h>
#include <mach/mach_types.h>
extern kern_return_t	vm_allocate(vm_map_t, vm_offset_t *, vm_size_t,
boolean_t);
extern kern_return_t	vm_deallocate(vm_map_t, vm_offset_t, vm_size_t);
extern kern_return_t	vm_inherit(vm_map_t, vm_offset_t, vm_size_t,
vm_inherit_t);
extern kern_return_t	vm_protect(vm_map_t, vm_offset_t, vm_size_t, boolean_t,
vm_prot_t);
extern kern_return_t	vm_statistics(vm_map_t, vm_statistics_data_t *);
extern kern_return_t	vm_cache_statistics(vm_map_t, vm_cache_statistics_data_t *);
extern kern_return_t	vm_read(vm_map_t, vm_address_t, vm_size_t, pointer_t *,
vm_size_t *);
extern kern_return_t	vm_write(vm_map_t, vm_address_t, pointer_t, vm_size_t);
extern kern_return_t	vm_copy(vm_map_t, vm_address_t, vm_size_t,
vm_address_t);
extern kern_return_t	vm_map(vm_map_t, vm_offset_t *, vm_size_t, vm_offset_t,
boolean_t, ipc_port_t, vm_offset_t, boolean_t,
vm_prot_t, vm_prot_t, vm_inherit_t);
#endif