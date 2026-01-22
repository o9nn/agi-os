#ifndef	_VM_VM_KERN_H_
#define _VM_VM_KERN_H_
#include <mach/kern_return.h>
#include <vm/vm_map.h>
extern kern_return_t    projected_buffer_allocate(vm_map_t, vm_size_t, int,
vm_offset_t *, vm_offset_t *,
vm_prot_t, vm_inherit_t);
extern kern_return_t    projected_buffer_deallocate(vm_map_t, vm_offset_t,
vm_offset_t);
extern kern_return_t    projected_buffer_map(vm_map_t, vm_offset_t, vm_size_t,
vm_offset_t *, vm_prot_t,
vm_inherit_t);
extern kern_return_t    projected_buffer_collect(vm_map_t);
extern void		kmem_init(vm_offset_t, vm_offset_t);
extern kern_return_t	kmem_alloc(vm_map_t, vm_offset_t *, vm_size_t);
extern kern_return_t	kmem_alloc_pageable(vm_map_t, vm_offset_t *,
vm_size_t);
extern kern_return_t	kmem_valloc(vm_map_t, vm_offset_t *, vm_size_t);
extern kern_return_t	kmem_alloc_wired(vm_map_t, vm_offset_t *, vm_size_t);
extern kern_return_t	kmem_alloc_aligned(vm_map_t, vm_offset_t *, vm_size_t);
extern void*		kmem_map_aligned_table(phys_addr_t, vm_size_t, int);
extern void		kmem_free(vm_map_t, vm_offset_t, vm_size_t);
extern void		kmem_submap(vm_map_t, vm_map_t, vm_offset_t *,
vm_offset_t *, vm_size_t);
extern kern_return_t	kmem_io_map_copyout(vm_map_t, vm_offset_t *,
vm_offset_t *, vm_size_t *,
vm_map_copy_t, vm_size_t);
extern void		kmem_io_map_deallocate(vm_map_t, vm_offset_t,
vm_size_t);
extern int
copyinmap (vm_map_t map, char *fromaddr, char *toaddr, int length);
extern int
copyoutmap (vm_map_t map, char *fromaddr, char *toaddr, int length);
extern vm_map_t	kernel_map;
extern vm_map_t	kernel_pageable_map;
extern vm_map_t ipc_kernel_map;
extern boolean_t projected_buffer_in_range(
vm_map_t map,
vm_offset_t start,
vm_offset_t end);
extern void kmem_alloc_pages(
vm_object_t	object,
vm_offset_t	offset,
vm_offset_t	start,
vm_offset_t	end,
vm_prot_t	protection);
extern void kmem_remap_pages(
vm_object_t	object,
vm_offset_t	offset,
vm_offset_t	start,
vm_offset_t	end,
vm_prot_t	protection);
#endif