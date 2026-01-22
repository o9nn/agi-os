#ifndef _VM_PMAP_H_
#define _VM_PMAP_H_
#include <machine/pmap.h>
#include <mach/machine/vm_types.h>
#include <mach/vm_prot.h>
#include <mach/boolean.h>
#include <kern/thread.h>
extern vm_offset_t pmap_steal_memory(vm_size_t);
extern void pmap_init(void);
#ifndef MACHINE_PAGES
extern void pmap_virtual_space(vm_offset_t *, vm_offset_t *);
#endif
pmap_t pmap_create(vm_size_t size);
#ifndef pmap_kernel
extern pmap_t pmap_kernel(void);
#endif
extern void pmap_reference(pmap_t pmap);
extern void pmap_destroy(pmap_t pmap);
extern void pmap_enter(pmap_t pmap, vm_offset_t va, phys_addr_t pa,
vm_prot_t prot, boolean_t wired);
void pmap_remove(pmap_t pmap, vm_offset_t sva, vm_offset_t eva);
void pmap_protect(pmap_t pmap, vm_offset_t sva, vm_offset_t eva, vm_prot_t prot);
extern void pmap_activate(pmap_t, thread_t, int);
extern void pmap_deactivate(pmap_t, thread_t, int);
void pmap_page_protect(phys_addr_t pa, vm_prot_t prot);
void pmap_clear_reference(phys_addr_t pa);
#ifndef pmap_is_referenced
boolean_t pmap_is_referenced(phys_addr_t pa);
#endif
void pmap_clear_modify(phys_addr_t pa);
boolean_t pmap_is_modified(phys_addr_t pa);
extern phys_addr_t pmap_extract(pmap_t, vm_offset_t);
extern void pmap_collect(pmap_t);
int pmap_whatis(pmap_t, vm_offset_t);
extern void pmap_change_wiring(pmap_t, vm_offset_t, boolean_t);
#ifndef pmap_copy
extern void pmap_copy(pmap_t, pmap_t, vm_offset_t, vm_size_t,
vm_offset_t);
#endif
#ifndef pmap_attribute
extern kern_return_t pmap_attribute(void);
#endif
extern vm_offset_t pmap_grab_page (void);
extern void pmap_pageable(
pmap_t pmap,
vm_offset_t start,
vm_offset_t end,
boolean_t pageable);
extern vm_offset_t pmap_map_bd(
vm_offset_t virt,
phys_addr_t start,
phys_addr_t end,
vm_prot_t prot);
#ifndef PMAP_ACTIVATE_USER
#define PMAP_ACTIVATE_USER(pmap, thread, cpu) \
MACRO_BEGIN \
if ((pmap) != kernel_pmap) \
PMAP_ACTIVATE(pmap, thread, cpu); \
MACRO_END
#endif
#ifndef PMAP_DEACTIVATE_USER
#define PMAP_DEACTIVATE_USER(pmap, thread, cpu) \
MACRO_BEGIN \
if ((pmap) != kernel_pmap) \
PMAP_DEACTIVATE(pmap, thread, cpu); \
MACRO_END
#endif
#ifndef PMAP_ACTIVATE_KERNEL
#define PMAP_ACTIVATE_KERNEL(cpu) \
PMAP_ACTIVATE(kernel_pmap, THREAD_NULL, cpu)
#endif
#ifndef PMAP_DEACTIVATE_KERNEL
#define PMAP_DEACTIVATE_KERNEL(cpu) \
PMAP_DEACTIVATE(kernel_pmap, THREAD_NULL, cpu)
#endif
extern pmap_t kernel_pmap;
#endif