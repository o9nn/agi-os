#ifndef	_MACH_VM_PARAM_H_
#define _MACH_VM_PARAM_H_
#include <mach/machine/vm_param.h>
#include <mach/machine/vm_types.h>
#ifdef PAGE_SHIFT
#ifndef PAGE_SIZE
#define PAGE_SIZE (1 << PAGE_SHIFT)
#endif
#ifndef PAGE_MASK
#define PAGE_MASK (PAGE_SIZE-1)
#endif
#define atop(x)		(((vm_size_t)(x)) >> PAGE_SHIFT)
#define ptoa(x)		((vm_offset_t)((x) << PAGE_SHIFT))
#define round_page(x)	((vm_offset_t)((((vm_offset_t)(x)) + PAGE_MASK) & ~PAGE_MASK))
#define trunc_page(x)	((vm_offset_t)(((vm_offset_t)(x)) & ~PAGE_MASK))
#define round_phys(x)	((phys_addr_t)((((phys_addr_t)(x)) + PAGE_MASK) & ~PAGE_MASK))
#define trunc_phys(x)	((phys_addr_t)(((phys_addr_t)(x)) & ~PAGE_MASK))
#define	page_aligned(x)	((((vm_offset_t) (x)) & PAGE_MASK) == 0)
#define	phys_aligned(x)	((((phys_addr_t) (x)) & PAGE_MASK) == 0)
#endif
#endif