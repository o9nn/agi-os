#ifndef	_PMAP_MACHINE_
#define _PMAP_MACHINE_	1
#ifndef	__ASSEMBLER__
#include <kern/lock.h>
#include <mach/machine/vm_param.h>
#include <mach/vm_statistics.h>
#include <mach/kern_return.h>
#include <mach/vm_prot.h>
#include <i386/proc_reg.h>
#if defined(__i386__) || defined(__x86_64__)
#define	INTEL_PGBYTES		I386_PGBYTES
#define INTEL_PGSHIFT		I386_PGSHIFT
#define	intel_btop(x)		i386_btop(x)
#define	intel_ptob(x)		i386_ptob(x)
#define	intel_round_page(x)	i386_round_page(x)
#define	intel_trunc_page(x)	i386_trunc_page(x)
#define trunc_intel_to_vm(x)	trunc_i386_to_vm(x)
#define round_intel_to_vm(x)	round_i386_to_vm(x)
#define vm_to_intel(x)		vm_to_i386(x)
#endif
typedef phys_addr_t pt_entry_t;
#define PT_ENTRY_NULL	((pt_entry_t *) 0)
#endif
#define INTEL_OFFMASK	0xfff
#ifdef __x86_64__
#define L4SHIFT		39
#define L4MASK		0x1ff
#define PDPNUM_KERNEL	(((VM_MAX_KERNEL_ADDRESS - VM_MIN_KERNEL_ADDRESS) >> PDPSHIFT) + 1)
#define PDPMASK		0x1ff
#define PDPSHIFT	30
#define PDESHIFT	21
#define PDEMASK		0x1ff
#define PTESHIFT	12
#define PTEMASK		0x1ff
#elif PAE
#define PDPNUM		4
#define PDPMASK	3
#define PDPSHIFT	30
#define PDESHIFT	21
#define PDEMASK		0x1ff
#define PTESHIFT	12
#define PTEMASK		0x1ff
#else
#define PDPNUM		1
#define PDESHIFT	22
#define PDEMASK		0x3ff
#define PTESHIFT	12
#define PTEMASK		0x3ff
#endif
#ifdef __x86_64__
#define lin2l4num(a)	(((a) >> L4SHIFT) & L4MASK)
#endif
#define lin2pdenum(a)	(((a) >> PDESHIFT) & PDEMASK)
#if PAE
#ifdef __x86_64__
#define lin2pdenum_cont(a)	(((a) >> PDESHIFT) & 0x3ff)
#else
#define lin2pdenum_cont(a)	(((a) >> PDESHIFT) & 0x7ff)
#endif
#else
#define lin2pdenum_cont(a)	lin2pdenum(a)
#endif
#if PAE || defined(__x86_64__)
#define lin2pdpnum(a)	(((a) >> PDPSHIFT) & PDPMASK)
#endif
#define pdenum2lin(a)	((vm_offset_t)(a) << PDESHIFT)
#ifdef __x86_64__
#define pagenum2lin(l4num, l3num, l2num, l1num) \
(((vm_offset_t)(l4num) << L4SHIFT) +        \
((vm_offset_t)(l3num) << PDPSHIFT) +       \
((vm_offset_t)(l2num) << PDESHIFT) +       \
((vm_offset_t)(l1num) << PTESHIFT))
#elif PAE
#define pagenum2lin(l4num, l3num, l2num, l1num) \
(((vm_offset_t)(l3num) << PDPSHIFT) +       \
((vm_offset_t)(l2num) << PDESHIFT) +       \
((vm_offset_t)(l1num) << PTESHIFT))
#else
#define pagenum2lin(l4num, l3num, l2num, l1num) \
(((vm_offset_t)(l2num) << PDESHIFT) +       \
((vm_offset_t)(l1num) << PTESHIFT))
#endif
#define ptenum(a)	(((a) >> PTESHIFT) & PTEMASK)
#define NPTES	(intel_ptob(1)/sizeof(pt_entry_t))
#define NPDES	(PDPNUM * (intel_ptob(1)/sizeof(pt_entry_t)))
#define INTEL_PTE_VALID		0x00000001
#define INTEL_PTE_WRITE		0x00000002
#define INTEL_PTE_USER		0x00000004
#define INTEL_PTE_WTHRU		0x00000008
#define INTEL_PTE_NCACHE 	0x00000010
#define INTEL_PTE_REF		0x00000020
#define INTEL_PTE_MOD		0x00000040
#define INTEL_PTE_PS		0x00000080
#ifdef	MACH_PV_PAGETABLES
#define INTEL_PTE_GLOBAL	0x00000000
#else
#define INTEL_PTE_GLOBAL	0x00000100
#endif
#define INTEL_PTE_WIRED		0x00000200
#ifdef __x86_64__
#define INTEL_PTE_PFN		0xfffffffffffff000ULL
#elif defined(PAE)
#define INTEL_PTE_PFN		0x00007ffffffff000ULL
#else
#define INTEL_PTE_PFN		0xfffff000
#endif
#define	pa_to_pte(a)		((a) & INTEL_PTE_PFN)
#ifdef	MACH_PSEUDO_PHYS
#define	pte_to_pa(p)		ma_to_pa((p) & INTEL_PTE_PFN)
#else
#define	pte_to_pa(p)		((p) & INTEL_PTE_PFN)
#endif
#define	pte_increment_pa(p)	((p) += INTEL_OFFMASK+1)
#define ptetokv(a)	(phystokv(pte_to_pa(a)))
#ifndef	__ASSEMBLER__
typedef	volatile long	cpu_set;
struct pmap {
#ifdef __x86_64__
pt_entry_t	*l4base;
#ifdef MACH_HYP
pt_entry_t	*user_l4base;
pt_entry_t	*user_pdpbase;
#endif
#elif PAE
pt_entry_t	*pdpbase;
#else
pt_entry_t	*dirbase;
#endif
int		ref_count;
decl_simple_lock_data(,lock)
struct pmap_statistics	stats;
cpu_set		cpus_using;
};
typedef struct pmap	*pmap_t;
#define PMAP_NULL	((pmap_t) 0)
#ifdef	MACH_PV_PAGETABLES
extern void pmap_set_page_readwrite(void *addr);
extern void pmap_set_page_readonly(void *addr);
extern void pmap_set_page_readonly_init(void *addr);
extern void pmap_map_mfn(void *addr, unsigned long mfn);
extern void pmap_clear_bootstrap_pagetable(pt_entry_t *addr);
#endif
#ifdef __x86_64__
#ifdef MACH_HYP
#define	set_pmap(pmap)	\
MACRO_BEGIN					\
set_cr3(kvtophys((vm_offset_t)(pmap)->l4base)); \
if (pmap->user_l4base) \
if (!hyp_set_user_cr3(kvtophys((vm_offset_t)(pmap)->user_l4base))) \
panic("set_user_cr3"); \
MACRO_END
#else
#define	set_pmap(pmap)	set_cr3(kvtophys((vm_offset_t)(pmap)->l4base))
#endif
#elif PAE
#define	set_pmap(pmap)	set_cr3(kvtophys((vm_offset_t)(pmap)->pdpbase))
#else
#define	set_pmap(pmap)	set_cr3(kvtophys((vm_offset_t)(pmap)->dirbase))
#endif
typedef struct {
pt_entry_t	*entry;
vm_offset_t	vaddr;
} pmap_mapwindow_t;
extern pmap_mapwindow_t *pmap_get_mapwindow(pt_entry_t entry);
extern void pmap_put_mapwindow(pmap_mapwindow_t *map);
#define PMAP_NMAPWINDOWS 2
#if	NCPUS > 1
extern cpu_set		cpus_active;
extern cpu_set		cpus_idle;
extern volatile
boolean_t	cpu_update_needed[NCPUS];
void		process_pmap_updates(pmap_t);
extern	pmap_t	kernel_pmap;
#endif
void		pmap_update_interrupt(void);
pt_entry_t *pmap_pte(const pmap_t pmap, vm_offset_t addr);
#if	NCPUS > 1
#define	PMAP_ACTIVATE_KERNEL(my_cpu)					\
MACRO_BEGIN 								\
\
\
i_bit_clear((my_cpu), &cpus_active);				\
\
\
simple_lock(&kernel_pmap->lock);				\
\
\
if (cpu_update_needed[(my_cpu)])				\
process_pmap_updates(kernel_pmap);				\
\
\
i_bit_set((my_cpu), &kernel_pmap->cpus_using);			\
\
\
i_bit_set((my_cpu), &cpus_active);				\
\
simple_unlock(&kernel_pmap->lock);				\
MACRO_END
#define	PMAP_DEACTIVATE_KERNEL(my_cpu)					\
MACRO_BEGIN								\
\
i_bit_clear((my_cpu), &kernel_pmap->cpus_using);		\
MACRO_END
#define PMAP_ACTIVATE_USER(pmap, th, my_cpu)				\
MACRO_BEGIN								\
pmap_t		tpmap = (pmap);					\
\
if (tpmap == kernel_pmap) {					\
\
set_pmap(tpmap);						\
}								\
else {								\
\
i_bit_clear((my_cpu), &cpus_active);			\
\
\
simple_lock(&tpmap->lock);					\
\
\
set_pmap(tpmap);						\
\
\
i_bit_set((my_cpu), &tpmap->cpus_using);			\
\
\
i_bit_set((my_cpu), &cpus_active);				\
\
simple_unlock(&tpmap->lock);				\
}								\
MACRO_END
#define PMAP_DEACTIVATE_USER(pmap, thread, my_cpu)			\
MACRO_BEGIN								\
pmap_t		tpmap = (pmap);					\
\
\
if (tpmap != kernel_pmap) {					\
\
i_bit_clear((my_cpu), &(pmap)->cpus_using);			\
}								\
MACRO_END
#define MARK_CPU_IDLE(my_cpu)						\
MACRO_BEGIN								\
\
int	s = splvm();						\
i_bit_set((my_cpu), &cpus_idle);				\
i_bit_clear((my_cpu), &cpus_active);				\
splx(s);							\
MACRO_END
#define MARK_CPU_ACTIVE(my_cpu)						\
MACRO_BEGIN								\
\
int	s = splvm();						\
\
i_bit_clear((my_cpu), &cpus_idle);				\
__sync_synchronize();						\
\
if (cpu_update_needed[(my_cpu)])				\
pmap_update_interrupt();					\
\
\
i_bit_set((my_cpu), &cpus_active);				\
splx(s);							\
MACRO_END
#else
#define	PMAP_ACTIVATE_KERNEL(my_cpu)					\
MACRO_BEGIN								\
(void) (my_cpu);						\
kernel_pmap->cpus_using = TRUE;					\
MACRO_END
#define	PMAP_DEACTIVATE_KERNEL(my_cpu)					\
MACRO_BEGIN\								\
(void) (my_cpu);						\
kernel_pmap->cpus_using = FALSE;				\
MACRO_END
#define	PMAP_ACTIVATE_USER(pmap, th, my_cpu)				\
MACRO_BEGIN								\
pmap_t		tpmap = (pmap);					\
(void) (th);							\
(void) (my_cpu);						\
\
set_pmap(tpmap);						\
if (tpmap != kernel_pmap) {					\
tpmap->cpus_using = TRUE;					\
}								\
MACRO_END
#define PMAP_DEACTIVATE_USER(pmap, thread, cpu)				\
MACRO_BEGIN								\
(void) (thread);						\
(void) (cpu);							\
if ((pmap) != kernel_pmap)					\
(pmap)->cpus_using = FALSE;					\
MACRO_END
#endif
#define PMAP_CONTEXT(pmap, thread)
#define	pmap_kernel()			(kernel_pmap)
#define pmap_resident_count(pmap)	((pmap)->stats.resident_count)
#define pmap_phys_address(frame)	((intel_ptob((phys_addr_t) frame)))
#define pmap_phys_to_frame(phys)	((int) (intel_btop(phys)))
#define	pmap_copy(dst_pmap,src_pmap,dst_addr,len,src_addr)
#define	pmap_attribute(pmap,addr,size,attr,value) \
(KERN_INVALID_ADDRESS)
extern pt_entry_t *kernel_page_dir;
extern vm_offset_t kernel_virtual_start;
extern vm_offset_t kernel_virtual_end;
extern void pmap_bootstrap(void);
extern void pmap_set_page_dir(void);
extern void pmap_make_temporary_mapping(void);
extern void pmap_remove_temporary_mapping(void);
extern void pmap_unmap_page_zero (void);
extern void pmap_zero_page (phys_addr_t);
extern void pmap_copy_page (phys_addr_t, phys_addr_t);
extern void
copy_to_phys(
vm_offset_t 	src_addr_v,
phys_addr_t 	dst_addr_p,
int 		count);
extern void
copy_from_phys(
phys_addr_t 	src_addr_p,
vm_offset_t 	dst_addr_v,
int 		count);
extern phys_addr_t kvtophys (vm_offset_t);
#if NCPUS > 1
void signal_cpus(
cpu_set		use_list,
pmap_t		pmap,
vm_offset_t	start,
vm_offset_t	end);
#endif
#endif
#endif