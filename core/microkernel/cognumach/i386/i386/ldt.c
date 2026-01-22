#include <mach/machine/eflags.h>
#include <mach/machine/vm_types.h>
#include <mach/xen.h>
#include <intel/pmap.h>
#include <kern/debug.h>
#include "vm_param.h"
#include "seg.h"
#include "gdt.h"
#include "ldt.h"
#include "locore.h"
#include "mp_desc.h"
#include "msr.h"
#ifdef	MACH_PV_DESCRIPTORS
extern
#endif
struct real_descriptor ldt[LDTSZ];
#if defined(__x86_64__) && ! defined(USER32)
#define USER_SEGMENT_SIZEBITS SZ_64
#else
#define USER_SEGMENT_SIZEBITS SZ_32
#endif
static void
ldt_fill(struct real_descriptor *myldt, struct real_descriptor *mygdt)
{
#ifdef	MACH_PV_DESCRIPTORS
#ifdef	MACH_PV_PAGETABLES
pmap_set_page_readwrite(myldt);
#endif
#else
_fill_gdt_sys_descriptor(mygdt, KERNEL_LDT,
kvtolin(myldt), (LDTSZ * sizeof(struct real_descriptor))-1,
ACC_PL_K|ACC_LDT, 0);
#endif
#if defined(__x86_64__) && ! defined(USER32)
if (!CPU_HAS_FEATURE(CPU_FEATURE_SEP))
panic("syscall support is missing on 64 bit");
wrmsr(MSR_REG_EFER, rdmsr(MSR_REG_EFER) | MSR_EFER_SCE);
wrmsr(MSR_REG_LSTAR, (vm_offset_t)syscall64);
wrmsr(MSR_REG_STAR, ((((long)USER_CS - 16) << 16) | (long)KERNEL_CS) << 32);
wrmsr(MSR_REG_FMASK, EFL_IF | EFL_IOPL_USER);
#else
fill_ldt_gate(myldt, USER_SCALL,
(vm_offset_t)&syscall, KERNEL_CS,
ACC_PL_U|ACC_CALL_GATE, 0);
#endif
fill_ldt_descriptor(myldt, USER_CS,
VM_MIN_USER_ADDRESS,
VM_MAX_USER_ADDRESS-VM_MIN_USER_ADDRESS-4096,
ACC_PL_U|ACC_CODE_R, USER_SEGMENT_SIZEBITS);
fill_ldt_descriptor(myldt, USER_DS,
VM_MIN_USER_ADDRESS,
VM_MAX_USER_ADDRESS-VM_MIN_USER_ADDRESS-4096,
ACC_PL_U|ACC_DATA_W, USER_SEGMENT_SIZEBITS);
#ifdef	MACH_PV_DESCRIPTORS
hyp_set_ldt(myldt, LDTSZ);
#else
lldt(KERNEL_LDT);
#endif
}
void
ldt_init(void)
{
ldt_fill(ldt, gdt);
}
#if NCPUS > 1
void
ap_ldt_init(int cpu)
{
ldt_fill(mp_desc_table[cpu]->ldt, mp_gdt[cpu]);
}
#endif