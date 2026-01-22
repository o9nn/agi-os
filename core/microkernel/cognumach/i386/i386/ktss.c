#include "vm_param.h"
#include "seg.h"
#include "gdt.h"
#include "ktss.h"
#include "mp_desc.h"
struct task_tss ktss;
static void
ktss_fill(struct task_tss *myktss, struct real_descriptor *mygdt)
{
static int exception_stack[1024];
#ifdef __x86_64__
static int double_fault_stack[1024];
#endif
#ifdef	MACH_RING1
if (hyp_stack_switch(KERNEL_DS, (unsigned long)(exception_stack+1024)))
panic("couldn't register exception stack\n");
#else
_fill_gdt_sys_descriptor(mygdt, KERNEL_TSS,
kvtolin(myktss), sizeof(struct task_tss) - 1,
ACC_PL_K|ACC_TSS, 0);
#ifdef __x86_64__
myktss->tss.rsp0 = (unsigned long)(exception_stack+1024);
myktss->tss.ist1 = (unsigned long)(double_fault_stack+1024);
#else
myktss->tss.ss0 = KERNEL_DS;
myktss->tss.esp0 = (unsigned long)(exception_stack+1024);
#endif
myktss->tss.io_bit_map_offset = IOPB_INVAL;
myktss->barrier = 0xff;
ltr(KERNEL_TSS);
#endif
}
void
ktss_init(void)
{
ktss_fill(&ktss, gdt);
}
#if NCPUS > 1
void
ap_ktss_init(int cpu)
{
ktss_fill(&mp_desc_table[cpu]->ktss, mp_gdt[cpu]);
}
#endif