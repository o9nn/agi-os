#include <kern/printf.h>
#include <kern/debug.h>
#include <kern/mach_clock.h>
#include <mach/machine/eflags.h>
#include <machine/thread.h>
#include <machine/ipl.h>
#include <machine/model_dep.h>
#include <vm/vm_page.h>
#include <vm/pmap.h>
#include <xen/xen.h>
unsigned long cr3;
void hyp_failsafe_c_callback(struct failsafe_callback_regs *regs) {
printf("Fail-Safe callback!\n");
printf("IP: %08X CS: %4X DS: %4X ES: %4X FS: %4X GS: %4X FLAGS %08X MASK %04X\n", regs->ip, regs->cs_and_mask & 0xffff, regs->ds, regs->es, regs->fs, regs->gs, regs->flags, regs->cs_and_mask >> 16);
panic("failsafe");
}
extern char return_to_iret[];
void hypclock_machine_intr(int old_ipl, void *ret_addr, struct i386_interrupt_state *regs, uint64_t delta) {
if (ret_addr == &return_to_iret) {
clock_interrupt(delta/1000,
(regs->efl & EFL_VM) ||
((regs->cs & 0x02) != 0),
old_ipl == SPL0,
regs->eip);
} else
clock_interrupt(delta/1000, FALSE, FALSE, 0);
}
void hyp_p2m_init(void) {
unsigned long nb_pfns = vm_page_table_size();
#ifdef MACH_PSEUDO_PHYS
#define P2M_PAGE_ENTRIES (PAGE_SIZE / sizeof(unsigned long))
unsigned long *l3 = (unsigned long *)phystokv(pmap_grab_page()), *l2 = NULL;
unsigned long i;
for (i = 0; i < (nb_pfns + P2M_PAGE_ENTRIES) / P2M_PAGE_ENTRIES; i++) {
if (!(i % P2M_PAGE_ENTRIES)) {
l2 = (unsigned long *) phystokv(pmap_grab_page());
l3[i / P2M_PAGE_ENTRIES] = kv_to_mfn(l2);
}
l2[i % P2M_PAGE_ENTRIES] = kv_to_mfn(&mfn_list[i * P2M_PAGE_ENTRIES]);
}
hyp_shared_info.arch.pfn_to_mfn_frame_list_list = kv_to_mfn(l3);
#endif
hyp_shared_info.arch.max_pfn = nb_pfns;
}