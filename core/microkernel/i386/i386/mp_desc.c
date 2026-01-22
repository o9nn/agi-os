#include <kern/assert.h>
#include <kern/cpu_number.h>
#include <kern/debug.h>
#include <kern/printf.h>
#include <kern/smp.h>
#include <kern/startup.h>
#include <kern/kmutex.h>
#include <mach/machine.h>
#include <mach/xen.h>
#include <vm/vm_kern.h>
#include <i386/mp_desc.h>
#include <i386/lock.h>
#include <i386/apic.h>
#include <i386/locore.h>
#include <i386/fpu.h>
#include <i386/gdt.h>
#include <i386at/idt.h>
#include <i386at/int_init.h>
#include <i386/cpu.h>
#include <i386/smp.h>
#include <i386at/model_dep.h>
#include <machine/ktss.h>
#include <machine/smp.h>
#include <machine/tss.h>
#include <machine/io_perm.h>
#include <machine/vm_param.h>
#include <i386at/acpi_parse_apic.h>
#include <string.h>
vm_offset_t	int_stack_top[NCPUS];
vm_offset_t	int_stack_base[NCPUS];
#ifdef MACH_LDEBUG
unsigned long	in_interrupt[NCPUS];
#endif
uint8_t solid_intstack[NCPUS*INTSTACK_SIZE] __aligned(NCPUS*INTSTACK_SIZE);
void
interrupt_stack_alloc(void)
{
int i;
for (i = 0; i < NCPUS; i++) {
int_stack_base[i] = (vm_offset_t) &solid_intstack[i * INTSTACK_SIZE];
int_stack_top[i] = (vm_offset_t) &solid_intstack[(i + 1) * INTSTACK_SIZE] - 4;
}
}
#if	NCPUS > 1
phys_addr_t apboot_addr;
extern void *apboot, *apbootend;
extern volatile ApicLocalUnit* lapic;
struct mp_desc_table	*mp_desc_table[NCPUS] = { 0 };
struct task_tss		*mp_ktss[NCPUS] = { 0 };
struct real_descriptor	*mp_gdt[NCPUS] = { 0 };
extern struct real_gate		idt[IDTSZ];
extern struct real_descriptor	gdt[GDTSZ];
extern struct real_descriptor	ldt[LDTSZ];
int
mp_desc_init(int mycpu)
{
struct mp_desc_table *mpt;
vm_offset_t mem;
if (mycpu == 0) {
mp_ktss[mycpu] = (struct task_tss *) &ktss;
mp_gdt[mycpu] = gdt;
return 0;
}
else {
if (!init_alloc_aligned(sizeof(struct mp_desc_table), &mem))
panic("not enough memory for descriptor tables");
mpt = (struct mp_desc_table *)phystokv(mem);
mp_desc_table[mycpu] = mpt;
mp_ktss[mycpu] = &mpt->ktss;
mp_gdt[mycpu] = mpt->gdt;
memset(mpt->idt, 0, sizeof(idt));
memset(mpt->gdt, 0, sizeof(gdt));
memset(mpt->ldt, 0, sizeof(ldt));
memset(&mpt->ktss, 0, sizeof(struct task_tss));
return mycpu;
}
}
int simple_lock_pause_loop = 100;
unsigned int simple_lock_pause_count = 0;
void
simple_lock_pause(void)
{
static volatile int dummy;
int i;
simple_lock_pause_count++;
for (i = 0; i < simple_lock_pause_loop; i++)
dummy++;
}
kern_return_t
cpu_control(int cpu, const int *info, unsigned int count)
{
printf("cpu_control(%d, %p, %d) not implemented\n",
cpu, info, count);
return KERN_FAILURE;
}
void
interrupt_processor(int cpu)
{
smp_pmap_update(APIC_LOGICAL_ID(cpu));
}
static void
paging_enable(void)
{
#ifndef MACH_HYP
#if PAE
set_cr4(get_cr4() | CR4_PAE);
#endif
set_cr0(get_cr0() | CR0_PG );
set_cr0(get_cr0() & ~(CR0_CD | CR0_NW));
if (CPU_HAS_FEATURE(CPU_FEATURE_PGE))
set_cr4(get_cr4() | CR4_PGE);
#endif
}
void
cpu_setup(int cpu)
{
pmap_set_page_dir();
printf("AP=(%u) pagedir done\n", cpu);
paging_enable();
flush_instr_queue();
printf("AP=(%u) paging done\n", cpu);
init_percpu(cpu);
mp_desc_init(cpu);
printf("AP=(%u) mpdesc done\n", cpu);
ap_gdt_init(cpu);
printf("AP=(%u) gdt done\n", cpu);
ap_idt_init(cpu);
printf("AP=(%u) idt done\n", cpu);
ap_int_init(cpu);
printf("AP=(%u) int done\n", cpu);
ap_ldt_init(cpu);
printf("AP=(%u) ldt done\n", cpu);
ap_ktss_init(cpu);
printf("AP=(%u) ktss done\n", cpu);
machine_slot[cpu].cpu_subtype = CPU_SUBTYPE_AT386;
machine_slot[cpu].cpu_type = machine_slot[0].cpu_type;
init_fpu();
lapic_setup();
lapic_enable();
cpu_launch_first_thread(THREAD_NULL);
}
void
cpu_ap_main()
{
int cpu = cpu_number();
assert(cpu > 0);
cpu_setup(cpu);
}
void
start_other_cpus(void)
{
int ncpus = smp_get_numcpus();
if (ncpus == 1)
return;
memcpy((void*) phystokv(apboot_addr), (void*) &apboot,
(uint32_t)&apbootend - (uint32_t)&apboot);
unsigned cpu = cpu_number_slow();
assert (cpu == 0);
splhigh();
lapic_disable();
pmap_make_temporary_mapping();
for (cpu = 1; cpu < ncpus; cpu++) {
machine_slot[cpu].running = FALSE;
}
smp_startup_cpus(apic_get_current_cpu(), apboot_addr);
for (cpu = 1; cpu < ncpus; cpu++) {
printf("Waiting for AP %d\n", cpu);
do {
cpu_pause();
} while (machine_slot[cpu].running == FALSE);
}
printf("BSP: Completed SMP init\n");
pmap_remove_temporary_mapping();
ncpus = (ncpus < APIC_LOGICAL_CPU_GROUPS) ? ncpus : APIC_LOGICAL_CPU_GROUPS;
for (cpu = 1; cpu < ncpus; cpu++) {
interrupt_processor(cpu);
}
lapic_enable();
}
#endif