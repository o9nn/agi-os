#include <i386/apic.h>
#include <i386/cpu.h>
#include <i386at/idt.h>
#include <string.h>
#include <vm/vm_kern.h>
#include <kern/printf.h>
#include <kern/kalloc.h>
uint32_t hpet_period_nsec;
static ApicLocalUnit dummy_lapic = {0};
volatile ApicLocalUnit* lapic = &dummy_lapic;
int cpu_id_lut[UINT8_MAX + 1] = {0};
ApicInfo apic_data;
uint8_t apic_id_mask = 0xf;
int
apic_data_init(void)
{
apic_data.cpu_lapic_list = NULL;
apic_data.ncpus = 0;
apic_data.nioapics = 0;
apic_data.nirqoverride = 0;
apic_data.cpu_lapic_list = (uint16_t*) kalloc(NCPUS*sizeof(uint16_t));
if (apic_data.cpu_lapic_list == NULL)
return -1;
return 0;
}
void
apic_lapic_init(ApicLocalUnit* lapic_ptr)
{
lapic = lapic_ptr;
}
void
apic_add_cpu(uint16_t apic_id)
{
apic_data.cpu_lapic_list[apic_data.ncpus] = apic_id;
apic_data.ncpus++;
}
void
apic_add_ioapic(IoApicData ioapic)
{
apic_data.ioapic_list[apic_data.nioapics] = ioapic;
apic_data.nioapics++;
}
void
apic_add_irq_override(IrqOverrideData irq_over)
{
apic_data.irq_override_list[apic_data.nirqoverride] = irq_over;
apic_data.nirqoverride++;
}
IrqOverrideData *
acpi_get_irq_override(uint8_t pin)
{
int i;
for (i = 0; i < apic_data.nirqoverride; i++) {
if (apic_data.irq_override_list[i].irq == pin) {
return &apic_data.irq_override_list[i];
}
}
return NULL;
}
int
apic_get_cpu_apic_id(int kernel_id)
{
if (kernel_id >= NCPUS)
return -1;
return apic_data.cpu_lapic_list[kernel_id];
}
int
apic_get_cpu_kernel_id(uint16_t apic_id)
{
return cpu_id_lut[apic_id];
}
volatile ApicLocalUnit*
apic_get_lapic(void)
{
return lapic;
}
struct IoApicData *
apic_get_ioapic(int kernel_id)
{
if (kernel_id < MAX_IOAPICS)
return &apic_data.ioapic_list[kernel_id];
return NULL;
}
uint8_t
apic_get_numcpus(void)
{
return apic_data.ncpus;
}
uint8_t
apic_get_num_ioapics(void)
{
return apic_data.nioapics;
}
int
apic_get_total_gsis(void)
{
int id;
int gsis = 0;
for (id = 0; id < apic_get_num_ioapics(); id++)
gsis += apic_get_ioapic(id)->ngsis;
return gsis;
}
int
apic_get_current_cpu(void)
{
unsigned int eax, ebx, ecx, edx;
eax = 1;
ecx = 0;
cpuid(eax, ebx, ecx, edx);
return (ebx >> 24) & apic_id_mask;
}
int apic_refit_cpulist(void)
{
uint16_t* old_list = apic_data.cpu_lapic_list;
uint16_t* new_list = NULL;
if (old_list == NULL)
return -1;
new_list = (uint16_t*) kalloc(apic_data.ncpus*sizeof(uint16_t));
if (new_list == NULL)
return -1;
for (int i = 0; i < apic_data.ncpus; i++)
new_list[i] = old_list[i];
apic_data.cpu_lapic_list = new_list;
kfree((vm_offset_t) old_list, NCPUS*sizeof(uint16_t));
return 0;
}
void apic_generate_cpu_id_lut(void)
{
int i, apic_id;
for (i = 0; i < apic_data.ncpus; i++) {
apic_id = apic_get_cpu_apic_id(i);
if (apic_id >= 0)
cpu_id_lut[apic_id] = i;
else
printf("apic_get_cpu_apic_id(%d) failed...\n", i);
}
}
void apic_print_info(void)
{
int i;
int ncpus, nioapics;
ncpus = apic_get_numcpus();
nioapics = apic_get_num_ioapics();
uint16_t lapic_id;
uint16_t ioapic_id;
IoApicData *ioapic;
printf("CPUS:\n");
for (i = 0; i < ncpus; i++) {
lapic_id = apic_get_cpu_apic_id(i);
printf(" CPU %d - APIC ID %x - addr=0x%p\n", i, lapic_id, apic_get_lapic());
}
printf("IOAPICS:\n");
for (i = 0; i < nioapics; i++) {
ioapic = apic_get_ioapic(i);
if (!ioapic) {
printf("ERROR: invalid IOAPIC ID %x\n", i);
} else {
ioapic_id = ioapic->apic_id;
printf(" IOAPIC %d - APIC ID %x - addr=0x%p\n", i, ioapic_id, ioapic->ioapic);
}
}
}
void apic_send_ipi(unsigned dest_shorthand, unsigned deliv_mode, unsigned dest_mode, unsigned level, unsigned trig_mode, unsigned vector, unsigned dest_id)
{
IcrLReg icrl_values;
IcrHReg icrh_values;
icrl_values.r = lapic->icr_low.r;
icrh_values.r = lapic->icr_high.r;
icrl_values.destination_shorthand = dest_shorthand;
icrl_values.delivery_mode = deliv_mode;
icrl_values.destination_mode = dest_mode;
icrl_values.level = level;
icrl_values.trigger_mode = trig_mode;
icrl_values.vector = vector;
icrh_values.destination_field = dest_id;
lapic->icr_high.r = icrh_values.r;
lapic->icr_low.r = icrl_values.r;
}
void
lapic_enable(void)
{
lapic->spurious_vector.r |= LAPIC_ENABLE;
}
void
lapic_disable(void)
{
lapic->spurious_vector.r &= ~LAPIC_ENABLE;
}
void
fix_apic_id_mask(void)
{
if (lapic->version.r & APIC_VERSION_HAS_EXT_APIC_SPACE) {
if (lapic->extended_feature.r & APIC_EXT_FEATURE_HAS_8BITID) {
if (!(lapic->extended_control.r & APIC_EXT_CTRL_ENABLE_8BITID)) {
printf("WARNING: Only 4 bit APIC ids\n");
apic_id_mask = 0xf;
return;
}
}
}
printf("8 bit APIC ids\n");
apic_id_mask = 0xff;
}
void
lapic_setup(void)
{
unsigned long flags;
volatile uint32_t dummy;
int cpu = cpu_number_slow();
cpu_intr_save(&flags);
dummy = lapic->dest_format.r;
lapic->dest_format.r = 0xffffffff;
dummy = lapic->logical_dest.r;
lapic->logical_dest.r = APIC_LOGICAL_ID(cpu) << 24;
dummy = lapic->lvt_lint0.r;
lapic->lvt_lint0.r = dummy | LAPIC_DISABLE;
dummy = lapic->lvt_lint1.r;
lapic->lvt_lint1.r = dummy | LAPIC_DISABLE;
dummy = lapic->lvt_performance_monitor.r;
lapic->lvt_performance_monitor.r = dummy | LAPIC_DISABLE;
if (cpu > 0)
{
dummy = lapic->lvt_timer.r;
lapic->lvt_timer.r = dummy | LAPIC_DISABLE;
}
dummy = lapic->task_pri.r;
lapic->task_pri.r = 0;
dummy = lapic->spurious_vector.r;
lapic->spurious_vector.r = IOAPIC_SPURIOUS_BASE
| LAPIC_ENABLE_DIRECTED_EOI;
lapic->error_status.r = 0;
cpu_intr_restore(flags);
}
void
lapic_eoi(void)
{
lapic->eoi.r = 0;
}
#define HPET32(x) *((volatile uint32_t *)((uint8_t *)hpet_addr + x))
#define HPET_CAP_PERIOD 0x04
#define HPET_CFG 0x10
# define HPET_CFG_ENABLE (1 << 0)
# define HPET_LEGACY_ROUTE (1 << 1)
#define HPET_COUNTER 0xf0
#define HPET_T0_CFG 0x100
# define HPET_T0_32BIT_MODE (1 << 8)
# define HPET_T0_VAL_SET (1 << 6)
# define HPET_T0_TYPE_PERIODIC (1 << 3)
# define HPET_T0_INT_ENABLE (1 << 2)
#define HPET_T0_COMPARATOR 0x108
#define FSEC_PER_NSEC 1000000
#define NSEC_PER_USEC 1000
void
hpet_init(void)
{
uint32_t period;
uint32_t val;
assert(hpet_addr != 0);
period = HPET32(HPET_CAP_PERIOD);
hpet_period_nsec = period / FSEC_PER_NSEC;
printf("HPET ticks every %d nanoseconds\n", hpet_period_nsec);
val = HPET32(HPET_CFG);
val = val & ~(HPET_LEGACY_ROUTE | HPET_CFG_ENABLE);
HPET32(HPET_CFG) = val;
HPET32(HPET_COUNTER) = 0;
val = HPET32(HPET_T0_CFG);
val = (val & ~HPET_T0_INT_ENABLE) | HPET_T0_32BIT_MODE | HPET_T0_TYPE_PERIODIC | HPET_T0_VAL_SET;
HPET32(HPET_T0_CFG) = val;
HPET32(HPET_T0_COMPARATOR) = 0xffffffff;
HPET32(HPET_CFG) |= HPET_CFG_ENABLE;
printf("HPET enabled\n");
}
void
hpet_udelay(uint32_t us)
{
uint32_t start, now;
uint32_t max_delay_us = 0xffffffff / NSEC_PER_USEC;
if (us > max_delay_us) {
printf("HPET ERROR: Delay too long, %d usec, truncating to %d usec\n",
us, max_delay_us);
us = max_delay_us;
}
us = (us * NSEC_PER_USEC) / hpet_period_nsec;
start = HPET32(HPET_COUNTER);
do {
now = HPET32(HPET_COUNTER);
} while (now - start < us);
}
void
hpet_mdelay(uint32_t ms)
{
hpet_udelay(ms * 1000);
}
uint32_t
hpclock_read_counter(void)
{
#ifdef APIC
return HPET32(HPET_COUNTER);
#else
return 0;
#endif
}
uint32_t
hpclock_get_counter_period_nsec(void)
{
return hpet_period_nsec;
}