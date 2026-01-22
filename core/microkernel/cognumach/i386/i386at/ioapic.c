#include <sys/types.h>
#include <i386/ipl.h>
#include <machine/irq.h>
#include <i386/fpu.h>
#include <i386/hardclock.h>
#include <i386at/kd.h>
#include <i386at/idt.h>
#include <i386/pio.h>
#include <i386/pit.h>
#include <i386/pic.h>
#include <i386/smp.h>
#include <mach/machine.h>
#include <kern/printf.h>
#include <kern/timer.h>
#include <kern/lock.h>
static int has_irq_specific_eoi = 0;
int timer_pin;
uint32_t lapic_timer_val = 0;
uint32_t calibrated_ticks = 0;
spl_t curr_ipl[NCPUS] = {0};
int spl_init = 0;
def_simple_lock_irq_data(static, ioapic_lock)
int iunit[NINTR] = {0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
16, 17, 18, 19, 20, 21, 22, 23,
24, 25, 26, 27, 28, 29, 30, 31,
32, 33, 34, 35, 36, 37, 38, 39,
40, 41, 42, 43, 44, 45, 46, 47,
48, 49, 50, 51, 52, 53, 54, 55,
56, 57, 58, 59, 60, 61, 62, 63 };
interrupt_handler_fn ivect[NINTR] = {
(interrupt_handler_fn)hardclock,
kdintr,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
fpintr,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
intnull,
};
struct irqinfo irqinfo[NINTR];
void
picdisable(void)
{
int i;
asm("cli");
for (i = 0; i < NCPUS; i++)
curr_ipl[i] = SPLHI;
outb ( PIC_SLAVE_OCW, PICS_MASK );
outb ( PIC_MASTER_OCW, PICM_MASK );
}
void
intnull(int unit_dev)
{
printf("intnull(%d)\n", unit_dev);
}
static uint32_t
ioapic_read(uint8_t id, uint8_t reg)
{
volatile ApicIoUnit *ioapic = apic_get_ioapic(id)->ioapic;
ioapic->select.r = reg;
return ioapic->window.r;
}
static void
ioapic_write(uint8_t id, uint8_t reg, uint32_t value)
{
volatile ApicIoUnit *ioapic = apic_get_ioapic(id)->ioapic;
ioapic->select.r = reg;
ioapic->window.r = value;
}
static void
ioapic_read_entry(int apic, int pin, struct ioapic_route_entry *e)
{
union ioapic_route_entry_union entry;
entry.lo = ioapic_read(apic, APIC_IO_REDIR_LOW(pin));
entry.hi = ioapic_read(apic, APIC_IO_REDIR_HIGH(pin));
*e = entry.both;
}
static void
ioapic_write_entry(int apic, int pin, struct ioapic_route_entry e)
{
union ioapic_route_entry_union entry = {{0, 0}};
entry.both = e;
ioapic_write(apic, APIC_IO_REDIR_HIGH(pin), entry.hi);
ioapic_write(apic, APIC_IO_REDIR_LOW(pin), entry.lo);
}
static void
ioapic_toggle_entry(int apic, int pin, int mask)
{
union ioapic_route_entry_union entry;
spl_t s = simple_lock_irq(&ioapic_lock);
ioapic_read_entry(apic, pin, &entry.both);
entry.both.mask = mask & 0x1;
ioapic_write(apic, APIC_IO_REDIR_LOW(pin), entry.lo);
simple_unlock_irq(s, &ioapic_lock);
}
static int
ioapic_version(int apic)
{
return (ioapic_read(apic, APIC_IO_VERSION) >> APIC_IO_VERSION_SHIFT) & 0xff;
}
static int
ioapic_gsis(int apic)
{
return ((ioapic_read(apic, APIC_IO_VERSION) >> APIC_IO_ENTRIES_SHIFT) & 0xff) + 1;
}
static void timer_expiry_callback(void *arg)
{
volatile int *done = arg;
*done = 1;
}
static uint32_t
timer_measure_10x_apic_hz(void)
{
volatile int done = 0;
uint32_t start = 0xffffffff;
timer_elt_data_t tmp_timer;
tmp_timer.fcn = timer_expiry_callback;
tmp_timer.param = (void *)&done;
printf("timer calibration...");
lapic->init_count.r = start;
set_timeout(&tmp_timer, 10);
do {
cpu_pause();
} while (!done);
lapic->lvt_timer.r |= LAPIC_DISABLE;
printf(" done\n");
return start - lapic->cur_count.r;
}
void
calibrate_lapic_timer(void)
{
spl_t s;
lapic->divider_config.r = LAPIC_TIMER_DIVIDE_2;
lapic->lvt_timer.r = IOAPIC_INT_BASE;
if (!calibrated_ticks) {
s = splhigh();
spl0();
calibrated_ticks = timer_measure_10x_apic_hz() / 10;
splx(s);
}
}
void
lapic_enable_timer(void)
{
lapic->init_count.r = calibrated_ticks;
lapic->divider_config.r = LAPIC_TIMER_DIVIDE_2;
lapic->lvt_timer.r = IOAPIC_INT_BASE | LAPIC_TIMER_PERIODIC;
lapic->divider_config.r = LAPIC_TIMER_DIVIDE_2;
printf("LAPIC timer configured on cpu%d\n", cpu_number());
}
void
ioapic_toggle(int pin, int mask)
{
int apic = 0;
ioapic_toggle_entry(apic, pin, mask);
}
void
ioapic_irq_eoi(int pin)
{
int apic = 0;
union ioapic_route_entry_union oldentry, entry;
if (pin == 0)
goto skip_specific_eoi;
spl_t s = simple_lock_irq(&ioapic_lock);
if (!has_irq_specific_eoi) {
ioapic_read_entry(apic, pin, &entry.both);
oldentry = entry;
entry.both.mask = IOAPIC_MASK_DISABLED;
entry.both.trigger = IOAPIC_EDGE_TRIGGERED;
ioapic_write_entry(apic, pin, entry.both);
ioapic_write_entry(apic, pin, oldentry.both);
} else {
volatile ApicIoUnit *ioapic = apic_get_ioapic(apic)->ioapic;
ioapic->eoi.r = irqinfo[pin].vector;
}
simple_unlock_irq(s, &ioapic_lock);
skip_specific_eoi:
lapic_eoi ();
}
static unsigned int
override_irq(IrqOverrideData *override, union ioapic_route_entry_union *entry)
{
if (override->flags & APIC_IRQ_OVERRIDE_TRIGGER_MASK) {
entry->both.trigger = (override->flags & APIC_IRQ_OVERRIDE_LEVEL_TRIGGERED) ?
IOAPIC_LEVEL_TRIGGERED : IOAPIC_EDGE_TRIGGERED;
} else {
if (override->bus == 0) {
entry->both.trigger = IOAPIC_EDGE_TRIGGERED;
} else {
entry->both.trigger = IOAPIC_LEVEL_TRIGGERED;
}
}
if (override->flags & APIC_IRQ_OVERRIDE_POLARITY_MASK) {
entry->both.polarity = (override->flags & APIC_IRQ_OVERRIDE_ACTIVE_LOW) ?
IOAPIC_ACTIVE_LOW : IOAPIC_ACTIVE_HIGH;
} else {
if (override->bus == 0) {
if (entry->both.trigger == IOAPIC_LEVEL_TRIGGERED) {
entry->both.polarity = IOAPIC_ACTIVE_LOW;
} else {
entry->both.polarity = IOAPIC_ACTIVE_HIGH;
}
}
}
printf("IRQ override: pin=%d gsi=%d trigger=%s polarity=%s\n",
override->irq, override->gsi,
entry->both.trigger == IOAPIC_LEVEL_TRIGGERED ? "LEVEL" : "EDGE",
entry->both.polarity == IOAPIC_ACTIVE_LOW ? "LOW" : "HIGH");
return override->gsi;
}
void
ioapic_configure(void)
{
int gsi, apic = 0, pin;
IrqOverrideData *irq_over;
int timer_gsi;
int version = ioapic_version(apic);
int ngsis = ioapic_gsis(apic);
int ngsis2 = 0;
if (version >= 0x20) {
has_irq_specific_eoi = 1;
}
printf("IOAPIC version 0x%x\n", version);
lapic->spurious_vector.r = IOAPIC_SPURIOUS_BASE;
union ioapic_route_entry_union entry = {{0, 0}};
entry.both.delvmode = IOAPIC_FIXED;
entry.both.destmode = IOAPIC_PHYSICAL;
entry.both.mask = IOAPIC_MASK_DISABLED;
entry.both.dest = lapic->apic_id.r >> 24;
for (pin = 0; pin < 14; pin++) {
gsi = pin;
entry.both.trigger = IOAPIC_EDGE_TRIGGERED;
entry.both.polarity = IOAPIC_ACTIVE_HIGH;
if ((irq_over = acpi_get_irq_override(pin))) {
gsi = override_irq(irq_over, &entry);
}
entry.both.vector = IOAPIC_INT_BASE + gsi;
ioapic_write_entry(apic, pin, entry.both);
irqinfo[pin].vector = entry.both.vector;
irqinfo[pin].trigger = entry.both.trigger;
mask_irq(pin);
if (pin == 0) {
timer_gsi = gsi;
} else {
if (gsi == timer_gsi) {
timer_pin = pin;
entry.both.vector = IOAPIC_INT_BASE;
ioapic_write_entry(apic, timer_pin, entry.both);
mask_irq(0);
}
}
}
for (pin = 14; pin < ngsis; pin++) {
gsi = pin;
entry.both.trigger = IOAPIC_LEVEL_TRIGGERED;
entry.both.polarity = IOAPIC_ACTIVE_LOW;
if ((irq_over = acpi_get_irq_override(pin))) {
gsi = override_irq(irq_over, &entry);
}
entry.both.vector = IOAPIC_INT_BASE + gsi;
ioapic_write_entry(apic, pin, entry.both);
irqinfo[pin].vector = entry.both.vector;
irqinfo[pin].trigger = entry.both.trigger;
mask_irq(pin);
}
printf("IOAPIC 0 configured with GSI 0-%d\n", ngsis - 1);
if (apic_get_num_ioapics() > 1) {
apic = 1;
ngsis2 = ioapic_gsis(apic);
for (pin = 0; pin < ngsis2; pin++) {
gsi = pin + ngsis;
entry.both.trigger = IOAPIC_LEVEL_TRIGGERED;
entry.both.polarity = IOAPIC_ACTIVE_LOW;
if ((irq_over = acpi_get_irq_override(pin + ngsis))) {
gsi = override_irq(irq_over, &entry);
}
entry.both.vector = IOAPIC_INT_BASE + gsi;
ioapic_write_entry(apic, pin, entry.both);
irqinfo[pin + ngsis].vector = entry.both.vector;
irqinfo[pin + ngsis].trigger = entry.both.trigger;
mask_irq(pin + ngsis);
}
printf("IOAPIC 1 configured with GSI %d-%d\n", ngsis, ngsis + ngsis2 - 1);
}
lapic_setup();
lapic_enable();
}