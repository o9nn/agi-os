#include <string.h>
#include <i386/apic.h>
#include <i386/smp.h>
#include <i386/cpu.h>
#include <i386/pio.h>
#include <i386/vm_param.h>
#include <i386at/idt.h>
#include <i386at/cram.h>
#include <i386at/acpi_parse_apic.h>
#include <kern/printf.h>
#include <mach/machine.h>
#include <kern/smp.h>
static void smp_data_init(void)
{
uint8_t numcpus = apic_get_numcpus();
smp_set_numcpus(numcpus);
for(int i = 0; i < numcpus; i++){
machine_slot[i].is_cpu = TRUE;
}
}
static void smp_send_ipi(unsigned logical_id, unsigned vector)
{
unsigned long flags;
cpu_intr_save(&flags);
do {
cpu_pause();
} while(lapic->icr_low.delivery_status == SEND_PENDING);
apic_send_ipi(NO_SHORTHAND, FIXED, LOGICAL, ASSERT, EDGE, vector, logical_id);
cpu_intr_restore(flags);
}
void smp_remote_ast(unsigned logical_id)
{
smp_send_ipi(logical_id, CALL_AST_CHECK);
}
void smp_pmap_update(unsigned logical_id)
{
smp_send_ipi(logical_id, CALL_PMAP_UPDATE);
}
static void
wait_for_ipi(void)
{
while (lapic->icr_low.delivery_status == SEND_PENDING) {
cpu_pause();
}
}
static int
smp_send_ipi_init(int bsp_apic_id)
{
int err;
lapic->error_status.r = 0;
err = lapic->error_status.r;
apic_send_ipi(ALL_EXCLUDING_SELF, INIT, PHYSICAL, ASSERT, EDGE, 0, bsp_apic_id);
wait_for_ipi();
apic_send_ipi(ALL_EXCLUDING_SELF, INIT, PHYSICAL, DE_ASSERT, EDGE, 0, bsp_apic_id);
wait_for_ipi();
err = lapic->error_status.r;
if (err) {
printf("ESR error upon INIT 0x%x\n", err);
}
return 0;
}
static int
smp_send_ipi_startup_twice(int bsp_apic_id, int vector)
{
int i, accept_err, send_err;
volatile int err;
for (i = 0; i < 2; i++) {
lapic->error_status.r = 0;
err = lapic->error_status.r;
(void) err;
apic_send_ipi(ALL_EXCLUDING_SELF, STARTUP, PHYSICAL, DE_ASSERT, EDGE, vector, bsp_apic_id);
hpet_udelay(10);
wait_for_ipi();
send_err = lapic->error_status.r;
hpet_udelay(10);
lapic->error_status.r = 0;
accept_err = lapic->error_status.r & 0xef;
if (send_err || accept_err)
break;
}
if (send_err)
printf("ESR error: DID NOT SEND? 0x%x\n", send_err);
if (accept_err)
printf("ESR error: delivery 0x%x\n", accept_err);
return send_err | accept_err;
}
int smp_startup_cpus(unsigned bsp_apic_id, phys_addr_t start_eip)
{
int err;
#if 0
outb(CMOS_ADDR, CMOS_SHUTDOWN);
outb(CMOS_DATA, CM_JMP_467);
uint16_t dword[2];
dword[0] = 0;
dword[1] = start_eip >> 4;
memcpy((uint8_t *)phystokv(0x467), dword, 4);
#endif
asm("wbinvd":::"memory");
printf("Sending IPIs from BSP APIC ID %u...\n", bsp_apic_id);
smp_send_ipi_init(bsp_apic_id);
err = smp_send_ipi_startup_twice(bsp_apic_id, start_eip >> STARTUP_VECTOR_SHIFT);
if (err) {
printf("FATAL: APs failed to start\n");
while(1) {
cpu_pause();
}
}
printf("done\n");
return 0;
}
int smp_init(void)
{
smp_data_init();
return 0;
}