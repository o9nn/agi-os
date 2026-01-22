#include <string.h>
#include <stdint.h>
#include <mach/machine.h>
#include <kern/printf.h>
#include <kern/debug.h>
#include <i386/vm_param.h>
#include <i386/apic.h>
#include <i386at/acpi_parse_apic.h>
#include <vm/vm_kern.h>
static struct acpi_apic *apic_madt = NULL;
unsigned lapic_addr;
uint32_t *hpet_addr;
void
acpi_print_info(phys_addr_t rsdp, void *rsdt, int acpi_rsdt_n)
{
printf("ACPI:\n");
printf(" rsdp = 0x%llx\n", (unsigned long long) rsdp);
printf(" rsdt/xsdt = 0x%p (n = %d)\n", rsdt, acpi_rsdt_n);
}
static uint8_t
acpi_checksum(void *addr, uint32_t length)
{
uint8_t *bytes = addr;
uint8_t checksum = 0;
unsigned int i;
for (i = 0; i < length; i++)
checksum += bytes[i];
return checksum;
}
static int
acpi_check_signature(const uint8_t table_signature[], const char *real_signature, uint8_t length)
{
return memcmp(table_signature, real_signature, length);
}
static int8_t
acpi_check_rsdp(struct acpi_rsdp2 *rsdp, phys_addr_t *sdt_base)
{
int is_rsdp;
uint8_t cksum;
is_rsdp = acpi_check_signature(rsdp->v1.signature, ACPI_RSDP_SIG, 8*sizeof(uint8_t));
if (is_rsdp != ACPI_SUCCESS)
return ACPI_BAD_SIGNATURE;
if (rsdp->v1.revision == 0) {
*sdt_base = rsdp->v1.rsdt_addr;
printf("ACPI v1.0\n");
cksum = acpi_checksum((void *)(&rsdp->v1), sizeof(struct acpi_rsdp));
if (cksum != 0)
return ACPI_BAD_CHECKSUM;
return 1;
} else if (rsdp->v1.revision == 2) {
*sdt_base = rsdp->xsdt_addr;
printf("ACPI >= v2.0\n");
cksum = acpi_checksum((void *)rsdp, sizeof(struct acpi_rsdp2));
if (cksum != 0)
return ACPI_BAD_CHECKSUM;
return 2;
}
return ACPI_NO_RSDP;
}
static int8_t
acpi_check_rsdp_align(void *addr)
{
if ((uintptr_t)addr & (ACPI_RSDP_ALIGN-1))
return ACPI_BAD_ALIGN;
return ACPI_SUCCESS;
}
static phys_addr_t
acpi_search_rsdp(void *addr, uint32_t length, int *is_64bit)
{
void *end;
int version = 0;
phys_addr_t sdt_base = 0;
for (end = addr+length; addr < end; addr += ACPI_RSDP_ALIGN) {
if ((addr != NULL) && ((version = acpi_check_rsdp(addr, &sdt_base)) > 0)) {
*is_64bit = (version == 2);
return sdt_base;
}
}
return 0;
}
static phys_addr_t
acpi_get_rsdp(int *is_64bit)
{
uint16_t *start = 0;
phys_addr_t base = 0;
phys_addr_t rsdp = 0;
start = (uint16_t*) phystokv(0x040e);
base = phystokv((*start) << 4);
if (acpi_check_rsdp_align((void *)base) == ACPI_BAD_ALIGN)
return 0;
rsdp = acpi_search_rsdp((void *)base, 1024, is_64bit);
if (rsdp == 0) {
rsdp = acpi_search_rsdp((void *)phystokv(0xe0000), 0x100000 - 0x0e0000, is_64bit);
}
return rsdp;
}
static struct acpi_rsdt*
acpi_get_rsdt(phys_addr_t rsdp_phys, int* acpi_rsdt_n)
{
struct acpi_rsdt *rsdt = NULL;
int signature_check;
rsdt = (struct acpi_rsdt*) kmem_map_aligned_table(rsdp_phys, sizeof(struct acpi_rsdt), VM_PROT_READ);
if (rsdt == NULL)
return NULL;
signature_check = acpi_check_signature(rsdt->header.signature, ACPI_RSDT_SIG,
4*sizeof(uint8_t));
if (signature_check != ACPI_SUCCESS)
return NULL;
*acpi_rsdt_n = (rsdt->header.length - sizeof(rsdt->header))
/ sizeof(rsdt->entry[0]);
return rsdt;
}
static struct acpi_xsdt*
acpi_get_xsdt(phys_addr_t rsdp_phys, int* acpi_xsdt_n)
{
struct acpi_xsdt *xsdt = NULL;
int signature_check;
xsdt = (struct acpi_xsdt*) kmem_map_aligned_table(rsdp_phys, sizeof(struct acpi_xsdt), VM_PROT_READ);
if (xsdt == NULL)
return NULL;
signature_check = acpi_check_signature(xsdt->header.signature, ACPI_XSDT_SIG,
4*sizeof(uint8_t));
if (signature_check != ACPI_SUCCESS)
return NULL;
*acpi_xsdt_n = (xsdt->header.length - sizeof(xsdt->header))
/ sizeof(xsdt->entry[0]);
return xsdt;
}
static struct acpi_apic*
acpi_get_apic(struct acpi_rsdt *rsdt, int acpi_rsdt_n)
{
struct acpi_dhdr *descr_header;
struct acpi_apic *madt = NULL;
int check_signature;
uint64_t map_addr;
for (int i = 0; i < acpi_rsdt_n; i++) {
descr_header = (struct acpi_dhdr*) kmem_map_aligned_table(rsdt->entry[i], sizeof(struct acpi_dhdr),
VM_PROT_READ);
check_signature = acpi_check_signature(descr_header->signature, ACPI_APIC_SIG, 4*sizeof(uint8_t));
if (check_signature == ACPI_SUCCESS)
madt = (struct acpi_apic*) descr_header;
check_signature = acpi_check_signature(descr_header->signature, ACPI_HPET_SIG, 4*sizeof(uint8_t));
if (check_signature == ACPI_SUCCESS) {
map_addr = ((struct acpi_hpet *)descr_header)->address.addr64;
assert (map_addr != 0);
hpet_addr = (uint32_t *)kmem_map_aligned_table(map_addr, 1024, VM_PROT_READ | VM_PROT_WRITE);
printf("HPET at physical address 0x%llx\n", map_addr);
}
}
return madt;
}
static struct acpi_apic*
acpi_get_apic2(struct acpi_xsdt *xsdt, int acpi_xsdt_n)
{
struct acpi_dhdr *descr_header;
struct acpi_apic *madt = NULL;
int check_signature;
uint64_t map_addr;
for (int i = 0; i < acpi_xsdt_n; i++) {
descr_header = (struct acpi_dhdr*) kmem_map_aligned_table(xsdt->entry[i], sizeof(struct acpi_dhdr),
VM_PROT_READ);
check_signature = acpi_check_signature(descr_header->signature, ACPI_APIC_SIG, 4*sizeof(uint8_t));
if (check_signature == ACPI_SUCCESS)
madt = (struct acpi_apic *)descr_header;
check_signature = acpi_check_signature(descr_header->signature, ACPI_HPET_SIG, 4*sizeof(uint8_t));
if (check_signature == ACPI_SUCCESS) {
map_addr = ((struct acpi_hpet *)descr_header)->address.addr64;
assert (map_addr != 0);
hpet_addr = (uint32_t *)kmem_map_aligned_table(map_addr, 1024, VM_PROT_READ | VM_PROT_WRITE);
printf("HPET at physical address 0x%llx\n", map_addr);
}
}
return madt;
}
static void
acpi_apic_add_lapic(struct acpi_apic_lapic *lapic_entry)
{
if (lapic_entry->flags & (ACPI_LAPIC_FLAG_ENABLED | ACPI_LAPIC_FLAG_CAPABLE)) {
apic_add_cpu(lapic_entry->apic_id & apic_id_mask);
}
}
static void
acpi_apic_add_ioapic(struct acpi_apic_ioapic *ioapic_entry)
{
IoApicData io_apic;
io_apic.apic_id = ioapic_entry->apic_id;
io_apic.addr = ioapic_entry->addr;
io_apic.gsi_base = ioapic_entry->gsi_base;
io_apic.ioapic = (ApicIoUnit *)kmem_map_aligned_table(ioapic_entry->addr,
sizeof(ApicIoUnit),
VM_PROT_READ | VM_PROT_WRITE);
io_apic.ioapic->select.r = APIC_IO_VERSION;
io_apic.ngsis = ((io_apic.ioapic->window.r >> APIC_IO_ENTRIES_SHIFT) & 0xff) + 1;
apic_add_ioapic(io_apic);
}
static void
acpi_apic_add_irq_override(struct acpi_apic_irq_override* irq_override)
{
IrqOverrideData irq_over;
irq_over.bus = irq_override->bus;
irq_over.irq = irq_override->irq;
irq_over.gsi = irq_override->gsi;
irq_over.flags = irq_override->flags;
apic_add_irq_override(irq_over);
}
static int
acpi_apic_parse_table(struct acpi_apic *apic)
{
struct acpi_apic_dhdr *apic_entry = NULL;
vm_offset_t end = 0;
uint8_t numcpus = 1;
apic_entry = (struct acpi_apic_dhdr*) apic->entry;
end = (vm_offset_t) apic + apic->header.length;
printf("APIC entry=0x%p end=0x%x\n", apic_entry, end);
numcpus = apic_get_numcpus();
while ((vm_offset_t)apic_entry < end) {
struct acpi_apic_lapic *lapic_entry;
struct acpi_apic_ioapic *ioapic_entry;
struct acpi_apic_irq_override *irq_override_entry;
printf("APIC entry=0x%p end=0x%x\n", apic_entry, end);
switch(apic_entry->type) {
case ACPI_APIC_ENTRY_LAPIC:
if(numcpus < NCPUS) {
lapic_entry = (struct acpi_apic_lapic*) apic_entry;
acpi_apic_add_lapic(lapic_entry);
}
break;
case ACPI_APIC_ENTRY_IOAPIC:
ioapic_entry = (struct acpi_apic_ioapic*) apic_entry;
acpi_apic_add_ioapic(ioapic_entry);
break;
case ACPI_APIC_ENTRY_IRQ_OVERRIDE:
irq_override_entry = (struct acpi_apic_irq_override*) apic_entry;
acpi_apic_add_irq_override(irq_override_entry);
break;
default:
printf("Unhandled APIC entry type 0x%x\n", apic_entry->type);
break;
}
apic_entry = (struct acpi_apic_dhdr*)((vm_offset_t) apic_entry
+ apic_entry->length);
numcpus = apic_get_numcpus();
}
return ACPI_SUCCESS;
}
static int
acpi_apic_setup(struct acpi_apic *apic)
{
ApicLocalUnit* lapic_unit;
uint8_t ncpus, nioapics;
lapic_addr = apic->lapic_addr;
lapic_unit = kmem_map_aligned_table(apic->lapic_addr, sizeof(ApicLocalUnit),
VM_PROT_READ | VM_PROT_WRITE);
if (lapic_unit == NULL)
return ACPI_NO_LAPIC;
apic_lapic_init(lapic_unit);
fix_apic_id_mask();
acpi_apic_parse_table(apic);
ncpus = apic_get_numcpus();
nioapics = apic_get_num_ioapics();
if (ncpus == 0 || nioapics == 0 || ncpus > NCPUS)
return ACPI_APIC_FAILURE;
if(ncpus < NCPUS) {
int refit = apic_refit_cpulist();
if (refit != 0)
return ACPI_FIT_FAILURE;
}
apic_generate_cpu_id_lut();
return ACPI_SUCCESS;
}
int
acpi_apic_init(void)
{
phys_addr_t rsdp = 0;
struct acpi_rsdt *rsdt = 0;
struct acpi_xsdt *xsdt = 0;
int acpi_rsdt_n;
int ret_acpi_setup;
int apic_init_success = 0;
int is_64bit = 0;
uint8_t checksum;
rsdp = acpi_get_rsdp(&is_64bit);
if (rsdp == 0)
return ACPI_NO_RSDP;
if (!is_64bit) {
rsdt = acpi_get_rsdt(rsdp, &acpi_rsdt_n);
if (rsdt == NULL)
return ACPI_NO_RSDT;
checksum = acpi_checksum((void *)rsdt, rsdt->header.length);
if (checksum != 0)
return ACPI_BAD_CHECKSUM;
apic_madt = acpi_get_apic(rsdt, acpi_rsdt_n);
if (apic_madt == NULL)
return ACPI_NO_APIC;
checksum = acpi_checksum((void *)apic_madt, apic_madt->header.length);
if (checksum != 0)
return ACPI_BAD_CHECKSUM;
acpi_print_info(rsdp, rsdt, acpi_rsdt_n);
} else {
xsdt = acpi_get_xsdt(rsdp, &acpi_rsdt_n);
if (xsdt == NULL)
return ACPI_NO_RSDT;
checksum = acpi_checksum((void *)xsdt, xsdt->header.length);
if (checksum != 0)
return ACPI_BAD_CHECKSUM;
apic_madt = acpi_get_apic2(xsdt, acpi_rsdt_n);
if (apic_madt == NULL)
return ACPI_NO_APIC;
checksum = acpi_checksum((void *)apic_madt, apic_madt->header.length);
if (checksum != 0)
return ACPI_BAD_CHECKSUM;
acpi_print_info(rsdp, xsdt, acpi_rsdt_n);
}
apic_init_success = apic_data_init();
if (apic_init_success != ACPI_SUCCESS)
return ACPI_APIC_FAILURE;
ret_acpi_setup = acpi_apic_setup(apic_madt);
if (ret_acpi_setup != ACPI_SUCCESS)
return ret_acpi_setup;
apic_print_info();
return ACPI_SUCCESS;
}