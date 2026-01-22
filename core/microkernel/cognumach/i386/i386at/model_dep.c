#include <inttypes.h>
#include <string.h>
#include <device/cons.h>
#include <mach/vm_param.h>
#include <mach/vm_prot.h>
#include <mach/machine.h>
#include <mach/machine/multiboot.h>
#include <mach/xen.h>
#include <kern/assert.h>
#include <kern/cpu_number.h>
#include <kern/debug.h>
#include <kern/mach_clock.h>
#include <kern/macros.h>
#include <kern/printf.h>
#include <kern/startup.h>
#include <kern/smp.h>
#include <sys/types.h>
#include <vm/vm_page.h>
#include <i386/fpu.h>
#include <i386/gdt.h>
#include <i386/ktss.h>
#include <i386/ldt.h>
#include <i386/spl.h>
#include <i386/mp_desc.h>
#include <i386/pit.h>
#include <i386/pmap.h>
#include <i386/proc_reg.h>
#include <i386/vm_param.h>
#include <i386/locore.h>
#include <i386/model_dep.h>
#include <i386/smp.h>
#include <i386/seg.h>
#include <i386at/acpi_parse_apic.h>
#include <i386at/autoconf.h>
#include <i386at/biosmem.h>
#include <i386at/elf.h>
#include <i386at/idt.h>
#include <i386at/int_init.h>
#include <i386at/kd.h>
#include <i386at/rtc.h>
#include <i386at/mbinfo.h>
#include <i386at/model_dep.h>
#include <machine/irq.h>
#ifdef	MACH_XEN
#include <xen/console.h>
#include <xen/store.h>
#include <xen/evt.h>
#include <xen/xen.h>
#endif
#if	ENABLE_IMMEDIATE_CONSOLE
#include "immc.h"
#endif
#if MACH_KDB
#include <ddb/db_sym.h>
#include <i386/db_interface.h>
static unsigned elf_shdr_num;
static vm_size_t elf_shdr_size;
static vm_offset_t elf_shdr_addr;
static unsigned elf_shdr_shndx;
#endif
#define RESERVED_BIOS 0x10000
#ifdef MACH_XEN
struct start_info boot_info;
#ifdef MACH_PSEUDO_PHYS
unsigned long *mfn_list;
#if VM_MIN_KERNEL_ADDRESS != LINEAR_MIN_KERNEL_ADDRESS
unsigned long *pfn_list = (void*) PFN_LIST;
#endif
#endif
#if VM_MIN_KERNEL_ADDRESS != LINEAR_MIN_KERNEL_ADDRESS
unsigned long la_shift = VM_MIN_KERNEL_ADDRESS;
#endif
#else
struct multiboot_raw_info boot_info;
#endif
char *kernel_cmdline = "";
extern char	version[];
extern struct pseudo_descriptor gdt_descr_tmp;
extern uint32_t apboot_jmp_offset;
boolean_t	rebootflag = FALSE;
#ifdef LINUX_DEV
extern void linux_init(void);
#endif
void machine_init(void)
{
biosmem_free_usable();
init_fpu();
#ifdef MACH_HYP
hyp_init();
#else
#if defined(APIC)
int err;
err = acpi_apic_init();
if (err) {
printf("acpi_apic_init failed with %d\n", err);
for (;;);
}
#endif
#if (NCPUS > 1)
smp_init();
#endif
init_irqs();
#if defined(APIC)
ioapic_configure();
#endif
clkstart();
cninit();
#ifdef LINUX_DEV
linux_init();
#endif
probeio();
#endif
inittodr();
#ifndef MACH_HYP
*(unsigned short *)phystokv(0x472) = 0x1234;
#endif
#if VM_MIN_KERNEL_ADDRESS == 0
pmap_unmap_page_zero();
#endif
#if NCPUS > 1
gdt_descr_tmp.linear_base += apboot_addr;
apboot_jmp_offset += apboot_addr;
#endif
#ifdef APIC
hpet_init();
#endif
}
void machine_idle (int cpu)
{
#ifdef	MACH_HYP
hyp_idle();
#else
assert (cpu == cpu_number ());
asm volatile ("hlt" : : : "memory");
#endif
}
void machine_relax (void)
{
asm volatile ("rep; nop" : : : "memory");
}
void halt_cpu(void)
{
#ifdef	MACH_HYP
hyp_halt();
#else
asm volatile("cli");
while (TRUE)
machine_idle (cpu_number ());
#endif
}
void halt_all_cpus(boolean_t reboot)
{
if (reboot) {
#ifdef	MACH_HYP
hyp_reboot();
#endif
kdreboot();
}
else {
rebootflag = TRUE;
#ifdef	MACH_HYP
hyp_halt();
#endif
printf("Shutdown completed successfully, now in tight loop.\n");
printf("You can safely power off the system or hit ctl-alt-del to reboot\n");
(void) spl0();
}
while (TRUE)
machine_idle (cpu_number ());
}
void db_halt_cpu(void)
{
halt_all_cpus(0);
}
void db_reset_cpu(void)
{
halt_all_cpus(1);
}
#ifndef	MACH_HYP
static void
register_boot_data(const struct multiboot_raw_info *mbi)
{
struct multiboot_raw_module *mod;
struct elf_shdr *shdr;
unsigned long tmp;
unsigned int i;
extern char _start[], _end[];
biosmem_register_boot_data(_kvtophys(&_start), _kvtophys(&_end), FALSE);
if ((mbi->flags & MULTIBOOT_LOADER_CMDLINE) && (mbi->cmdline != 0)) {
biosmem_register_boot_data(mbi->cmdline,
mbi->cmdline
+ strlen((void *)phystokv(mbi->cmdline)) + 1, TRUE);
}
if (mbi->flags & MULTIBOOT_LOADER_MODULES && mbi->mods_count) {
i = mbi->mods_count * sizeof(struct multiboot_raw_module);
biosmem_register_boot_data(mbi->mods_addr, mbi->mods_addr + i, TRUE);
tmp = phystokv(mbi->mods_addr);
for (i = 0; i < mbi->mods_count; i++) {
mod = (struct multiboot_raw_module *)tmp + i;
if (mod->mod_end != mod->mod_start)
biosmem_register_boot_data(mod->mod_start, mod->mod_end, TRUE);
if (mod->string != 0) {
biosmem_register_boot_data(mod->string,
mod->string
+ strlen((void *)phystokv(mod->string)) + 1,
TRUE);
}
}
}
if (mbi->flags & MULTIBOOT_LOADER_SHDR) {
tmp = mbi->shdr_num * mbi->shdr_size;
if (tmp != 0)
biosmem_register_boot_data(mbi->shdr_addr, mbi->shdr_addr + tmp, FALSE);
tmp = phystokv(mbi->shdr_addr);
for (i = 0; i < mbi->shdr_num; i++) {
shdr = (struct elf_shdr *)(tmp + (i * mbi->shdr_size));
if ((shdr->type != ELF_SHT_SYMTAB)
&& (shdr->type != ELF_SHT_STRTAB))
continue;
if (shdr->size != 0)
biosmem_register_boot_data(shdr->addr, shdr->addr + shdr->size, FALSE);
}
}
mbinfo_register_boot_data(mbi);
}
#endif
static void
i386at_init(void)
{
#ifndef	MACH_HYP
# ifdef APIC
picdisable();
# else
picinit();
# endif
#else
hyp_intrinit();
#endif
spl_init = 1;
#ifdef MACH_HYP
biosmem_xen_bootstrap();
#else
register_boot_data((struct multiboot_raw_info *) &boot_info);
biosmem_bootstrap((struct multiboot_raw_info *) &boot_info);
#endif
#ifdef MACH_XEN
kernel_cmdline = (char*) boot_info.cmd_line;
#else
vm_offset_t addr;
if (boot_info.flags & MULTIBOOT_CMDLINE) {
int len = strlen ((char*)phystokv(boot_info.cmdline)) + 1;
if (! init_alloc_aligned(round_page(len), &addr))
panic("could not allocate memory for multiboot command line");
kernel_cmdline = (char*) phystokv(addr);
memcpy(kernel_cmdline, (void *)phystokv(boot_info.cmdline), len);
boot_info.cmdline = addr;
}
if (boot_info.flags & MULTIBOOT_MODS && boot_info.mods_count) {
struct multiboot_raw_module *m;
int i;
if (! init_alloc_aligned(
round_page(boot_info.mods_count * sizeof(*m)), &addr))
panic("could not allocate memory for multiboot modules");
m = (void*) phystokv(addr);
memcpy(m, (void*) phystokv(boot_info.mods_addr), boot_info.mods_count * sizeof(*m));
boot_info.mods_addr = addr;
for (i = 0; i < boot_info.mods_count; i++) {
vm_size_t size = m[i].mod_end - m[i].mod_start;
if (! init_alloc_aligned(round_page(size), &addr))
panic("could not allocate memory for multiboot "
"module %d", i);
memcpy((void*) phystokv(addr), (void*) phystokv(m[i].mod_start), size);
m[i].mod_start = addr;
m[i].mod_end = addr + size;
size = strlen((char*) phystokv(m[i].string)) + 1;
if (! init_alloc_aligned(round_page(size), &addr))
panic("could not allocate memory for multiboot "
"module command line %d", i);
memcpy((void*) phystokv(addr), (void*) phystokv(m[i].string), size);
m[i].string = addr;
}
}
#endif
pmap_bootstrap();
biosmem_setup();
pmap_make_temporary_mapping();
pmap_set_page_dir();
#ifndef	MACH_HYP
set_cr0(get_cr0() | CR0_PG | CR0_WP);
set_cr0(get_cr0() & ~(CR0_CD | CR0_NW));
if (CPU_HAS_FEATURE(CPU_FEATURE_PGE))
set_cr4(get_cr4() | CR4_PGE);
#endif
flush_instr_queue();
#ifdef	MACH_PV_PAGETABLES
pmap_clear_bootstrap_pagetable((void *)boot_info.pt_base);
#endif
gdt_init();
idt_init();
#ifndef	MACH_HYP
int_init();
#endif
ldt_init();
ktss_init();
#ifndef MACH_XEN
init_percpu(0);
#endif
#if NCPUS > 1
mp_desc_init(0);
#endif
pmap_remove_temporary_mapping();
#ifdef	MACH_XEN
hyp_p2m_init();
#endif
interrupt_stack_alloc();
}
void c_boot_entry(vm_offset_t bi)
{
#if	ENABLE_IMMEDIATE_CONSOLE
romputc = immc_romputc;
#endif
boot_info = *(typeof(boot_info)*)phystokv(bi);
int cpu_type;
printf("%s", version);
printf("\n");
#ifdef MACH_XEN
printf("Running on %s.\n", boot_info.magic);
if (boot_info.flags & SIF_PRIVILEGED)
panic("Mach can't run as dom0.");
#ifdef MACH_PSEUDO_PHYS
mfn_list = (void*)boot_info.mfn_list;
#endif
#else
#if	MACH_KDB
if ((boot_info.flags & MULTIBOOT_ELF_SHDR)
&& boot_info.shdr_num)
{
elf_shdr_num = boot_info.shdr_num;
elf_shdr_size = boot_info.shdr_size;
elf_shdr_addr = (vm_offset_t)phystokv(boot_info.shdr_addr);
elf_shdr_shndx = boot_info.shdr_strndx;
printf("ELF section header table at %08" PRIxPTR "\n", elf_shdr_addr);
}
#endif
#endif
cpu_type = discover_x86_cpu_type ();
i386at_init();
#if	MACH_KDB
if (elf_shdr_num)
{
elf_db_sym_init(elf_shdr_num,elf_shdr_size,
elf_shdr_addr, elf_shdr_shndx,
"mach", NULL);
}
#endif
machine_slot[0].is_cpu = TRUE;
machine_slot[0].cpu_subtype = CPU_SUBTYPE_AT386;
#if defined(__x86_64__) && !defined(USER32)
machine_slot[0].cpu_type = CPU_TYPE_X86_64;
#else
switch (cpu_type)
{
default:
printf("warning: unknown cpu type %d, assuming i386\n", cpu_type);
case 3:
machine_slot[0].cpu_type = CPU_TYPE_I386;
break;
case 4:
machine_slot[0].cpu_type = CPU_TYPE_I486;
break;
case 5:
machine_slot[0].cpu_type = CPU_TYPE_PENTIUM;
break;
case 6:
case 15:
machine_slot[0].cpu_type = CPU_TYPE_PENTIUMPRO;
break;
}
#endif
setup_main();
}
#include <mach/vm_prot.h>
#include <vm/pmap.h>
#include <mach/time_value.h>
vm_offset_t
timemmap(dev_t dev, vm_offset_t off, vm_prot_t prot)
{
extern time_value_t *mtime;
if (prot & VM_PROT_WRITE) return (-1);
return (i386_btop(pmap_extract(pmap_kernel(), (vm_offset_t) mtime)));
}
void
startrtclock(void)
{
#ifdef APIC
unmask_irq(timer_pin);
calibrate_lapic_timer();
if (cpu_number() != 0) {
lapic_enable_timer();
}
#else
clkstart();
#ifndef MACH_HYP
unmask_irq(0);
#endif
#endif
}
void
inittodr(void)
{
time_value64_t	new_time;
uint64_t	newsecs;
(void) readtodc(&newsecs);
new_time.seconds = newsecs;
new_time.nanoseconds = 0;
{
spl_t	s = splhigh();
time = new_time;
splx(s);
}
}
void
resettodr(void)
{
writetodc();
}
boolean_t
init_alloc_aligned(vm_size_t size, vm_offset_t *addrp)
{
*addrp = biosmem_bootalloc(vm_page_atop(vm_page_round(size)));
if (*addrp == 0)
return FALSE;
return TRUE;
}
vm_offset_t
pmap_grab_page(void)
{
vm_offset_t addr;
if (!init_alloc_aligned(PAGE_SIZE, &addr))
panic("Not enough memory to initialize Mach");
return addr;
}