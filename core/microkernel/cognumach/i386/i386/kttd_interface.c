#if MACH_TTD
#include <sys/types.h>
#include <kern/printf.h>
#include <mach/machine/eflags.h>
#include <kern/thread.h>
#include <kern/processor.h>
#include <mach/thread_status.h>
#include <mach/vm_param.h>
#include <i386/seg.h>
#include <i386/constants.h>
#include <ttd/ttd_types.h>
#include <ttd/ttd_stub.h>
#include <machine/kttd_machdep.h>
struct i386_saved_state *kttd_last_saved_statep;
struct i386_saved_state kttd_nested_saved_state;
unsigned last_kttd_sp;
struct i386_saved_state kttd_regs;
extern int kttd_debug;
extern boolean_t kttd_enabled;
extern vm_offset_t virtual_end;
#define I386_BREAKPOINT I386_BREAKPOINT_OPCODE
extern vm_map_t kernel_map;
boolean_t kttd_console_init(void)
{
return(ttd_ip_bootp());
}
void kttd_break(void)
{
if (!kttd_enabled)
return;
asm("int3");
}
void kttd_halt_processors(void)
{
}
boolean_t kttd_supported(void)
{
return ((int)ttd_get_packet != NULL);
}
ttd_machine_type get_ttd_machine_type(void)
{
return TTD_AT386;
}
void kttd_machine_getregs(struct i386_gdb_register_state *ttd_state)
{
ttd_state->gs = kttd_regs.gs;
ttd_state->fs = kttd_regs.fs;
ttd_state->es = kttd_regs.es;
ttd_state->ds = kttd_regs.ds;
ttd_state->edi = kttd_regs.edi;
ttd_state->esi = kttd_regs.esi;
ttd_state->ebp = kttd_regs.ebp;
ttd_state->esp = kttd_regs.uesp;
ttd_state->ebx = kttd_regs.ebx;
ttd_state->edx = kttd_regs.edx;
ttd_state->ecx = kttd_regs.ecx;
ttd_state->eax = kttd_regs.eax;
ttd_state->eip = kttd_regs.eip;
ttd_state->cs = kttd_regs.cs;
ttd_state->efl = kttd_regs.efl;
ttd_state->ss = kttd_regs.ss;
}
void kttd_machine_setregs(struct i386_gdb_register_state *ttd_state)
{
if (kttd_regs.gs != ttd_state->gs) {
if (kttd_debug)
printf("gs 0x%x:0x%x, ", kttd_regs.gs, ttd_state->gs);
kttd_regs.gs = ttd_state->gs;
}
if (kttd_regs.fs != ttd_state->fs) {
if (kttd_debug)
printf("fs 0x%x:0x%x, ", kttd_regs.fs, ttd_state->fs);
kttd_regs.fs = ttd_state->fs;
}
if (kttd_regs.es != ttd_state->es) {
if (kttd_debug)
printf("es 0x%x:0x%x, ", kttd_regs.es, ttd_state->es);
kttd_regs.es = ttd_state->es;
}
if (kttd_regs.ds != ttd_state->ds) {
if (kttd_debug)
printf("ds 0x%x:0x%x, ", kttd_regs.ds, ttd_state->ds);
kttd_regs.ds = ttd_state->ds;
}
if (kttd_regs.edi != ttd_state->edi) {
if (kttd_debug)
printf("edi 0x%x:0x%x, ", kttd_regs.edi, ttd_state->edi);
kttd_regs.edi = ttd_state->edi;
}
if (kttd_regs.esi != ttd_state->esi) {
if (kttd_debug)
printf("esi 0x%x:0x%x, ", kttd_regs.esi, ttd_state->esi);
kttd_regs.esi = ttd_state->esi;
}
if (kttd_regs.ebp != ttd_state->ebp) {
if (kttd_debug)
printf("ebp 0x%x:0x%x, ", kttd_regs.ebp, ttd_state->ebp);
kttd_regs.ebp = ttd_state->ebp;
}
if (kttd_regs.ebx != ttd_state->ebx) {
if (kttd_debug)
printf("ebx 0x%x:0x%x, ", kttd_regs.ebx, ttd_state->ebx);
kttd_regs.ebx = ttd_state->ebx;
}
if (kttd_regs.edx != ttd_state->edx) {
if (kttd_debug)
printf("edx 0x%x:0x%x, ", kttd_regs.edx, ttd_state->edx);
kttd_regs.edx = ttd_state->edx;
}
if (kttd_regs.ecx != ttd_state->ecx) {
if (kttd_debug)
printf("ecx 0x%x:0x%x, ", kttd_regs.ecx, ttd_state->ecx);
kttd_regs.ecx = ttd_state->ecx;
}
if (kttd_regs.eax != ttd_state->eax) {
if (kttd_debug)
printf("eax 0x%x:0x%x, ", kttd_regs.eax, ttd_state->eax);
kttd_regs.eax = ttd_state->eax;
}
if (kttd_regs.eip != ttd_state->eip) {
if (kttd_debug)
printf("eip 0x%x:0x%x, ", kttd_regs.eip, ttd_state->eip);
kttd_regs.eip = ttd_state->eip;
}
if (kttd_regs.cs != ttd_state->cs) {
if (kttd_debug)
printf("cs 0x%x:0x%x, ", kttd_regs.cs, ttd_state->cs);
kttd_regs.cs = ttd_state->cs;
}
if (kttd_regs.efl != ttd_state->efl) {
if (kttd_debug)
printf("efl 0x%x:0x%x, ", kttd_regs.efl, ttd_state->efl);
kttd_regs.efl = ttd_state->efl;
}
#if 0
if (kttd_regs.ss != ttd_state->ss) {
if (kttd_debug)
printf("ss 0x%x:0x%x, ", kttd_regs.ss, ttd_state->ss);
kttd_regs.ss = ttd_state->ss;
}
#endif
}
boolean_t kttd_mem_access(vm_offset_t offset, vm_prot_t access)
{
kern_return_t code;
if (offset >= VM_MIN_KERNEL_ADDRESS && offset < virtual_end)
return TRUE;
if (offset >= virtual_end) {
if (kttd_debug)
printf(">>>>>>>>>>Faulting in memory: 0x%x, 0x%x\n",
trunc_page(offset), access);
code = vm_fault(kernel_map, trunc_page(offset), access, FALSE,
FALSE, (void (*)()) 0);
} else {
#if 1
if ((current_thread() != THREAD_NULL) &&
(current_thread()->task->map->pmap != kernel_pmap) &&
(current_thread()->task->map->pmap != PMAP_NULL)) {
code = vm_fault(current_thread()->task->map,
trunc_page(offset), access, FALSE,
FALSE, (void (*)()) 0);
}else{
return FALSE;
}
#else
if (kttd_debug)
printf("==========Would've tried to map in user area 0x%x\n",
trunc_page(offset));
return FALSE;
#endif
}
return (code == KERN_SUCCESS);
}
void kttd_flush_cache(vm_offset_t offset, vm_size_t length)
{
return;
}
boolean_t kttd_insert_breakpoint(vm_address_t address,
ttd_saved_inst *saved_inst)
{
*saved_inst = *(unsigned char *)address;
*(unsigned char *)address = I386_BREAKPOINT;
return TRUE;
}
boolean_t kttd_remove_breakpoint(vm_address_t address,
ttd_saved_inst saved_inst)
{
*(unsigned char *)address = (saved_inst & BYTE_MASK);
return TRUE;
}
boolean_t kttd_set_machine_single_step(void)
{
kttd_regs.efl |= EFL_TF;
return TRUE;
}
boolean_t kttd_clear_machine_single_step(void)
{
kttd_regs.efl &= ~EFL_TF;
return TRUE;
}
void kttd_type_to_ttdtrap(int type)
{
}
boolean_t kttd_trap(int type, int code, struct i386_saved_state *regs)
{
int s;
if (kttd_debug)
printf("kttd_TRAP, before splhigh()\n");
if (!kttd_supported()) {
kttd_enabled = FALSE;
return FALSE;
}
s = splhigh();
if (++kttd_active > MAX_KTTD_ACTIVE) {
printf("kttd_trap: RE-ENTERED!!!\n");
}
if (kttd_debug)
printf("kttd_TRAP, after splhigh()\n");
kttd_regs = *regs;
if ((regs->cs & 0x3) == KERNEL_RING) {
kttd_regs.uesp = (int)&regs->uesp;
kttd_regs.ss = KERNEL_DS;
}
if (type != -1) {
kttd_current_request = NULL;
kttd_current_length = 0;
kttd_current_kmsg = NULL;
kttd_run_status = FULL_STOP;
}else{
if ((kttd_current_request == NULL) ||
(kttd_current_length == 0) ||
(kttd_current_kmsg == NULL) ||
(kttd_run_status != ONE_STOP)) {
printf("kttd_trap: INSANITY!!!\n");
}
}
kttd_task_trap(type, code, (regs->cs & 0x3) != 0);
regs->eip = kttd_regs.eip;
regs->efl = kttd_regs.efl;
regs->eax = kttd_regs.eax;
regs->ecx = kttd_regs.ecx;
regs->edx = kttd_regs.edx;
regs->ebx = kttd_regs.ebx;
if ((regs->cs & 0x3) != KERNEL_RING) {
regs->uesp = kttd_regs.uesp;
regs->ss = kttd_regs.ss & SEGMENT_SELECTOR_MASK;
}
regs->ebp = kttd_regs.ebp;
regs->esi = kttd_regs.esi;
regs->edi = kttd_regs.edi;
regs->es = kttd_regs.es & SEGMENT_SELECTOR_MASK;
regs->cs = kttd_regs.cs & SEGMENT_SELECTOR_MASK;
regs->ds = kttd_regs.ds & SEGMENT_SELECTOR_MASK;
regs->fs = kttd_regs.fs & SEGMENT_SELECTOR_MASK;
regs->gs = kttd_regs.gs & SEGMENT_SELECTOR_MASK;
if (--kttd_active < MIN_KTTD_ACTIVE)
printf("ttd_trap: kttd_active < 0\n");
if (kttd_debug) {
printf("Leaving kttd_trap, kttd_active = %d\n", kttd_active);
}
if (type == -1) {
if (kttd_run_status == RUNNING)
printf("kttd_trap: $$$$$ run_status already RUNNING! $$$$$\n");
kttd_run_status = RUNNING;
}
kttd_run_status = RUNNING;
(void) splx(s);
return TRUE;
}
struct int_regs {
int edi;
int esi;
int ebp;
int ebx;
struct i386_interrupt_state *is;
};
void
kttd_netentry(struct int_regs *int_regs)
{
struct i386_interrupt_state *is = int_regs->is;
int s;
if (kttd_debug)
printf("kttd_NETENTRY before slphigh()\n");
s = splhigh();
if (kttd_debug)
printf("kttd_NETENTRY after slphigh()\n");
if ((is->cs & 0x3) != KERNEL_RING) {
struct i386_interrupt_state_user *user_is = (struct i386_interrupt_state_user *)is;
kttd_regs.uesp = user_is->uesp;
kttd_regs.ss = user_is->ss;
}
else {
kttd_regs.ss = KERNEL_DS;
kttd_regs.uesp= (int)(is+1);
}
kttd_regs.efl = is->efl;
kttd_regs.cs = is->cs;
kttd_regs.eip = is->eip;
kttd_regs.eax = is->eax;
kttd_regs.ecx = is->ecx;
kttd_regs.edx = is->edx;
kttd_regs.ebx = int_regs->ebx;
kttd_regs.ebp = int_regs->ebp;
kttd_regs.esi = int_regs->esi;
kttd_regs.edi = int_regs->edi;
kttd_regs.ds = is->ds;
kttd_regs.es = is->es;
kttd_regs.fs = is->fs;
kttd_regs.gs = is->gs;
kttd_active++;
kttd_task_trap(-1, 0, (kttd_regs.cs & 0x3) != 0);
kttd_active--;
if ((kttd_regs.cs & 0x3) != KERNEL_RING) {
struct i386_interrupt_state_user *user_is = (struct i386_interrupt_state_user *)is;
user_is->uesp = kttd_regs.uesp;
user_is->ss = kttd_regs.ss & 0xffff;
}
is->efl = kttd_regs.efl;
is->cs = kttd_regs.cs & SEGMENT_SELECTOR_MASK;
is->eip = kttd_regs.eip;
is->eax = kttd_regs.eax;
is->ecx = kttd_regs.ecx;
is->edx = kttd_regs.edx;
int_regs->ebx = kttd_regs.ebx;
int_regs->ebp = kttd_regs.ebp;
int_regs->esi = kttd_regs.esi;
int_regs->edi = kttd_regs.edi;
is->ds = kttd_regs.ds & SEGMENT_SELECTOR_MASK;
is->es = kttd_regs.es & SEGMENT_SELECTOR_MASK;
is->fs = kttd_regs.fs & SEGMENT_SELECTOR_MASK;
is->gs = kttd_regs.gs & SEGMENT_SELECTOR_MASK;
if (kttd_run_status == RUNNING)
printf("kttd_netentry: %%%%% run_status already RUNNING! %%%%%\n");
kttd_run_status = RUNNING;
(void) splx(s);
}
#endif