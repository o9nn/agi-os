#include <string.h>
#include <mach/exception.h>
#include <mach/machine/thread_status.h>
#include <mach/machine/fp_reg.h>
#include <kern/debug.h>
#include <machine/spl.h>
#include <kern/printf.h>
#include <kern/thread.h>
#include <kern/slab.h>
#include <i386/thread.h>
#include <i386/fpu.h>
#include <i386/pio.h>
#include <i386/irq.h>
#include <i386/locore.h>
#include <i386/trap.h>
#include "cpu_number.h"
#if 0
#include <i386/ipl.h>
#define ASSERT_IPL(L) \
MACRO_BEGIN \
if (curr_ipl[cpu_number()] != L) { \
printf("IPL is %d, expected %d\n", curr_ipl[cpu_number()], L); \
panic("fpu: wrong ipl"); \
} \
MACRO_END
#else
#define ASSERT_IPL(L)
#endif
_Static_assert(sizeof(struct i386_xfp_xstate_header) == 8*8,
"struct i386_xfp_xstate_header size");
_Static_assert(sizeof(struct i386_xfp_save) == 512 + 8*8,
"struct i386_xfp_save size");
int			fp_kind = FP_387;
enum fp_save_kind	fp_save_kind = FP_FNSAVE;
uint64_t		fp_xsave_support;
unsigned		fp_xsave_size = sizeof(struct i386_xfp_save);
struct i386_fpsave_state *fp_default_state;
struct kmem_cache	ifps_cache;
static unsigned long	mxcsr_feature_mask = 0xffffffff;
#if	NCPUS == 1
volatile thread_t	fp_thread = THREAD_NULL;
volatile thread_t	fp_intr_thread = THREAD_NULL;
#define	clear_fpu() \
MACRO_BEGIN \
set_ts(); \
fp_thread = THREAD_NULL; \
MACRO_END
#else
#define	clear_fpu() \
MACRO_BEGIN \
set_ts(); \
MACRO_END
#endif
void
init_fpu(void)
{
unsigned short	status, control;
#ifdef	MACH_RING1
clear_ts();
#else
unsigned int native = 0;
if (machine_slot[cpu_number()].cpu_type >= CPU_TYPE_I486)
native = CR0_NE;
set_cr0((get_cr0() & ~(CR0_EM|CR0_TS)) | native);
#endif
fninit();
status = fnstsw();
fnstcw(&control);
if ((status & 0xff) == 0 &&
(control & 0x103f) == 0x3f)
{
volatile double fp_infinity, fp_one, fp_zero;
fp_one = 1.0;
fp_zero = 0.0;
fp_infinity = fp_one / fp_zero;
if (fp_infinity == -fp_infinity) {
fp_kind = FP_287;
fp_save_kind = FP_FNSAVE;
asm volatile(".byte 0xdb; .byte 0xe4");
}
else {
fp_kind = FP_387;
fp_save_kind = FP_FNSAVE;
if (CPU_HAS_FEATURE(CPU_FEATURE_XSAVE)) {
unsigned eax, ebx, ecx, edx;
unsigned xsave_cpu_features;
eax = 0xd;
ecx = 0x0;
cpuid(eax, ebx, ecx, edx);
fp_xsave_support = eax + (((uint64_t) edx) << 32);
#ifndef MACH_RING1
set_cr4(get_cr4() | CR4_OSFXSR | CR4_OSXSAVE);
set_xcr0(fp_xsave_support);
#endif
eax = 0xd;
ecx = 0x1;
cpuid(eax, ebx, ecx, edx);
xsave_cpu_features = eax;
if (xsave_cpu_features & CPU_FEATURE_XSAVES) {
fp_xsave_size = ebx;
if (fp_xsave_size < sizeof(struct i386_xfp_save))
panic("CPU-provided xstate size %d "
"is smaller than our minimum %d!\n",
fp_xsave_size,
(int) sizeof(struct i386_xfp_save));
fp_save_kind = FP_XSAVES;
} else {
eax = 0xd;
ecx = 0x0;
cpuid(eax, ebx, ecx, edx);
fp_xsave_size = ebx;
if(fp_xsave_size < sizeof(struct i386_xfp_save))
panic("CPU-provided xstate size %d "
"is smaller than our minimum %d!\n",
fp_xsave_size,
(int) sizeof(struct i386_xfp_save));
if (xsave_cpu_features & CPU_FEATURE_XSAVEOPT)
fp_save_kind = FP_XSAVEOPT;
else if (xsave_cpu_features & CPU_FEATURE_XSAVEC)
fp_save_kind = FP_XSAVEC;
else
fp_save_kind = FP_XSAVE;
}
fp_kind = FP_387X;
}
else if (CPU_HAS_FEATURE(CPU_FEATURE_FXSR)) {
#ifndef MACH_RING1
set_cr4(get_cr4() | CR4_OSFXSR);
#endif
fp_kind = FP_387FX;
fp_save_kind = FP_FXSAVE;
}
if (fp_save_kind != FP_FNSAVE) {
static
struct i386_xfp_save save;
unsigned long mask;
fxsave(&save);
mask = save.fp_mxcsr_mask;
if (!mask)
mask = 0x0000ffbf;
mxcsr_feature_mask &= mask;
}
}
#ifdef MACH_RING1
set_ts();
#else
set_cr0(get_cr0() | CR0_TS | CR0_MP);
#endif
}
else {
panic("No FPU!");
}
}
kern_return_t
i386_get_xstate_size(host_t host, vm_size_t *size)
{
if (host == HOST_NULL)
return KERN_INVALID_ARGUMENT;
*size = sizeof(struct i386_xfloat_state) + fp_xsave_size;
return KERN_SUCCESS;
}
void
fpu_module_init(void)
{
kmem_cache_init(&ifps_cache, "i386_fpsave_state",
offsetof(struct i386_fpsave_state, xfp_save_state) + fp_xsave_size,
alignof(struct i386_fpsave_state),
NULL, 0);
fp_default_state = (struct i386_fpsave_state *) kmem_cache_alloc(&ifps_cache);
memset(fp_default_state, 0, offsetof(struct i386_fpsave_state, xfp_save_state) + fp_xsave_size);
clear_ts();
fninit();
fpu_save(fp_default_state);
set_ts();
}
void
fp_free(struct i386_fpsave_state *fps)
{
ASSERT_IPL(SPL0);
#if	NCPUS == 1
if ((fp_thread != THREAD_NULL) && (fp_thread->pcb->ims.ifps == fps)) {
clear_ts();
fwait();
clear_fpu();
}
#endif
kmem_cache_free(&ifps_cache, (vm_offset_t) fps);
}
static inline unsigned short
twd_i387_to_fxsr (unsigned short twd)
{
unsigned int tmp;
tmp = ~twd;
tmp = (tmp | (tmp>>1)) & 0x5555;
tmp = (tmp | (tmp >> 1)) & 0x3333;
tmp = (tmp | (tmp >> 2)) & 0x0f0f;
tmp = (tmp | (tmp >> 4)) & 0x00ff;
return tmp;
}
static inline unsigned long
twd_fxsr_to_i387 (struct i386_xfp_save *fxsave)
{
struct {
unsigned short significand[4];
unsigned short exponent;
unsigned short padding[3];
} *st = NULL;
unsigned long tos = (fxsave->fp_status >> 11) & 7;
unsigned long twd = (unsigned long) fxsave->fp_tag;
unsigned long tag;
unsigned long ret = 0xffff0000u;
int i;
#define FPREG_ADDR(f, n)	((void *)&(f)->fp_reg_word + (n) * 16);
for (i = 0 ; i < 8 ; i++) {
if (twd & 0x1) {
st = FPREG_ADDR (fxsave, (i - tos) & 7);
switch (st->exponent & 0x7fff) {
case 0x7fff:
tag = 2;
break;
case 0x0000:
if (!st->significand[0] &&
!st->significand[1] &&
!st->significand[2] &&
!st->significand[3] ) {
tag = 1;
} else {
tag = 2;
}
break;
default:
if (st->significand[3] & 0x8000) {
tag = 0;
} else {
tag = 2;
}
break;
}
} else {
tag = 3;
}
ret |= (tag << (2 * i));
twd = twd >> 1;
}
return ret;
}
kern_return_t
fpu_set_state(const thread_t thread, void *state, int flavor)
{
pcb_t pcb = thread->pcb;
struct i386_float_state *fstate = (struct i386_float_state*)state;
struct i386_xfloat_state *xfstate = (struct i386_xfloat_state*)state;
struct i386_fpsave_state *ifps;
struct i386_fpsave_state *new_ifps;
ASSERT_IPL(SPL0);
if (fp_kind == FP_NO)
return KERN_FAILURE;
if (flavor == i386_XFLOAT_STATE && xfstate->initialized && xfstate->fp_save_kind != fp_save_kind)
return KERN_INVALID_ARGUMENT;
#if	NCPUS == 1
if (fp_thread == thread) {
clear_ts();
fwait();
clear_fpu();
}
#endif
if ((flavor == i386_FLOAT_STATE && fstate->initialized == 0) ||
(flavor == i386_XFLOAT_STATE && xfstate->initialized == 0)) {
simple_lock(&pcb->lock);
ifps = pcb->ims.ifps;
pcb->ims.ifps = 0;
simple_unlock(&pcb->lock);
if (ifps != 0) {
kmem_cache_free(&ifps_cache, (vm_offset_t) ifps);
}
}
else {
new_ifps = 0;
Retry:
simple_lock(&pcb->lock);
ifps = pcb->ims.ifps;
if (ifps == 0) {
if (new_ifps == 0) {
simple_unlock(&pcb->lock);
new_ifps = (struct i386_fpsave_state *) kmem_cache_alloc(&ifps_cache);
goto Retry;
}
ifps = new_ifps;
new_ifps = 0;
pcb->ims.ifps = ifps;
}
memset(ifps, 0, offsetof(struct i386_fpsave_state, xfp_save_state) + fp_xsave_size);
ifps->fp_valid = TRUE;
if (flavor == i386_FLOAT_STATE) {
struct i386_fp_save *user_fp_state;
struct i386_fp_regs *user_fp_regs;
user_fp_state = (struct i386_fp_save *) &fstate->hw_state[0];
user_fp_regs  = (struct i386_fp_regs *)
&fstate->hw_state[sizeof(struct i386_fp_save)];
if (fp_save_kind != FP_FNSAVE) {
int i;
ifps->xfp_save_state.fp_control = user_fp_state->fp_control;
ifps->xfp_save_state.fp_status  = user_fp_state->fp_status;
ifps->xfp_save_state.fp_tag	    = twd_i387_to_fxsr(user_fp_state->fp_tag);
ifps->xfp_save_state.fp_eip	    = user_fp_state->fp_eip;
ifps->xfp_save_state.fp_cs	    = user_fp_state->fp_cs;
ifps->xfp_save_state.fp_opcode  = user_fp_state->fp_opcode;
ifps->xfp_save_state.fp_dp	    = user_fp_state->fp_dp;
ifps->xfp_save_state.fp_ds	    = user_fp_state->fp_ds;
for (i=0; i<8; i++)
memcpy(&ifps->xfp_save_state.fp_reg_word[i], &user_fp_regs->fp_reg_word[i], sizeof(user_fp_regs->fp_reg_word[i]));
} else {
ifps->fp_save_state.fp_control = user_fp_state->fp_control;
ifps->fp_save_state.fp_status  = user_fp_state->fp_status;
ifps->fp_save_state.fp_tag	   = user_fp_state->fp_tag;
ifps->fp_save_state.fp_eip	   = user_fp_state->fp_eip;
ifps->fp_save_state.fp_cs	   = user_fp_state->fp_cs;
ifps->fp_save_state.fp_opcode  = user_fp_state->fp_opcode;
ifps->fp_save_state.fp_dp	   = user_fp_state->fp_dp;
ifps->fp_save_state.fp_ds	   = user_fp_state->fp_ds;
ifps->fp_regs = *user_fp_regs;
}
} else if (flavor == i386_XFLOAT_STATE) {
int i;
struct i386_xfp_save *user_fp_state = (struct i386_xfp_save *) &xfstate->hw_state[0];
ifps->xfp_save_state.fp_control = user_fp_state->fp_control;
ifps->xfp_save_state.fp_status  = user_fp_state->fp_status;
ifps->xfp_save_state.fp_tag     = user_fp_state->fp_tag;
ifps->xfp_save_state.fp_eip     = user_fp_state->fp_eip;
ifps->xfp_save_state.fp_cs      = user_fp_state->fp_cs;
ifps->xfp_save_state.fp_opcode  = user_fp_state->fp_opcode;
ifps->xfp_save_state.fp_dp      = user_fp_state->fp_dp;
ifps->xfp_save_state.fp_ds      = user_fp_state->fp_ds;
ifps->xfp_save_state.fp_dp3     = user_fp_state->fp_dp3;
ifps->xfp_save_state.fp_mxcsr   = user_fp_state->fp_mxcsr & mxcsr_feature_mask;
ifps->xfp_save_state.fp_mxcsr_mask = user_fp_state->fp_mxcsr_mask & mxcsr_feature_mask;;
for (i=0; i<8; i++)
memcpy(&ifps->xfp_save_state.fp_reg_word[i], &user_fp_state->fp_reg_word[i], sizeof(user_fp_state->fp_reg_word[i]));
for (i=0; i<16; i++)
memcpy(&ifps->xfp_save_state.fp_xreg_word[i], &user_fp_state->fp_xreg_word[i], sizeof(user_fp_state->fp_xreg_word[i]));
memcpy(&ifps->xfp_save_state.header, &user_fp_state->header,
sizeof(ifps->xfp_save_state.header));
if (fp_xsave_size > sizeof(struct i386_xfp_save)) {
memcpy(&ifps->xfp_save_state.extended, &user_fp_state->extended,
fp_xsave_size - sizeof(struct i386_xfp_save));
}
}
simple_unlock(&pcb->lock);
if (new_ifps != 0)
kmem_cache_free(&ifps_cache, (vm_offset_t) new_ifps);
}
return KERN_SUCCESS;
}
kern_return_t
fpu_get_state(const thread_t thread, void *state, int flavor)
{
pcb_t pcb = thread->pcb;
struct i386_float_state *fstate = (struct i386_float_state*)state;
struct i386_xfloat_state *xfstate = (struct i386_xfloat_state*)state;
struct i386_fpsave_state *ifps;
ASSERT_IPL(SPL0);
if (fp_kind == FP_NO)
return KERN_FAILURE;
if (flavor != i386_FLOAT_STATE && fp_save_kind == FP_FNSAVE)
return KERN_FAILURE;
simple_lock(&pcb->lock);
ifps = pcb->ims.ifps;
if (ifps == 0) {
simple_unlock(&pcb->lock);
if (flavor == i386_FLOAT_STATE)
memset(fstate, 0, sizeof(struct i386_float_state));
else if (flavor == i386_XFLOAT_STATE)
memset(xfstate, 0, sizeof(struct i386_xfloat_state) + fp_xsave_size);
return KERN_SUCCESS;
}
#if	NCPUS == 1
if (thread == fp_thread)
#else
if (thread == current_thread())
#endif
{
clear_ts();
fp_save(thread);
clear_fpu();
}
if (flavor == i386_FLOAT_STATE) {
struct i386_fp_save *user_fp_state;
struct i386_fp_regs *user_fp_regs;
fstate->fpkind = fp_kind;
fstate->exc_status = 0;
fstate->initialized = ifps->fp_valid;
user_fp_state = (struct i386_fp_save *) &fstate->hw_state[0];
user_fp_regs  = (struct i386_fp_regs *)
&fstate->hw_state[sizeof(struct i386_fp_save)];
memset(user_fp_state,  0, sizeof(struct i386_fp_save));
if (fp_save_kind != FP_FNSAVE) {
int i;
user_fp_state->fp_control = ifps->xfp_save_state.fp_control;
user_fp_state->fp_status  = ifps->xfp_save_state.fp_status;
user_fp_state->fp_tag     = twd_fxsr_to_i387(&ifps->xfp_save_state);
user_fp_state->fp_eip     = ifps->xfp_save_state.fp_eip;
user_fp_state->fp_cs      = ifps->xfp_save_state.fp_cs;
user_fp_state->fp_opcode  = ifps->xfp_save_state.fp_opcode;
user_fp_state->fp_dp      = ifps->xfp_save_state.fp_dp;
user_fp_state->fp_ds      = ifps->xfp_save_state.fp_ds;
for (i=0; i<8; i++)
memcpy(&user_fp_regs->fp_reg_word[i], &ifps->xfp_save_state.fp_reg_word[i], sizeof(user_fp_regs->fp_reg_word[i]));
} else {
user_fp_state->fp_control = ifps->fp_save_state.fp_control;
user_fp_state->fp_status  = ifps->fp_save_state.fp_status;
user_fp_state->fp_tag     = ifps->fp_save_state.fp_tag;
user_fp_state->fp_eip     = ifps->fp_save_state.fp_eip;
user_fp_state->fp_cs      = ifps->fp_save_state.fp_cs;
user_fp_state->fp_opcode  = ifps->fp_save_state.fp_opcode;
user_fp_state->fp_dp      = ifps->fp_save_state.fp_dp;
user_fp_state->fp_ds      = ifps->fp_save_state.fp_ds;
*user_fp_regs = ifps->fp_regs;
}
} else if (flavor == i386_XFLOAT_STATE) {
int i;
struct i386_xfp_save *user_fp_state;
xfstate->fpkind = fp_kind;
xfstate->exc_status = 0;
xfstate->initialized = ifps->fp_valid;
xfstate->fp_save_kind = fp_save_kind;
user_fp_state = (struct i386_xfp_save *) &xfstate->hw_state[0];
memset(user_fp_state, 0, sizeof(struct i386_xfp_save));
user_fp_state->fp_control = ifps->xfp_save_state.fp_control;
user_fp_state->fp_status  = ifps->xfp_save_state.fp_status;
user_fp_state->fp_tag     = ifps->xfp_save_state.fp_tag;
user_fp_state->fp_eip     = ifps->xfp_save_state.fp_eip;
user_fp_state->fp_cs      = ifps->xfp_save_state.fp_cs;
user_fp_state->fp_opcode  = ifps->xfp_save_state.fp_opcode;
user_fp_state->fp_dp      = ifps->xfp_save_state.fp_dp;
user_fp_state->fp_ds      = ifps->xfp_save_state.fp_ds;
user_fp_state->fp_dp3     = ifps->xfp_save_state.fp_dp3;
user_fp_state->fp_mxcsr   = ifps->xfp_save_state.fp_mxcsr;
user_fp_state->fp_mxcsr_mask = ifps->xfp_save_state.fp_mxcsr_mask;
for (i=0; i<8; i++)
memcpy(&user_fp_state->fp_reg_word[i], &ifps->xfp_save_state.fp_reg_word[i], sizeof(user_fp_state->fp_reg_word[i]));
for (i=0; i<16; i++)
memcpy(&user_fp_state->fp_xreg_word[i], &ifps->xfp_save_state.fp_xreg_word[i], sizeof(user_fp_state->fp_xreg_word[i]));
memcpy(&user_fp_state->header, &ifps->xfp_save_state.header,
sizeof(ifps->xfp_save_state.header));
if (fp_xsave_size > sizeof(struct i386_xfp_save)) {
memcpy(&user_fp_state->extended, &ifps->xfp_save_state.extended,
fp_xsave_size - sizeof(struct i386_xfp_save));
}
}
simple_unlock(&pcb->lock);
return KERN_SUCCESS;
}
static void fpinit(thread_t thread)
{
unsigned short	control;
ASSERT_IPL(SPL0);
clear_ts();
fpu_rstor(fp_default_state);
control = thread->pcb->init_control;
if (control)
fldcw(control);
}
void fpinherit(thread_t parent_thread, thread_t thread)
{
pcb_t pcb = parent_thread->pcb;
struct i386_fpsave_state *ifps;
ifps = pcb->ims.ifps;
if (ifps) {
if (ifps->fp_valid == TRUE)
thread->pcb->init_control = ifps->fp_save_state.fp_control;
else
fnstcw(&thread->pcb->init_control);
}
}
void
fpnoextflt(void)
{
ASSERT_IPL(SPL0);
clear_ts();
#if	NCPUS == 1
if (fp_thread == current_thread())
return;
fwait();
if (fp_thread != THREAD_NULL) {
fp_save(fp_thread);
}
fp_thread = current_thread();
#endif
fp_load(current_thread());
}
void
fpextovrflt(void)
{
thread_t	thread = current_thread();
pcb_t		pcb;
struct i386_fpsave_state *ifps;
#if	NCPUS == 1
if (fp_thread != thread) {
panic("fpextovrflt");
}
#endif
pcb = thread->pcb;
simple_lock(&pcb->lock);
ifps = pcb->ims.ifps;
pcb->ims.ifps = 0;
simple_unlock(&pcb->lock);
clear_ts();
fninit();
clear_fpu();
if (ifps)
kmem_cache_free(&ifps_cache, (vm_offset_t) ifps);
i386_exception(EXC_BAD_ACCESS, VM_PROT_READ|VM_PROT_EXECUTE, 0);
}
static int
fphandleerr(void)
{
thread_t	thread = current_thread();
#if	NCPUS == 1
if (fp_thread == THREAD_NULL) {
printf("fphandleerr: FPU not belonging to anyone!\n");
clear_ts();
fninit();
clear_fpu();
return 1;
}
if (fp_thread != thread) {
clear_ts();
fp_save(fp_thread);
fp_thread->pcb->ims.ifps->fp_valid = 2;
fninit();
clear_fpu();
return 1;
}
#endif
clear_ts();
fp_save(thread);
fninit();
clear_fpu();
return 0;
}
void
fpexterrflt(void)
{
thread_t	thread = current_thread();
if (fphandleerr())
return;
i386_exception(EXC_ARITHMETIC,
EXC_I386_EXTERR,
fp_save_kind != FP_FNSAVE ?
thread->pcb->ims.ifps->xfp_save_state.fp_status :
thread->pcb->ims.ifps->fp_save_state.fp_status);
}
#ifndef MACH_RING1
void
fpastintr(void)
{
thread_t	thread = current_thread();
ASSERT_IPL(SPL0);
#if	NCPUS == 1
if (fp_thread != THREAD_NULL) {
panic("fpexterrflt");
return;
}
if (fp_intr_thread != thread) {
if (fp_intr_thread == THREAD_NULL) {
panic("fpexterrflt: fp_intr_thread == THREAD_NULL");
return;
}
fp_intr_thread->pcb->ims.ifps->fp_valid = 2;
fp_intr_thread = THREAD_NULL;
return;
}
fp_intr_thread = THREAD_NULL;
#else
fp_save(thread);
#endif
i386_exception(EXC_ARITHMETIC,
EXC_I386_EXTERR,
fp_save_kind != FP_FNSAVE ?
thread->pcb->ims.ifps->xfp_save_state.fp_status :
thread->pcb->ims.ifps->fp_save_state.fp_status);
}
#endif
void
fp_save(thread_t thread)
{
pcb_t pcb = thread->pcb;
struct i386_fpsave_state *ifps = pcb->ims.ifps;
if (ifps != 0 && !ifps->fp_valid)
fpu_save(ifps);
}
void
fp_load(thread_t thread)
{
pcb_t pcb = thread->pcb;
struct i386_fpsave_state *ifps;
ASSERT_IPL(SPL0);
ifps = pcb->ims.ifps;
if (ifps == 0) {
ifps = (struct i386_fpsave_state *) kmem_cache_alloc(&ifps_cache);
memcpy(ifps, fp_default_state, offsetof(struct i386_fpsave_state, xfp_save_state) + fp_xsave_size);
pcb->ims.ifps = ifps;
fpinit(thread);
#if 1
} else if (ifps->fp_valid == 2) {
ifps->fp_valid = TRUE;
clear_fpu();
i386_exception(EXC_ARITHMETIC,
EXC_I386_EXTERR,
fp_save_kind != FP_FNSAVE ?
thread->pcb->ims.ifps->xfp_save_state.fp_status :
thread->pcb->ims.ifps->fp_save_state.fp_status);
#endif
} else if (! ifps->fp_valid) {
printf("fp_load: invalid FPU state!\n");
fninit ();
} else {
fpu_rstor(ifps);
}
ifps->fp_valid = FALSE;
}
#if	(defined(AT386) || defined(ATX86_64)) && !defined(MACH_XEN)
void
fpintr(int unit)
{
spl_t	s;
#if	NCPUS == 1
thread_t thread = current_thread();
#endif
ASSERT_IPL(SPL1);
outb(0xf0, 0);
if (fphandleerr())
return;
#if	NCPUS == 1
if (fp_intr_thread != THREAD_NULL && fp_intr_thread != thread)
panic("fp_intr: already caught intr");
fp_intr_thread = thread;
#endif
s = splsched();
ast_on(cpu_number(), AST_I386_FP);
splx(s);
}
#endif