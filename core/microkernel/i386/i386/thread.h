#ifndef	_I386_THREAD_H_
#define _I386_THREAD_H_
#include <mach/boolean.h>
#include <mach/machine/vm_types.h>
#include <mach/machine/fp_reg.h>
#include <mach/machine/thread_status.h>
#include <kern/lock.h>
#include "gdt.h"
struct i386_saved_state {
#if !defined(__x86_64__) || defined(USER32)
unsigned long	gs;
unsigned long	fs;
unsigned long	es;
unsigned long	ds;
#endif
#ifdef __x86_64__
unsigned long	r15;
unsigned long	r14;
unsigned long	r13;
unsigned long	r12;
unsigned long	r11;
unsigned long	r10;
unsigned long	r9;
unsigned long	r8;
#endif
unsigned long	edi;
unsigned long	esi;
unsigned long	ebp;
unsigned long	cr2;
unsigned long	ebx;
unsigned long	edx;
unsigned long	ecx;
unsigned long	eax;
unsigned long	trapno;
unsigned long	err;
unsigned long	eip;
unsigned long	cs;
unsigned long	efl;
unsigned long	uesp;
unsigned long	ss;
#if !defined(__x86_64__) || defined(USER32)
struct v86_segs {
unsigned long v86_es;
unsigned long v86_ds;
unsigned long v86_fs;
unsigned long v86_gs;
} v86_segs;
#endif
};
struct i386_exception_link {
struct i386_saved_state *saved_state;
};
struct i386_kernel_state {
long			k_ebx;
long			k_esp;
long			k_ebp;
#ifdef __i386__
long			k_edi;
long			k_esi;
#endif
long			k_eip;
#ifdef __x86_64__
long			k_r12;
long			k_r13;
long			k_r14;
long			k_r15;
#endif
};
struct i386_fpsave_state {
boolean_t		fp_valid;
union {
struct {
struct i386_fp_save	fp_save_state;
struct i386_fp_regs	fp_regs;
};
struct i386_xfp_save	xfp_save_state;
};
};
#if !defined(__x86_64__) || defined(USER32)
struct v86_assist_state {
vm_offset_t		int_table;
unsigned short		int_count;
unsigned short		flags;
};
#define	V86_IF_PENDING		0x8000
#endif
#if defined(__x86_64__) && !defined(USER32)
struct i386_segment_base_state {
unsigned long fsbase;
unsigned long gsbase;
};
#endif
struct i386_interrupt_state {
#if !defined(__x86_64__) || defined(USER32)
long	gs;
long	fs;
long	es;
long	ds;
#endif
#ifdef __x86_64__
long	r11;
long	r10;
long	r9;
long	r8;
long	rdi;
long	rsi;
#endif
long	edx;
long	ecx;
long	eax;
long	eip;
long	cs;
long	efl;
};
struct i386_interrupt_state_user {
struct i386_interrupt_state interrupt_state;
long	uesp;
long	ss;
};
struct i386_machine_state {
struct user_ldt	*	ldt;
struct i386_fpsave_state *ifps;
#if !defined(__x86_64__) || defined(USER32)
struct v86_assist_state	v86s;
#endif
struct real_descriptor user_gdt[USER_GDT_SLOTS];
struct i386_debug_state ids;
#if defined(__x86_64__) && !defined(USER32)
struct i386_segment_base_state sbs;
#endif
};
typedef struct pcb {
struct i386_interrupt_state iis[2];
#ifdef __x86_64__
unsigned long pad;
#endif
struct i386_saved_state iss;
struct i386_machine_state ims;
decl_simple_lock_data(, lock)
unsigned short init_control;
#ifdef LINUX_DEV
void *data;
#endif
} *pcb_t;
#define STACK_IKS(stack)	\
((struct i386_kernel_state *)((stack) + KERNEL_STACK_SIZE) - 1)
#define STACK_IEL(stack)	\
((struct i386_exception_link *)STACK_IKS(stack) - 1)
#ifdef __x86_64__
#define KERNEL_STACK_ALIGN 16
#else
#define KERNEL_STACK_ALIGN 4
#endif
#if defined(__x86_64__) && !defined(USER32)
#define USER_STACK_ALIGN 16
#else
#define USER_STACK_ALIGN 4
#endif
#define USER_REGS(thread)	(&(thread)->pcb->iss)
#define syscall_emulation_sync(task)
#endif