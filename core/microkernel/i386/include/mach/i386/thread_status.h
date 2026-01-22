#ifndef _MACH_I386_THREAD_STATUS_H_
#define _MACH_I386_THREAD_STATUS_H_
#include <mach/machine/fp_reg.h>
#define i386_THREAD_STATE 1
#define i386_FLOAT_STATE 2
#define i386_ISA_PORT_MAP_STATE 3
#define i386_V86_ASSIST_STATE 4
#define i386_REGS_SEGS_STATE 5
#define i386_DEBUG_STATE 6
#define i386_FSGS_BASE_STATE 7
#define i386_XFLOAT_STATE 8
struct i386_thread_state {
#if defined(__x86_64__) && !defined(USER32)
uint64_t r8;
uint64_t r9;
uint64_t r10;
uint64_t r11;
uint64_t r12;
uint64_t r13;
uint64_t r14;
uint64_t r15;
uint64_t rdi;
uint64_t rsi;
uint64_t rbp;
uint64_t rsp;
uint64_t rbx;
uint64_t rdx;
uint64_t rcx;
uint64_t rax;
uint64_t rip;
#else
unsigned int gs;
unsigned int fs;
unsigned int es;
unsigned int ds;
unsigned int edi;
unsigned int esi;
unsigned int ebp;
unsigned int esp;
unsigned int ebx;
unsigned int edx;
unsigned int ecx;
unsigned int eax;
unsigned int eip;
#endif
unsigned int cs;
#if defined(__x86_64__) && !defined(USER32)
uint64_t rfl;
uint64_t ursp;
#else
unsigned int efl;
unsigned int uesp;
#endif
unsigned int ss;
};
#define i386_THREAD_STATE_COUNT (sizeof (struct i386_thread_state)/sizeof(unsigned int))
#define FP_STATE_BYTES \
(sizeof (struct i386_fp_save) + sizeof (struct i386_fp_regs))
struct i386_float_state {
int fpkind;
int initialized;
unsigned char hw_state[FP_STATE_BYTES];
int exc_status;
};
#define i386_FLOAT_STATE_COUNT (sizeof(struct i386_float_state)/sizeof(unsigned int))
struct i386_xfloat_state {
int fpkind;
int initialized;
int exc_status;
int fp_save_kind;
unsigned char hw_state[];
};
#define PORT_MAP_BITS 0x400
struct i386_isa_port_map_state {
unsigned char pm[PORT_MAP_BITS>>3];
};
#define i386_ISA_PORT_MAP_STATE_COUNT (sizeof(struct i386_isa_port_map_state)/sizeof(unsigned int))
struct i386_v86_assist_state {
unsigned int int_table;
int int_count;
};
struct v86_interrupt_table {
unsigned int count;
unsigned short mask;
unsigned short vec;
};
#define i386_V86_ASSIST_STATE_COUNT \
(sizeof(struct i386_v86_assist_state)/sizeof(unsigned int))
struct i386_debug_state {
unsigned int dr[8];
};
#define i386_DEBUG_STATE_COUNT \
(sizeof(struct i386_debug_state)/sizeof(unsigned int))
struct i386_fsgs_base_state {
unsigned long fs_base;
unsigned long gs_base;
};
#define i386_FSGS_BASE_STATE_COUNT \
(sizeof(struct i386_fsgs_base_state)/sizeof(unsigned int))
#endif