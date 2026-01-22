#ifndef	_I386_TSS_H_
#define	_I386_TSS_H_
#include <sys/types.h>
#include <mach/inline.h>
#include <machine/io_perm.h>
#ifdef __x86_64__
struct i386_tss {
uint32_t _reserved0;
uint64_t rsp0;
uint64_t rsp1;
uint64_t rsp2;
uint64_t _reserved1;
uint64_t ist1;
uint64_t ist2;
uint64_t ist3;
uint64_t ist4;
uint64_t ist5;
uint64_t ist6;
uint64_t ist7;
uint64_t _reserved2;
uint16_t _reserved3;
uint16_t io_bit_map_offset;
} __attribute__((__packed__));
#else
struct i386_tss {
int		back_link;
int		esp0;
int		ss0;
int		esp1;
int		ss1;
int		esp2;
int		ss2;
int		cr3;
int		eip;
int		eflags;
int		eax;
int		ecx;
int		edx;
int		ebx;
int		esp;
int		ebp;
int		esi;
int		edi;
int		es;
int		cs;
int		ss;
int		ds;
int		fs;
int		gs;
int		ldt;
unsigned short	trace_trap;
unsigned short	io_bit_map_offset;
};
#endif
struct task_tss
{
struct i386_tss tss;
unsigned char iopb[IOPB_BYTES];
unsigned char barrier;
};
static inline void
ltr(unsigned short segment)
{
__asm volatile("ltr %0" : : "r" (segment) : "memory");
}
#endif