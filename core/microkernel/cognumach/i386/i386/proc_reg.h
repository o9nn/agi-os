#ifndef	_I386_PROC_REG_H_
#define	_I386_PROC_REG_H_
#define	CR0_PG	0x80000000
#define	CR0_CD	0x40000000
#define	CR0_NW	0x20000000
#define	CR0_AM	0x00040000
#define	CR0_WP	0x00010000
#define	CR0_NE	0x00000020
#define	CR0_ET	0x00000010
#define	CR0_TS	0x00000008
#define	CR0_EM	0x00000004
#define	CR0_MP	0x00000002
#define	CR0_PE	0x00000001
#define	CR3_PCD	0x0010
#define	CR3_PWT	0x0008
#define	CR4_VME		0x0001
#define	CR4_PVI		0x0002
#define	CR4_TSD		0x0004
#define	CR4_DE		0x0008
#define	CR4_PSE		0x0010
#define	CR4_PAE		0x0020
#define	CR4_MCE		0x0040
#define	CR4_PGE		0x0080
#define	CR4_PCE		0x0100
#define	CR4_OSFXSR	0x0200
#define	CR4_OSXMMEXCPT	0x0400
#define	CR4_OSXSAVE	0x40000
#ifndef	__ASSEMBLER__
#ifdef	__GNUC__
#ifndef	MACH_HYP
#include <i386/gdt.h>
#include <i386/ldt.h>
#endif
static inline unsigned long
get_eflags(void)
{
unsigned long eflags;
#ifdef __x86_64__
asm("pushfq; popq %0" : "=r" (eflags));
#else
asm("pushfl; popl %0" : "=r" (eflags));
#endif
return eflags;
}
static inline void
set_eflags(unsigned long eflags)
{
#ifdef __x86_64__
asm volatile("pushq %0; popfq" : : "r" (eflags));
#else
asm volatile("pushl %0; popfl" : : "r" (eflags));
#endif
}
#define get_esp() \
({ \
register unsigned long _temp__ asm("esp"); \
_temp__; \
})
#ifdef __x86_64__
#define get_eflags() \
({ \
register unsigned long _temp__; \
asm("pushfq; popq %0" : "=r" (_temp__)); \
_temp__; \
})
#else
#define get_eflags() \
({ \
register unsigned long _temp__; \
asm("pushfl; popl %0" : "=r" (_temp__)); \
_temp__; \
})
#endif
#define	get_cr0() \
({ \
register unsigned long _temp__; \
asm volatile("mov %%cr0, %0" : "=r" (_temp__)); \
_temp__; \
})
#define	set_cr0(value) \
({ \
register unsigned long _temp__ = (value); \
asm volatile("mov %0, %%cr0" : : "r" (_temp__)); \
})
#define	get_cr2() \
({ \
register unsigned long _temp__; \
asm volatile("mov %%cr2, %0" : "=r" (_temp__)); \
_temp__; \
})
#ifdef	MACH_PV_PAGETABLES
extern unsigned long cr3;
#define get_cr3() (cr3)
#define	set_cr3(value) \
({ \
cr3 = (value); \
if (!hyp_set_cr3(value)) \
panic("set_cr3"); \
})
#else
#define	get_cr3() \
({ \
register unsigned long _temp_get_cr3__; \
asm volatile("mov %%cr3, %0" : "=r" (_temp_get_cr3__)); \
_temp_get_cr3__; \
})
#define	set_cr3(value) \
({ \
register unsigned long _temp_set_cr3__ = (value); \
asm volatile("mov %0, %%cr3" : : "r" (_temp_set_cr3__) : "memory"); \
})
#endif
#define flush_tlb() set_cr3(get_cr3())
#ifndef	MACH_PV_PAGETABLES
#define invlpg(addr) \
({ \
asm volatile("invlpg (%0)" : : "r" (addr)); \
})
#define invlpg_linear(start) \
({ \
asm volatile( \
"movw %w1,%%es\n" \
"\tinvlpg %%es:(%0)\n" \
"\tmovw %w2,%%es" \
:: "r" (start), "q" (LINEAR_DS), "q" (KERNEL_DS)); \
})
#define invlpg_linear_range(start, end) \
({ \
register unsigned long var = trunc_page(start); \
asm volatile( \
"movw %w2,%%es\n" \
"1:\tinvlpg %%es:(%0)\n" \
"\taddl %c4,%0\n" \
"\tcmpl %0,%1\n" \
"\tjb 1b\n" \
"\tmovw %w3,%%es" \
: "+r" (var) : "r" (end), \
"q" (LINEAR_DS), "q" (KERNEL_DS), "i" (PAGE_SIZE)); \
})
#endif
#define	get_cr4() \
({ \
register unsigned long _temp__; \
asm volatile("mov %%cr4, %0" : "=r" (_temp__)); \
_temp__; \
})
#define	set_cr4(value) \
({ \
register unsigned long _temp__ = (value); \
asm volatile("mov %0, %%cr4" : : "r" (_temp__)); \
})
#ifdef	MACH_RING1
#define	set_ts() \
hyp_fpu_taskswitch(1)
#define	clear_ts() \
hyp_fpu_taskswitch(0)
#else
#define	set_ts() \
set_cr0(get_cr0() | CR0_TS)
#define	clear_ts() \
asm volatile("clts")
#endif
#define	get_tr() \
({ \
unsigned short _seg__; \
asm volatile("str %0" : "=rm" (_seg__) ); \
_seg__; \
})
#define	set_tr(seg) \
asm volatile("ltr %0" : : "rm" ((unsigned short)(seg)) )
#define	get_ldt() \
({ \
unsigned short _seg__; \
asm volatile("sldt %0" : "=rm" (_seg__) ); \
_seg__; \
})
#define	set_ldt(seg) \
asm volatile("lldt %0" : : "rm" ((unsigned short)(seg)) )
#define flush_instr_queue() \
asm("jmp 0f\n" \
"0:\n")
#ifdef MACH_RING1
#define get_dr0() hyp_get_debugreg(0)
#else
#define	get_dr0() \
({ \
register unsigned long _temp__; \
asm volatile("mov %%dr0, %0" : "=r" (_temp__)); \
_temp__; \
})
#endif
#ifdef MACH_RING1
#define set_dr0(value) hyp_set_debugreg(0, value)
#else
#define	set_dr0(value) \
({ \
register unsigned long _temp__ = (value); \
asm volatile("mov %0,%%dr0" : : "r" (_temp__)); \
})
#endif
#ifdef MACH_RING1
#define get_dr1() hyp_get_debugreg(1)
#else
#define	get_dr1() \
({ \
register unsigned long _temp__; \
asm volatile("mov %%dr1, %0" : "=r" (_temp__)); \
_temp__; \
})
#endif
#ifdef MACH_RING1
#define set_dr1(value) hyp_set_debugreg(1, value)
#else
#define	set_dr1(value) \
({ \
register unsigned long _temp__ = (value); \
asm volatile("mov %0,%%dr1" : : "r" (_temp__)); \
})
#endif
#ifdef MACH_RING1
#define get_dr2() hyp_get_debugreg(2)
#else
#define	get_dr2() \
({ \
register unsigned long _temp__; \
asm volatile("mov %%dr2, %0" : "=r" (_temp__)); \
_temp__; \
})
#endif
#ifdef MACH_RING1
#define set_dr2(value) hyp_set_debugreg(2, value)
#else
#define	set_dr2(value) \
({ \
register unsigned long _temp__ = (value); \
asm volatile("mov %0,%%dr2" : : "r" (_temp__)); \
})
#endif
#ifdef MACH_RING1
#define get_dr3() hyp_get_debugreg(3)
#else
#define	get_dr3() \
({ \
register unsigned long _temp__; \
asm volatile("mov %%dr3, %0" : "=r" (_temp__)); \
_temp__; \
})
#endif
#ifdef MACH_RING1
#define set_dr3(value) hyp_set_debugreg(3, value)
#else
#define	set_dr3(value) \
({ \
register unsigned long _temp__ = (value); \
asm volatile("mov %0,%%dr3" : : "r" (_temp__)); \
})
#endif
#ifdef MACH_RING1
#define get_dr6() hyp_get_debugreg(6)
#else
#define	get_dr6() \
({ \
register unsigned long _temp__; \
asm volatile("mov %%dr6, %0" : "=r" (_temp__)); \
_temp__; \
})
#endif
#ifdef MACH_RING1
#define set_dr6(value) hyp_set_debugreg(6, value)
#else
#define	set_dr6(value) \
({ \
register unsigned long _temp__ = (value); \
asm volatile("mov %0,%%dr6" : : "r" (_temp__)); \
})
#endif
#ifdef MACH_RING1
#define get_dr7() hyp_get_debugreg(7)
#else
#define	get_dr7() \
({ \
register unsigned long _temp__; \
asm volatile("mov %%dr7, %0" : "=r" (_temp__)); \
_temp__; \
})
#endif
#ifdef MACH_RING1
#define set_dr7(value) hyp_set_debugreg(7, value)
#else
#define	set_dr7(value) \
({ \
register unsigned long _temp__ = (value); \
asm volatile("mov %0,%%dr7" : : "r" (_temp__)); \
})
#endif
#ifdef __x86_64__
#define cpuid(eax, ebx, ecx, edx) \
MACRO_BEGIN \
uint64_t sav_rbx; \
asm(	"mov %%rbx,%2\n\t" \
"cpuid\n\t" \
"xchg %2,%%rbx\n\t" \
"movl %k2,%1\n\t" \
: "+a" (eax), "=m" (ebx), "=&r" (sav_rbx), "+c" (ecx), "=&d" (edx)); \
MACRO_END
#else
#define cpuid(eax, ebx, ecx, edx) \
MACRO_BEGIN \
asm (	"mov %%ebx,%1\n\t" \
"cpuid\n\t" \
"xchg %%ebx,%1\n\t" \
: "+a" (eax), "=&SD" (ebx), "+c" (ecx), "=&d" (edx)); \
MACRO_END
#endif
#endif
#endif
#endif