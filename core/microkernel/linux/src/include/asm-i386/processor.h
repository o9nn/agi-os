#ifndef __ASM_I386_PROCESSOR_H
#define __ASM_I386_PROCESSOR_H
#include <asm/vm86.h>
#include <asm/math_emu.h>
extern char hard_math;
extern char x86;
extern char x86_vendor_id[13];
extern char x86_model;
extern char x86_mask;
extern int  x86_capability;
extern int  fdiv_bug;
extern char ignore_irq13;
extern char wp_works_ok;
extern char hlt_works_ok;
extern int  have_cpuid;
extern unsigned long cpu_hz;
#if 0
extern inline void cpuid(int op, int *eax, int *ebx, int *ecx, int *edx)
{
__asm__("cpuid"
: "=a" (*eax),
"=b" (*ebx),
"=c" (*ecx),
"=d" (*edx)
: "a" (op)
: "cc");
}
#endif
#define CX86_CCR2 0xc2
#define CX86_CCR3 0xc3
#define CX86_CCR4 0xe8
#define CX86_CCR5 0xe9
#define CX86_DIR0 0xfe
#define CX86_DIR1 0xff
extern inline unsigned char getCx86(unsigned char reg)
{
unsigned char data;
__asm__ __volatile__("movb %1,%%al\n\t"
"outb %%al,$0x22\n\t"
"inb $0x23,%%al" : "=a" (data) : "q" (reg));
return data;
}
extern inline void setCx86(unsigned char reg, unsigned char data)
{
__asm__ __volatile__("outb %%al,$0x22\n\t"
"movb %1,%%al\n\t"
"outb %%al,$0x23" : : "a" (reg), "q" (data));
}
extern int EISA_bus;
#define MCA_bus 0
#define MCA_bus__is_a_macro
#define TASK_SIZE	(0xC0000000UL)
#define MAX_USER_ADDR	TASK_SIZE
#define MMAP_SEARCH_START (TASK_SIZE/3)
#define IO_BITMAP_SIZE	32
struct i387_hard_struct {
long	cwd;
long	swd;
long	twd;
long	fip;
long	fcs;
long	foo;
long	fos;
long	st_space[20];
long	status;
};
struct i387_soft_struct {
long	cwd;
long	swd;
long	twd;
long	fip;
long	fcs;
long	foo;
long	fos;
long    top;
struct fpu_reg	regs[8];
unsigned char	lookahead;
struct info	*info;
unsigned long	entry_eip;
};
union i387_union {
struct i387_hard_struct hard;
struct i387_soft_struct soft;
};
struct thread_struct {
unsigned short	back_link,__blh;
unsigned long	esp0;
unsigned short	ss0,__ss0h;
unsigned long	esp1;
unsigned short	ss1,__ss1h;
unsigned long	esp2;
unsigned short	ss2,__ss2h;
unsigned long	cr3;
unsigned long	eip;
unsigned long	eflags;
unsigned long	eax,ecx,edx,ebx;
unsigned long	esp;
unsigned long	ebp;
unsigned long	esi;
unsigned long	edi;
unsigned short	es, __esh;
unsigned short	cs, __csh;
unsigned short	ss, __ssh;
unsigned short	ds, __dsh;
unsigned short	fs, __fsh;
unsigned short	gs, __gsh;
unsigned short	ldt, __ldth;
unsigned short	trace, bitmap;
unsigned long	io_bitmap[IO_BITMAP_SIZE+1];
unsigned long	tr;
unsigned long	cr2, trap_no, error_code;
union i387_union i387;
struct vm86_struct * vm86_info;
unsigned long screen_bitmap;
unsigned long v86flags, v86mask, v86mode;
};
#define INIT_MMAP { &init_mm, 0, 0x40000000, PAGE_SHARED, VM_READ | VM_WRITE | VM_EXEC }
#define INIT_TSS  { \
0,0, \
sizeof(init_kernel_stack) + (long) &init_kernel_stack, \
KERNEL_DS, 0, \
0,0,0,0,0,0, \
(long) &swapper_pg_dir, \
0,0,0,0,0,0,0,0,0,0, \
USER_DS,0,USER_DS,0,USER_DS,0,USER_DS,0,USER_DS,0,USER_DS,0, \
_LDT(0),0, \
0, 0x8000, \
{~0, },  \
_TSS(0), 0, 0,0, \
{ { 0, }, },   \
NULL, 0, 0, 0, 0  \
}
#define alloc_kernel_stack()    __get_free_page(GFP_KERNEL)
#define free_kernel_stack(page) free_page((page))
static inline void start_thread(struct pt_regs * regs, unsigned long eip, unsigned long esp)
{
regs->cs = USER_CS;
regs->ds = regs->es = regs->ss = regs->fs = regs->gs = USER_DS;
regs->eip = eip;
regs->esp = esp;
}
extern inline unsigned long thread_saved_pc(struct thread_struct *t)
{
return ((unsigned long *)t->esp)[3];
}
#endif