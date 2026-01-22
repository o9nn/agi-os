#ifndef _I386_MP_DESC_H_
#define _I386_MP_DESC_H_
#include <mach/kern_return.h>
#if MULTIPROCESSOR
#include "seg.h"
#include "tss.h"
#include <i386at/idt.h>
#include "gdt.h"
#include "ldt.h"
struct mp_desc_table {
struct real_gate idt[IDTSZ];
struct real_descriptor gdt[GDTSZ];
struct real_descriptor ldt[LDTSZ];
struct task_tss ktss;
};
extern struct mp_desc_table *mp_desc_table[NCPUS];
extern struct task_tss *mp_ktss[NCPUS];
extern struct real_descriptor *mp_gdt[NCPUS];
extern uint8_t solid_intstack[];
extern int mp_desc_init(int);
extern void interrupt_processor(int cpu);
#endif
extern void start_other_cpus(void);
extern kern_return_t cpu_control(int cpu, const int *info, unsigned int count);
extern void interrupt_stack_alloc(void);
#endif