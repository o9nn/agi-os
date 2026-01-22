#ifndef _I386_IRQ_H
#define _I386_IRQ_H
#ifdef APIC
# include <i386/apic.h>
#else
# include <i386/pic.h>
#endif
typedef unsigned int irq_t;
void init_irqs (void);
void __enable_irq (irq_t irq);
void __disable_irq (irq_t irq);
extern struct irqdev irqtab;
#endif