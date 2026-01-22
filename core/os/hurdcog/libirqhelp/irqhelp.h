#ifndef _HURD_IRQHELP_
#define _HURD_IRQHELP_
#include <mach.h>
#include <hurd/hurd_types.h>
#include <pthread.h>
#include <stdlib.h>
struct irq;
error_t irqhelp_init(void);
struct irq * irqhelp_install_interrupt_handler(int gsi, int bus, int dev, int fun,
void (*handler)(void *), void *context);
void * irqhelp_server_loop(void *arg);
void irqhelp_enable_irq(struct irq *irq);
void irqhelp_disable_irq(struct irq *irq);
error_t irqhelp_remove_interrupt_handler(struct irq *irq);
#endif