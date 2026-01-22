#ifndef LINUX_DEV_GLUE_GLUE_H
#define LINUX_DEV_GLUE_GLUE_H
#include <vm/vm_types.h>
#include <mach/machine/vm_types.h>
extern int linux_auto_config;
extern unsigned long alloc_contig_mem (unsigned, unsigned, unsigned, vm_page_t *);
extern void free_contig_mem (vm_page_t, unsigned);
extern void init_IRQ (void);
extern void restore_IRQ (void);
extern void linux_kmem_init (void);
extern void linux_net_emulation_init (void);
extern void device_setup (void);
extern void linux_timer_intr (void);
extern void linux_sched_init (void);
extern void pcmcia_init (void);
extern void linux_soft_intr (void);
extern int issig (void);
extern int linux_to_mach_error (int);
extern char *get_options(char *str, int *ints);
#endif