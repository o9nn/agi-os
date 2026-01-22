#ifndef _SMP_H_
#define _SMP_H_
#include <mach/machine/vm_types.h>
int smp_init(void);
void smp_remote_ast(unsigned logical_id);
void smp_pmap_update(unsigned logical_id);
int smp_startup_cpus(unsigned bsp_apic_id, phys_addr_t start_eip);
#define cpu_pause() asm volatile ("pause" : : : "memory")
#define STARTUP_VECTOR_SHIFT	(20 - 8)
#endif