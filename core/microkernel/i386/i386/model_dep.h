#ifndef _I386AT_MODEL_DEP_H_
#define _I386AT_MODEL_DEP_H_
#include <mach/std_types.h>
extern phys_addr_t apboot_addr;
extern void machine_init (void);
extern void machine_idle (int cpu);
extern void resettodr (void);
extern void startrtclock (void);
extern void halt_cpu (void) __attribute__ ((noreturn));
extern void halt_all_cpus (boolean_t reboot) __attribute__ ((noreturn));
extern void machine_relax (void);
extern void c_boot_entry(vm_offset_t bi);
#endif