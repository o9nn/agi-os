#include <i386/smp.h>
#include <i386/apic.h>
#include <kern/cpu_number.h>
#include <i386/percpu.h>
struct percpu percpu_array[NCPUS] = {0};
#ifndef MACH_XEN
void init_percpu(int cpu)
{
int apic_id = apic_get_current_cpu();
percpu_array[cpu].self = &percpu_array[cpu];
percpu_array[cpu].apic_id = apic_id;
percpu_array[cpu].cpu_id = cpu;
}
#endif