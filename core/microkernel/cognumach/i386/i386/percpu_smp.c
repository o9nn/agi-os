#include <kern/lock.h>
#include <kern/thread.h>
#include <kern/processor.h>
#include <kern/printf.h>
#include <machine/spl.h>
#include <i386/percpu.h>
#include <kern/cpu_number.h>
void smp_percpu_init(int cpu)
{
#if NCPUS > 1
struct percpu *pcpu = &percpu_array[cpu];
simple_lock_init(&pcpu->cpu_lock);
pcpu->ipi_count = 0;
pcpu->runq_lock_count = 0;
pcpu->context_switches = 0;
pcpu->is_running = FALSE;
pcpu->is_idle = FALSE;
pcpu->in_interrupt = 0;
pcpu->cached_thread = THREAD_NULL;
pcpu->cached_stack = 0;
pcpu->idle_thread = THREAD_NULL;
pcpu->sched_balance_policy = 0;
pcpu->machine_slot.is_cpu = TRUE;
pcpu->machine_slot.running = FALSE;
printf("SMP per-CPU initialization completed for CPU %d\n", cpu);
#endif
}
void smp_percpu_setup_stack(int cpu, vm_offset_t stack_base, vm_offset_t stack_top)
{
#if NCPUS > 1
struct percpu *pcpu = &percpu_array[cpu];
pcpu->int_stack_base = stack_base;
pcpu->int_stack_top = stack_top;
pcpu->kernel_stack = stack_top;
printf("SMP stack setup for CPU %d: base=0x%lx, top=0x%lx\n",
cpu, (unsigned long)stack_base, (unsigned long)stack_top);
#endif
}
boolean_t smp_cpu_is_idle(int cpu)
{
#if NCPUS > 1
if (cpu >= 0 && cpu < NCPUS) {
return percpu_array[cpu].is_idle;
}
#endif
return FALSE;
}
void smp_cpu_set_idle(int cpu, boolean_t idle)
{
#if NCPUS > 1
if (cpu >= 0 && cpu < NCPUS) {
struct percpu *pcpu = &percpu_array[cpu];
spl_t s = splsched();
simple_lock(&pcpu->cpu_lock);
pcpu->is_idle = idle;
pcpu->is_running = !idle;
simple_unlock(&pcpu->cpu_lock);
splx(s);
}
#endif
}
thread_t smp_get_idle_thread(int cpu)
{
#if NCPUS > 1
if (cpu >= 0 && cpu < NCPUS) {
return percpu_array[cpu].idle_thread;
}
#endif
return THREAD_NULL;
}
void smp_set_idle_thread(int cpu, thread_t thread)
{
#if NCPUS > 1
if (cpu >= 0 && cpu < NCPUS) {
struct percpu *pcpu = &percpu_array[cpu];
spl_t s = splsched();
simple_lock(&pcpu->cpu_lock);
pcpu->idle_thread = thread;
simple_unlock(&pcpu->cpu_lock);
splx(s);
}
#endif
}