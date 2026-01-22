#include <string.h>
#include <mach/boolean.h>
#include <mach/machine.h>
#include <mach/task_special_ports.h>
#include <mach/vm_param.h>
#include <ipc/ipc_init.h>
#include <kern/cpu_number.h>
#include <kern/debug.h>
#include <kern/gsync.h>
#include <kern/machine.h>
#include <kern/mach_factor.h>
#include <kern/mach_clock.h>
#include <kern/processor.h>
#include <kern/rdxtree.h>
#include <kern/sched_prim.h>
#include <kern/task.h>
#include <kern/thread.h>
#include <kern/thread_swap.h>
#include <kern/timer.h>
#include <kern/xpr.h>
#include <kern/vdso.h>
#include <mach/unified_debug.h>
#include <kern/perf_analysis.h>
#include <kern/dtrace.h>
#include <kern/new_feature.h>
#include <kern/printf.h>
#if MACH_KDB
#include <gdb_stub.h>
#endif
#include <kern/bootstrap.h>
#include <kern/startup.h>
#include "security_monitor.h"
#include "cfi_integrity.h"
#include <kern/printf.h>
#ifdef CONFIG_MACH_TRACING
#include <mach/lttng.h>
#endif
#include <mach/valgrind.h>
#include <kern/development_tools.h>
#include <kern/instrumentation_integration.h>
#include <vm/vm_kern.h>
#include <vm/vm_map.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <vm/vm_init.h>
#include <vm/vm_pageout.h>
#include <machine/spl.h>
#include <machine/pcb.h>
#include <machine/pmap.h>
#include <machine/model_dep.h>
#include <mach/version.h>
#include <device/device_init.h>
#include <device/intr.h>
#if MACH_KDB
#include <device/cons.h>
#include <gdb_stub.h>
#endif
#if ! MACH_KBD
boolean_t reboot_on_panic = TRUE;
#endif
#if NCPUS > 1
#include <machine/mp_desc.h>
#include <kern/smp.h>
#include <kern/machine.h>
#endif
extern char *kernel_cmdline;
void setup_main(void)
{
thread_t startup_thread;
phys_addr_t memsize;
#if MACH_KDB
if (strstr(kernel_cmdline, "-d ")) {
cninit();
SoftDebugger("init");
}
#else
if (strstr (kernel_cmdline, "-H ")) {
reboot_on_panic = FALSE;
}
#endif
panic_init();
unified_debug_init();
unified_debug_enable_all(TRUE);
#ifdef CONFIG_MACH_TRACING
mach_trace_early_init();
#endif
UNIFIED_DEBUG_FUNCTION_ENTRY(SYSDEBUG_SUBSYSTEM_KERNEL);
sched_init();
unified_debug_thread_init();
security_monitor_init();
cfi_init();
vm_mem_bootstrap();
unified_debug_vm_init();
rdxtree_cache_init();
ipc_bootstrap();
unified_debug_ipc_init();
vm_mem_init();
ipc_init();
vdso_init();
PMAP_ACTIVATE_KERNEL(master_cpu);
init_timers();
init_timeout();
#if XPR_DEBUG
xprbootstrap();
#endif
#if MACH_DTRACE
dtrace_init();
#endif
machine_init();
mapable_time_init();
console_timestamp_init();
perf_analysis_init();
feature_init();
#ifdef CONFIG_MACH_TRACING
mach_trace_init();
printf("LTTng-style kernel tracing initialized\n");
#endif
dynamic_probes_init();
instrumentation_integration_init();
valgrind_init();
development_tools_init();
#if MACH_KDB
gdb_stub_init();
#endif
machine_info.max_cpus = NCPUS;
memsize = vm_page_mem_size();
machine_info.memory_size = memsize;
if (machine_info.memory_size < memsize)
machine_info.memory_size = ~0;
machine_info.avail_cpus = 0;
machine_info.major_version = KERNEL_MAJOR_VERSION;
machine_info.minor_version = KERNEL_MINOR_VERSION;
task_init();
thread_init();
swapper_init();
#if MACH_HOST
pset_sys_init();
#endif
recompute_priorities(NULL);
compute_mach_factor();
gsync_setup ();
(void) thread_create(kernel_task, &startup_thread);
thread_set_name(startup_thread, "startup");
thread_start(startup_thread, start_kernel_threads);
thread_doswapin(startup_thread);
startup_thread->state |= TH_RUN;
(void) thread_resume(startup_thread);
cpu_launch_first_thread(startup_thread);
}
void start_kernel_threads(void)
{
int i;
for (i = 0; i < NCPUS; i++) {
if (machine_slot[i].is_cpu) {
thread_t th;
char name[10];
(void) thread_create(kernel_task, &th);
snprintf(name, sizeof(name), "idle/%d", i);
thread_set_name(th, name);
thread_bind(th, cpu_to_processor(i));
thread_start(th, idle_thread);
thread_doswapin(th);
(void) thread_resume(th);
}
}
(void) kernel_thread(kernel_task, "reaper", reaper_thread, (char *) 0);
(void) kernel_thread(kernel_task, "swapin", swapin_thread, (char *) 0);
(void) kernel_thread(kernel_task, "sched", sched_thread, (char *) 0);
#ifndef MACH_XEN
(void) kernel_thread(kernel_task, "intr", intr_thread, (char *)0);
#endif
#if NCPUS > 1
(void) kernel_thread(kernel_task, "action", action_thread, (char *) 0);
start_other_cpus();
#endif
device_service_create();
record_time_stamp (&kernel_task->creation_time);
bootstrap_create();
#if XPR_DEBUG
xprinit();
#endif
(void) spl0();
thread_set_name(current_thread(), "pageout");
vm_pageout();
}
void cpu_launch_first_thread(thread_t th)
{
int mycpu;
mycpu = cpu_number();
cpu_up(mycpu);
start_timer(&kernel_timer[mycpu]);
(void) splhigh();
if (th == THREAD_NULL)
th = choose_thread(cpu_to_processor(mycpu));
if (th == THREAD_NULL)
panic("cpu_launch_first_thread");
PMAP_ACTIVATE_KERNEL(mycpu);
percpu_assign(active_thread, th);
percpu_assign(active_stack, th->kernel_stack);
thread_lock(th);
th->state &= ~TH_UNINT;
thread_unlock(th);
timer_switch(&th->system_timer);
PMAP_ACTIVATE_USER(vm_map_pmap(th->task->map), th, mycpu);
startrtclock();
load_context(th);
}