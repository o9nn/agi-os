#include <kern/sched_prim.h>
#include <kern/smp.h>
#include <kern/processor.h>
#include <kern/thread.h>
#include <kern/cpu_number.h>
#include <kern/lock.h>
#include <kern/printf.h>
#include <machine/spl.h>
#ifndef INT_MAX
#define INT_MAX 2147483647
#endif
#if NCPUS > 1
#define SMP_LOAD_BALANCE_THRESHOLD 2
#define SMP_IDLE_STEAL_THRESHOLD 1
#define SMP_MIGRATION_COST 100
struct smp_sched_stats {
unsigned long migrations;
unsigned long load_balances;
unsigned long idle_steals;
unsigned long cache_misses;
} smp_sched_stats[NCPUS];
decl_simple_lock_data(static, smp_sched_lock);
void smp_sched_init(void)
{
int i;
simple_lock_init(&smp_sched_lock);
for (i = 0; i < NCPUS; i++) {
smp_sched_stats[i].migrations = 0;
smp_sched_stats[i].load_balances = 0;
smp_sched_stats[i].idle_steals = 0;
smp_sched_stats[i].cache_misses = 0;
}
printf("SMP scheduler enhancements initialized\n");
}
processor_t smp_choose_processor(thread_t thread, processor_set_t pset)
{
processor_t processor, best_processor = PROCESSOR_NULL;
int min_load = INT_MAX;
int current_cpu = cpu_number();
if (!thread || !pset) {
return PROCESSOR_NULL;
}
if (thread->bound_processor != PROCESSOR_NULL) {
return thread->bound_processor;
}
if (pset->idle_count > 0) {
processor_t idle_processor = (processor_t) queue_first(&pset->idle_queue);
#ifdef HW_FOOTPRINT
if (thread->last_processor &&
thread->last_processor->state == PROCESSOR_IDLE &&
thread->last_processor->processor_set == pset) {
return thread->last_processor;
}
#endif
return idle_processor;
}
queue_iterate(&pset->processors, processor, processor_t, processors) {
int cpu = processor_to_cpu_id(processor);
int load = smp_get_cpu_load(cpu);
if (thread->last_processor == processor) {
load -= 20;
}
if (load < min_load) {
min_load = load;
best_processor = processor;
}
}
return best_processor;
}
boolean_t smp_should_migrate_thread(thread_t thread, processor_t from_proc, processor_t to_proc)
{
int from_cpu, to_cpu;
int from_load, to_load;
if (!thread || !from_proc || !to_proc || from_proc == to_proc) {
return FALSE;
}
from_cpu = processor_to_cpu_id(from_proc);
to_cpu = processor_to_cpu_id(to_proc);
from_load = smp_get_cpu_load(from_cpu);
to_load = smp_get_cpu_load(to_cpu);
if ((from_load - to_load) < SMP_LOAD_BALANCE_THRESHOLD) {
return FALSE;
}
if (thread->last_processor == from_proc &&
(from_load - to_load) < SMP_MIGRATION_COST) {
return FALSE;
}
return TRUE;
}
void smp_balance_pset_load(processor_set_t pset)
{
processor_t busiest_proc = PROCESSOR_NULL, idlest_proc = PROCESSOR_NULL;
processor_t processor;
int max_load = 0, min_load = INT_MAX;
int current_cpu = cpu_number();
thread_t thread;
if (!pset || pset->processor_count <= 1) {
return;
}
simple_lock(&smp_sched_lock);
queue_iterate(&pset->processors, processor, processor_t, processors) {
int cpu = processor_to_cpu_id(processor);
int load = smp_get_cpu_load(cpu);
if (load > max_load) {
max_load = load;
busiest_proc = processor;
}
if (load < min_load) {
min_load = load;
idlest_proc = processor;
}
}
if (!busiest_proc || !idlest_proc ||
(max_load - min_load) < SMP_LOAD_BALANCE_THRESHOLD) {
simple_unlock(&smp_sched_lock);
return;
}
if (busiest_proc->runq.count > 1) {
queue_iterate(&busiest_proc->runq.runq[NRQS-1], thread, thread_t, links) {
if (smp_should_migrate_thread(thread, busiest_proc, idlest_proc)) {
int from_cpu = processor_to_cpu_id(busiest_proc);
smp_sched_stats[from_cpu].migrations++;
smp_sched_stats[current_cpu].load_balances++;
printf("SMP: Migrating thread from CPU %d to CPU %d (load: %d -> %d)\n",
from_cpu, processor_to_cpu_id(idlest_proc), max_load, min_load);
break;
}
}
}
simple_unlock(&smp_sched_lock);
}
boolean_t smp_steal_work(processor_t idle_proc)
{
processor_set_t pset;
processor_t processor;
thread_t thread;
int idle_cpu;
if (!idle_proc || idle_proc->state != PROCESSOR_IDLE) {
return FALSE;
}
idle_cpu = processor_to_cpu_id(idle_proc);
pset = idle_proc->processor_set;
if (!pset || pset->processor_count <= 1) {
return FALSE;
}
queue_iterate(&pset->processors, processor, processor_t, processors) {
if (processor == idle_proc) continue;
int cpu = processor_to_cpu_id(processor);
int load = smp_get_cpu_load(cpu);
if (load > SMP_IDLE_STEAL_THRESHOLD && processor->runq.count > 1) {
queue_iterate(&processor->runq.runq[NRQS-1], thread, thread_t, links) {
if (thread->bound_processor == PROCESSOR_NULL) {
smp_sched_stats[idle_cpu].idle_steals++;
printf("SMP: CPU %d stealing work from CPU %d\n", idle_cpu, cpu);
return TRUE;
}
}
}
}
return FALSE;
}
void smp_enhanced_thread_setrun(thread_t th, boolean_t may_preempt)
{
processor_t processor;
processor_set_t pset;
if (!th) return;
pset = th->processor_set;
processor = smp_choose_processor(th, pset);
if (processor && processor != current_processor()) {
int target_cpu = processor_to_cpu_id(processor);
smp_update_cpu_load(target_cpu);
th->last_processor = processor;
}
if ((sched_tick % 100) == 0) {
smp_balance_pset_load(pset);
}
}
int processor_to_cpu_id(processor_t processor)
{
return 0;
}
#endif