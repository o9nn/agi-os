#include <kern/smp.h>
#include <kern/printf.h>
#include <kern/lock.h>
#include <kern/cpu_number.h>
#include <machine/smp.h>
#include <kern/kalloc.h>
#include <kern/thread.h>
#include <kern/sched_prim.h>
#ifndef NCPUS
#define NCPUS 1
#endif
struct smp_data {
unsigned char num_cpus;
unsigned char online_cpus;
unsigned char active_cpus;
} smp_info;
#if NCPUS > 1
struct smp_cpu_info smp_cpu_info[NCPUS];
#else
struct smp_cpu_info smp_cpu_info[1];
#endif
decl_simple_lock_data(, smp_cpu_info_lock);
static volatile int smp_barrier_count = 0;
static volatile int smp_barrier_generation = 0;
decl_simple_lock_data(static, smp_barrier_lock);
static struct smp_work_queue *cpu_work_queues = NULL;
void smp_set_numcpus(unsigned char numcpus)
{
int i;
smp_info.num_cpus = numcpus;
smp_info.online_cpus = 1;
smp_info.active_cpus = 1;
simple_lock_init(&smp_cpu_info_lock);
simple_lock_init(&smp_barrier_lock);
for (i = 0; i < NCPUS && i < numcpus; i++) {
smp_cpu_info[i].cpu_id = i;
smp_cpu_info[i].is_online = (i == 0);
smp_cpu_info[i].is_active = (i == 0);
smp_cpu_info[i].load_average = 0;
smp_cpu_info[i].idle_time = 0;
smp_cpu_info[i].busy_time = 0;
}
printf("SMP: Initialized for %d CPUs\n", numcpus);
}
unsigned char smp_get_numcpus(void)
{
unsigned char numcpus = smp_info.num_cpus;
if (numcpus == 0)
return 1;
else
return numcpus;
}
boolean_t smp_cpu_is_online(unsigned char cpu)
{
if (cpu >= NCPUS) return FALSE;
boolean_t result;
simple_lock(&smp_cpu_info_lock);
result = smp_cpu_info[cpu].is_online;
simple_unlock(&smp_cpu_info_lock);
return result;
}
void smp_cpu_set_online(unsigned char cpu, boolean_t online)
{
if (cpu >= NCPUS) return;
simple_lock(&smp_cpu_info_lock);
if (smp_cpu_info[cpu].is_online != online) {
smp_cpu_info[cpu].is_online = online;
smp_cpu_info[cpu].is_active = online;
if (online) {
smp_info.online_cpus++;
smp_info.active_cpus++;
printf("SMP: CPU %d brought online\n", cpu);
} else {
if (smp_info.online_cpus > 0) smp_info.online_cpus--;
if (smp_info.active_cpus > 0) smp_info.active_cpus--;
printf("SMP: CPU %d taken offline\n", cpu);
}
}
simple_unlock(&smp_cpu_info_lock);
}
unsigned char smp_find_idle_cpu(void)
{
#if NCPUS > 1
int i;
unsigned char idle_cpu = 0;
unsigned int min_load = 4294967295U;
simple_lock(&smp_cpu_info_lock);
for (i = 0; i < smp_info.num_cpus; i++) {
if (smp_cpu_info[i].is_online && smp_cpu_info[i].load_average < min_load) {
min_load = smp_cpu_info[i].load_average;
idle_cpu = i;
}
}
simple_unlock(&smp_cpu_info_lock);
return idle_cpu;
#else
return 0;
#endif
}
unsigned char smp_select_cpu_for_thread(void)
{
return smp_find_idle_cpu();
}
void smp_update_cpu_load(unsigned char cpu)
{
if (cpu >= NCPUS) return;
simple_lock(&smp_cpu_info_lock);
smp_cpu_info[cpu].load_average = (smp_cpu_info[cpu].busy_time * 100) /
(smp_cpu_info[cpu].busy_time + smp_cpu_info[cpu].idle_time + 1);
simple_unlock(&smp_cpu_info_lock);
}
unsigned int smp_get_cpu_load(unsigned char cpu)
{
if (cpu >= NCPUS) return 0;
unsigned int load;
simple_lock(&smp_cpu_info_lock);
load = smp_cpu_info[cpu].load_average;
simple_unlock(&smp_cpu_info_lock);
return load;
}
void smp_balance_load(void)
{
#if NCPUS > 1
printf("SMP: Load balancing triggered\n");
#endif
}
void smp_cpu_barrier(void)
{
#if NCPUS > 1
int generation;
simple_lock(&smp_barrier_lock);
generation = smp_barrier_generation;
smp_barrier_count++;
if (smp_barrier_count >= smp_info.active_cpus) {
smp_barrier_count = 0;
smp_barrier_generation++;
simple_unlock(&smp_barrier_lock);
return;
}
simple_unlock(&smp_barrier_lock);
while (smp_barrier_generation == generation) {
continue;
}
#endif
}
void smp_synchronize_cpus(void)
{
#if NCPUS > 1
printf("SMP: Synchronizing all CPUs\n");
smp_cpu_barrier();
printf("SMP: CPU synchronization complete\n");
#endif
}
void smp_work_queue_init(void)
{
int cpu;
int num_cpus = smp_get_numcpus();
if (cpu_work_queues != NULL)
return;
cpu_work_queues = (struct smp_work_queue *)
kalloc((size_t)num_cpus * sizeof(struct smp_work_queue));
if (cpu_work_queues == NULL)
return;
for (cpu = 0; cpu < num_cpus; cpu++) {
queue_init(&cpu_work_queues[cpu].work_items);
simple_lock_init(&cpu_work_queues[cpu].lock);
cpu_work_queues[cpu].worker_thread = NULL;
cpu_work_queues[cpu].cpu_id = cpu;
cpu_work_queues[cpu].active = TRUE;
}
printf("SMP: Work queues initialized for %d CPUs\n", num_cpus);
}
kern_return_t smp_queue_work(int cpu, void (*func)(void *), void *arg)
{
struct smp_work_item *work_item;
struct smp_work_queue *wq;
int target_cpu = cpu;
if (cpu_work_queues == NULL)
return KERN_FAILURE;
if (target_cpu < 0 || target_cpu >= smp_get_numcpus()) {
target_cpu = smp_find_idle_cpu();
}
wq = &cpu_work_queues[target_cpu];
work_item = (struct smp_work_item *)kalloc(sizeof(struct smp_work_item));
if (work_item == NULL)
return KERN_RESOURCE_SHORTAGE;
work_item->func = func;
work_item->arg = arg;
work_item->flags = 0;
simple_lock(&wq->lock);
queue_enter(&wq->work_items, work_item, struct smp_work_item *, chain);
simple_unlock(&wq->lock);
if (wq->worker_thread != NULL) {
thread_wakeup((event_t)wq);
}
return KERN_SUCCESS;
}
kern_return_t smp_queue_work_on(int cpu, void (*func)(void *), void *arg)
{
if (cpu < 0 || cpu >= smp_get_numcpus())
return KERN_INVALID_ARGUMENT;
if (!smp_cpu_is_online(cpu))
return KERN_FAILURE;
return smp_queue_work(cpu, func, arg);
}
void smp_work_thread(void)
{
struct smp_work_queue *wq;
struct smp_work_item *work_item;
int cpu_id = 0;
#ifdef NCPUS
if (NCPUS > 1) {
}
#endif
if (cpu_work_queues == NULL || cpu_id >= smp_get_numcpus())
return;
wq = &cpu_work_queues[cpu_id];
wq->worker_thread = current_thread();
printf("SMP: Work thread started for CPU %d\n", cpu_id);
while (wq->active) {
simple_lock(&wq->lock);
if (queue_empty(&wq->work_items)) {
simple_unlock(&wq->lock);
assert_wait((event_t)wq, FALSE);
thread_block((continuation_t)0);
continue;
}
work_item = (struct smp_work_item *)queue_first(&wq->work_items);
queue_remove(&wq->work_items, work_item, struct smp_work_item *, chain);
simple_unlock(&wq->lock);
if (work_item->func != NULL) {
work_item->func(work_item->arg);
}
kfree((vm_offset_t)work_item, sizeof(struct smp_work_item));
}
printf("SMP: Work thread terminated for CPU %d\n", cpu_id);
}