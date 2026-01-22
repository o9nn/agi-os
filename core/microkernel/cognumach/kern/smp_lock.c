#include <kern/smp_lock.h>
#include <kern/smp.h>
#include <kern/cpu_number.h>
#include <kern/printf.h>
#include <kern/debug.h>
#include <machine/spl.h>
#if NCPUS > 1
struct smp_lock_stats {
unsigned long spinlock_acquisitions;
unsigned long spinlock_contentions;
unsigned long spinlock_spins;
unsigned long rwlock_read_acquisitions;
unsigned long rwlock_write_acquisitions;
unsigned long rwlock_read_contentions;
unsigned long rwlock_write_contentions;
} smp_lock_stats[NCPUS];
#define SMP_SPIN_MIN_DELAY 1
#define SMP_SPIN_MAX_DELAY 1024
#define SMP_SPIN_BACKOFF_FACTOR 2
static inline void smp_spin_delay(unsigned int delay)
{
unsigned int i;
for (i = 0; i < delay; i++) {
cpu_pause();
}
}
static inline boolean_t atomic_test_and_set(volatile unsigned int *lock)
{
unsigned int result;
asm volatile(
"lock xchgl %0, %1"
: "=r" (result), "+m" (*lock)
: "0" (1)
: "memory"
);
return (result == 0);
}
static inline boolean_t atomic_cmpxchg(volatile int *ptr, int old_val, int new_val)
{
unsigned char result;
asm volatile(
"lock cmpxchgl %2, %1; sete %0"
: "=q" (result), "+m" (*ptr)
: "r" (new_val), "a" (old_val)
: "memory"
);
return result;
}
void smp_spinlock_lock(smp_spinlock_t lock)
{
int current_cpu = cpu_number();
unsigned int delay = SMP_SPIN_MIN_DELAY;
unsigned int spins = 0;
smp_spinlock_assert(lock);
while (!atomic_test_and_set(&lock->lock_data)) {
smp_spin_delay(delay);
if (delay < SMP_SPIN_MAX_DELAY) {
delay *= SMP_SPIN_BACKOFF_FACTOR;
}
spins++;
if (spins > 10000) {
printf("SMP: Potential deadlock in spinlock %s, CPU %d spinning\n",
lock->name ? lock->name : "unnamed", current_cpu);
spins = 0;
}
}
lock->owner_cpu = current_cpu;
lock->spin_count++;
smp_lock_stats[current_cpu].spinlock_acquisitions++;
if (spins > 0) {
smp_lock_stats[current_cpu].spinlock_contentions++;
smp_lock_stats[current_cpu].spinlock_spins += spins;
}
}
boolean_t smp_spinlock_try_lock(smp_spinlock_t lock)
{
int current_cpu = cpu_number();
smp_spinlock_assert(lock);
if (atomic_test_and_set(&lock->lock_data)) {
lock->owner_cpu = current_cpu;
lock->spin_count++;
smp_lock_stats[current_cpu].spinlock_acquisitions++;
return TRUE;
}
return FALSE;
}
void smp_spinlock_unlock(smp_spinlock_t lock)
{
smp_spinlock_assert(lock);
if (lock->owner_cpu != cpu_number()) {
panic("SMP: Attempting to unlock spinlock %s not owned by current CPU %d (owner: %d)",
lock->name ? lock->name : "unnamed", cpu_number(), lock->owner_cpu);
}
lock->owner_cpu = -1;
asm volatile("mfence" ::: "memory");
lock->lock_data = 0;
}
boolean_t smp_spinlock_locked(smp_spinlock_t lock)
{
smp_spinlock_assert(lock);
return (lock->lock_data != 0);
}
void smp_rwlock_read_lock(smp_rwlock_t lock)
{
int current_cpu = cpu_number();
unsigned int spins = 0;
smp_rwlock_assert(lock);
while (1) {
int current_val = lock->lock_data;
if (current_val >= 0 && lock->waiting_writers == 0) {
if (atomic_cmpxchg(&lock->lock_data, current_val, current_val + 1)) {
__sync_fetch_and_add(&lock->reader_count, 1);
smp_lock_stats[current_cpu].rwlock_read_acquisitions++;
return;
}
}
smp_spin_delay(SMP_SPIN_MIN_DELAY);
spins++;
if (spins > 1000) {
smp_lock_stats[current_cpu].rwlock_read_contentions++;
spins = 0;
}
}
}
void smp_rwlock_read_unlock(smp_rwlock_t lock)
{
smp_rwlock_assert(lock);
__sync_fetch_and_sub(&lock->reader_count, 1);
__sync_fetch_and_sub(&lock->lock_data, 1);
}
void smp_rwlock_write_lock(smp_rwlock_t lock)
{
int current_cpu = cpu_number();
unsigned int spins = 0;
smp_rwlock_assert(lock);
__sync_fetch_and_add(&lock->waiting_writers, 1);
while (1) {
if (atomic_cmpxchg(&lock->lock_data, 0, -1)) {
lock->writer_cpu = current_cpu;
__sync_fetch_and_sub(&lock->waiting_writers, 1);
smp_lock_stats[current_cpu].rwlock_write_acquisitions++;
return;
}
smp_spin_delay(SMP_SPIN_MIN_DELAY);
spins++;
if (spins > 1000) {
smp_lock_stats[current_cpu].rwlock_write_contentions++;
spins = 0;
}
}
}
void smp_rwlock_write_unlock(smp_rwlock_t lock)
{
smp_rwlock_assert(lock);
if (lock->writer_cpu != cpu_number()) {
panic("SMP: Attempting to unlock rwlock %s not owned by current CPU %d (owner: %d)",
lock->name ? lock->name : "unnamed", cpu_number(), lock->writer_cpu);
}
lock->writer_cpu = -1;
asm volatile("mfence" ::: "memory");
lock->lock_data = 0;
}
boolean_t smp_rwlock_try_read_lock(smp_rwlock_t lock)
{
int current_cpu = cpu_number();
int current_val = lock->lock_data;
smp_rwlock_assert(lock);
if (current_val >= 0 && lock->waiting_writers == 0) {
if (atomic_cmpxchg(&lock->lock_data, current_val, current_val + 1)) {
__sync_fetch_and_add(&lock->reader_count, 1);
smp_lock_stats[current_cpu].rwlock_read_acquisitions++;
return TRUE;
}
}
return FALSE;
}
boolean_t smp_rwlock_try_write_lock(smp_rwlock_t lock)
{
int current_cpu = cpu_number();
smp_rwlock_assert(lock);
if (atomic_cmpxchg(&lock->lock_data, 0, -1)) {
lock->writer_cpu = current_cpu;
smp_lock_stats[current_cpu].rwlock_write_acquisitions++;
return TRUE;
}
return FALSE;
}
void smp_lock_stats_init(void)
{
int i;
for (i = 0; i < NCPUS; i++) {
smp_lock_stats[i].spinlock_acquisitions = 0;
smp_lock_stats[i].spinlock_contentions = 0;
smp_lock_stats[i].spinlock_spins = 0;
smp_lock_stats[i].rwlock_read_acquisitions = 0;
smp_lock_stats[i].rwlock_write_acquisitions = 0;
smp_lock_stats[i].rwlock_read_contentions = 0;
smp_lock_stats[i].rwlock_write_contentions = 0;
}
printf("SMP lock statistics initialized\n");
}
void smp_lock_stats_print(void)
{
int i;
printf("SMP Lock Statistics:\n");
for (i = 0; i < smp_get_numcpus(); i++) {
printf("CPU %d:\n", i);
printf("  Spinlock acq: %lu, cont: %lu, spins: %lu\n",
smp_lock_stats[i].spinlock_acquisitions,
smp_lock_stats[i].spinlock_contentions,
smp_lock_stats[i].spinlock_spins);
printf("  RWLock r_acq: %lu, w_acq: %lu, r_cont: %lu, w_cont: %lu\n",
smp_lock_stats[i].rwlock_read_acquisitions,
smp_lock_stats[i].rwlock_write_acquisitions,
smp_lock_stats[i].rwlock_read_contentions,
smp_lock_stats[i].rwlock_write_contentions);
}
}
#endif