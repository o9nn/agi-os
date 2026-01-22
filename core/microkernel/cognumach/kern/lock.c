#include <string.h>
#include <machine/smp.h>
#include <kern/debug.h>
#include <kern/lock.h>
#include <kern/thread.h>
#include <kern/sched_prim.h>
#define LOCK_THREAD_INVALID ((struct thread *)-1)
#if MACH_KDB
#include <machine/db_machdep.h>
#include <ddb/db_output.h>
#include <ddb/db_sym.h>
#endif
#if NCPUS > 1
#ifdef notdef
void simple_lock_init(simple_lock_t l)
{
*(boolean_t *)l = FALSE;
}
void simple_lock(simple_lock_t l)
{
while (test_and_set((boolean_t *)l))
cpu_pause();
}
void simple_unlock(simple_lock_t l)
{
*(boolean_t *)l = FALSE;
}
boolean_t simple_lock_try(simple_lock_t l)
{
return (!test_and_set((boolean_t *)l));
}
#endif
#endif
#if NCPUS > 1
static int lock_wait_time = 100;
#else
static int lock_wait_time = 0;
#endif
#if MACH_SLOCKS && NCPUS == 1
unsigned int simple_locks_taken = 0;
#define NSLINFO 1000
struct simple_locks_info {
simple_lock_t l;
const char *expr;
const char *loc;
} simple_locks_info[NSLINFO];
int do_check_simple_locks = 1;
void check_simple_locks(void)
{
assert(! do_check_simple_locks || simple_locks_taken == 0);
}
void check_simple_locks_enable(void)
{
do_check_simple_locks = 1;
}
void check_simple_locks_disable(void)
{
do_check_simple_locks = 0;
}
void simple_lock_init(
simple_lock_t l)
{
l->lock_data = 0;
}
void _simple_lock(
simple_lock_t l,
const char *expression,
const char *location)
{
struct simple_locks_info *info;
assert(l->lock_data == 0);
l->lock_data = 1;
info = &simple_locks_info[simple_locks_taken++];
barrier();
info->l = l;
info->expr = expression;
info->loc = location;
}
boolean_t _simple_lock_try(
simple_lock_t l,
const char *expression,
const char *location)
{
struct simple_locks_info *info;
if (l->lock_data != 0)
return FALSE;
l->lock_data = 1;
info = &simple_locks_info[simple_locks_taken++];
barrier();
info->l = l;
info->expr = expression;
info->loc = location;
return TRUE;
}
void _simple_unlock(
simple_lock_t l)
{
assert(l->lock_data != 0);
l->lock_data = 0;
if (simple_locks_info[simple_locks_taken-1].l != l) {
unsigned int i = simple_locks_taken;
do
if (i == 0)
panic("simple_unlock");
while (simple_locks_info[--i].l != l);
simple_locks_info[i] = simple_locks_info[simple_locks_taken-1];
}
barrier();
simple_locks_taken--;
simple_locks_info[simple_locks_taken] = (struct simple_locks_info) {0};
}
#endif
void lock_init(
lock_t l,
boolean_t can_sleep)
{
memset(l, 0, sizeof(lock_data_t));
simple_lock_init(&l->interlock);
l->want_write = FALSE;
l->want_upgrade = FALSE;
l->read_count = 0;
l->can_sleep = can_sleep;
l->thread = LOCK_THREAD_INVALID;
l->recursion_depth = 0;
}
void lock_sleepable(
lock_t l,
boolean_t can_sleep)
{
simple_lock(&l->interlock);
l->can_sleep = can_sleep;
simple_unlock(&l->interlock);
}
void lock_write(
lock_t l)
{
int i;
check_simple_locks();
simple_lock(&l->interlock);
if (l->thread == current_thread()) {
l->recursion_depth++;
simple_unlock(&l->interlock);
return;
}
while (l->want_write) {
if ((i = lock_wait_time) > 0) {
simple_unlock(&l->interlock);
while (--i > 0 && l->want_write)
cpu_pause();
simple_lock(&l->interlock);
}
if (l->can_sleep && l->want_write) {
l->waiting = TRUE;
thread_sleep(l,
simple_lock_addr(l->interlock), FALSE);
simple_lock(&l->interlock);
}
}
l->want_write = TRUE;
while ((l->read_count != 0) || l->want_upgrade) {
if ((i = lock_wait_time) > 0) {
simple_unlock(&l->interlock);
while (--i > 0 && (l->read_count != 0 ||
l->want_upgrade))
cpu_pause();
simple_lock(&l->interlock);
}
if (l->can_sleep && (l->read_count != 0 || l->want_upgrade)) {
l->waiting = TRUE;
thread_sleep(l,
simple_lock_addr(l->interlock), FALSE);
simple_lock(&l->interlock);
}
}
#if MACH_LDEBUG
l->writer = current_thread();
#endif
simple_unlock(&l->interlock);
}
void lock_done(
lock_t l)
{
simple_lock(&l->interlock);
if (l->read_count != 0)
l->read_count--;
else
if (l->recursion_depth != 0)
l->recursion_depth--;
else
if (l->want_upgrade) {
l->want_upgrade = FALSE;
#if MACH_LDEBUG
assert(l->writer == current_thread());
l->writer = THREAD_NULL;
#endif
} else {
l->want_write = FALSE;
#if MACH_LDEBUG
assert(l->writer == current_thread());
l->writer = THREAD_NULL;
#endif
}
if (l->waiting && (l->read_count == 0)) {
l->waiting = FALSE;
thread_wakeup(l);
}
simple_unlock(&l->interlock);
}
void lock_read(
lock_t l)
{
check_simple_locks();
simple_lock(&l->interlock);
if (l->thread == current_thread()) {
l->read_count++;
simple_unlock(&l->interlock);
return;
}
while (l->want_write || l->want_upgrade) {
int i;
if ((i = lock_wait_time) > 0) {
simple_unlock(&l->interlock);
while (--i > 0 && (l->want_write || l->want_upgrade))
cpu_pause();
simple_lock(&l->interlock);
}
if (l->can_sleep && (l->want_write || l->want_upgrade)) {
l->waiting = TRUE;
thread_sleep(l,
simple_lock_addr(l->interlock), FALSE);
simple_lock(&l->interlock);
}
}
l->read_count++;
simple_unlock(&l->interlock);
}
boolean_t lock_read_to_write(
lock_t l)
{
int i;
check_simple_locks();
simple_lock(&l->interlock);
l->read_count--;
if (l->thread == current_thread()) {
l->recursion_depth++;
simple_unlock(&l->interlock);
return(FALSE);
}
if (l->want_upgrade) {
if (l->waiting && (l->read_count == 0)) {
l->waiting = FALSE;
thread_wakeup(l);
}
simple_unlock(&l->interlock);
return TRUE;
}
l->want_upgrade = TRUE;
while (l->read_count != 0) {
if ((i = lock_wait_time) > 0) {
simple_unlock(&l->interlock);
while (--i > 0 && l->read_count != 0)
cpu_pause();
simple_lock(&l->interlock);
}
if (l->can_sleep && l->read_count != 0) {
l->waiting = TRUE;
thread_sleep(l,
simple_lock_addr(l->interlock), FALSE);
simple_lock(&l->interlock);
}
}
#if MACH_LDEBUG
l->writer = current_thread();
#endif
simple_unlock(&l->interlock);
return FALSE;
}
void lock_write_to_read(
lock_t l)
{
simple_lock(&l->interlock);
#if MACH_LDEBUG
assert(l->writer == current_thread());
#endif
l->read_count++;
if (l->recursion_depth != 0)
l->recursion_depth--;
else
if (l->want_upgrade)
l->want_upgrade = FALSE;
else
l->want_write = FALSE;
if (l->waiting) {
l->waiting = FALSE;
thread_wakeup(l);
}
#if MACH_LDEBUG
assert(l->writer == current_thread());
l->writer = THREAD_NULL;
#endif
simple_unlock(&l->interlock);
}
boolean_t lock_try_write(
lock_t l)
{
simple_lock(&l->interlock);
if (l->thread == current_thread()) {
l->recursion_depth++;
simple_unlock(&l->interlock);
return TRUE;
}
if (l->want_write || l->want_upgrade || l->read_count) {
simple_unlock(&l->interlock);
return FALSE;
}
l->want_write = TRUE;
#if MACH_LDEBUG
l->writer = current_thread();
#endif
simple_unlock(&l->interlock);
return TRUE;
}
boolean_t lock_try_read(
lock_t l)
{
simple_lock(&l->interlock);
if (l->thread == current_thread()) {
l->read_count++;
simple_unlock(&l->interlock);
return TRUE;
}
if (l->want_write || l->want_upgrade) {
simple_unlock(&l->interlock);
return FALSE;
}
l->read_count++;
simple_unlock(&l->interlock);
return TRUE;
}
boolean_t lock_try_read_to_write(
lock_t l)
{
check_simple_locks();
simple_lock(&l->interlock);
if (l->thread == current_thread()) {
l->read_count--;
l->recursion_depth++;
simple_unlock(&l->interlock);
return TRUE;
}
if (l->want_upgrade) {
simple_unlock(&l->interlock);
return FALSE;
}
l->want_upgrade = TRUE;
l->read_count--;
while (l->read_count != 0) {
l->waiting = TRUE;
thread_sleep(l,
simple_lock_addr(l->interlock), FALSE);
simple_lock(&l->interlock);
}
#if MACH_LDEBUG
l->writer = current_thread();
#endif
simple_unlock(&l->interlock);
return TRUE;
}
void lock_set_recursive(
lock_t l)
{
simple_lock(&l->interlock);
#if MACH_LDEBUG
assert(l->writer == current_thread());
#endif
if (!l->want_write) {
panic("lock_set_recursive: don't have write lock");
}
l->thread = current_thread();
simple_unlock(&l->interlock);
}
void lock_clear_recursive(
lock_t l)
{
simple_lock(&l->interlock);
if (l->thread != current_thread()) {
panic("lock_clear_recursive: wrong thread");
}
if (l->recursion_depth == 0)
l->thread = LOCK_THREAD_INVALID;
simple_unlock(&l->interlock);
}
#if MACH_KDB
#if MACH_SLOCKS && NCPUS == 1
void db_show_all_slocks(void)
{
int i;
struct simple_locks_info *info;
simple_lock_t l;
for (i = 0; i < simple_locks_taken; i++) {
info = &simple_locks_info[i];
db_printf("%d: %s (", i, info->expr);
db_printsym((uintptr_t) info->l, DB_STGY_ANY);
db_printf(") locked by %s\n", info->loc);
}
}
#else
void db_show_all_slocks(void)
{
#if MACH_LOCK_MON
lip();
#else
db_printf("simple lock info not available\n");
#endif
}
#endif
#endif