#include <string.h>
#include <mach/machine/vm_types.h>
#include <mach/vm_param.h>
#include <mach/task_info.h>
#include <mach/task_special_ports.h>
#include <mach_debug/mach_debug_types.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_types.h>
#include <kern/debug.h>
#include <kern/task.h>
#include <kern/thread.h>
#include <kern/slab.h>
#include <kern/gnumach.server.h>
#include <kern/kalloc.h>
#include <kern/mach.server.h>
#include <kern/mach_host.server.h>
#include <kern/processor.h>
#include <kern/printf.h>
#include <kern/sched_prim.h>
#include <kern/ipc_tt.h>
#include <kern/syscall_emulation.h>
#include <kern/task_notify.user.h>
#include <vm/vm_kern.h>
#include <machine/spl.h>
task_t kernel_task = TASK_NULL;
struct kmem_cache task_cache;
ipc_port_t new_task_notification = NULL;
void task_init(void)
{
kmem_cache_init(&task_cache, "task", sizeof(struct task), 0,
NULL, 0);
eml_init();
machine_task_module_init ();
(void) task_create_kernel(TASK_NULL, FALSE, &kernel_task);
(void) task_set_name(kernel_task, "gnumach");
vm_map_set_name(kernel_map, kernel_task->name);
}
kern_return_t task_create(
task_t parent_task,
boolean_t inherit_memory,
task_t *child_task)
{
if (parent_task == TASK_NULL)
return KERN_INVALID_TASK;
return task_create_kernel (parent_task, inherit_memory,
child_task);
}
kern_return_t
task_create_kernel(
task_t parent_task,
boolean_t inherit_memory,
task_t *child_task)
{
task_t new_task;
processor_set_t pset;
#if FAST_TAS
int i;
#endif
new_task = (task_t) kmem_cache_alloc(&task_cache);
if (new_task == TASK_NULL)
return KERN_RESOURCE_SHORTAGE;
new_task->ref_count = 2;
if (child_task == &kernel_task) {
new_task->map = kernel_map;
} else if (inherit_memory) {
new_task->map = vm_map_fork(parent_task->map);
} else {
pmap_t new_pmap = pmap_create((vm_size_t) 0);
if (new_pmap == PMAP_NULL)
new_task->map = VM_MAP_NULL;
else {
new_task->map = vm_map_create(new_pmap,
round_page(VM_MIN_USER_ADDRESS),
trunc_page(VM_MAX_USER_ADDRESS));
if (new_task->map == VM_MAP_NULL)
pmap_destroy(new_pmap);
}
}
if (new_task->map == VM_MAP_NULL) {
kmem_cache_free(&task_cache, (vm_address_t) new_task);
return KERN_RESOURCE_SHORTAGE;
}
if (child_task != &kernel_task)
vm_map_set_name(new_task->map, new_task->name);
simple_lock_init(&new_task->lock);
queue_init(&new_task->thread_list);
new_task->suspend_count = 0;
new_task->active = TRUE;
new_task->user_stop_count = 0;
new_task->thread_count = 0;
new_task->faults = 0;
new_task->zero_fills = 0;
new_task->reactivations = 0;
new_task->pageins = 0;
new_task->cow_faults = 0;
new_task->messages_sent = 0;
new_task->messages_received = 0;
eml_task_reference(new_task, parent_task);
ipc_task_init(new_task, parent_task);
machine_task_init (new_task);
time_value64_init(&new_task->total_user_time);
time_value64_init(&new_task->total_system_time);
record_time_stamp (&new_task->creation_time);
if (parent_task != TASK_NULL) {
task_lock(parent_task);
pset = parent_task->processor_set;
if (!pset->active)
pset = &default_pset;
pset_reference(pset);
new_task->priority = parent_task->priority;
task_unlock(parent_task);
}
else {
pset = &default_pset;
pset_reference(pset);
new_task->priority = BASEPRI_USER;
}
pset_lock(pset);
pset_add_task(pset, new_task);
pset_unlock(pset);
new_task->may_assign = TRUE;
new_task->assign_active = FALSE;
new_task->essential = FALSE;
#if MACH_PCSAMPLE
new_task->pc_sample.buffer = 0;
new_task->pc_sample.seqno = 0;
new_task->pc_sample.sampletypes = 0;
#endif
#if FAST_TAS
for (i = 0; i < TASK_FAST_TAS_NRAS; i++) {
if (inherit_memory) {
new_task->fast_tas_base[i] = parent_task->fast_tas_base[i];
new_task->fast_tas_end[i] = parent_task->fast_tas_end[i];
} else {
new_task->fast_tas_base[i] = (vm_offset_t)0;
new_task->fast_tas_end[i] = (vm_offset_t)0;
}
}
#endif
if (parent_task == TASK_NULL)
snprintf (new_task->name, sizeof new_task->name, "%p",
new_task);
else
snprintf (new_task->name, sizeof new_task->name, "(%.*s)",
(int) (sizeof new_task->name - 3), parent_task->name);
if (new_task_notification != NULL) {
task_reference (new_task);
task_reference (parent_task);
mach_notify_new_task (new_task_notification,
convert_task_to_port (new_task),
parent_task
? convert_task_to_port (parent_task)
: IP_NULL);
}
ipc_task_enable(new_task);
*child_task = new_task;
return KERN_SUCCESS;
}
void task_deallocate(
task_t task)
{
int c;
processor_set_t pset;
if (task == TASK_NULL)
return;
task_lock(task);
c = --(task->ref_count);
task_unlock(task);
if (c != 0)
return;
machine_task_terminate (task);
eml_task_deallocate(task);
pset = task->processor_set;
pset_lock(pset);
pset_remove_task(pset,task);
pset_unlock(pset);
pset_deallocate(pset);
vm_map_deallocate(task->map);
is_release(task->itk_space);
kmem_cache_free(&task_cache, (vm_offset_t) task);
}
void task_reference(
task_t task)
{
if (task == TASK_NULL)
return;
task_lock(task);
task->ref_count++;
task_unlock(task);
}
kern_return_t task_terminate(
task_t task)
{
thread_t thread, cur_thread;
queue_head_t *list;
task_t cur_task;
spl_t s;
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
list = &task->thread_list;
cur_task = current_task();
cur_thread = current_thread();
if (task == cur_task) {
task_lock(task);
if (!task->active) {
task_unlock(task);
return KERN_FAILURE;
}
s = splsched();
thread_lock(cur_thread);
if (!cur_thread->active) {
thread_unlock(cur_thread);
(void) splx(s);
task_unlock(task);
thread_terminate(cur_thread);
return KERN_FAILURE;
}
task_hold_locked(task);
task->active = FALSE;
queue_remove(list, cur_thread, thread_t, thread_list);
thread_unlock(cur_thread);
(void) splx(s);
task_unlock(task);
ipc_thread_disable(cur_thread);
ipc_thread_terminate(cur_thread);
}
else {
if ((vm_offset_t)task < (vm_offset_t)cur_task) {
task_lock(task);
task_lock(cur_task);
}
else {
task_lock(cur_task);
task_lock(task);
}
s = splsched();
thread_lock(cur_thread);
if ((!cur_task->active) ||(!cur_thread->active)) {
thread_unlock(cur_thread);
(void) splx(s);
task_unlock(task);
task_unlock(cur_task);
thread_terminate(cur_thread);
return KERN_FAILURE;
}
thread_unlock(cur_thread);
(void) splx(s);
task_unlock(cur_task);
if (!task->active) {
task_unlock(task);
return KERN_FAILURE;
}
task_hold_locked(task);
task->active = FALSE;
task_unlock(task);
}
(void) task_dowait(task,TRUE);
ipc_task_disable(task);
task_lock(task);
while (!queue_empty(list)) {
thread = (thread_t) queue_first(list);
thread_reference(thread);
task_unlock(task);
thread_force_terminate(thread);
thread_deallocate(thread);
thread_block(thread_no_continuation);
task_lock(task);
}
task_unlock(task);
ipc_task_terminate(task);
task_deallocate(task);
if (cur_thread->task == task) {
task_lock(task);
s = splsched();
queue_enter(list, cur_thread, thread_t, thread_list);
(void) splx(s);
task_unlock(task);
(void) thread_terminate(cur_thread);
}
return KERN_SUCCESS;
}
void task_hold_locked(
task_t task)
{
queue_head_t *list;
thread_t thread, cur_thread;
assert(task->active);
cur_thread = current_thread();
task->suspend_count++;
list = &task->thread_list;
queue_iterate(list, thread, thread_t, thread_list) {
if (thread != cur_thread)
thread_hold(thread);
}
}
kern_return_t task_hold(
task_t task)
{
task_lock(task);
if (!task->active) {
task_unlock(task);
return KERN_FAILURE;
}
task_hold_locked(task);
task_unlock(task);
return KERN_SUCCESS;
}
kern_return_t task_dowait(
task_t task,
boolean_t must_wait)
{
queue_head_t *list;
thread_t thread, cur_thread, prev_thread;
kern_return_t ret = KERN_SUCCESS;
cur_thread = current_thread();
list = &task->thread_list;
prev_thread = THREAD_NULL;
task_lock(task);
queue_iterate(list, thread, thread_t, thread_list) {
if (!(task->active) && !(must_wait)) {
ret = KERN_FAILURE;
break;
}
if (thread != cur_thread) {
thread_reference(thread);
task_unlock(task);
if (prev_thread != THREAD_NULL)
thread_deallocate(prev_thread);
(void) thread_dowait(thread, TRUE);
prev_thread = thread;
task_lock(task);
}
}
task_unlock(task);
if (prev_thread != THREAD_NULL)
thread_deallocate(prev_thread);
return ret;
}
kern_return_t task_release(
task_t task)
{
queue_head_t *list;
thread_t thread, next;
task_lock(task);
if (!task->active) {
task_unlock(task);
return KERN_FAILURE;
}
task->suspend_count--;
list = &task->thread_list;
thread = (thread_t) queue_first(list);
while (!queue_end(list, (queue_entry_t) thread)) {
next = (thread_t) queue_next(&thread->thread_list);
thread_release(thread);
thread = next;
}
task_unlock(task);
return KERN_SUCCESS;
}
kern_return_t task_threads(
task_t task,
thread_array_t *thread_list,
natural_t *count)
{
unsigned int actual;
thread_t thread;
thread_t *threads;
unsigned i;
vm_size_t size, size_needed;
vm_offset_t addr;
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
size = 0; addr = 0;
for (;;) {
task_lock(task);
if (!task->active) {
task_unlock(task);
return KERN_FAILURE;
}
actual = task->thread_count;
size_needed = actual * sizeof(mach_port_t);
if (size_needed <= size)
break;
task_unlock(task);
if (size != 0)
kfree(addr, size);
assert(size_needed > 0);
size = size_needed;
addr = kalloc(size);
if (addr == 0)
return KERN_RESOURCE_SHORTAGE;
}
threads = (thread_t *) addr;
for (i = 0, thread = (thread_t) queue_first(&task->thread_list);
i < actual;
i++, thread = (thread_t) queue_next(&thread->thread_list)) {
thread_reference(thread);
threads[i] = thread;
}
assert(queue_end(&task->thread_list, (queue_entry_t) thread));
task_unlock(task);
if (actual == 0) {
*thread_list = 0;
*count = 0;
if (size != 0)
kfree(addr, size);
} else {
if (size_needed < size) {
vm_offset_t newaddr;
newaddr = kalloc(size_needed);
if (newaddr == 0) {
for (i = 0; i < actual; i++)
thread_deallocate(threads[i]);
kfree(addr, size);
return KERN_RESOURCE_SHORTAGE;
}
memcpy((void *) newaddr, (void *) addr, size_needed);
kfree(addr, size);
threads = (thread_t *) newaddr;
}
*thread_list = (mach_port_t *) threads;
*count = actual;
for (i = 0; i < actual; i++)
((ipc_port_t *) threads)[i] =
convert_thread_to_port(threads[i]);
}
return KERN_SUCCESS;
}
kern_return_t task_suspend(
task_t task)
{
boolean_t hold;
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
hold = FALSE;
task_lock(task);
if ((task->user_stop_count)++ == 0)
hold = TRUE;
task_unlock(task);
if (!hold) {
return KERN_SUCCESS;
}
if (task_hold(task) != KERN_SUCCESS)
return KERN_FAILURE;
if (task_dowait(task, FALSE) != KERN_SUCCESS)
return KERN_FAILURE;
if (current_task() == task) {
spl_t s;
thread_hold(current_thread());
s = splsched();
ast_on(cpu_number(), AST_BLOCK);
(void) splx(s);
}
return KERN_SUCCESS;
}
kern_return_t task_resume(
task_t task)
{
boolean_t release;
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
release = FALSE;
task_lock(task);
if (task->user_stop_count > 0) {
if (--(task->user_stop_count) == 0)
release = TRUE;
}
else {
task_unlock(task);
return KERN_FAILURE;
}
task_unlock(task);
if (release)
return task_release(task);
return KERN_SUCCESS;
}
kern_return_t task_info(
task_t task,
int flavor,
task_info_t task_info_out,
natural_t *task_info_count)
{
vm_map_t map;
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
switch (flavor) {
case TASK_BASIC_INFO:
{
task_basic_info_t basic_info;
if (*task_info_count <
TASK_BASIC_INFO_COUNT - 3 * sizeof(time_value64_t)/sizeof(integer_t))
return KERN_INVALID_ARGUMENT;
basic_info = (task_basic_info_t) task_info_out;
map = (task == kernel_task) ? kernel_map : task->map;
basic_info->virtual_size = map->size;
basic_info->resident_size = ((rpc_vm_size_t) pmap_resident_count(map->pmap))
* PAGE_SIZE;
task_lock(task);
basic_info->base_priority = task->priority;
basic_info->suspend_count = task->user_stop_count;
TIME_VALUE64_TO_TIME_VALUE(&task->total_user_time,
&basic_info->user_time);
TIME_VALUE64_TO_TIME_VALUE(&task->total_system_time,
&basic_info->system_time);
time_value64_t creation_time64;
read_time_stamp(&task->creation_time, &creation_time64);
TIME_VALUE64_TO_TIME_VALUE(&creation_time64, &basic_info->creation_time);
if (*task_info_count == TASK_BASIC_INFO_COUNT) {
basic_info->user_time64 = task->total_user_time;
basic_info->system_time64 = task->total_system_time;
basic_info->creation_time64 = creation_time64;
}
task_unlock(task);
if (*task_info_count > TASK_BASIC_INFO_COUNT)
*task_info_count = TASK_BASIC_INFO_COUNT;
break;
}
case TASK_EVENTS_INFO:
{
task_events_info_t event_info;
if (*task_info_count < TASK_EVENTS_INFO_COUNT) {
return KERN_INVALID_ARGUMENT;
}
event_info = (task_events_info_t) task_info_out;
task_lock(task);
event_info->faults = task->faults;
event_info->zero_fills = task->zero_fills;
event_info->reactivations = task->reactivations;
event_info->pageins = task->pageins;
event_info->cow_faults = task->cow_faults;
event_info->messages_sent = task->messages_sent;
event_info->messages_received = task->messages_received;
task_unlock(task);
*task_info_count = TASK_EVENTS_INFO_COUNT;
break;
}
case TASK_THREAD_TIMES_INFO:
{
task_thread_times_info_t times_info;
thread_t thread;
if (*task_info_count < TASK_THREAD_TIMES_INFO_COUNT - (2 * sizeof(time_value64_t)) / sizeof(integer_t)) {
return KERN_INVALID_ARGUMENT;
}
times_info = (task_thread_times_info_t) task_info_out;
time_value64_t acc_user_time, acc_system_time;
time_value64_init(&acc_user_time);
time_value64_init(&acc_system_time);
task_lock(task);
queue_iterate(&task->thread_list, thread,
thread_t, thread_list)
{
time_value64_t user_time, system_time;
spl_t s;
s = splsched();
thread_lock(thread);
thread_read_times(thread, &user_time, &system_time);
thread_unlock(thread);
splx(s);
time_value64_add(&acc_user_time, &user_time);
time_value64_add(&acc_system_time, &system_time);
}
task_unlock(task);
TIME_VALUE64_TO_TIME_VALUE(&acc_user_time, &times_info->user_time);
TIME_VALUE64_TO_TIME_VALUE(&acc_system_time, &times_info->system_time);
if (*task_info_count >= TASK_THREAD_TIMES_INFO_COUNT) {
times_info->user_time64 = acc_user_time;
times_info->system_time64 = acc_system_time;
}
if (*task_info_count > TASK_THREAD_TIMES_INFO_COUNT)
*task_info_count = TASK_THREAD_TIMES_INFO_COUNT;
break;
}
default:
return KERN_INVALID_ARGUMENT;
}
return KERN_SUCCESS;
}
#if MACH_HOST
kern_return_t
task_assign(
task_t task,
processor_set_t new_pset,
boolean_t assign_threads)
{
kern_return_t ret = KERN_SUCCESS;
thread_t thread, prev_thread;
queue_head_t *list;
processor_set_t pset;
if (task == TASK_NULL || new_pset == PROCESSOR_SET_NULL) {
return KERN_INVALID_ARGUMENT;
}
task_lock(task);
while (task->may_assign == FALSE) {
task->assign_active = TRUE;
assert_wait((event_t)&task->assign_active, TRUE);
task_unlock(task);
thread_block(thread_no_continuation);
task_lock(task);
}
if (task->processor_set == new_pset) {
task_unlock(task);
return KERN_SUCCESS;
}
task->may_assign = FALSE;
task_unlock(task);
pset = task->processor_set;
Restart:
if ((vm_offset_t) pset < (vm_offset_t) new_pset) {
pset_lock(pset);
pset_lock(new_pset);
}
else {
pset_lock(new_pset);
pset_lock(pset);
}
if (!new_pset->active) {
pset_unlock(pset);
pset_unlock(new_pset);
new_pset = &default_pset;
goto Restart;
}
pset_reference(new_pset);
task_lock(task);
pset_remove_task(pset, task);
pset_add_task(new_pset, task);
pset_unlock(pset);
pset_unlock(new_pset);
if (assign_threads == FALSE) {
task->may_assign = TRUE;
if (task->assign_active) {
task->assign_active = FALSE;
thread_wakeup((event_t) &task->assign_active);
}
task_unlock(task);
pset_deallocate(pset);
return KERN_SUCCESS;
}
if (current_thread()->task == task) {
task_unlock(task);
thread_freeze(current_thread());
task_lock(task);
}
list = &task->thread_list;
prev_thread = THREAD_NULL;
queue_iterate(list, thread, thread_t, thread_list) {
if (!(task->active)) {
ret = KERN_FAILURE;
break;
}
if (thread != current_thread()) {
thread_reference(thread);
task_unlock(task);
if (prev_thread != THREAD_NULL)
thread_deallocate(prev_thread);
thread_assign(thread,new_pset);
prev_thread = thread;
task_lock(task);
}
}
task->may_assign = TRUE;
if (task->assign_active) {
task->assign_active = FALSE;
thread_wakeup((event_t)&task->assign_active);
}
task_unlock(task);
if (prev_thread != THREAD_NULL)
thread_deallocate(prev_thread);
if (current_thread()->task == task)
thread_doassign(current_thread(), new_pset, TRUE);
pset_deallocate(pset);
return ret;
}
#else
kern_return_t
task_assign(
task_t task,
processor_set_t new_pset,
boolean_t assign_threads)
{
return KERN_FAILURE;
}
#endif
kern_return_t
task_assign_default(
task_t task,
boolean_t assign_threads)
{
return task_assign(task, &default_pset, assign_threads);
}
kern_return_t task_get_assignment(
task_t task,
processor_set_t *pset)
{
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
if (!task->active)
return KERN_FAILURE;
*pset = task->processor_set;
pset_reference(*pset);
return KERN_SUCCESS;
}
kern_return_t
task_priority(
task_t task,
int priority,
boolean_t change_threads)
{
kern_return_t ret = KERN_SUCCESS;
if (task == TASK_NULL || invalid_pri(priority))
return KERN_INVALID_ARGUMENT;
task_lock(task);
task->priority = priority;
if (change_threads) {
thread_t thread;
queue_head_t *list;
list = &task->thread_list;
queue_iterate(list, thread, thread_t, thread_list) {
if (thread_priority(thread, priority, FALSE)
!= KERN_SUCCESS)
ret = KERN_FAILURE;
}
}
task_unlock(task);
return ret;
}
kern_return_t
task_set_name(
task_t task,
const_kernel_debug_name_t name)
{
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
strncpy(task->name, name, sizeof task->name - 1);
task->name[sizeof task->name - 1] = '\0';
return KERN_SUCCESS;
}
kern_return_t
task_set_essential(
task_t task,
boolean_t essential)
{
if (task == TASK_NULL)
return KERN_INVALID_ARGUMENT;
task->essential = !!essential;
return KERN_SUCCESS;
}
static void task_collect_scan(void)
{
task_t task, prev_task;
processor_set_t pset, prev_pset;
prev_task = TASK_NULL;
prev_pset = PROCESSOR_SET_NULL;
simple_lock(&all_psets_lock);
queue_iterate(&all_psets, pset, processor_set_t, all_psets) {
pset_lock(pset);
queue_iterate(&pset->tasks, task, task_t, pset_tasks) {
task_reference(task);
pset_reference(pset);
pset_unlock(pset);
simple_unlock(&all_psets_lock);
machine_task_collect (task);
pmap_collect(task->map->pmap);
if (prev_task != TASK_NULL)
task_deallocate(prev_task);
prev_task = task;
if (prev_pset != PROCESSOR_SET_NULL)
pset_deallocate(prev_pset);
prev_pset = pset;
simple_lock(&all_psets_lock);
pset_lock(pset);
}
pset_unlock(pset);
}
simple_unlock(&all_psets_lock);
if (prev_task != TASK_NULL)
task_deallocate(prev_task);
if (prev_pset != PROCESSOR_SET_NULL)
pset_deallocate(prev_pset);
}
boolean_t task_collect_allowed = TRUE;
unsigned task_collect_last_tick = 0;
unsigned task_collect_max_rate = 0;
void consider_task_collect(void)
{
if (task_collect_max_rate == 0)
task_collect_max_rate = hz;
if (task_collect_allowed &&
(sched_tick > (task_collect_last_tick +
task_collect_max_rate / (hz / 1)))) {
task_collect_last_tick = sched_tick;
task_collect_scan();
}
}
kern_return_t
task_ras_control(
task_t task,
vm_offset_t pc,
vm_offset_t endpc,
int flavor)
{
kern_return_t ret = KERN_FAILURE;
#if FAST_TAS
int i;
ret = KERN_SUCCESS;
task_lock(task);
switch (flavor) {
case TASK_RAS_CONTROL_PURGE_ALL:
for (i = 0; i < TASK_FAST_TAS_NRAS; i++) {
task->fast_tas_base[i] = task->fast_tas_end[i] = 0;
}
break;
case TASK_RAS_CONTROL_PURGE_ONE:
for (i = 0; i < TASK_FAST_TAS_NRAS; i++) {
if ( (task->fast_tas_base[i] == pc)
&& (task->fast_tas_end[i] == endpc)) {
while (i < TASK_FAST_TAS_NRAS-1) {
task->fast_tas_base[i] = task->fast_tas_base[i+1];
task->fast_tas_end[i] = task->fast_tas_end[i+1];
i++;
}
task->fast_tas_base[TASK_FAST_TAS_NRAS-1] = 0;
task->fast_tas_end[TASK_FAST_TAS_NRAS-1] = 0;
break;
}
}
if (i == TASK_FAST_TAS_NRAS) {
ret = KERN_INVALID_ADDRESS;
}
break;
case TASK_RAS_CONTROL_PURGE_ALL_AND_INSTALL_ONE:
for (i = 0; i < TASK_FAST_TAS_NRAS; i++) {
task->fast_tas_base[i] = task->fast_tas_end[i] = 0;
}
case TASK_RAS_CONTROL_INSTALL_ONE:
for (i = 0; i < TASK_FAST_TAS_NRAS; i++) {
if ( (task->fast_tas_base[i] == pc)
&& (task->fast_tas_end[i] == endpc)) {
break;
}
if ((task->fast_tas_base[i] == 0) && (task->fast_tas_end[i] == 0)){
task->fast_tas_base[i] = pc;
task->fast_tas_end[i] = endpc;
break;
}
}
if (i == TASK_FAST_TAS_NRAS) {
ret = KERN_RESOURCE_SHORTAGE;
}
break;
default: ret = KERN_INVALID_VALUE;
break;
}
task_unlock(task);
#endif
return ret;
}
kern_return_t
register_new_task_notification(
const host_t host,
ipc_port_t notification)
{
if (host == HOST_NULL)
return KERN_INVALID_HOST;
if (new_task_notification != NULL)
return KERN_NO_ACCESS;
new_task_notification = notification;
return KERN_SUCCESS;
}