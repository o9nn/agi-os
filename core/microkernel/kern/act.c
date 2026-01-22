#ifdef MIGRATING_THREADS
#include <string.h>
#include <mach/kern_return.h>
#include <mach/alert.h>
#include <kern/slab.h>
#include <kern/thread.h>
#include <kern/task.h>
#include <kern/debug.h>
#include <kern/act.h>
#include <kern/current.h>
#include "ipc_target.h"
static void special_handler(ReturnHandler *rh, struct Act *cur_act);
#ifdef ACT_STATIC_KLUDGE
#undef ACT_STATIC_KLUDGE
#define ACT_STATIC_KLUDGE 300
#endif
#ifndef ACT_STATIC_KLUDGE
static struct kmem_cache act_cache;
#else
static Act *act_freelist;
static Act free_acts[ACT_STATIC_KLUDGE];
#endif
Act null_act;
void
global_act_init(void)
{
#ifndef ACT_STATIC_KLUDGE
kmem_cache_init(&act_cache, "Act", sizeof(struct Act), 0,
NULL, 0);
#else
int i;
printf("activations: [%p-%p]\n", &free_acts[0], &free_acts[ACT_STATIC_KLUDGE]);
act_freelist = &free_acts[0];
free_acts[0].ipt_next = 0;
for (i = 1; i < ACT_STATIC_KLUDGE; i++) {
free_acts[i].ipt_next = act_freelist;
act_freelist = &free_acts[i];
}
#endif
#if 0
simple_lock_init(&null_act.lock);
refcount_init(&null_act.ref_count, 1);
#endif
act_machine_init();
}
kern_return_t act_create(task_t task, vm_offset_t user_stack,
vm_offset_t user_rbuf, vm_size_t user_rbuf_size,
struct Act **new_act)
{
Act *act;
#ifndef ACT_STATIC_KLUDGE
act = (Act*)kmem_cache_alloc(&act_cache);
if (act == 0)
return(KERN_RESOURCE_SHORTAGE);
#else
act = act_freelist;
if (act == 0) panic("out of activations");
act_freelist = act->ipt_next;
act->ipt_next = 0;
#endif
memset(act, 0, sizeof(*act));
#ifdef DEBUG
act->lower = act->higher = 0;
#endif
simple_lock_init(&act->lock);
refcount_init(&act->ref_count, 2);
act->task = task;
task_reference(task);
act->ipt = 0;
act->thread = 0;
act->suspend_count = 0;
act->active = 1;
act->handlers = 0;
act->special_handler.next = 0;
act->special_handler.handler = special_handler;
ipc_act_init(task, act);
act_machine_create(task, act, user_stack, user_rbuf, user_rbuf_size);
task_lock(task);
act->task_links.next = task->acts.next;
act->task_links.prev = &task->acts;
task->acts.next->prev = &act->task_links;
task->acts.next = &act->task_links;
task->act_count++;
task_unlock(task);
*new_act = act;
return KERN_SUCCESS;
}
static void act_free(Act *inc)
{
act_machine_destroy(inc);
ipc_act_destroy(inc);
task_deallocate(inc->task);
#ifndef ACT_STATIC_KLUDGE
kmem_cache_free(&act_cache, (vm_offset_t)inc);
#else
inc->ipt_next = act_freelist;
act_freelist = inc;
#endif
}
void act_deallocate(Act *inc)
{
refcount_drop(&inc->ref_count, act_free(inc));
}
void act_attach(Act *act, thread_t thread, unsigned init_alert_mask)
{
Act *lower;
act->thread = thread;
refcount_take(&act->ref_count);
lower = thread->top_act;
act->lower = lower;
lower->higher = act;
thread->top_act = act;
act->alert_mask = init_alert_mask;
act->alerts = lower->alerts & init_alert_mask;
}
void act_detach(Act *cur_act)
{
thread_t cur_thread = cur_act->thread;
thread_lock(cur_thread);
act_lock(cur_act);
cur_thread->top_act = cur_act->lower;
cur_act->thread = 0;
#ifdef DEBUG
cur_act->lower = cur_act->higher = 0;
#endif
thread_unlock(cur_thread);
if (cur_act->ipt)
{
ipt_lock(cur_act->ipt);
cur_act->ipt_next = cur_act->ipt->ipt_acts;
cur_act->ipt->ipt_acts = cur_act;
ipt_unlock(cur_act->ipt);
#if 0
printf("  return to ipt %x\n", cur_act->ipt);
#endif
}
act_unlock(cur_act);
refcount_drop(&cur_act->ref_count, act_free(cur_act));
}
void act_execute_returnhandlers(void)
{
Act *act = current_act();
#if 0
printf("execute_returnhandlers\n");
#endif
while (1) {
ReturnHandler *rh;
act_lock(act);
rh = act->handlers;
if (!rh) {
act_unlock(act);
return;
}
act->handlers = rh->next;
act_unlock(act);
(*rh->handler)(rh, act);
}
}
static void act_nudge(struct Act *act)
{
thread_wakeup(&act->suspend_count);
act_machine_nudge(act);
}
static void install_special_handler(struct Act *act)
{
ReturnHandler **rh;
for (rh = &act->handlers; *rh; rh = &(*rh)->next);
if (rh != &act->special_handler.next) {
*rh = &act->special_handler;
}
act_nudge(act);
}
static void special_handler(ReturnHandler *rh, struct Act *cur_act)
{
act_lock(cur_act);
if (!cur_act->active) {
act_unlock(cur_act);
act_machine_return(KERN_TERMINATED);
}
if (cur_act->suspend_count) {
act_unlock(cur_act);
thread_wait((int)&cur_act->suspend_count, FALSE);
act_lock(cur_act);
if (cur_act->suspend_count)
install_special_handler(cur_act);
}
act_unlock(cur_act);
}
#if 0
static __dead void act_throughcall_return(Act *act)
{
act_detach(act);
act_terminate(act);
act_deallocate(act);
thread_terminate_self();
}
__dead void act_throughcall(task_t task, void (*infunc)())
{
thread_t thread = current_thread();
Act *act;
ReturnHandler rh;
int rc;
rc = act_create(task, 0, 0, 0, &act);
if (rc) return rc;
act->return_routine = act_throughcall_return;
thread_lock(thread);
act_lock(act);
act_attach(thread, act, 0);
rh.handler = infunc;
rh.next = act->handlers;
act->handlers = &rh;
act_unlock(act);
thread_unlock(thread);
act_machine_throughcall(act);
}
Act *act_grab(struct ipc_target *ipt)
{
Act *act;
ipt_lock(ipt);
retry:
act = ipt->acts;
if (!act)
goto none_avail;
ipt->acts = act->ipt_next;
act_lock(act);
if (!act->active) {
#if 0
printf("dropping terminated act %08x\n", act);
#endif
act->ipt = 0;
act_unlock(act);
act_deallocate(act);
goto retry;
}
none_avail:
ipt_unlock(ipt);
return act;
}
kern_return_t act_upcall(struct Act *act, unsigned init_alert_mask,
vm_offset_t user_entrypoint, vm_offset_t user_data)
{
thread_t cur_thread = current_thread();
int rc;
act_attach(cur_thread, act, init_alert_mask);
rc = act_machine_upcall(act, user_entrypoint, user_data);
act_detach(act);
return rc;
}
#endif
static thread_t act_lock_thread(Act *act)
{
thread_t thread;
retry:
act_lock(act);
thread = act->thread;
if (thread == 0)
{
act_unlock(act);
return 0;
}
thread_reference(thread);
act_unlock(act);
thread_lock(thread);
act_lock(act);
if (act->thread != thread)
{
act_unlock(act);
thread_unlock(thread);
thread_deallocate(thread);
goto retry;
}
thread_deallocate(thread);
return thread;
}
kern_return_t act_terminate_task_locked(struct Act *act)
{
act_lock(act);
if (act->active)
{
act->task_links.next->prev = act->task_links.prev;
act->task_links.prev->next = act->task_links.next;
act->task->act_count--;
act_set_target(act, 0);
act->active = 0;
install_special_handler(act);
act_deallocate(act);
}
act_unlock(act);
return KERN_SUCCESS;
}
kern_return_t act_terminate(struct Act *act)
{
kern_return_t rc;
task_lock(act->task);
rc = act_terminate_task_locked(act);
task_unlock(act->task);
return rc;
}
kern_return_t act_yank(Act *act)
{
thread_t thread = act_lock_thread(act);
#if 0
printf("act_yank inc %08x thread %08x\n", act, thread);
#endif
if (thread)
{
if (thread->top_act != act)
{
printf("detaching act %p from thread %p\n", act, thread);
act_nudge(act);
}
thread_unlock(thread);
}
act_unlock(act);
act_abort(act);
return KERN_SUCCESS;
}
kern_return_t act_set_target(Act *act, struct ipc_target *ipt)
{
act_lock(act);
if (ipt == 0)
{
Act **lact;
ipt = act->ipt;
if (ipt == 0)
return;
ipt_lock(ipt);
for (lact = &ipt->ipt_acts; *lact; lact = &((*lact)->ipt_next))
if (act == *lact)
{
*lact = act->ipt_next;
break;
}
ipt_unlock(ipt);
act->ipt = 0;
act_deallocate(act);
return;
}
if (act->ipt != ipt)
{
if (act->ipt != 0)
{
act_unlock(act);
return KERN_FAILURE;
}
act->ipt = ipt;
ipt->ipt_type |= IPT_TYPE_MIGRATE_RPC;
act_reference(act);
ipt_reference(ipt);
if ((act->thread == 0) && (act->suspend_count == 0))
{
ipt_lock(ipt);
act->ipt_next = ipt->ipt_acts;
act->ipt->ipt_acts = act;
ipt_unlock(ipt);
}
}
act_unlock(act);
return KERN_SUCCESS;
}
kern_return_t act_alert(struct Act *act, unsigned alerts)
{
thread_t thread = act_lock_thread(act);
#if 0
printf("act_alert %08x: %08x\n", act, alerts);
#endif
if (thread)
{
struct Act *act_up = act;
while ((alerts) && (act_up != thread->top_act))
{
act_up = act_up->higher;
alerts &= act_up->alert_mask;
act_up->alerts |= alerts;
}
thread_unlock(thread);
}
act_unlock(act);
return KERN_SUCCESS;
}
kern_return_t act_abort(struct Act *act)
{
return act_alert(act, ALERT_ABORT_STRONG);
}
kern_return_t act_abort_safely(struct Act *act)
{
return act_alert(act, ALERT_ABORT_SAFE);
}
kern_return_t act_alert_mask(struct Act *act, unsigned alert_mask)
{
panic("act_alert_mask\n");
return KERN_SUCCESS;
}
kern_return_t act_suspend(struct Act *act)
{
thread_t thread = act_lock_thread(act);
kern_return_t rc = KERN_SUCCESS;
#if 0
printf("act_suspend %08x\n", act);
#endif
if (act->active)
{
if (act->suspend_count++ == 0)
{
install_special_handler(act);
act_nudge(act);
}
}
else
rc = KERN_TERMINATED;
if (thread)
thread_unlock(thread);
act_unlock(act);
return rc;
}
kern_return_t act_resume(struct Act *act)
{
#if 0
printf("act_resume %08x from %d\n", act, act->suspend_count);
#endif
act_lock(act);
if (!act->active)
{
act_unlock(act);
return KERN_TERMINATED;
}
if (act->suspend_count > 0) {
if (--act->suspend_count == 0) {
thread_wakeup(&act->suspend_count);
}
}
act_unlock(act);
return KERN_SUCCESS;
}
typedef struct GetSetState {
struct ReturnHandler rh;
int flavor;
void *state;
int *pcount;
int result;
} GetSetState;
kern_return_t get_set_state(struct Act *act, int flavor, void *state, int *pcount,
void (*handler)(ReturnHandler *rh, struct Act *act))
{
GetSetState gss;
gss.rh.handler = handler;
gss.flavor = flavor;
gss.state = state;
gss.pcount = pcount;
act_lock(act);
gss.rh.next = act->handlers;
act->handlers = &gss.rh;
act_nudge(act);
act_unlock(act);
thread_wait((int)&gss, 0);
return gss.result;
}
static void get_state_handler(ReturnHandler *rh, struct Act *act)
{
GetSetState *gss = (GetSetState*)rh;
gss->result = act_machine_get_state(act, gss->flavor, gss->state, gss->pcount);
thread_wakeup((int)gss);
}
kern_return_t act_get_state(struct Act *act, int flavor, natural_t *state, natural_t *pcount)
{
return get_set_state(act, flavor, state, pcount, get_state_handler);
}
static void set_state_handler(ReturnHandler *rh, struct Act *act)
{
GetSetState *gss = (GetSetState*)rh;
gss->result = act_machine_set_state(act, gss->flavor, gss->state, *gss->pcount);
thread_wakeup((int)gss);
}
kern_return_t act_set_state(struct Act *act, int flavor, natural_t *state, natural_t count)
{
return get_set_state(act, flavor, state, &count, set_state_handler);
}
#include <mach/thread_info.h>
#include <mach/thread_special_ports.h>
#include <ipc/ipc_port.h>
kern_return_t act_thread_info(Act *act, int flavor,
thread_info_t thread_info_out, unsigned *thread_info_count)
{
return thread_info(act->thread, flavor, thread_info_out, thread_info_count);
}
kern_return_t
act_thread_assign(Act *act, processor_set_t new_pset)
{
return thread_assign(act->thread, new_pset);
}
kern_return_t
act_thread_assign_default(Act *act)
{
return thread_assign_default(act->thread);
}
kern_return_t
act_thread_get_assignment(Act *act, processor_set_t *pset)
{
return thread_get_assignment(act->thread, pset);
}
kern_return_t
act_thread_priority(Act *act, int priority, boolean_t set_max)
{
return thread_priority(act->thread, priority, set_max);
}
kern_return_t
act_thread_max_priority(Act *act, processor_set_t *pset, int max_priority)
{
return thread_max_priority(act->thread, pset, max_priority);
}
kern_return_t
act_thread_policy(Act *act, int policy, int data)
{
return thread_policy(act->thread, policy, data);
}
kern_return_t
act_thread_wire(struct host *host, Act *act, boolean_t wired)
{
return thread_wire(host, act->thread, wired);
}
kern_return_t
act_thread_depress_abort(Act *act)
{
return thread_depress_abort(act->thread);
}
kern_return_t
act_get_special_port(Act *act, int which, ipc_port_t *portp)
{
ipc_port_t *whichp;
ipc_port_t port;
#if 0
printf("act_get_special_port\n");
#endif
if (act == 0)
return KERN_INVALID_ARGUMENT;
switch (which) {
case THREAD_KERNEL_PORT:
whichp = &act->self_port;
break;
case THREAD_EXCEPTION_PORT:
whichp = &act->exception_port;
break;
default:
return KERN_INVALID_ARGUMENT;
}
thread_lock(act->thread);
if (act->self_port == IP_NULL) {
thread_unlock(act->thread);
return KERN_FAILURE;
}
port = ipc_port_copy_send(*whichp);
thread_unlock(act->thread);
*portp = port;
return KERN_SUCCESS;
}
kern_return_t
act_set_special_port(Act *act, int which, ipc_port_t port)
{
ipc_port_t *whichp;
ipc_port_t old;
#if 0
printf("act_set_special_port\n");
#endif
if (act == 0)
return KERN_INVALID_ARGUMENT;
switch (which) {
case THREAD_KERNEL_PORT:
whichp = &act->self_port;
break;
case THREAD_EXCEPTION_PORT:
whichp = &act->exception_port;
break;
default:
return KERN_INVALID_ARGUMENT;
}
thread_lock(act->thread);
if (act->self_port == IP_NULL) {
thread_unlock(act->thread);
return KERN_FAILURE;
}
old = *whichp;
*whichp = port;
thread_unlock(act->thread);
if (IP_VALID(old))
ipc_port_release_send(old);
return KERN_SUCCESS;
}
kern_return_t
act_get_state_immediate(
Act *act,
int flavor,
void *old_state,
unsigned int *old_state_count)
{
act_lock(act);
if (act->thread && act->thread->top_act != act) {
kern_return_t ret = act_machine_get_state(act, flavor,
old_state, old_state_count);
act_unlock(act);
return ret;
}
act_unlock(act);
return act_get_state(act, flavor, old_state, old_state_count);
}
kern_return_t
act_set_state_immediate(
Act *act,
int flavor,
void *new_state,
unsigned int new_state_count)
{
act_lock(act);
if (act->thread && act->thread->top_act != act) {
kern_return_t ret = act_machine_set_state(act, flavor,
new_state, new_state_count);
act_unlock(act);
return ret;
}
act_unlock(act);
return act_set_state(act, flavor, new_state, new_state_count);
}
void act_count(void)
{
int i;
Act *act;
static int amin = ACT_STATIC_KLUDGE;
i = 0;
for (act = act_freelist; act; act = act->ipt_next)
i++;
if (i < amin)
amin = i;
printf("%d of %d activations in use, %d max\n",
ACT_STATIC_KLUDGE-i, ACT_STATIC_KLUDGE, ACT_STATIC_KLUDGE-amin);
}
void dump_act(act)
Act *act;
{
act_count();
kact_count();
while (act) {
printf("%p: thread=%p, task=%p, hi=%p, lo=%p, ref=%x\n",
act, act->thread, act->task,
act->higher, act->lower, act->ref_count);
printf("\talerts=%x, mask=%x, susp=%x, active=%x\n",
act->alerts, act->alert_mask,
act->suspend_count, act->active);
machine_dump_act(&act->mact);
if (act == act->lower)
break;
act = act->lower;
}
}
#ifdef ACTWATCH
Act *
get_next_act(int sp)
{
static int i;
while (1) {
if (i == ACT_STATIC_KLUDGE) {
i = 0;
return 0;
}
Act *act = &free_acts[i];
i++;
if (act->mact.space == sp)
return act;
}
}
#endif
#endif