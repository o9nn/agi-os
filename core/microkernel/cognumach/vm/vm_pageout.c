#include <device/net_io.h>
#include <mach/mach_types.h>
#include <mach/memory_object.h>
#include <vm/memory_object_default.user.h>
#include <vm/memory_object_user.user.h>
#include <mach/vm_param.h>
#include <mach/vm_statistics.h>
#include <kern/counters.h>
#include <kern/debug.h>
#include <kern/slab.h>
#include <kern/task.h>
#include <kern/thread.h>
#include <kern/printf.h>
#include <vm/memory_object.h>
#include <vm/pmap.h>
#include <vm/vm_map.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <vm/vm_pageout.h>
#include <machine/locore.h>
#define DEBUG 0
#define VM_PAGEOUT_TIMEOUT 50
static int vm_pageout_requested;
static int vm_pageout_continue;
vm_page_t
vm_pageout_setup(
vm_page_t		m,
vm_offset_t		paging_offset,
vm_object_t		new_object,
vm_offset_t		new_offset,
boolean_t		flush)
{
vm_object_t	old_object = m->object;
vm_page_t	holding_page = 0;
vm_page_t	new_m;
assert(m->busy && !m->absent && !m->fictitious);
if (!flush) {
for (;;) {
vm_object_lock(new_object);
new_m = vm_page_alloc(new_object, new_offset);
vm_object_unlock(new_object);
if (new_m != VM_PAGE_NULL) {
break;
}
VM_PAGE_WAIT(NULL);
}
}
if (flush) {
while ((holding_page = vm_page_grab_fictitious())
== VM_PAGE_NULL)
vm_page_more_fictitious();
vm_object_lock(old_object);
vm_page_lock_queues();
vm_page_remove(m);
vm_page_unlock_queues();
PAGE_WAKEUP_DONE(m);
vm_page_lock_queues();
vm_page_insert(holding_page, old_object, m->offset);
vm_page_unlock_queues();
#if	MACH_PAGEMAP
vm_external_state_set(old_object->existence_info,
paging_offset,
VM_EXTERNAL_STATE_EXISTS);
#endif
vm_object_unlock(old_object);
vm_object_lock(new_object);
vm_page_lock_queues();
vm_page_insert(m, new_object, new_offset);
vm_page_unlock_queues();
m->dirty = TRUE;
m->precious = FALSE;
m->page_lock = VM_PROT_NONE;
m->unlock_request = VM_PROT_NONE;
}
else {
vm_page_copy(m, new_m);
vm_object_lock(old_object);
m->dirty = FALSE;
pmap_clear_modify(m->phys_addr);
vm_page_lock_queues();
vm_page_deactivate(m);
vm_page_unlock_queues();
PAGE_WAKEUP_DONE(m);
#if	MACH_PAGEMAP
vm_external_state_set(old_object->existence_info,
paging_offset,
VM_EXTERNAL_STATE_EXISTS);
#endif
vm_object_unlock(old_object);
vm_object_lock(new_object);
m = new_m;
m->dirty = TRUE;
assert(!m->precious);
PAGE_WAKEUP_DONE(m);
}
vm_page_lock_queues();
vm_stat.pageouts++;
if (m->laundry) {
assert(!old_object->internal);
m->laundry = FALSE;
} else if (old_object->internal ||
memory_manager_default_port(old_object->pager)) {
m->laundry = TRUE;
vm_page_laundry_count++;
vm_page_wire(m);
} else {
m->external_laundry = TRUE;
if (vm_page_external_laundry_count >= 0) {
vm_page_external_laundry_count++;
}
vm_page_activate(m);
}
vm_page_unlock_queues();
vm_object_unlock(new_object);
return (flush ? holding_page : VM_PAGE_NULL);
}
void
vm_pageout_page(
vm_page_t		m,
boolean_t		initial,
boolean_t		flush)
{
vm_map_copy_t		copy;
vm_object_t		old_object;
vm_object_t		new_object;
vm_page_t		holding_page;
vm_offset_t		paging_offset;
kern_return_t		rc;
boolean_t		precious_clean;
assert(vm_object_lock_taken(m->object));
assert(m->busy);
precious_clean = (!m->dirty) && m->precious;
if (precious_clean && !flush) {
PAGE_WAKEUP_DONE(m);
return;
}
if (m->absent || m->error || (!m->dirty && !m->precious)) {
VM_PAGE_FREE(m);
return;
}
old_object = m->object;
paging_offset = m->offset + old_object->paging_offset;
vm_object_paging_begin(old_object);
vm_object_unlock(old_object);
new_object = vm_object_allocate(PAGE_SIZE);
new_object->used_for_pageout = TRUE;
holding_page = vm_pageout_setup(m,
paging_offset,
new_object,
0,
flush);
rc = vm_map_copyin_object(new_object, 0, PAGE_SIZE, &copy);
assert(rc == KERN_SUCCESS);
if (initial) {
rc = memory_object_data_initialize(
old_object->pager,
old_object->pager_request,
paging_offset, (pointer_t) copy, PAGE_SIZE);
}
else {
rc = memory_object_data_return(
old_object->pager,
old_object->pager_request,
paging_offset, (pointer_t) copy, PAGE_SIZE,
!precious_clean, !flush);
}
if (rc != KERN_SUCCESS)
vm_map_copy_discard(copy);
vm_object_lock(old_object);
if (holding_page != VM_PAGE_NULL)
VM_PAGE_FREE(holding_page);
vm_object_paging_end(old_object);
}
static boolean_t vm_pageout_scan(boolean_t *should_wait)
{
boolean_t done;
done = vm_page_balance();
if (done) {
return TRUE;
}
simple_unlock(&vm_page_queue_free_lock);
stack_collect();
net_kmsg_collect();
consider_task_collect();
if (0)
consider_thread_collect();
slab_collect();
vm_page_refill_inactive();
return vm_page_evict(should_wait);
}
void vm_pageout(void)
{
boolean_t done, should_wait;
current_thread()->vm_privilege = 1;
stack_privilege(current_thread());
thread_set_own_priority(0);
for (;;) {
done = vm_pageout_scan(&should_wait);
if (done) {
thread_sleep(&vm_pageout_requested,
simple_lock_addr(vm_page_queue_free_lock),
FALSE);
} else if (should_wait) {
assert_wait(&vm_pageout_continue, FALSE);
thread_set_timeout(VM_PAGEOUT_TIMEOUT * hz / 1000);
simple_unlock(&vm_page_queue_free_lock);
thread_block(NULL);
#if DEBUG
if (current_thread()->wait_result != THREAD_AWAKENED) {
printf("vm_pageout: timeout,"
" vm_page_laundry_count:%d"
" vm_page_external_laundry_count:%d\n",
vm_page_laundry_count,
vm_page_external_laundry_count);
}
#endif
} else {
simple_unlock(&vm_page_queue_free_lock);
}
}
}
void vm_pageout_start(void)
{
if (!current_thread())
return;
thread_wakeup_one(&vm_pageout_requested);
}
void vm_pageout_resume(void)
{
thread_wakeup_one(&vm_pageout_continue);
}