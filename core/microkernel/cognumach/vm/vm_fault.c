#include <kern/printf.h>
#include <vm/vm_fault.h>
#include <mach/kern_return.h>
#include <mach/message.h>
#include <kern/counters.h>
#include <kern/debug.h>
#include <kern/thread.h>
#include <kern/sched_prim.h>
#include <kern/dtrace.h>
#include <vm/vm_map.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <vm/pmap.h>
#include <mach/vm_statistics.h>
#include <vm/vm_pageout.h>
#include <mach/vm_param.h>
#include <mach/memory_object.h>
#include <vm/memory_object_user.user.h>
#include <kern/macros.h>
#include <kern/slab.h>
#if	MACH_PCSAMPLE
#include <kern/pc_sample.h>
#endif
typedef struct vm_fault_state {
struct vm_map *vmf_map;
vm_offset_t vmf_vaddr;
vm_prot_t vmf_fault_type;
boolean_t vmf_change_wiring;
vm_fault_continuation_t vmf_continuation;
vm_map_version_t vmf_version;
boolean_t vmf_wired;
struct vm_object *vmf_object;
vm_offset_t vmf_offset;
vm_prot_t vmf_prot;
boolean_t vmfp_backoff;
struct vm_object *vmfp_object;
vm_offset_t vmfp_offset;
struct vm_page *vmfp_first_m;
vm_prot_t vmfp_access;
} vm_fault_state_t;
struct kmem_cache	vm_fault_state_cache;
int		vm_object_absent_max = 50;
boolean_t	vm_fault_dirty_handling = FALSE;
boolean_t	vm_fault_interruptible = TRUE;
boolean_t	software_reference_bits = TRUE;
#if	MACH_KDB
extern struct db_watchpoint *db_watchpoint_list;
#endif
void vm_fault_init(void)
{
kmem_cache_init(&vm_fault_state_cache, "vm_fault_state",
sizeof(vm_fault_state_t), 0, NULL, 0);
}
void
vm_fault_cleanup(
vm_object_t	object,
vm_page_t	top_page)
{
assert(vm_object_lock_taken(object));
vm_object_paging_end(object);
vm_object_unlock(object);
if (top_page != VM_PAGE_NULL) {
object = top_page->object;
vm_object_lock(object);
VM_PAGE_FREE(top_page);
vm_object_paging_end(object);
vm_object_unlock(object);
}
}
#if	MACH_PCSAMPLE
#define	vm_stat_sample(flavor) \
MACRO_BEGIN \
thread_t _thread_ = current_thread(); \
\
if (_thread_ != THREAD_NULL) \
take_pc_sample_macro(_thread_, (flavor), 1, 0); \
MACRO_END
#else
#define	vm_stat_sample(x)
#endif
vm_fault_return_t vm_fault_page(
vm_object_t	first_object,
vm_offset_t	first_offset,
vm_prot_t	fault_type,
boolean_t	must_be_resident,
boolean_t	interruptible,
vm_prot_t	*protection,
vm_page_t	*result_page,
vm_page_t	*top_page,
boolean_t	resume,
continuation_t	continuation)
{
vm_page_t	m;
vm_object_t	object;
vm_offset_t	offset;
vm_page_t	first_m;
vm_object_t	next_object;
vm_object_t	copy_object;
boolean_t	look_for_page;
vm_prot_t	access_required;
if (resume) {
vm_fault_state_t *state =
(vm_fault_state_t *) current_thread()->ith_other;
if (state->vmfp_backoff)
goto after_block_and_backoff;
object = state->vmfp_object;
offset = state->vmfp_offset;
first_m = state->vmfp_first_m;
access_required = state->vmfp_access;
goto after_thread_block;
}
vm_stat_sample(SAMPLED_PC_VM_FAULTS_ANY);
vm_stat.faults++;
current_task()->faults++;
#define RELEASE_PAGE(m)					\
MACRO_BEGIN					\
PAGE_WAKEUP_DONE(m);				\
vm_page_lock_queues();				\
if (!m->active && !m->inactive)			\
vm_page_activate(m);			\
vm_page_unlock_queues();			\
MACRO_END
if (vm_fault_dirty_handling
#if	MACH_KDB
|| db_watchpoint_list
#endif
) {
if (!(fault_type & VM_PROT_WRITE))
*protection &= ~VM_PROT_WRITE;
}
if (!vm_fault_interruptible)
interruptible = FALSE;
object = first_object;
offset = first_offset;
first_m = VM_PAGE_NULL;
access_required = fault_type;
while (TRUE) {
m = vm_page_lookup(object, offset);
if (m != VM_PAGE_NULL) {
if (m->busy) {
kern_return_t	wait_result;
PAGE_ASSERT_WAIT(m, interruptible);
vm_object_unlock(object);
if (continuation != thread_no_continuation) {
vm_fault_state_t *state =
(vm_fault_state_t *) current_thread()->ith_other;
state->vmfp_backoff = FALSE;
state->vmfp_object = object;
state->vmfp_offset = offset;
state->vmfp_first_m = first_m;
state->vmfp_access =
access_required;
state->vmf_prot = *protection;
counter(c_vm_fault_page_block_busy_user++);
thread_block(continuation);
} else
{
counter(c_vm_fault_page_block_busy_kernel++);
thread_block((void (*)()) 0);
}
after_thread_block:
wait_result = current_thread()->wait_result;
vm_object_lock(object);
if (wait_result != THREAD_AWAKENED) {
vm_fault_cleanup(object, first_m);
if (wait_result == THREAD_RESTART)
return(VM_FAULT_RETRY);
else
return(VM_FAULT_INTERRUPTED);
}
continue;
}
if (m->error) {
VM_PAGE_FREE(m);
vm_fault_cleanup(object, first_m);
return(VM_FAULT_MEMORY_ERROR);
}
if (m->absent) {
offset += object->shadow_offset;
access_required = VM_PROT_READ;
next_object = object->shadow;
if (next_object == VM_OBJECT_NULL) {
vm_page_t real_m;
assert(!must_be_resident);
real_m = vm_page_grab(VM_PAGE_HIGHMEM);
if (real_m == VM_PAGE_NULL) {
vm_fault_cleanup(object, first_m);
return(VM_FAULT_MEMORY_SHORTAGE);
}
if (object != first_object) {
VM_PAGE_FREE(m);
vm_object_paging_end(object);
vm_object_unlock(object);
object = first_object;
offset = first_offset;
m = first_m;
first_m = VM_PAGE_NULL;
vm_object_lock(object);
}
VM_PAGE_FREE(m);
assert(real_m->busy);
vm_page_lock_queues();
vm_page_insert(real_m, object, offset);
vm_page_unlock_queues();
m = real_m;
vm_object_unlock(object);
vm_page_zero_fill(m);
vm_stat_sample(SAMPLED_PC_VM_ZFILL_FAULTS);
vm_stat.zero_fill_count++;
current_task()->zero_fills++;
vm_object_lock(object);
pmap_clear_modify(m->phys_addr);
break;
} else {
if (must_be_resident) {
vm_object_paging_end(object);
} else if (object != first_object) {
vm_object_paging_end(object);
VM_PAGE_FREE(m);
} else {
first_m = m;
m->absent = FALSE;
vm_object_absent_release(object);
m->busy = TRUE;
vm_page_lock_queues();
VM_PAGE_QUEUES_REMOVE(m);
vm_page_unlock_queues();
}
vm_object_lock(next_object);
vm_object_unlock(object);
object = next_object;
vm_object_paging_begin(object);
continue;
}
}
if (access_required & m->page_lock) {
if ((access_required & m->unlock_request) != access_required) {
vm_prot_t	new_unlock_request;
kern_return_t	rc;
if (!object->pager_ready) {
vm_object_assert_wait(object,
VM_OBJECT_EVENT_PAGER_READY,
interruptible);
goto block_and_backoff;
}
new_unlock_request = m->unlock_request =
(access_required | m->unlock_request);
vm_object_unlock(object);
if ((rc = memory_object_data_unlock(
object->pager,
object->pager_request,
offset + object->paging_offset,
PAGE_SIZE,
new_unlock_request))
!= KERN_SUCCESS) {
printf("vm_fault: memory_object_data_unlock failed\n");
vm_object_lock(object);
vm_fault_cleanup(object, first_m);
return((rc == MACH_SEND_INTERRUPTED) ?
VM_FAULT_INTERRUPTED :
VM_FAULT_MEMORY_ERROR);
}
vm_object_lock(object);
continue;
}
PAGE_ASSERT_WAIT(m, interruptible);
goto block_and_backoff;
}
if (!software_reference_bits) {
vm_page_lock_queues();
if (m->inactive)  {
vm_stat_sample(SAMPLED_PC_VM_REACTIVATION_FAULTS);
vm_stat.reactivations++;
current_task()->reactivations++;
}
VM_PAGE_QUEUES_REMOVE(m);
vm_page_unlock_queues();
}
assert(!m->busy);
m->busy = TRUE;
assert(!m->absent);
break;
}
look_for_page =
(object->pager_created)
#if	MACH_PAGEMAP
&& (vm_external_state_get(object->existence_info, offset + object->paging_offset) !=
VM_EXTERNAL_STATE_ABSENT)
#endif
;
if ((look_for_page || (object == first_object))
&& !must_be_resident) {
m = vm_page_grab_fictitious();
if (m == VM_PAGE_NULL) {
vm_fault_cleanup(object, first_m);
return(VM_FAULT_FICTITIOUS_SHORTAGE);
}
vm_page_lock_queues();
vm_page_insert(m, object, offset);
vm_page_unlock_queues();
}
if (look_for_page && !must_be_resident) {
kern_return_t	rc;
if (!object->pager_ready) {
vm_object_assert_wait(object,
VM_OBJECT_EVENT_PAGER_READY,
interruptible);
VM_PAGE_FREE(m);
goto block_and_backoff;
}
if (object->internal) {
if (m->fictitious && !vm_page_convert(&m)) {
VM_PAGE_FREE(m);
vm_fault_cleanup(object, first_m);
return(VM_FAULT_MEMORY_SHORTAGE);
}
} else if (object->absent_count >
vm_object_absent_max) {
vm_object_absent_assert_wait(object, interruptible);
VM_PAGE_FREE(m);
goto block_and_backoff;
}
m->absent = TRUE;
object->absent_count++;
vm_object_unlock(object);
vm_stat.pageins++;
vm_stat_sample(SAMPLED_PC_VM_PAGEIN_FAULTS);
current_task()->pageins++;
if ((rc = memory_object_data_request(object->pager,
object->pager_request,
m->offset + object->paging_offset,
PAGE_SIZE, access_required)) != KERN_SUCCESS) {
if (object->pager && rc != MACH_SEND_INTERRUPTED)
printf("%s(0x%p, 0x%p, 0x%zx, 0x%x, 0x%x) failed, %x\n",
"memory_object_data_request",
object->pager,
object->pager_request,
m->offset + object->paging_offset,
PAGE_SIZE, access_required, rc);
vm_object_lock(object);
if (m == vm_page_lookup(object,offset) &&
m->absent && m->busy)
VM_PAGE_FREE(m);
vm_fault_cleanup(object, first_m);
return((rc == MACH_SEND_INTERRUPTED) ?
VM_FAULT_INTERRUPTED :
VM_FAULT_MEMORY_ERROR);
}
vm_object_lock(object);
continue;
}
if (object == first_object)
first_m = m;
else
{
assert(m == VM_PAGE_NULL);
}
access_required = VM_PROT_READ;
offset += object->shadow_offset;
next_object = object->shadow;
if (next_object == VM_OBJECT_NULL) {
assert(!must_be_resident);
if (object != first_object) {
vm_object_paging_end(object);
vm_object_unlock(object);
object = first_object;
offset = first_offset;
vm_object_lock(object);
}
m = first_m;
assert(m->object == object);
first_m = VM_PAGE_NULL;
if (m->fictitious && !vm_page_convert(&m)) {
VM_PAGE_FREE(m);
vm_fault_cleanup(object, VM_PAGE_NULL);
return(VM_FAULT_MEMORY_SHORTAGE);
}
vm_object_unlock(object);
vm_page_zero_fill(m);
vm_stat_sample(SAMPLED_PC_VM_ZFILL_FAULTS);
vm_stat.zero_fill_count++;
current_task()->zero_fills++;
vm_object_lock(object);
pmap_clear_modify(m->phys_addr);
break;
}
else {
vm_object_lock(next_object);
if ((object != first_object) || must_be_resident)
vm_object_paging_end(object);
vm_object_unlock(object);
object = next_object;
vm_object_paging_begin(object);
}
}
assert(m->busy && !m->absent);
assert((first_m == VM_PAGE_NULL) ||
(first_m->busy && !first_m->absent &&
!first_m->active && !first_m->inactive));
if (object != first_object) {
if (fault_type & VM_PROT_WRITE) {
vm_page_t copy_m;
assert(!must_be_resident);
copy_m = vm_page_grab(VM_PAGE_HIGHMEM);
if (copy_m == VM_PAGE_NULL) {
RELEASE_PAGE(m);
vm_fault_cleanup(object, first_m);
return(VM_FAULT_MEMORY_SHORTAGE);
}
vm_object_unlock(object);
vm_page_copy(m, copy_m);
vm_object_lock(object);
vm_page_lock_queues();
vm_page_deactivate(m);
pmap_page_protect(m->phys_addr, VM_PROT_NONE);
vm_page_unlock_queues();
PAGE_WAKEUP_DONE(m);
vm_object_paging_end(object);
vm_object_unlock(object);
vm_stat.cow_faults++;
vm_stat_sample(SAMPLED_PC_VM_COW_FAULTS);
current_task()->cow_faults++;
object = first_object;
offset = first_offset;
vm_object_lock(object);
VM_PAGE_FREE(first_m);
first_m = VM_PAGE_NULL;
assert(copy_m->busy);
vm_page_lock_queues();
vm_page_insert(copy_m, object, offset);
vm_page_unlock_queues();
m = copy_m;
vm_object_paging_end(object);
vm_object_collapse(object);
vm_object_paging_begin(object);
}
else {
*protection &= (~VM_PROT_WRITE);
}
}
while ((copy_object = first_object->copy) != VM_OBJECT_NULL) {
vm_offset_t	copy_offset;
vm_page_t	copy_m;
if ((fault_type & VM_PROT_WRITE) == 0) {
*protection &= ~VM_PROT_WRITE;
break;
}
if (must_be_resident)
break;
if (!vm_object_lock_try(copy_object)) {
vm_object_unlock(object);
simple_lock_pause();
vm_object_lock(object);
continue;
}
assert(copy_object->ref_count > 0);
copy_object->ref_count++;
copy_offset = first_offset - copy_object->shadow_offset;
copy_m = vm_page_lookup(copy_object, copy_offset);
if (copy_m != VM_PAGE_NULL) {
if (copy_m->busy) {
PAGE_ASSERT_WAIT(copy_m, interruptible);
RELEASE_PAGE(m);
copy_object->ref_count--;
assert(copy_object->ref_count > 0);
vm_object_unlock(copy_object);
goto block_and_backoff;
}
}
else {
copy_m = vm_page_alloc(copy_object, copy_offset);
if (copy_m == VM_PAGE_NULL) {
RELEASE_PAGE(m);
copy_object->ref_count--;
assert(copy_object->ref_count > 0);
vm_object_unlock(copy_object);
vm_fault_cleanup(object, first_m);
return(VM_FAULT_MEMORY_SHORTAGE);
}
vm_page_copy(m, copy_m);
vm_page_lock_queues();
pmap_page_protect(m->phys_addr, VM_PROT_NONE);
copy_m->dirty = TRUE;
vm_page_unlock_queues();
if (!copy_object->pager_created) {
vm_page_lock_queues();
vm_page_activate(copy_m);
vm_page_unlock_queues();
PAGE_WAKEUP_DONE(copy_m);
} else {
vm_object_unlock(object);
vm_pageout_page(copy_m, TRUE, TRUE);
if ((copy_object->shadow != object) ||
(copy_object->ref_count == 1)) {
vm_object_unlock(copy_object);
vm_object_deallocate(copy_object);
vm_object_lock(object);
continue;
}
vm_object_lock(object);
}
if (m->wanted) {
m->wanted = FALSE;
thread_wakeup_with_result((event_t) m,
THREAD_RESTART);
}
}
copy_object->ref_count--;
assert(copy_object->ref_count > 0);
vm_object_unlock(copy_object);
break;
}
*result_page = m;
*top_page = first_m;
if (vm_fault_dirty_handling && (*protection & VM_PROT_WRITE))
m->dirty = TRUE;
return(VM_FAULT_SUCCESS);
block_and_backoff:
vm_fault_cleanup(object, first_m);
if (continuation != thread_no_continuation) {
vm_fault_state_t *state =
(vm_fault_state_t *) current_thread()->ith_other;
state->vmfp_backoff = TRUE;
state->vmf_prot = *protection;
counter(c_vm_fault_page_block_backoff_user++);
thread_block(continuation);
} else
{
counter(c_vm_fault_page_block_backoff_kernel++);
thread_block((void (*)()) 0);
}
after_block_and_backoff:
if (current_thread()->wait_result == THREAD_AWAKENED)
return VM_FAULT_RETRY;
else
return VM_FAULT_INTERRUPTED;
#undef	RELEASE_PAGE
}
static void
vm_fault_continue(void)
{
vm_fault_state_t *state =
(vm_fault_state_t *) current_thread()->ith_other;
(void) vm_fault(state->vmf_map,
state->vmf_vaddr,
state->vmf_fault_type,
state->vmf_change_wiring,
TRUE, state->vmf_continuation);
}
kern_return_t vm_fault(
vm_map_t	map,
vm_offset_t	vaddr,
vm_prot_t	fault_type,
boolean_t	change_wiring,
boolean_t	resume,
vm_fault_continuation_t	continuation)
{
vm_map_version_t	version;
boolean_t		wired;
vm_object_t		object;
vm_offset_t		offset;
DTRACE_VM_FAULT(vaddr, fault_type);
vm_prot_t		prot;
vm_object_t		old_copy_object;
vm_page_t		result_page;
vm_page_t		top_page;
kern_return_t		kr;
vm_page_t		m;
if (resume) {
vm_fault_state_t *state =
(vm_fault_state_t *) current_thread()->ith_other;
object = state->vmf_object;
if (object == VM_OBJECT_NULL)
goto RetryFault;
version = state->vmf_version;
wired = state->vmf_wired;
offset = state->vmf_offset;
prot = state->vmf_prot;
kr = vm_fault_page(object, offset, fault_type,
(change_wiring && !wired), !change_wiring,
&prot, &result_page, &top_page,
TRUE, vm_fault_continue);
goto after_vm_fault_page;
}
if (continuation != vm_fault_no_continuation) {
char *	state;
state = (char *) kmem_cache_alloc(&vm_fault_state_cache);
current_thread()->ith_other = state;
}
RetryFault: ;
if ((kr = vm_map_lookup(&map, vaddr, fault_type, FALSE, &version,
&object, &offset,
&prot, &wired)) != KERN_SUCCESS) {
goto done;
}
if (wired)
fault_type = prot;
assert(object->ref_count > 0);
object->ref_count++;
vm_object_paging_begin(object);
if (continuation != vm_fault_no_continuation) {
vm_fault_state_t *state =
(vm_fault_state_t *) current_thread()->ith_other;
state->vmf_map = map;
state->vmf_vaddr = vaddr;
state->vmf_fault_type = fault_type;
state->vmf_change_wiring = change_wiring;
state->vmf_continuation = continuation;
state->vmf_version = version;
state->vmf_wired = wired;
state->vmf_object = object;
state->vmf_offset = offset;
state->vmf_prot = prot;
kr = vm_fault_page(object, offset, fault_type,
(change_wiring && !wired), !change_wiring,
&prot, &result_page, &top_page,
FALSE, vm_fault_continue);
} else
{
kr = vm_fault_page(object, offset, fault_type,
(change_wiring && !wired), !change_wiring,
&prot, &result_page, &top_page,
FALSE, (void (*)()) 0);
}
after_vm_fault_page:
if (kr != VM_FAULT_SUCCESS)
vm_object_deallocate(object);
switch (kr) {
case VM_FAULT_SUCCESS:
break;
case VM_FAULT_RETRY:
goto RetryFault;
case VM_FAULT_INTERRUPTED:
kr = KERN_SUCCESS;
goto done;
case VM_FAULT_MEMORY_SHORTAGE:
if (continuation != vm_fault_no_continuation) {
vm_fault_state_t *state =
(vm_fault_state_t *) current_thread()->ith_other;
state->vmf_map = map;
state->vmf_vaddr = vaddr;
state->vmf_fault_type = fault_type;
state->vmf_change_wiring = change_wiring;
state->vmf_continuation = continuation;
state->vmf_object = VM_OBJECT_NULL;
VM_PAGE_WAIT(vm_fault_continue);
} else
VM_PAGE_WAIT((void (*)()) 0);
goto RetryFault;
case VM_FAULT_FICTITIOUS_SHORTAGE:
vm_page_more_fictitious();
goto RetryFault;
case VM_FAULT_MEMORY_ERROR:
kr = KERN_MEMORY_ERROR;
goto done;
}
m = result_page;
assert((change_wiring && !wired) ?
(top_page == VM_PAGE_NULL) :
((top_page == VM_PAGE_NULL) == (m->object == object)));
#define UNLOCK_AND_DEALLOCATE				\
MACRO_BEGIN					\
vm_fault_cleanup(m->object, top_page);		\
vm_object_deallocate(object);			\
MACRO_END
#define RELEASE_PAGE(m)					\
MACRO_BEGIN					\
PAGE_WAKEUP_DONE(m);				\
vm_page_lock_queues();				\
if (!m->active && !m->inactive)			\
vm_page_activate(m);			\
vm_page_unlock_queues();			\
MACRO_END
old_copy_object = m->object->copy;
vm_object_unlock(m->object);
while (!vm_map_verify(map, &version)) {
vm_object_t	retry_object;
vm_offset_t	retry_offset;
vm_prot_t	retry_prot;
kr = vm_map_lookup(&map, vaddr,
fault_type & ~VM_PROT_WRITE, FALSE, &version,
&retry_object, &retry_offset, &retry_prot,
&wired);
if (kr != KERN_SUCCESS) {
vm_object_lock(m->object);
RELEASE_PAGE(m);
UNLOCK_AND_DEALLOCATE;
goto done;
}
vm_object_unlock(retry_object);
vm_object_lock(m->object);
if ((retry_object != object) ||
(retry_offset != offset)) {
RELEASE_PAGE(m);
UNLOCK_AND_DEALLOCATE;
goto RetryFault;
}
prot &= retry_prot;
vm_object_unlock(m->object);
}
vm_object_lock(m->object);
if (m->object->copy != old_copy_object)
prot &= ~VM_PROT_WRITE;
if (wired && (prot != fault_type)) {
vm_map_verify_done(map, &version);
RELEASE_PAGE(m);
UNLOCK_AND_DEALLOCATE;
goto RetryFault;
}
vm_object_unlock(m->object);
PMAP_ENTER(map->pmap, vaddr, m, prot, wired);
vm_object_lock(m->object);
vm_page_lock_queues();
if (change_wiring) {
if (wired)
vm_page_wire(m);
else
vm_page_unwire(m);
} else if (software_reference_bits) {
if (!m->active && !m->inactive)
vm_page_activate(m);
m->reference = TRUE;
} else {
vm_page_activate(m);
}
vm_page_unlock_queues();
vm_map_verify_done(map, &version);
PAGE_WAKEUP_DONE(m);
kr = KERN_SUCCESS;
UNLOCK_AND_DEALLOCATE;
#undef	UNLOCK_AND_DEALLOCATE
#undef	RELEASE_PAGE
done:
if (continuation != vm_fault_no_continuation) {
vm_fault_state_t *state =
(vm_fault_state_t *) current_thread()->ith_other;
kmem_cache_free(&vm_fault_state_cache, (vm_offset_t) state);
(*continuation)(kr);
}
return(kr);
}
void vm_fault_wire(
vm_map_t	map,
vm_map_entry_t	entry)
{
vm_offset_t	va;
pmap_t		pmap;
vm_offset_t	end_addr = entry->vme_end;
pmap = vm_map_pmap(map);
pmap_pageable(pmap, entry->vme_start, end_addr, FALSE);
for (va = entry->vme_start; va < end_addr; va += PAGE_SIZE) {
if (vm_fault_wire_fast(map, va, entry) != KERN_SUCCESS)
(void) vm_fault(map, va, VM_PROT_NONE, TRUE,
FALSE, vm_fault_no_continuation);
}
}
void vm_fault_unwire(
vm_map_t	map,
vm_map_entry_t	entry)
{
vm_offset_t	va;
pmap_t		pmap;
vm_offset_t	end_addr = entry->vme_end;
vm_object_t	object;
pmap = vm_map_pmap(map);
object = (entry->is_sub_map)
? VM_OBJECT_NULL : entry->object.vm_object;
for (va = entry->vme_start; va < end_addr; va += PAGE_SIZE) {
pmap_change_wiring(pmap, va, FALSE);
if (object == VM_OBJECT_NULL) {
vm_map_lock_set_recursive(map);
(void) vm_fault(map, va, VM_PROT_NONE, TRUE,
FALSE, vm_fault_no_continuation);
vm_map_lock_clear_recursive(map);
} else {
vm_prot_t	prot;
vm_page_t	result_page;
vm_page_t	top_page;
vm_fault_return_t result;
do {
prot = VM_PROT_NONE;
vm_object_lock(object);
vm_object_paging_begin(object);
result = vm_fault_page(object,
entry->offset +
(va - entry->vme_start),
VM_PROT_NONE, TRUE,
FALSE, &prot,
&result_page,
&top_page,
FALSE, (void (*)()) 0);
} while (result == VM_FAULT_RETRY);
if (result != VM_FAULT_SUCCESS)
panic("vm_fault_unwire: failure");
vm_page_lock_queues();
vm_page_unwire(result_page);
vm_page_unlock_queues();
PAGE_WAKEUP_DONE(result_page);
vm_fault_cleanup(result_page->object, top_page);
}
}
pmap_pageable(pmap, entry->vme_start, end_addr, TRUE);
}
kern_return_t vm_fault_wire_fast(
vm_map_t	map,
vm_offset_t	va,
vm_map_entry_t	entry)
{
vm_object_t		object;
vm_offset_t		offset;
vm_page_t		m;
vm_prot_t		prot;
vm_stat.faults++;
current_task()->faults++;
#undef	RELEASE_PAGE
#define RELEASE_PAGE(m)					\
MACRO_BEGIN						\
PAGE_WAKEUP_DONE(m);				\
vm_page_lock_queues();				\
vm_page_unwire(m);				\
vm_page_unlock_queues();			\
MACRO_END
#undef	UNLOCK_THINGS
#define UNLOCK_THINGS					\
MACRO_BEGIN						\
object->paging_in_progress--;			\
vm_object_unlock(object);			\
MACRO_END
#undef	UNLOCK_AND_DEALLOCATE
#define UNLOCK_AND_DEALLOCATE				\
MACRO_BEGIN						\
UNLOCK_THINGS;					\
vm_object_deallocate(object);			\
MACRO_END
#define GIVE_UP						\
MACRO_BEGIN						\
UNLOCK_AND_DEALLOCATE;				\
return(KERN_FAILURE);				\
MACRO_END
if (entry->is_sub_map)
return(KERN_FAILURE);
object = entry->object.vm_object;
offset = (va - entry->vme_start) + entry->offset;
prot = entry->protection;
vm_object_lock(object);
assert(object->ref_count > 0);
object->ref_count++;
object->paging_in_progress++;
m = vm_page_lookup(object, offset);
if ((m == VM_PAGE_NULL) || (m->error) ||
(m->busy) || (m->absent) || (prot & m->page_lock)) {
GIVE_UP;
}
vm_page_lock_queues();
vm_page_wire(m);
vm_page_unlock_queues();
assert(!m->busy);
m->busy = TRUE;
assert(!m->absent);
if ((object->copy != VM_OBJECT_NULL) && (prot & VM_PROT_WRITE)) {
RELEASE_PAGE(m);
GIVE_UP;
}
vm_object_unlock(object);
PMAP_ENTER(map->pmap, va, m, prot, TRUE);
vm_object_lock(object);
PAGE_WAKEUP_DONE(m);
UNLOCK_AND_DEALLOCATE;
return(KERN_SUCCESS);
}
static void vm_fault_copy_cleanup(
vm_page_t	page,
vm_page_t	top_page)
{
vm_object_t	object = page->object;
vm_object_lock(object);
PAGE_WAKEUP_DONE(page);
vm_page_lock_queues();
if (!page->active && !page->inactive)
vm_page_activate(page);
vm_page_unlock_queues();
vm_fault_cleanup(object, top_page);
}
kern_return_t	vm_fault_copy(
vm_object_t	src_object,
vm_offset_t	src_offset,
vm_size_t	*src_size,
vm_object_t	dst_object,
vm_offset_t	dst_offset,
vm_map_t	dst_map,
vm_map_version_t *dst_version,
boolean_t	interruptible)
{
vm_page_t		result_page;
vm_prot_t		prot;
vm_page_t		src_page;
vm_page_t		src_top_page;
vm_page_t		dst_page;
vm_page_t		dst_top_page;
vm_size_t		amount_done;
vm_object_t		old_copy_object;
#define	RETURN(x)					\
MACRO_BEGIN					\
*src_size = amount_done;			\
MACRO_RETURN(x);				\
MACRO_END
amount_done = 0;
do {
RetrySourceFault: ;
if (src_object == VM_OBJECT_NULL) {
src_page = VM_PAGE_NULL;
} else {
prot = VM_PROT_READ;
vm_object_lock(src_object);
vm_object_paging_begin(src_object);
switch (vm_fault_page(src_object, src_offset,
VM_PROT_READ, FALSE, interruptible,
&prot, &result_page, &src_top_page,
FALSE, (void (*)()) 0)) {
case VM_FAULT_SUCCESS:
break;
case VM_FAULT_RETRY:
goto RetrySourceFault;
case VM_FAULT_INTERRUPTED:
RETURN(MACH_SEND_INTERRUPTED);
case VM_FAULT_MEMORY_SHORTAGE:
VM_PAGE_WAIT((void (*)()) 0);
goto RetrySourceFault;
case VM_FAULT_FICTITIOUS_SHORTAGE:
vm_page_more_fictitious();
goto RetrySourceFault;
case VM_FAULT_MEMORY_ERROR:
return(KERN_MEMORY_ERROR);
}
src_page = result_page;
assert((src_top_page == VM_PAGE_NULL) ==
(src_page->object == src_object));
assert ((prot & VM_PROT_READ) != VM_PROT_NONE);
vm_object_unlock(src_page->object);
}
RetryDestinationFault: ;
prot = VM_PROT_WRITE;
vm_object_lock(dst_object);
vm_object_paging_begin(dst_object);
switch (vm_fault_page(dst_object, dst_offset, VM_PROT_WRITE,
FALSE, FALSE ,
&prot, &result_page, &dst_top_page,
FALSE, (void (*)()) 0)) {
case VM_FAULT_SUCCESS:
break;
case VM_FAULT_RETRY:
goto RetryDestinationFault;
case VM_FAULT_INTERRUPTED:
if (src_page != VM_PAGE_NULL)
vm_fault_copy_cleanup(src_page,
src_top_page);
RETURN(MACH_SEND_INTERRUPTED);
case VM_FAULT_MEMORY_SHORTAGE:
VM_PAGE_WAIT((void (*)()) 0);
goto RetryDestinationFault;
case VM_FAULT_FICTITIOUS_SHORTAGE:
vm_page_more_fictitious();
goto RetryDestinationFault;
case VM_FAULT_MEMORY_ERROR:
if (src_page != VM_PAGE_NULL)
vm_fault_copy_cleanup(src_page,
src_top_page);
return(KERN_MEMORY_ERROR);
}
assert ((prot & VM_PROT_WRITE) != VM_PROT_NONE);
dst_page = result_page;
old_copy_object = dst_page->object->copy;
vm_object_unlock(dst_page->object);
if (!vm_map_verify(dst_map, dst_version)) {
BailOut: ;
if (src_page != VM_PAGE_NULL)
vm_fault_copy_cleanup(src_page, src_top_page);
vm_fault_copy_cleanup(dst_page, dst_top_page);
break;
}
vm_object_lock(dst_page->object);
if (dst_page->object->copy != old_copy_object) {
vm_object_unlock(dst_page->object);
vm_map_verify_done(dst_map, dst_version);
goto BailOut;
}
vm_object_unlock(dst_page->object);
if (src_page == VM_PAGE_NULL)
vm_page_zero_fill(dst_page);
else
vm_page_copy(src_page, dst_page);
dst_page->dirty = TRUE;
vm_map_verify_done(dst_map, dst_version);
if (src_page != VM_PAGE_NULL)
vm_fault_copy_cleanup(src_page, src_top_page);
vm_fault_copy_cleanup(dst_page, dst_top_page);
amount_done += PAGE_SIZE;
src_offset += PAGE_SIZE;
dst_offset += PAGE_SIZE;
} while (amount_done != *src_size);
RETURN(KERN_SUCCESS);
#undef	RETURN
}
#ifdef	notdef
vm_fault_return_t vm_fault_page_overwrite(
vm_object_t	dst_object,
vm_offset_t	dst_offset,
vm_page_t	*result_page)
{
vm_page_t	dst_page;
#define	interruptible	FALSE
while (TRUE) {
while ((dst_page = vm_page_lookup(dst_object, dst_offset))
== VM_PAGE_NULL) {
dst_page = vm_page_alloc(dst_object, dst_offset);
if (dst_page == VM_PAGE_NULL) {
vm_object_unlock(dst_object);
VM_PAGE_WAIT((void (*)()) 0);
vm_object_lock(dst_object);
continue;
}
dst_page->overwriting = TRUE;
dst_page->page_lock = VM_PROT_WRITE;
dst_page->absent = TRUE;
dst_object->absent_count++;
break;
#define	DISCARD_PAGE						\
MACRO_BEGIN						\
vm_object_lock(dst_object);				\
dst_page = vm_page_lookup(dst_object, dst_offset);	\
if ((dst_page != VM_PAGE_NULL) && dst_page->overwriting) \
VM_PAGE_FREE(dst_page);				\
vm_object_unlock(dst_object);				\
MACRO_END
}
if (dst_page->page_lock & VM_PROT_WRITE) {
if ( ! (dst_page->unlock_request & VM_PROT_WRITE)) {
vm_prot_t	u;
kern_return_t	rc;
if (!dst_object->pager_ready) {
vm_object_assert_wait(dst_object,
VM_OBJECT_EVENT_PAGER_READY,
interruptible);
vm_object_unlock(dst_object);
thread_block((void (*)()) 0);
if (current_thread()->wait_result !=
THREAD_AWAKENED) {
DISCARD_PAGE;
return(VM_FAULT_INTERRUPTED);
}
continue;
}
u = dst_page->unlock_request |= VM_PROT_WRITE;
vm_object_unlock(dst_object);
if ((rc = memory_object_data_unlock(
dst_object->pager,
dst_object->pager_request,
dst_offset + dst_object->paging_offset,
PAGE_SIZE,
u)) != KERN_SUCCESS) {
printf("vm_object_overwrite: memory_object_data_unlock failed\n");
DISCARD_PAGE;
return((rc == MACH_SEND_INTERRUPTED) ?
VM_FAULT_INTERRUPTED :
VM_FAULT_MEMORY_ERROR);
}
vm_object_lock(dst_object);
continue;
}
} else {
if ( ! (dst_page->busy || dst_page->absent || dst_page->error) )
break;
}
PAGE_ASSERT_WAIT(dst_page, interruptible);
vm_object_unlock(dst_object);
thread_block((void (*)()) 0);
if (current_thread()->wait_result != THREAD_AWAKENED) {
DISCARD_PAGE;
return(VM_FAULT_INTERRUPTED);
}
}
*result_page = dst_page;
return(VM_FAULT_SUCCESS);
#undef	interruptible
#undef	DISCARD_PAGE
}
#endif