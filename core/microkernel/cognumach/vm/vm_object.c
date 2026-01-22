#include <kern/printf.h>
#include <string.h>
#include <stdint.h>
#include <mach/memory_object.h>
#include <vm/memory_object_default.user.h>
#include <vm/memory_object_user.user.h>
#include <machine/vm_param.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_space.h>
#include <kern/assert.h>
#include <kern/debug.h>
#include <kern/mach.server.h>
#include <kern/lock.h>
#include <kern/queue.h>
#include <kern/xpr.h>
#include <kern/slab.h>
#include <vm/memory_object.h>
#include <vm/vm_fault.h>
#include <vm/vm_map.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <vm/vm_pageout.h>
#include <vm/vm_resident.h>
#if MACH_KDB
#include <ddb/db_output.h>
#endif
void memory_object_release(
ipc_port_t pager,
pager_request_t pager_request,
ipc_port_t pager_name);
struct kmem_cache vm_object_cache;
static struct vm_object kernel_object_store;
vm_object_t kernel_object = &kernel_object_store;
queue_head_t vm_object_cached_list;
def_simple_lock_data(static,vm_object_cached_lock_data)
#define vm_object_cache_lock() \
simple_lock(&vm_object_cached_lock_data)
#define vm_object_cache_lock_try() \
simple_lock_try(&vm_object_cached_lock_data)
#define vm_object_cache_unlock() \
simple_unlock(&vm_object_cached_lock_data)
#define vm_object_cache_locked() \
simple_lock_taken(&vm_object_cached_lock_data)
struct vm_object vm_object_template;
static void _vm_object_setup(
vm_object_t object,
vm_size_t size)
{
*object = vm_object_template;
queue_init(&object->memq);
vm_object_lock_init(object);
object->size = size;
}
static vm_object_t _vm_object_allocate(
vm_size_t size)
{
vm_object_t object;
object = (vm_object_t) kmem_cache_alloc(&vm_object_cache);
if (!object)
return 0;
_vm_object_setup(object, size);
return object;
}
vm_object_t vm_object_allocate(
vm_size_t size)
{
vm_object_t object;
ipc_port_t port;
object = _vm_object_allocate(size);
if (object == 0)
panic("vm_object_allocate");
port = ipc_port_alloc_kernel();
if (port == IP_NULL)
panic("vm_object_allocate");
object->pager_name = port;
ipc_kobject_set(port, (ipc_kobject_t) object, IKOT_PAGING_NAME);
return object;
}
void vm_object_bootstrap(void)
{
kmem_cache_init(&vm_object_cache, "vm_object",
sizeof(struct vm_object), 0, NULL, 0);
queue_init(&vm_object_cached_list);
simple_lock_init(&vm_object_cached_lock_data);
vm_object_template.ref_count = 1;
vm_object_template.size = 0;
vm_object_template.resident_page_count = 0;
vm_object_template.copy = VM_OBJECT_NULL;
vm_object_template.shadow = VM_OBJECT_NULL;
vm_object_template.shadow_offset = (vm_offset_t) 0;
vm_object_template.pager = IP_NULL;
vm_object_template.paging_offset = 0;
vm_object_template.pager_request = PAGER_REQUEST_NULL;
vm_object_template.pager_name = IP_NULL;
vm_object_template.pager_created = FALSE;
vm_object_template.pager_initialized = FALSE;
vm_object_template.pager_ready = FALSE;
vm_object_template.copy_strategy = MEMORY_OBJECT_COPY_NONE;
vm_object_template.use_shared_copy = FALSE;
vm_object_template.shadowed = FALSE;
vm_object_template.absent_count = 0;
vm_object_template.all_wanted = 0;
vm_object_template.paging_in_progress = 0;
vm_object_template.used_for_pageout = FALSE;
vm_object_template.can_persist = FALSE;
vm_object_template.cached = FALSE;
vm_object_template.internal = TRUE;
vm_object_template.temporary = TRUE;
vm_object_template.alive = TRUE;
vm_object_template.lock_in_progress = FALSE;
vm_object_template.lock_restart = FALSE;
vm_object_template.last_alloc = (vm_offset_t) 0;
vm_object_template.readahead_next = (vm_offset_t) 0;
vm_object_template.readahead_count = 0;
vm_object_template.readahead_window = vm_page_readahead_min;
vm_object_template.block_cache = NULL;
vm_object_template.block_cache_enabled = FALSE;
#if MACH_PAGEMAP
vm_object_template.existence_info = VM_EXTERNAL_NULL;
#endif
_vm_object_setup(kernel_object,
VM_MAX_KERNEL_ADDRESS - VM_MIN_KERNEL_ADDRESS);
_vm_object_setup(vm_submap_object,
VM_MAX_KERNEL_ADDRESS - VM_MIN_KERNEL_ADDRESS);
#if MACH_PAGEMAP
vm_external_module_initialize();
#endif
}
void vm_object_init(void)
{
kernel_object->pager_name = ipc_port_alloc_kernel();
ipc_kobject_set(kernel_object->pager_name,
(ipc_kobject_t) kernel_object,
IKOT_PAGING_NAME);
}
static void vm_object_cache_add(
vm_object_t object)
{
assert(vm_object_lock_taken(object));
assert(vm_object_cache_locked());
assert(!object->cached);
queue_enter(&vm_object_cached_list, object, vm_object_t, cached_list);
object->cached = TRUE;
}
static void vm_object_cache_remove(
vm_object_t object)
{
assert(vm_object_lock_taken(object));
assert(vm_object_cache_locked());
assert(object->cached);
queue_remove(&vm_object_cached_list, object, vm_object_t, cached_list);
object->cached = FALSE;
}
void vm_object_collect(
vm_object_t object)
{
vm_object_unlock(object);
vm_object_cache_lock();
vm_object_lock(object);
if (!vm_object_collectable(object)) {
vm_object_unlock(object);
vm_object_cache_unlock();
return;
}
vm_object_cache_remove(object);
vm_object_terminate(object);
}
void vm_object_reference(
vm_object_t object)
{
if (object == VM_OBJECT_NULL)
return;
vm_object_lock(object);
assert(object->ref_count > 0);
object->ref_count++;
vm_object_unlock(object);
}
void vm_object_deallocate(
vm_object_t object)
{
vm_object_t temp;
while (object != VM_OBJECT_NULL) {
vm_object_cache_lock();
vm_object_lock(object);
if (--(object->ref_count) > 0) {
vm_object_unlock(object);
vm_object_cache_unlock();
return;
}
if (object->can_persist && (object->resident_page_count > 0)) {
vm_object_cache_add(object);
vm_object_cache_unlock();
vm_object_unlock(object);
return;
}
if (object->pager_created &&
!object->pager_initialized) {
object->ref_count++;
vm_object_assert_wait(object,
VM_OBJECT_EVENT_INITIALIZED, FALSE);
vm_object_unlock(object);
vm_object_cache_unlock();
thread_block((void (*)()) 0);
continue;
}
temp = object->shadow;
vm_object_terminate(object);
object = temp;
}
}
void vm_object_terminate(
vm_object_t object)
{
vm_page_t p;
vm_object_t shadow_object;
assert(vm_object_lock_taken(object));
assert(vm_object_cache_locked());
assert(object->alive);
object->alive = FALSE;
vm_object_remove(object);
vm_object_cache_unlock();
if ((shadow_object = object->shadow) != VM_OBJECT_NULL) {
vm_object_lock(shadow_object);
assert((shadow_object->copy == object) ||
(shadow_object->copy == VM_OBJECT_NULL));
shadow_object->copy = VM_OBJECT_NULL;
vm_object_unlock(shadow_object);
}
vm_object_paging_wait(object, FALSE);
if ((object->temporary) || (object->pager == IP_NULL)) {
while (!queue_empty(&object->memq)) {
p = (vm_page_t) queue_first(&object->memq);
VM_PAGE_CHECK(p);
VM_PAGE_FREE(p);
}
} else while (!queue_empty(&object->memq)) {
p = (vm_page_t) queue_first(&object->memq);
VM_PAGE_CHECK(p);
vm_page_lock_queues();
VM_PAGE_QUEUES_REMOVE(p);
vm_page_unlock_queues();
if (p->absent || p->private) {
goto free_page;
}
if (!p->dirty)
p->dirty = pmap_is_modified(p->phys_addr);
if (p->dirty || p->precious) {
p->busy = TRUE;
vm_pageout_page(p, FALSE, TRUE);
} else {
free_page:
VM_PAGE_FREE(p);
}
}
assert(object->ref_count == 0);
assert(object->paging_in_progress == 0);
assert(!object->cached);
if (!object->internal) {
assert(object->resident_page_count == 0);
vm_page_lock_queues();
vm_object_external_count--;
vm_page_unlock_queues();
}
vm_object_unlock(object);
if (object->pager != IP_NULL) {
memory_object_release(object->pager,
object->pager_request,
object->pager_name);
} else if (object->pager_name != IP_NULL) {
ipc_port_dealloc_kernel(object->pager_name);
}
#if MACH_PAGEMAP
vm_external_destroy(object->existence_info);
#endif
kmem_cache_free(&vm_object_cache, (vm_offset_t) object);
}
void
vm_object_pager_wakeup(
ipc_port_t pager)
{
boolean_t someone_waiting;
vm_object_cache_lock();
assert(ip_kotype(pager) == IKOT_PAGER_TERMINATING);
someone_waiting = (pager->ip_kobject != IKO_NULL);
if (ip_active(pager))
ipc_kobject_set(pager, IKO_NULL, IKOT_NONE);
vm_object_cache_unlock();
if (someone_waiting) {
thread_wakeup((event_t) pager);
}
}
void memory_object_release(
ipc_port_t pager,
pager_request_t pager_request,
ipc_port_t pager_name)
{
ip_reference(pager);
(void) memory_object_terminate(pager, pager_request, pager_name);
vm_object_pager_wakeup(pager);
ip_release(pager);
}
static void vm_object_abort_activity(
vm_object_t object)
{
vm_page_t p;
vm_page_t next;
assert(vm_object_lock_taken(object));
p = (vm_page_t) queue_first(&object->memq);
while (!queue_end(&object->memq, (queue_entry_t) p)) {
next = (vm_page_t) queue_next(&p->listq);
if (p->busy && p->absent) {
VM_PAGE_FREE(p);
}
else {
p->unlock_request = VM_PROT_NONE;
PAGE_WAKEUP(p);
}
p = next;
}
object->pager_ready = TRUE;
vm_object_wakeup(object, VM_OBJECT_EVENT_PAGER_READY);
}
kern_return_t memory_object_destroy(
vm_object_t object,
kern_return_t reason)
{
ipc_port_t old_object, old_name;
pager_request_t old_control;
if (object == VM_OBJECT_NULL)
return KERN_SUCCESS;
vm_object_cache_lock();
vm_object_lock(object);
vm_object_remove(object);
object->can_persist = FALSE;
vm_object_cache_unlock();
old_object = object->pager;
object->pager = IP_NULL;
old_control = object->pager_request;
object->pager_request = PAGER_REQUEST_NULL;
old_name = object->pager_name;
object->pager_name = IP_NULL;
vm_object_paging_wait(object, FALSE);
vm_object_unlock(object);
if (old_object != IP_NULL) {
memory_object_release(old_object, old_control,
old_name);
} else if (old_name != IP_NULL) {
ipc_port_dealloc_kernel(object->pager_name);
}
vm_object_deallocate(object);
return KERN_SUCCESS;
}
boolean_t vm_object_pmap_protect_by_page = FALSE;
void vm_object_pmap_protect(
vm_object_t object,
vm_offset_t offset,
vm_size_t size,
pmap_t pmap,
vm_offset_t pmap_start,
vm_prot_t prot)
{
if (object == VM_OBJECT_NULL)
return;
vm_object_lock(object);
assert(object->temporary && object->internal);
while (TRUE) {
if (object->resident_page_count > atop(size) / 2 &&
pmap != PMAP_NULL) {
vm_object_unlock(object);
pmap_protect(pmap, pmap_start, pmap_start + size, prot);
return;
}
{
vm_page_t p;
vm_offset_t end;
end = offset + size;
queue_iterate(&object->memq, p, vm_page_t, listq) {
if (!p->fictitious &&
(offset <= p->offset) &&
(p->offset < end)) {
if ((pmap == PMAP_NULL) ||
vm_object_pmap_protect_by_page) {
pmap_page_protect(p->phys_addr,
prot & ~p->page_lock);
} else {
vm_offset_t start =
pmap_start +
(p->offset - offset);
pmap_protect(pmap,
start,
start + PAGE_SIZE,
prot);
}
}
}
}
if (prot == VM_PROT_NONE) {
vm_object_t next_object;
next_object = object->shadow;
if (next_object != VM_OBJECT_NULL) {
offset += object->shadow_offset;
vm_object_lock(next_object);
vm_object_unlock(object);
object = next_object;
}
else {
break;
}
}
else {
break;
}
}
vm_object_unlock(object);
}
void vm_object_pmap_remove(
vm_object_t object,
vm_offset_t start,
vm_offset_t end)
{
vm_page_t p;
if (object == VM_OBJECT_NULL)
return;
vm_object_lock(object);
while (TRUE) {
queue_iterate(&object->memq, p, vm_page_t, listq) {
if (!p->fictitious &&
(start <= p->offset) &&
(p->offset < end))
pmap_page_protect(p->phys_addr, VM_PROT_NONE);
}
if (object->shadow == VM_OBJECT_NULL)
break;
vm_object_t prev_object = object;
start += object->shadow_offset;
end += object->shadow_offset;
object = object->shadow;
vm_object_lock(object);
vm_object_unlock(prev_object);
}
vm_object_unlock(object);
}
kern_return_t vm_object_copy_slowly(
vm_object_t src_object,
vm_offset_t src_offset,
vm_size_t size,
boolean_t interruptible,
vm_object_t *_result_object)
{
vm_object_t new_object;
vm_offset_t new_offset;
assert(vm_object_lock_taken(src_object));
if (size == 0) {
vm_object_unlock(src_object);
*_result_object = VM_OBJECT_NULL;
return KERN_INVALID_ARGUMENT;
}
assert(src_object->ref_count > 0);
src_object->ref_count++;
vm_object_unlock(src_object);
new_object = vm_object_allocate(size);
new_offset = 0;
assert(size == trunc_page(size));
for ( ;
size != 0 ;
src_offset += PAGE_SIZE, new_offset += PAGE_SIZE, size -= PAGE_SIZE
) {
vm_page_t new_page;
vm_fault_return_t result;
vm_object_lock(new_object);
while ((new_page = vm_page_alloc(new_object, new_offset))
== VM_PAGE_NULL) {
vm_object_unlock(new_object);
VM_PAGE_WAIT((void (*)()) 0);
vm_object_lock(new_object);
}
vm_object_unlock(new_object);
do {
vm_prot_t prot = VM_PROT_READ;
vm_page_t _result_page;
vm_page_t top_page;
vm_page_t result_page;
vm_object_lock(src_object);
src_object->paging_in_progress++;
result = vm_fault_page(src_object, src_offset,
VM_PROT_READ, FALSE, interruptible,
&prot, &_result_page, &top_page,
FALSE, (void (*)()) 0);
switch(result) {
case VM_FAULT_SUCCESS:
result_page = _result_page;
vm_object_unlock(result_page->object);
vm_page_copy(result_page, new_page);
new_page->busy = FALSE;
new_page->dirty = TRUE;
vm_object_lock(result_page->object);
PAGE_WAKEUP_DONE(result_page);
vm_page_lock_queues();
if (!result_page->active &&
!result_page->inactive)
vm_page_activate(result_page);
vm_page_activate(new_page);
vm_page_unlock_queues();
vm_fault_cleanup(result_page->object,
top_page);
break;
case VM_FAULT_RETRY:
break;
case VM_FAULT_MEMORY_SHORTAGE:
VM_PAGE_WAIT((void (*)()) 0);
break;
case VM_FAULT_FICTITIOUS_SHORTAGE:
vm_page_more_fictitious();
break;
case VM_FAULT_INTERRUPTED:
vm_page_free(new_page);
vm_object_deallocate(new_object);
vm_object_deallocate(src_object);
*_result_object = VM_OBJECT_NULL;
return MACH_SEND_INTERRUPTED;
case VM_FAULT_MEMORY_ERROR:
vm_page_free(new_page);
vm_object_deallocate(new_object);
vm_object_deallocate(src_object);
*_result_object = VM_OBJECT_NULL;
return KERN_MEMORY_ERROR;
}
} while (result != VM_FAULT_SUCCESS);
}
vm_object_deallocate(src_object);
*_result_object = new_object;
return KERN_SUCCESS;
}
boolean_t vm_object_copy_temporary(
vm_object_t *_object,
vm_offset_t *_offset,
boolean_t *_src_needs_copy,
boolean_t *_dst_needs_copy)
{
vm_object_t object = *_object;
if (object == VM_OBJECT_NULL) {
*_src_needs_copy = FALSE;
*_dst_needs_copy = FALSE;
return TRUE;
}
vm_object_lock(object);
if (object->temporary) {
if (object->use_shared_copy) {
vm_object_unlock(object);
object = vm_object_copy_delayed(object);
*_object = object;
*_src_needs_copy = FALSE;
*_dst_needs_copy = TRUE;
return TRUE;
}
assert(object->ref_count > 0);
object->ref_count++;
object->shadowed = TRUE;
vm_object_unlock(object);
*_src_needs_copy = TRUE;
*_dst_needs_copy = TRUE;
return TRUE;
}
if (object->pager_ready &&
(object->copy_strategy == MEMORY_OBJECT_COPY_DELAY)) {
}
vm_object_unlock(object);
return FALSE;
}
static kern_return_t vm_object_copy_call(
vm_object_t src_object,
vm_offset_t src_offset,
vm_size_t size,
vm_object_t *_result_object)
{
vm_offset_t src_end = src_offset + size;
ipc_port_t new_memory_object;
vm_object_t new_object;
vm_page_t p;
assert(vm_object_lock_taken(src_object));
new_memory_object = ipc_port_alloc_kernel();
if (new_memory_object == IP_NULL)
return KERN_RESOURCE_SHORTAGE;
assert(src_object->ref_count > 0);
src_object->ref_count++;
vm_object_paging_begin(src_object);
vm_object_unlock(src_object);
(void) ipc_port_make_send(new_memory_object);
(void) memory_object_copy(src_object->pager,
src_object->pager_request,
src_offset, size,
new_memory_object);
vm_object_lock(src_object);
vm_object_paging_end(src_object);
queue_iterate(&src_object->memq, p, vm_page_t, listq) {
if (!p->fictitious &&
(src_offset <= p->offset) &&
(p->offset < src_end) &&
!(p->page_lock & VM_PROT_WRITE)) {
p->page_lock |= VM_PROT_WRITE;
pmap_page_protect(p->phys_addr, VM_PROT_ALL & ~p->page_lock);
}
}
vm_object_unlock(src_object);
new_object = vm_object_enter(new_memory_object, size, FALSE);
assert(new_object);
new_object->shadow = src_object;
new_object->shadow_offset = src_offset;
ipc_port_release_send(new_memory_object);
*_result_object = new_object;
return KERN_SUCCESS;
}
vm_object_t vm_object_copy_delayed(
vm_object_t src_object)
{
vm_object_t new_copy;
vm_object_t old_copy;
vm_page_t p;
new_copy = vm_object_allocate(src_object->size);
vm_object_lock(src_object);
Retry:
old_copy = src_object->copy;
if (old_copy != VM_OBJECT_NULL) {
if (!vm_object_lock_try(old_copy)) {
vm_object_unlock(src_object);
simple_lock_pause();
vm_object_lock(src_object);
goto Retry;
}
if (old_copy->resident_page_count == 0 &&
!old_copy->pager_created) {
assert(old_copy->ref_count > 0);
old_copy->ref_count++;
vm_object_unlock(old_copy);
vm_object_unlock(src_object);
vm_object_deallocate(new_copy);
return old_copy;
}
assert((old_copy->shadow == src_object) &&
(old_copy->shadow_offset == (vm_offset_t) 0));
src_object->ref_count--;
assert(src_object->ref_count > 0);
old_copy->shadow = new_copy;
assert(new_copy->ref_count > 0);
new_copy->ref_count++;
vm_object_unlock(old_copy);
}
new_copy->shadow = src_object;
new_copy->shadow_offset = 0;
new_copy->shadowed = TRUE;
assert(src_object->ref_count > 0);
src_object->ref_count++;
src_object->copy = new_copy;
queue_iterate(&src_object->memq, p, vm_page_t, listq) {
if (!p->fictitious)
pmap_page_protect(p->phys_addr,
(VM_PROT_ALL & ~VM_PROT_WRITE &
~p->page_lock));
}
vm_object_unlock(src_object);
return new_copy;
}
kern_return_t vm_object_copy_strategically(
vm_object_t src_object,
vm_offset_t src_offset,
vm_size_t size,
vm_object_t *dst_object,
vm_offset_t *dst_offset,
boolean_t *dst_needs_copy)
{
kern_return_t result = KERN_SUCCESS;
boolean_t interruptible = TRUE;
assert(src_object != VM_OBJECT_NULL);
vm_object_lock(src_object);
while (!src_object->pager_ready) {
vm_object_wait( src_object,
VM_OBJECT_EVENT_PAGER_READY,
interruptible);
if (interruptible &&
(current_thread()->wait_result != THREAD_AWAKENED)) {
*dst_object = VM_OBJECT_NULL;
*dst_offset = 0;
*dst_needs_copy = FALSE;
return MACH_SEND_INTERRUPTED;
}
vm_object_lock(src_object);
}
if (src_object->temporary) {
src_object->copy_strategy = MEMORY_OBJECT_COPY_DELAY;
}
switch (src_object->copy_strategy) {
case MEMORY_OBJECT_COPY_NONE:
if ((result = vm_object_copy_slowly(
src_object,
src_offset,
size,
interruptible,
dst_object))
== KERN_SUCCESS) {
*dst_offset = 0;
*dst_needs_copy = FALSE;
}
break;
case MEMORY_OBJECT_COPY_CALL:
if ((result = vm_object_copy_call(
src_object,
src_offset,
size,
dst_object))
== KERN_SUCCESS) {
*dst_offset = 0;
*dst_needs_copy = FALSE;
}
break;
case MEMORY_OBJECT_COPY_DELAY:
vm_object_unlock(src_object);
*dst_object = vm_object_copy_delayed(src_object);
*dst_offset = src_offset;
*dst_needs_copy = TRUE;
result = KERN_SUCCESS;
break;
}
return result;
}
void vm_object_shadow(
vm_object_t *object,
vm_offset_t *offset,
vm_size_t length)
{
vm_object_t source;
vm_object_t result;
source = *object;
if ((result = vm_object_allocate(length)) == VM_OBJECT_NULL)
panic("vm_object_shadow: no object for shadowing");
result->shadow = source;
result->shadow_offset = *offset;
*offset = 0;
*object = result;
}
vm_object_t vm_object_lookup(
ipc_port_t port)
{
vm_object_t object = VM_OBJECT_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
(ip_kotype(port) == IKOT_PAGING_REQUEST)) {
vm_object_cache_lock();
object = (vm_object_t) port->ip_kobject;
vm_object_lock(object);
assert(object->alive);
if (object->ref_count == 0)
vm_object_cache_remove(object);
object->ref_count++;
vm_object_unlock(object);
vm_object_cache_unlock();
}
ip_unlock(port);
}
return object;
}
vm_object_t vm_object_lookup_name(
ipc_port_t port)
{
vm_object_t object = VM_OBJECT_NULL;
if (IP_VALID(port)) {
ip_lock(port);
if (ip_active(port) &&
(ip_kotype(port) == IKOT_PAGING_NAME)) {
vm_object_cache_lock();
object = (vm_object_t) port->ip_kobject;
vm_object_lock(object);
assert(object->alive);
if (object->ref_count == 0)
vm_object_cache_remove(object);
object->ref_count++;
vm_object_unlock(object);
vm_object_cache_unlock();
}
ip_unlock(port);
}
return object;
}
void vm_object_destroy(
ipc_port_t pager)
{
vm_object_t object;
pager_request_t old_request;
ipc_port_t old_name;
vm_object_cache_lock();
if (ip_kotype(pager) != IKOT_PAGER) {
vm_object_cache_unlock();
return;
}
object = (vm_object_t) pager->ip_kobject;
vm_object_lock(object);
if (object->ref_count == 0)
vm_object_cache_remove(object);
object->ref_count++;
object->can_persist = FALSE;
assert(object->pager == pager);
object->pager = IP_NULL;
vm_object_remove(object);
old_request = object->pager_request;
object->pager_request = PAGER_REQUEST_NULL;
old_name = object->pager_name;
object->pager_name = IP_NULL;
vm_object_unlock(object);
vm_object_cache_unlock();
ipc_port_release_send(pager);
if (old_request != IP_NULL)
ipc_port_dealloc_kernel(old_request);
if (old_name != IP_NULL)
ipc_port_dealloc_kernel(old_name);
vm_object_lock(object);
vm_object_abort_activity(object);
vm_object_unlock(object);
vm_object_deallocate(object);
}
vm_object_t vm_object_enter(
ipc_port_t pager,
vm_size_t size,
boolean_t internal)
{
vm_object_t object;
vm_object_t new_object;
boolean_t must_init;
ipc_kobject_type_t po;
restart:
if (!IP_VALID(pager))
return vm_object_allocate(size);
new_object = VM_OBJECT_NULL;
must_init = FALSE;
vm_object_cache_lock();
for (;;) {
po = ip_kotype(pager);
if (po == IKOT_PAGER_TERMINATING) {
pager->ip_kobject = (ipc_kobject_t) pager;
assert_wait((event_t) pager, FALSE);
vm_object_cache_unlock();
thread_block((void (*)()) 0);
goto restart;
}
if (po != IKOT_NONE) {
break;
}
if (new_object == VM_OBJECT_NULL) {
vm_object_cache_unlock();
new_object = vm_object_allocate(size);
vm_object_cache_lock();
} else {
ipc_kobject_set(pager,
(ipc_kobject_t) new_object,
IKOT_PAGER);
new_object = VM_OBJECT_NULL;
must_init = TRUE;
}
}
if (internal)
must_init = TRUE;
object = (po == IKOT_PAGER) ? (vm_object_t) pager->ip_kobject
: VM_OBJECT_NULL;
if ((object != VM_OBJECT_NULL) && !must_init) {
vm_object_lock(object);
if (object->ref_count == 0)
vm_object_cache_remove(object);
object->ref_count++;
vm_object_unlock(object);
vm_stat.hits++;
}
assert((object == VM_OBJECT_NULL) || (object->ref_count > 0) ||
((object->paging_in_progress != 0) && internal));
vm_stat.lookups++;
vm_object_cache_unlock();
if (new_object != VM_OBJECT_NULL)
vm_object_deallocate(new_object);
if (object == VM_OBJECT_NULL)
return(object);
if (must_init) {
pager = ipc_port_copy_send(pager);
if (!IP_VALID(pager))
panic("vm_object_enter: port died");
object->pager_created = TRUE;
object->pager = pager;
object->pager_request = ipc_port_alloc_kernel();
if (object->pager_request == IP_NULL)
panic("vm_object_enter: pager request alloc");
ipc_kobject_set(object->pager_request,
(ipc_kobject_t) object,
IKOT_PAGING_REQUEST);
if (internal) {
ipc_port_t DMM = memory_manager_default_reference();
object->internal = TRUE;
assert(object->temporary);
object->pager_ready = TRUE;
(void) memory_object_create(DMM,
pager,
object->size,
object->pager_request,
object->pager_name,
PAGE_SIZE);
} else {
object->internal = FALSE;
object->temporary = FALSE;
assert(object->resident_page_count == 0);
vm_object_external_count++;
object->pager_ready = FALSE;
(void) memory_object_init(pager,
object->pager_request,
object->pager_name,
PAGE_SIZE);
}
vm_object_lock(object);
object->pager_initialized = TRUE;
vm_object_wakeup(object, VM_OBJECT_EVENT_INITIALIZED);
} else {
vm_object_lock(object);
}
while (!object->pager_initialized) {
vm_object_wait( object,
VM_OBJECT_EVENT_INITIALIZED,
FALSE);
vm_object_lock(object);
}
vm_object_unlock(object);
return object;
}
void vm_object_pager_create(
vm_object_t object)
{
ipc_port_t pager;
assert(vm_object_lock_taken(object));
if (object->pager_created) {
while (!object->pager_initialized) {
vm_object_wait( object,
VM_OBJECT_EVENT_PAGER_READY,
FALSE);
vm_object_lock(object);
}
return;
}
object->pager_created = TRUE;
vm_object_paging_begin(object);
vm_object_unlock(object);
#if MACH_PAGEMAP
object->existence_info = vm_external_create(
object->size +
object->paging_offset);
assert((object->size + object->paging_offset) >=
object->size);
#endif
pager = ipc_port_alloc_kernel();
if (pager == IP_NULL)
panic("vm_object_pager_create: allocate pager port");
(void) ipc_port_make_send(pager);
ipc_kobject_set(pager, (ipc_kobject_t) object, IKOT_PAGER);
if (vm_object_enter(pager, object->size, TRUE) != object)
panic("vm_object_pager_create: mismatch");
ipc_port_release_send(pager);
vm_object_lock(object);
vm_object_paging_end(object);
}
void vm_object_remove(
vm_object_t object)
{
ipc_port_t port;
assert(vm_object_cache_locked());
if ((port = object->pager) != IP_NULL) {
if (ip_kotype(port) == IKOT_PAGER)
ipc_kobject_set(port, IKO_NULL,
IKOT_PAGER_TERMINATING);
else if (ip_kotype(port) != IKOT_NONE)
panic("vm_object_remove: bad object port");
}
if ((port = object->pager_request) != IP_NULL) {
if (ip_kotype(port) == IKOT_PAGING_REQUEST)
ipc_kobject_set(port, IKO_NULL, IKOT_NONE);
else if (ip_kotype(port) != IKOT_NONE)
panic("vm_object_remove: bad request port");
}
if ((port = object->pager_name) != IP_NULL) {
if (ip_kotype(port) == IKOT_PAGING_NAME)
ipc_kobject_set(port, IKO_NULL, IKOT_NONE);
else if (ip_kotype(port) != IKOT_NONE)
panic("vm_object_remove: bad name port");
}
}
long object_collapses = 0;
long object_bypasses = 0;
int vm_object_collapse_debug = 0;
boolean_t vm_object_collapse_allowed = TRUE;
boolean_t vm_object_collapse_bypass_allowed = TRUE;
void vm_object_collapse(
vm_object_t object)
{
vm_object_t backing_object;
vm_offset_t backing_offset;
vm_size_t size;
vm_offset_t new_offset;
vm_page_t p, pp;
ipc_port_t old_name_port;
assert(vm_object_lock_taken(object));
if (!vm_object_collapse_allowed)
return;
while (TRUE) {
if (object == VM_OBJECT_NULL ||
object->pager_created ||
object->paging_in_progress != 0 ||
object->absent_count != 0)
return;
if ((backing_object = object->shadow) == VM_OBJECT_NULL)
return;
vm_object_lock(backing_object);
if (!backing_object->internal ||
backing_object->paging_in_progress != 0) {
vm_object_unlock(backing_object);
return;
}
if (backing_object->shadow != VM_OBJECT_NULL &&
backing_object->shadow->copy != VM_OBJECT_NULL) {
vm_object_unlock(backing_object);
return;
}
backing_offset = object->shadow_offset;
size = object->size;
if (backing_object->ref_count == 1) {
if (!vm_object_cache_lock_try()) {
vm_object_unlock(backing_object);
return;
}
while (!queue_empty(&backing_object->memq)) {
p = (vm_page_t)
queue_first(&backing_object->memq);
new_offset = (p->offset - backing_offset);
assert(!p->busy || p->absent);
if (p->offset < backing_offset ||
new_offset >= size) {
VM_PAGE_FREE(p);
} else {
pp = vm_page_lookup(object, new_offset);
if (pp != VM_PAGE_NULL && !pp->absent) {
VM_PAGE_FREE(p);
}
else {
assert(pp == VM_PAGE_NULL || !
"vm_object_collapse: bad case");
vm_page_rename(p, object, new_offset);
}
}
}
switch (vm_object_collapse_debug) {
case 0:
break;
case 1:
if ((backing_object->pager == IP_NULL) &&
(backing_object->pager_request ==
PAGER_REQUEST_NULL))
break;
default:
printf("vm_object_collapse: %p (pager %p, request %p) up to %p\n",
backing_object, backing_object->pager, backing_object->pager_request,
object);
if (vm_object_collapse_debug > 2)
SoftDebugger("vm_object_collapse");
}
object->pager = backing_object->pager;
if (object->pager != IP_NULL)
ipc_kobject_set(object->pager,
(ipc_kobject_t) object,
IKOT_PAGER);
object->pager_initialized = backing_object->pager_initialized;
object->pager_ready = backing_object->pager_ready;
object->pager_created = backing_object->pager_created;
object->pager_request = backing_object->pager_request;
if (object->pager_request != IP_NULL)
ipc_kobject_set(object->pager_request,
(ipc_kobject_t) object,
IKOT_PAGING_REQUEST);
old_name_port = object->pager_name;
if (old_name_port != IP_NULL)
ipc_kobject_set(old_name_port,
IKO_NULL, IKOT_NONE);
object->pager_name = backing_object->pager_name;
if (object->pager_name != IP_NULL)
ipc_kobject_set(object->pager_name,
(ipc_kobject_t) object,
IKOT_PAGING_NAME);
vm_object_cache_unlock();
if (object->pager != IP_NULL)
object->paging_offset =
backing_object->paging_offset +
backing_offset;
#if MACH_PAGEMAP
assert(object->existence_info == VM_EXTERNAL_NULL);
object->existence_info = backing_object->existence_info;
#endif
object->shadow = backing_object->shadow;
object->shadow_offset += backing_object->shadow_offset;
if (object->shadow != VM_OBJECT_NULL &&
object->shadow->copy != VM_OBJECT_NULL) {
panic("vm_object_collapse: we collapsed a copy-object!");
}
assert(
(backing_object->ref_count == 1) &&
(backing_object->resident_page_count == 0) &&
(backing_object->paging_in_progress == 0)
);
assert(backing_object->alive);
assert(!backing_object->cached);
backing_object->alive = FALSE;
vm_object_unlock(backing_object);
vm_object_unlock(object);
if (old_name_port != IP_NULL)
ipc_port_dealloc_kernel(old_name_port);
kmem_cache_free(&vm_object_cache, (vm_offset_t) backing_object);
vm_object_lock(object);
object_collapses++;
}
else {
if (!vm_object_collapse_bypass_allowed) {
vm_object_unlock(backing_object);
return;
}
if (backing_object->pager_created) {
vm_object_unlock(backing_object);
return;
}
queue_iterate(&backing_object->memq, p,
vm_page_t, listq)
{
new_offset = (p->offset - backing_offset);
if (p->offset >= backing_offset &&
new_offset <= size &&
(pp = vm_page_lookup(object, new_offset))
== VM_PAGE_NULL) {
vm_object_unlock(backing_object);
return;
}
}
vm_object_reference(object->shadow = backing_object->shadow);
object->shadow_offset += backing_object->shadow_offset;
if (backing_object->copy == object)
backing_object->copy = VM_OBJECT_NULL;
backing_object->ref_count--;
assert(backing_object->ref_count > 0);
vm_object_unlock(backing_object);
object_bypasses ++;
}
}
}
unsigned int vm_object_page_remove_lookup = 0;
unsigned int vm_object_page_remove_iterate = 0;
void vm_object_page_remove(
vm_object_t object,
vm_offset_t start,
vm_offset_t end)
{
vm_page_t p, next;
assert(vm_object_lock_taken(object));
if (atop(end - start) < object->resident_page_count/16) {
vm_object_page_remove_lookup++;
for (; start < end; start += PAGE_SIZE) {
p = vm_page_lookup(object, start);
if (p != VM_PAGE_NULL) {
if (!p->fictitious)
pmap_page_protect(p->phys_addr,
VM_PROT_NONE);
VM_PAGE_FREE(p);
}
}
} else {
vm_object_page_remove_iterate++;
p = (vm_page_t) queue_first(&object->memq);
while (!queue_end(&object->memq, (queue_entry_t) p)) {
next = (vm_page_t) queue_next(&p->listq);
if ((start <= p->offset) && (p->offset < end)) {
if (!p->fictitious)
pmap_page_protect(p->phys_addr,
VM_PROT_NONE);
VM_PAGE_FREE(p);
}
p = next;
}
}
}
boolean_t vm_object_coalesce(
vm_object_t prev_object,
vm_object_t next_object,
vm_offset_t prev_offset,
vm_offset_t next_offset,
vm_size_t prev_size,
vm_size_t next_size,
vm_object_t *new_object,
vm_offset_t *new_offset)
{
vm_object_t object;
vm_size_t newsize;
if (prev_object == next_object) {
if (prev_object == VM_OBJECT_NULL) {
*new_object = VM_OBJECT_NULL;
*new_offset = 0;
return TRUE;
}
if (prev_offset + prev_size == next_offset) {
*new_object = prev_object;
*new_offset = prev_offset;
vm_object_deallocate(prev_object);
return TRUE;
}
return FALSE;
}
if (next_object != VM_OBJECT_NULL) {
if (prev_object != VM_OBJECT_NULL)
return FALSE;
object = next_object;
} else {
object = prev_object;
}
vm_object_lock(object);
vm_object_collapse(object);
if ((object->ref_count > 1) ||
object->pager_created ||
object->used_for_pageout ||
(object->shadow != VM_OBJECT_NULL) ||
(object->copy != VM_OBJECT_NULL) ||
(object->paging_in_progress != 0)) {
vm_object_unlock(object);
return FALSE;
}
if (object == prev_object) {
vm_object_page_remove(object,
prev_offset + prev_size,
prev_offset + prev_size + next_size);
newsize = prev_offset + prev_size + next_size;
if (newsize > object->size)
object->size = newsize;
*new_offset = prev_offset;
} else {
if (next_offset < prev_size) {
vm_object_unlock(object);
return FALSE;
}
vm_object_page_remove(object,
next_offset - prev_size,
next_offset);
*new_offset = next_offset - prev_size;
}
vm_object_unlock(object);
*new_object = object;
return TRUE;
}
vm_object_t vm_object_request_object(
ipc_port_t p)
{
return vm_object_lookup(p);
}
ipc_port_t vm_object_name(
vm_object_t object)
{
ipc_port_t p;
if (object == VM_OBJECT_NULL)
return IP_NULL;
vm_object_lock(object);
while (object->shadow != VM_OBJECT_NULL) {
vm_object_t new_object = object->shadow;
vm_object_lock(new_object);
vm_object_unlock(object);
object = new_object;
}
p = object->pager_name;
if (p != IP_NULL)
p = ipc_port_make_send(p);
vm_object_unlock(object);
return p;
}
kern_return_t
vm_object_page_map(
vm_object_t object,
vm_offset_t offset,
vm_size_t size,
phys_addr_t (*map_fn)(void *, vm_offset_t),
void * map_fn_data)
{
int num_pages;
int i;
vm_page_t m;
vm_page_t old_page;
phys_addr_t addr;
num_pages = atop(size);
for (i = 0; i < num_pages; i++, offset += PAGE_SIZE) {
addr = (*map_fn)(map_fn_data, offset);
if (addr == vm_page_fictitious_addr)
return KERN_NO_ACCESS;
while ((m = vm_page_grab_fictitious()) == VM_PAGE_NULL)
vm_page_more_fictitious();
vm_object_lock(object);
if ((old_page = vm_page_lookup(object, offset))
!= VM_PAGE_NULL)
{
VM_PAGE_FREE(old_page);
}
vm_page_init(m);
m->phys_addr = addr;
m->private = TRUE;
m->wire_count = 1;
vm_page_lock_queues();
vm_page_insert(m, object, offset);
vm_page_unlock_queues();
PAGE_WAKEUP_DONE(m);
vm_object_unlock(object);
}
return KERN_SUCCESS;
}
#if MACH_KDB
#include <vm/vm_print.h>
#define printf kdbprintf
boolean_t vm_object_print_pages = FALSE;
void vm_object_print_part(
vm_object_t object,
vm_offset_t offset,
vm_size_t size)
{
vm_page_t p;
int count, count2;
if (object == VM_OBJECT_NULL)
return;
iprintf("Object 0x%X: size=0x%X, %d references",
(vm_offset_t) object, (vm_offset_t) object->size,
object->ref_count);
printf("\n");
iprintf("%lu resident pages,", object->resident_page_count);
printf(" %d absent pages,", object->absent_count);
printf(" %d paging ops\n", object->paging_in_progress);
indent += 1;
iprintf("memory object=0x%X (offset=0x%X),",
(vm_offset_t) object->pager, (vm_offset_t) object->paging_offset);
printf("control=0x%X, name=0x%X\n",
(vm_offset_t) object->pager_request, (vm_offset_t) object->pager_name);
iprintf("%s%s",
object->pager_ready ? " ready" : "",
object->pager_created ? " created" : "");
printf("%s,%s ",
object->pager_initialized ? "" : "uninitialized",
object->temporary ? "temporary" : "permanent");
printf("%s%s,",
object->internal ? "internal" : "external",
object->can_persist ? " cacheable" : "");
printf("copy_strategy=%d\n", (vm_offset_t)object->copy_strategy);
iprintf("shadow=0x%X (offset=0x%X),",
(vm_offset_t) object->shadow, (vm_offset_t) object->shadow_offset);
printf("copy=0x%X\n", (vm_offset_t) object->copy);
count = 0;
count2 = 0;
p = (vm_page_t) queue_first(&object->memq);
while (!queue_end(&object->memq, (queue_entry_t) p)) {
if (p->offset >= offset && p->offset + PAGE_SIZE <= size) {
if (p->wire_count)
count++;
count2++;
}
p = (vm_page_t) queue_next(&p->listq);
}
iprintf("wired: %d/%d\n", count, count2);
indent += 1;
if (vm_object_print_pages) {
count = 0;
p = (vm_page_t) queue_first(&object->memq);
while (!queue_end(&object->memq, (queue_entry_t) p)) {
if (p->offset >= offset && p->offset + PAGE_SIZE <= size) {
if (count == 0) iprintf("memory:=");
else if (count == 4) {printf("\n"); iprintf(" ..."); count = 0;}
else printf(",");
count++;
printf("(off=0x%X,page=0x%X)", p->offset, (vm_offset_t) p);
}
p = (vm_page_t) queue_next(&p->listq);
}
if (count != 0)
printf("\n");
}
indent -= 2;
}
void vm_object_print(
vm_object_t object)
{
vm_object_print_part(object, 0, UINTPTR_MAX);
}
#endif
#include <vm/vm_block_cache.h>
kern_return_t
vm_object_enable_block_cache(vm_object_t object, vm_size_t block_size)
{
block_cache_t cache;
if (object == VM_OBJECT_NULL)
return KERN_INVALID_ARGUMENT;
if (block_size < BLOCK_CACHE_MIN_BLOCK_SIZE ||
block_size > BLOCK_CACHE_MAX_BLOCK_SIZE ||
(block_size & (block_size - 1)) != 0)
return KERN_INVALID_ARGUMENT;
vm_object_lock(object);
if (object->block_cache_enabled) {
vm_object_unlock(object);
return KERN_SUCCESS;
}
cache = block_cache_create(object, block_size);
if (cache == NULL) {
vm_object_unlock(object);
return KERN_RESOURCE_SHORTAGE;
}
object->block_cache = cache;
object->block_cache_enabled = TRUE;
vm_object_unlock(object);
return KERN_SUCCESS;
}
void
vm_object_disable_block_cache(vm_object_t object)
{
block_cache_t cache;
if (object == VM_OBJECT_NULL)
return;
vm_object_lock(object);
if (!object->block_cache_enabled) {
vm_object_unlock(object);
return;
}
cache = object->block_cache;
object->block_cache = NULL;
object->block_cache_enabled = FALSE;
vm_object_unlock(object);
if (cache != NULL)
block_cache_destroy(cache);
}
boolean_t
vm_object_has_block_cache(vm_object_t object)
{
boolean_t enabled;
if (object == VM_OBJECT_NULL)
return FALSE;
vm_object_lock(object);
enabled = object->block_cache_enabled;
vm_object_unlock(object);
return enabled;
}