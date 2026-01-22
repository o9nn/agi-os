#include <mach/std_types.h>
#include <mach/mach_types.h>
#include <mach/kern_return.h>
#include <vm/vm_map.h>
#include <vm/vm_object.h>
#include <mach/memory_object.h>
#include <mach/boolean.h>
#include <mach/vm_prot.h>
#include <mach/message.h>
#include <vm/memory_object_user.user.h>
#include <vm/memory_object_default.user.h>
#include <vm/memory_object.h>
#include <vm/vm_page.h>
#include <vm/vm_pageout.h>
#include <vm/pmap.h>
#include <kern/debug.h>
#include <kern/thread.h>
#include <kern/host.h>
#include <kern/mach.server.h>
#include <vm/vm_kern.h>
#include <vm/vm_map.h>
#include <ipc/ipc_port.h>
#if MACH_PAGEMAP
#include <vm/vm_external.h>
#endif
typedef int memory_object_lock_result_t;
ipc_port_t memory_manager_default = IP_NULL;
def_simple_lock_data(static,memory_manager_default_lock)
kern_return_t memory_object_data_supply(
vm_object_t object,
vm_offset_t offset,
vm_offset_t vm_data_copy,
unsigned int data_cnt,
vm_prot_t lock_value,
boolean_t precious,
ipc_port_t reply_to,
mach_msg_type_name_t reply_to_type)
{
kern_return_t result = KERN_SUCCESS;
vm_offset_t error_offset = 0;
vm_page_t m;
vm_page_t data_m;
vm_size_t original_length;
vm_offset_t original_offset;
vm_page_t *page_list;
boolean_t was_absent;
vm_map_copy_t data_copy = (vm_map_copy_t)vm_data_copy;
vm_map_copy_t orig_copy = data_copy;
if (object == VM_OBJECT_NULL) {
return(KERN_INVALID_ARGUMENT);
}
if (lock_value & ~VM_PROT_ALL) {
vm_object_deallocate(object);
return(KERN_INVALID_ARGUMENT);
}
if ((data_cnt % PAGE_SIZE) != 0) {
vm_object_deallocate(object);
return(KERN_INVALID_ARGUMENT);
}
original_length = data_cnt;
original_offset = offset;
assert(data_copy->type == VM_MAP_COPY_PAGE_LIST);
page_list = &data_copy->cpy_page_list[0];
vm_object_lock(object);
vm_object_paging_begin(object);
offset -= object->paging_offset;
for (; data_cnt > 0 ; data_cnt -= PAGE_SIZE, offset += PAGE_SIZE) {
assert(data_copy->cpy_npages > 0);
data_m = *page_list;
if (data_m == VM_PAGE_NULL || data_m->tabled ||
data_m->error || data_m->absent || data_m->fictitious) {
panic("Data_supply: bad page");
}
retry_lookup:
m = vm_page_lookup(object,offset);
if (m == VM_PAGE_NULL) {
was_absent = FALSE;
}
else {
if (m->absent && m->busy) {
VM_PAGE_FREE(m);
was_absent = TRUE;
}
else {
if (m->busy) {
PAGE_ASSERT_WAIT(m, FALSE);
vm_object_unlock(object);
thread_block((void (*)()) 0);
vm_object_lock(object);
goto retry_lookup;
}
result = KERN_MEMORY_PRESENT;
error_offset = offset + object->paging_offset;
break;
}
}
data_m->busy = FALSE;
data_m->dirty = FALSE;
pmap_clear_modify(data_m->phys_addr);
data_m->page_lock = lock_value;
data_m->unlock_request = VM_PROT_NONE;
data_m->precious = precious;
vm_page_lock_queues();
vm_page_insert(data_m, object, offset);
if (was_absent)
vm_page_activate(data_m);
else
vm_page_deactivate(data_m);
vm_page_unlock_queues();
*page_list++ = VM_PAGE_NULL;
if (--(data_copy->cpy_npages) == 0 &&
vm_map_copy_has_cont(data_copy)) {
vm_map_copy_t new_copy;
vm_object_unlock(object);
vm_map_copy_invoke_cont(data_copy, &new_copy, &result);
if (result == KERN_SUCCESS) {
if (data_copy != orig_copy) {
vm_map_copy_discard(data_copy);
}
if ((data_copy = new_copy) != VM_MAP_COPY_NULL)
page_list = &data_copy->cpy_page_list[0];
vm_object_lock(object);
}
else {
vm_object_lock(object);
error_offset = offset + object->paging_offset +
PAGE_SIZE;
break;
}
}
}
vm_object_paging_end(object);
vm_object_unlock(object);
if (vm_map_copy_has_cont(data_copy))
vm_map_copy_abort_cont(data_copy);
if (IP_VALID(reply_to)) {
memory_object_supply_completed(
reply_to, reply_to_type,
object->pager_request,
original_offset,
original_length,
result,
error_offset);
}
vm_object_deallocate(object);
if (data_copy != orig_copy)
vm_map_copy_discard(data_copy);
if (result == KERN_SUCCESS)
vm_map_copy_discard(orig_copy);
return(result);
}
kern_return_t memory_object_data_error(
vm_object_t object,
vm_offset_t offset,
vm_size_t size,
kern_return_t error_value)
{
if (object == VM_OBJECT_NULL)
return(KERN_INVALID_ARGUMENT);
if (size != round_page(size))
return(KERN_INVALID_ARGUMENT);
vm_object_lock(object);
offset -= object->paging_offset;
while (size != 0) {
vm_page_t m;
m = vm_page_lookup(object, offset);
if ((m != VM_PAGE_NULL) && m->busy && m->absent) {
m->error = TRUE;
m->absent = FALSE;
vm_object_absent_release(object);
PAGE_WAKEUP_DONE(m);
vm_page_lock_queues();
vm_page_activate(m);
vm_page_unlock_queues();
}
size -= PAGE_SIZE;
offset += PAGE_SIZE;
}
vm_object_unlock(object);
vm_object_deallocate(object);
return(KERN_SUCCESS);
}
kern_return_t memory_object_data_unavailable(
vm_object_t object,
vm_offset_t offset,
vm_size_t size)
{
#if MACH_PAGEMAP
vm_external_t existence_info = VM_EXTERNAL_NULL;
#endif
if (object == VM_OBJECT_NULL)
return(KERN_INVALID_ARGUMENT);
if (size != round_page(size))
return(KERN_INVALID_ARGUMENT);
#if MACH_PAGEMAP
if ((offset == 0) && (size > VM_EXTERNAL_LARGE_SIZE) &&
(object->existence_info == VM_EXTERNAL_NULL)) {
existence_info = vm_external_create(VM_EXTERNAL_SMALL_SIZE);
}
#endif
vm_object_lock(object);
#if MACH_PAGEMAP
if (existence_info != VM_EXTERNAL_NULL) {
object->existence_info = existence_info;
}
if ((offset == 0) && (size > VM_EXTERNAL_LARGE_SIZE)) {
vm_object_unlock(object);
vm_object_deallocate(object);
return(KERN_SUCCESS);
}
#endif
offset -= object->paging_offset;
while (size != 0) {
vm_page_t m;
m = vm_page_lookup(object, offset);
if ((m != VM_PAGE_NULL) && m->busy && m->absent) {
PAGE_WAKEUP_DONE(m);
vm_page_lock_queues();
vm_page_activate(m);
vm_page_unlock_queues();
}
size -= PAGE_SIZE;
offset += PAGE_SIZE;
}
vm_object_unlock(object);
vm_object_deallocate(object);
return(KERN_SUCCESS);
}
#define MEMORY_OBJECT_LOCK_RESULT_DONE 0
#define MEMORY_OBJECT_LOCK_RESULT_MUST_BLOCK 1
#define MEMORY_OBJECT_LOCK_RESULT_MUST_CLEAN 2
#define MEMORY_OBJECT_LOCK_RESULT_MUST_RETURN 3
static memory_object_lock_result_t memory_object_lock_page(
vm_page_t m,
memory_object_return_t should_return,
boolean_t should_flush,
vm_prot_t prot)
{
if (m->absent)
return(MEMORY_OBJECT_LOCK_RESULT_DONE);
if (m->busy)
return(MEMORY_OBJECT_LOCK_RESULT_MUST_BLOCK);
assert(!m->fictitious);
if (m->wire_count != 0) {
if (!should_flush &&
((m->page_lock == prot) || (prot == VM_PROT_NO_CHANGE)) &&
((should_return == MEMORY_OBJECT_RETURN_NONE) ||
(!m->dirty && !pmap_is_modified(m->phys_addr) &&
(!m->precious ||
should_return != MEMORY_OBJECT_RETURN_ALL)))) {
m->unlock_request = VM_PROT_NONE;
PAGE_WAKEUP(m);
return(MEMORY_OBJECT_LOCK_RESULT_DONE);
}
return(MEMORY_OBJECT_LOCK_RESULT_MUST_BLOCK);
}
if (should_flush)
prot = VM_PROT_ALL;
if (prot != VM_PROT_NO_CHANGE) {
if ((m->page_lock ^ prot) & prot) {
pmap_page_protect(m->phys_addr, VM_PROT_ALL & ~prot);
}
m->page_lock = prot;
m->unlock_request = VM_PROT_NONE;
PAGE_WAKEUP(m);
}
if (should_return != MEMORY_OBJECT_RETURN_NONE) {
if (!m->dirty)
m->dirty = pmap_is_modified(m->phys_addr);
if (m->dirty || (m->precious &&
should_return == MEMORY_OBJECT_RETURN_ALL)) {
vm_page_lock_queues();
VM_PAGE_QUEUES_REMOVE(m);
vm_page_unlock_queues();
if (!should_flush)
pmap_page_protect(m->phys_addr,
VM_PROT_NONE);
if (m->dirty)
return(MEMORY_OBJECT_LOCK_RESULT_MUST_CLEAN);
else
return(MEMORY_OBJECT_LOCK_RESULT_MUST_RETURN);
}
}
if (should_flush) {
VM_PAGE_FREE(m);
} else {
extern boolean_t vm_page_deactivate_hint;
if (vm_page_deactivate_hint &&
(should_return != MEMORY_OBJECT_RETURN_NONE)) {
vm_page_lock_queues();
vm_page_deactivate(m);
vm_page_unlock_queues();
}
}
return(MEMORY_OBJECT_LOCK_RESULT_DONE);
}
kern_return_t
memory_object_lock_request(
vm_object_t object,
vm_offset_t offset,
vm_size_t size,
memory_object_return_t should_return,
boolean_t should_flush,
vm_prot_t prot,
ipc_port_t reply_to,
mach_msg_type_name_t reply_to_type)
{
vm_page_t m;
vm_offset_t original_offset = offset;
vm_size_t original_size = size;
vm_offset_t paging_offset = 0;
vm_object_t new_object = VM_OBJECT_NULL;
vm_offset_t new_offset = 0;
vm_offset_t last_offset = offset;
int page_lock_result;
int pageout_action = 0;
#define DATA_WRITE_MAX 32
vm_page_t holding_pages[DATA_WRITE_MAX];
if (object == VM_OBJECT_NULL ||
((prot & ~VM_PROT_ALL) != 0 && prot != VM_PROT_NO_CHANGE))
return (KERN_INVALID_ARGUMENT);
size = round_page(size);
vm_object_lock(object);
vm_object_paging_begin(object);
offset -= object->paging_offset;
#define PAGEOUT_PAGES \
MACRO_BEGIN \
vm_map_copy_t copy; \
unsigned i; \
vm_page_t hp; \
\
vm_object_unlock(object); \
\
(void) vm_map_copyin_object(new_object, 0, new_offset, &copy); \
\
(void) memory_object_data_return( \
object->pager, \
object->pager_request, \
paging_offset, \
(pointer_t) copy, \
new_offset, \
(pageout_action == MEMORY_OBJECT_LOCK_RESULT_MUST_CLEAN), \
!should_flush); \
\
vm_object_lock(object); \
\
for (i = 0; i < atop(new_offset); i++) { \
hp = holding_pages[i]; \
if (hp != VM_PAGE_NULL) \
VM_PAGE_FREE(hp); \
} \
\
new_object = VM_OBJECT_NULL; \
MACRO_END
for (;
size != 0;
size -= PAGE_SIZE, offset += PAGE_SIZE)
{
if (new_object != VM_OBJECT_NULL &&
new_offset >= PAGE_SIZE * DATA_WRITE_MAX)
{
PAGEOUT_PAGES;
}
while ((m = vm_page_lookup(object, offset)) != VM_PAGE_NULL) {
switch ((page_lock_result = memory_object_lock_page(m,
should_return,
should_flush,
prot)))
{
case MEMORY_OBJECT_LOCK_RESULT_DONE:
if (new_object != VM_OBJECT_NULL) {
PAGEOUT_PAGES;
continue;
}
break;
case MEMORY_OBJECT_LOCK_RESULT_MUST_BLOCK:
if (new_object != VM_OBJECT_NULL) {
PAGEOUT_PAGES;
continue;
}
PAGE_ASSERT_WAIT(m, FALSE);
vm_object_unlock(object);
thread_block((void (*)()) 0);
vm_object_lock(object);
continue;
case MEMORY_OBJECT_LOCK_RESULT_MUST_CLEAN:
case MEMORY_OBJECT_LOCK_RESULT_MUST_RETURN:
m->busy = TRUE;
if (new_object != VM_OBJECT_NULL &&
(last_offset != offset ||
pageout_action != page_lock_result)) {
PAGEOUT_PAGES;
}
vm_object_unlock(object);
if (new_object == VM_OBJECT_NULL) {
new_object = vm_object_allocate(original_size);
new_offset = 0;
paging_offset = m->offset +
object->paging_offset;
pageout_action = page_lock_result;
}
m = vm_pageout_setup(m,
m->offset + object->paging_offset,
new_object,
new_offset,
should_flush);
holding_pages[atop(new_offset)] = m;
new_offset += PAGE_SIZE;
last_offset = offset + PAGE_SIZE;
vm_object_lock(object);
break;
}
break;
}
}
if (new_object != VM_OBJECT_NULL) {
PAGEOUT_PAGES;
}
if (IP_VALID(reply_to)) {
vm_object_unlock(object);
(void) memory_object_lock_completed(reply_to, reply_to_type,
object->pager_request, original_offset, original_size);
vm_object_lock(object);
}
vm_object_paging_end(object);
vm_object_unlock(object);
vm_object_deallocate(object);
return (KERN_SUCCESS);
}
static kern_return_t
memory_object_set_attributes_common(
vm_object_t object,
boolean_t may_cache,
memory_object_copy_strategy_t copy_strategy)
{
if (object == VM_OBJECT_NULL)
return(KERN_INVALID_ARGUMENT);
switch(copy_strategy) {
case MEMORY_OBJECT_COPY_NONE:
case MEMORY_OBJECT_COPY_CALL:
case MEMORY_OBJECT_COPY_DELAY:
case MEMORY_OBJECT_COPY_TEMPORARY:
break;
default:
vm_object_deallocate(object);
return(KERN_INVALID_ARGUMENT);
}
if (may_cache)
may_cache = TRUE;
vm_object_lock(object);
if (!object->pager_ready) {
vm_object_wakeup(object, VM_OBJECT_EVENT_PAGER_READY);
}
object->can_persist = may_cache;
object->pager_ready = TRUE;
if (copy_strategy == MEMORY_OBJECT_COPY_TEMPORARY) {
object->temporary = TRUE;
} else {
object->copy_strategy = copy_strategy;
}
vm_object_unlock(object);
vm_object_deallocate(object);
return(KERN_SUCCESS);
}
kern_return_t memory_object_change_attributes(
vm_object_t object,
boolean_t may_cache,
memory_object_copy_strategy_t copy_strategy,
ipc_port_t reply_to,
mach_msg_type_name_t reply_to_type)
{
kern_return_t result;
result = memory_object_set_attributes_common(object, may_cache,
copy_strategy);
if (IP_VALID(reply_to)) {
(void) memory_object_change_completed(reply_to, reply_to_type,
may_cache, copy_strategy);
}
return(result);
}
kern_return_t memory_object_ready(
vm_object_t object,
boolean_t may_cache,
memory_object_copy_strategy_t copy_strategy)
{
return memory_object_set_attributes_common(object, may_cache,
copy_strategy);
}
kern_return_t memory_object_get_attributes(
vm_object_t object,
boolean_t *object_ready,
boolean_t *may_cache,
memory_object_copy_strategy_t *copy_strategy)
{
if (object == VM_OBJECT_NULL)
return(KERN_INVALID_ARGUMENT);
vm_object_lock(object);
*may_cache = object->can_persist;
*object_ready = object->pager_ready;
*copy_strategy = object->copy_strategy;
vm_object_unlock(object);
vm_object_deallocate(object);
return(KERN_SUCCESS);
}
kern_return_t vm_set_default_memory_manager(
const host_t host,
ipc_port_t *default_manager)
{
ipc_port_t current_manager;
ipc_port_t new_manager;
ipc_port_t returned_manager;
if (host == HOST_NULL)
return(KERN_INVALID_HOST);
new_manager = *default_manager;
simple_lock(&memory_manager_default_lock);
current_manager = memory_manager_default;
if (new_manager == IP_NULL) {
returned_manager = ipc_port_copy_send(current_manager);
} else {
returned_manager = current_manager;
memory_manager_default = new_manager;
thread_wakeup((event_t) &memory_manager_default);
}
simple_unlock(&memory_manager_default_lock);
*default_manager = returned_manager;
return(KERN_SUCCESS);
}
ipc_port_t memory_manager_default_reference(void)
{
ipc_port_t current_manager;
simple_lock(&memory_manager_default_lock);
while (current_manager = ipc_port_copy_send(memory_manager_default),
!IP_VALID(current_manager)) {
thread_sleep((event_t) &memory_manager_default,
simple_lock_addr(memory_manager_default_lock),
FALSE);
simple_lock(&memory_manager_default_lock);
}
simple_unlock(&memory_manager_default_lock);
return current_manager;
}
boolean_t memory_manager_default_port(const ipc_port_t port)
{
ipc_port_t current;
boolean_t result;
simple_lock(&memory_manager_default_lock);
current = memory_manager_default;
if (IP_VALID(current)) {
result = port->ip_receiver == current->ip_receiver;
} else
result = FALSE;
simple_unlock(&memory_manager_default_lock);
return result;
}
void memory_manager_default_init(void)
{
memory_manager_default = IP_NULL;
simple_lock_init(&memory_manager_default_lock);
}