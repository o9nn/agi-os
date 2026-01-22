#include <vm/vm_fault.h>
#include <mach/kern_return.h>
#include <vm/vm_map.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <vm/pmap.h>
#include <intel/read_fault.h>
#include <kern/macros.h>
#if (__i386__ && !(__i486__ || __i586__ || __i686__))
kern_return_t
intel_read_fault(
vm_map_t map,
vm_offset_t vaddr)
{
vm_map_version_t version;
vm_object_t object;
vm_offset_t offset;
vm_prot_t prot;
vm_page_t result_page;
vm_page_t top_page;
boolean_t wired;
kern_return_t result;
vm_page_t m;
RetryFault:
result = vm_map_lookup(&map, vaddr, VM_PROT_READ, FALSE, &version,
&object, &offset, &prot, &wired);
if (result != KERN_SUCCESS)
return (result);
assert(object->ref_count > 0);
object->ref_count++;
vm_object_paging_begin(object);
result = vm_fault_page(object, offset, VM_PROT_READ, FALSE, TRUE,
&prot, &result_page, &top_page,
FALSE, (void (*)()) 0);
if (result != VM_FAULT_SUCCESS) {
vm_object_deallocate(object);
switch (result) {
case VM_FAULT_RETRY:
goto RetryFault;
case VM_FAULT_INTERRUPTED:
return (KERN_SUCCESS);
case VM_FAULT_MEMORY_SHORTAGE:
VM_PAGE_WAIT((void (*)()) 0);
goto RetryFault;
case VM_FAULT_FICTITIOUS_SHORTAGE:
vm_page_more_fictitious();
goto RetryFault;
case VM_FAULT_MEMORY_ERROR:
return (KERN_MEMORY_ERROR);
}
}
m = result_page;
#define UNLOCK_AND_DEALLOCATE \
MACRO_BEGIN \
vm_fault_cleanup(m->object, top_page); \
vm_object_deallocate(object); \
MACRO_END
#define RELEASE_PAGE(m) \
MACRO_BEGIN \
PAGE_WAKEUP_DONE(m); \
vm_page_lock_queues(); \
if (!m->active && !m->inactive) \
vm_page_activate(m); \
vm_page_unlock_queues(); \
MACRO_END
vm_object_unlock(m->object);
while (!vm_map_verify(map, &version)) {
vm_object_t retry_object;
vm_offset_t retry_offset;
vm_prot_t retry_prot;
result = vm_map_lookup(&map, vaddr, VM_PROT_READ, FALSE, &version,
&retry_object, &retry_offset, &retry_prot,
&wired);
if (result != KERN_SUCCESS) {
vm_object_lock(m->object);
RELEASE_PAGE(m);
UNLOCK_AND_DEALLOCATE;
return (result);
}
vm_object_unlock(retry_object);
if (retry_object != object || retry_offset != offset) {
vm_object_lock(m->object);
RELEASE_PAGE(m);
UNLOCK_AND_DEALLOCATE;
goto RetryFault;
}
}
PMAP_ENTER(map->pmap, vaddr, m, VM_PROT_READ|VM_PROT_WRITE, wired);
vm_object_lock(m->object);
vm_page_lock_queues();
if (!m->active && !m->inactive)
vm_page_activate(m);
m->reference = TRUE;
vm_page_unlock_queues();
vm_map_verify_done(map, &version);
PAGE_WAKEUP_DONE(m);
UNLOCK_AND_DEALLOCATE;
#undef UNLOCK_AND_DEALLOCATE
#undef RELEASE_PAGE
return (KERN_SUCCESS);
}
#endif