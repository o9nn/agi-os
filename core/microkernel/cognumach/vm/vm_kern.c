#include <string.h>
#include <mach/kern_return.h>
#include <machine/locore.h>
#include <machine/vm_param.h>
#include <kern/assert.h>
#include <kern/debug.h>
#include <kern/lock.h>
#include <kern/slab.h>
#include <kern/thread.h>
#include <kern/printf.h>
#include <vm/pmap.h>
#include <vm/vm_fault.h>
#include <vm/vm_kern.h>
#include <vm/vm_map.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <vm/vm_pageout.h>
#include <mach/mach_safety.h>
static struct vm_map kernel_map_store;
vm_map_t kernel_map = &kernel_map_store;
vm_map_t kernel_pageable_map;
kern_return_t
projected_buffer_allocate(
vm_map_t map,
vm_size_t size,
int persistence,
vm_offset_t *kernel_p,
vm_offset_t *user_p,
vm_prot_t protection,
vm_inherit_t inheritance)
{
vm_object_t object;
vm_map_entry_t u_entry, k_entry;
vm_offset_t addr;
phys_addr_t physical_addr;
vm_size_t r_size;
kern_return_t kr;
if (map == VM_MAP_NULL || map == kernel_map)
return(KERN_INVALID_ARGUMENT);
size = round_page(size);
object = vm_object_allocate(size);
vm_map_lock(kernel_map);
kr = vm_map_find_entry(kernel_map, &addr, size, (vm_offset_t) 0,
VM_OBJECT_NULL, &k_entry);
if (kr != KERN_SUCCESS) {
vm_map_unlock(kernel_map);
vm_object_deallocate(object);
return kr;
}
k_entry->object.vm_object = object;
if (!persistence)
k_entry->projected_on = (vm_map_entry_t) -1;
vm_map_unlock(kernel_map);
*kernel_p = addr;
vm_map_lock(map);
kr = vm_map_find_entry(map, &addr, size, (vm_offset_t) 0,
VM_OBJECT_NULL, &u_entry);
if (kr != KERN_SUCCESS) {
vm_map_unlock(map);
vm_map_lock(kernel_map);
vm_map_entry_delete(kernel_map, k_entry);
vm_map_unlock(kernel_map);
vm_object_deallocate(object);
return kr;
}
u_entry->object.vm_object = object;
vm_object_reference(object);
u_entry->projected_on = k_entry;
u_entry->protection = protection;
u_entry->max_protection = protection;
u_entry->inheritance = inheritance;
vm_map_unlock(map);
*user_p = addr;
kmem_alloc_pages(object, 0,
*kernel_p, *kernel_p + size,
VM_PROT_READ | VM_PROT_WRITE);
memset((void*) *kernel_p, 0, size);
pmap_pageable(map->pmap, *user_p, *user_p + size, FALSE);
for (r_size = 0; r_size < size; r_size += PAGE_SIZE) {
physical_addr = pmap_extract(kernel_pmap, *kernel_p + r_size);
pmap_enter(map->pmap, *user_p + r_size, physical_addr,
protection, TRUE);
}
return(KERN_SUCCESS);
}
kern_return_t
projected_buffer_map(
vm_map_t map,
vm_offset_t kernel_addr,
vm_size_t size,
vm_offset_t *user_p,
vm_prot_t protection,
vm_inherit_t inheritance)
{
vm_map_entry_t u_entry, k_entry;
vm_offset_t user_addr;
phys_addr_t physical_addr;
vm_size_t r_size;
kern_return_t kr;
size = round_page(size);
if (map == VM_MAP_NULL || map == kernel_map ||
!vm_map_lookup_entry(kernel_map, kernel_addr, &k_entry) ||
kernel_addr + size > k_entry->vme_end)
return(KERN_INVALID_ARGUMENT);
vm_map_lock(map);
kr = vm_map_find_entry(map, &user_addr, size, (vm_offset_t) 0,
VM_OBJECT_NULL, &u_entry);
if (kr != KERN_SUCCESS) {
vm_map_unlock(map);
return kr;
}
u_entry->object.vm_object = k_entry->object.vm_object;
vm_object_reference(k_entry->object.vm_object);
u_entry->offset = kernel_addr - k_entry->vme_start + k_entry->offset;
u_entry->projected_on = k_entry;
u_entry->protection = protection;
u_entry->max_protection = protection;
u_entry->inheritance = inheritance;
u_entry->wired_count = k_entry->wired_count;
vm_map_unlock(map);
*user_p = user_addr;
pmap_pageable(map->pmap, user_addr, user_addr + size,
!k_entry->wired_count);
for (r_size = 0; r_size < size; r_size += PAGE_SIZE) {
physical_addr = pmap_extract(kernel_pmap, kernel_addr + r_size);
pmap_enter(map->pmap, user_addr + r_size, physical_addr,
protection, k_entry->wired_count);
}
return(KERN_SUCCESS);
}
kern_return_t
projected_buffer_deallocate(
vm_map_t map,
vm_offset_t start,
vm_offset_t end)
{
vm_map_entry_t entry, k_entry;
if (map == VM_MAP_NULL || map == kernel_map)
return KERN_INVALID_ARGUMENT;
vm_map_lock(map);
if (!vm_map_lookup_entry(map, start, &entry) ||
end > entry->vme_end ||
(k_entry = entry->projected_on) == 0) {
vm_map_unlock(map);
return(KERN_INVALID_ARGUMENT);
}
if (entry->vme_start < start)
_vm_map_clip_start(&map->hdr, entry, start, 1);
if (entry->vme_end > end)
_vm_map_clip_end(&map->hdr, entry, end, 1);
if (map->first_free == entry)
map->first_free = entry->vme_prev;
entry->projected_on = 0;
entry->wired_count = 0;
vm_map_entry_delete(map, entry);
vm_map_unlock(map);
vm_map_lock(kernel_map);
if (k_entry->projected_on == (vm_map_entry_t) -1 &&
k_entry->object.vm_object->ref_count == 1) {
if (kernel_map->first_free == k_entry)
kernel_map->first_free = k_entry->vme_prev;
k_entry->projected_on = 0;
vm_map_entry_delete(kernel_map, k_entry);
}
vm_map_unlock(kernel_map);
return(KERN_SUCCESS);
}
kern_return_t
projected_buffer_collect(vm_map_t map)
{
vm_map_entry_t entry, next;
if (map == VM_MAP_NULL || map == kernel_map)
return(KERN_INVALID_ARGUMENT);
for (entry = vm_map_first_entry(map);
entry != vm_map_to_entry(map);
entry = next) {
next = entry->vme_next;
if (entry->projected_on != 0)
projected_buffer_deallocate(map, entry->vme_start, entry->vme_end);
}
return(KERN_SUCCESS);
}
boolean_t
projected_buffer_in_range(
vm_map_t map,
vm_offset_t start,
vm_offset_t end)
{
vm_map_entry_t entry;
if (map == VM_MAP_NULL || map == kernel_map)
return(FALSE);
if (!vm_map_lookup_entry(map, start, &entry))
entry = entry->vme_next;
while (entry != vm_map_to_entry(map) && entry->projected_on == 0 &&
entry->vme_start <= end) {
entry = entry->vme_next;
}
return(entry != vm_map_to_entry(map) && entry->vme_start <= end);
}
kern_return_t
kmem_alloc(
vm_map_t map,
vm_offset_t *addrp,
vm_size_t size)
{
vm_object_t object;
vm_map_entry_t entry;
vm_offset_t addr;
unsigned int attempts;
kern_return_t kr;
if (!addrp) {
return KERN_INVALID_ARGUMENT;
}
if (size > VM_MAX_KERNEL_ADDRESS || size == 0) {
return KERN_INVALID_ARGUMENT;
}
vm_size_t rounded_size = (size + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
if (rounded_size < size) {
printf("kmem_alloc: size overflow after rounding\n");
return KERN_INVALID_ARGUMENT;
}
size = rounded_size;
object = vm_object_allocate(size);
attempts = 0;
retry:
vm_map_lock(map);
kr = vm_map_find_entry(map, &addr, size, (vm_offset_t) 0,
VM_OBJECT_NULL, &entry);
if (kr != KERN_SUCCESS) {
vm_map_unlock(map);
if (attempts == 0) {
attempts++;
slab_collect();
goto retry;
}
printf_once("no more room for kmem_alloc in %p (%s)\n",
map, map->name);
vm_object_deallocate(object);
return kr;
}
entry->object.vm_object = object;
entry->offset = 0;
vm_map_unlock(map);
kmem_alloc_pages(object, 0,
addr, addr + size,
VM_PROT_DEFAULT);
*addrp = addr;
return KERN_SUCCESS;
}
kern_return_t
kmem_valloc(
vm_map_t map,
vm_offset_t *addrp,
vm_size_t size)
{
vm_map_entry_t entry;
vm_offset_t offset;
vm_offset_t addr;
unsigned int attempts;
kern_return_t kr;
size = round_page(size);
attempts = 0;
retry:
vm_map_lock(map);
kr = vm_map_find_entry(map, &addr, size, (vm_offset_t) 0,
kernel_object, &entry);
if (kr != KERN_SUCCESS) {
vm_map_unlock(map);
if (attempts == 0) {
attempts++;
slab_collect();
goto retry;
}
printf_once("no more room for kmem_valloc in %p (%s)\n",
map, map->name);
return kr;
}
offset = addr - VM_MIN_KERNEL_ADDRESS;
if (entry->object.vm_object == VM_OBJECT_NULL) {
vm_object_reference(kernel_object);
entry->object.vm_object = kernel_object;
entry->offset = offset;
}
vm_map_unlock(map);
*addrp = addr;
return KERN_SUCCESS;
}
kern_return_t
kmem_alloc_wired(
vm_map_t map,
vm_offset_t *addrp,
vm_size_t size)
{
vm_offset_t offset;
vm_offset_t addr;
kern_return_t kr;
kr = kmem_valloc(map, &addr, size);
if (kr != KERN_SUCCESS)
return kr;
offset = addr - VM_MIN_KERNEL_ADDRESS;
kmem_alloc_pages(kernel_object, offset,
addr, addr + size,
VM_PROT_DEFAULT);
*addrp = addr;
return KERN_SUCCESS;
}
kern_return_t
kmem_alloc_aligned(
vm_map_t map,
vm_offset_t *addrp,
vm_size_t size)
{
vm_map_entry_t entry;
vm_offset_t offset;
vm_offset_t addr;
unsigned int attempts;
kern_return_t kr;
if ((size & (size - 1)) != 0)
panic("kmem_alloc_aligned");
size = round_page(size);
attempts = 0;
retry:
vm_map_lock(map);
kr = vm_map_find_entry(map, &addr, size, size - 1,
kernel_object, &entry);
if (kr != KERN_SUCCESS) {
vm_map_unlock(map);
if (attempts == 0) {
attempts++;
slab_collect();
goto retry;
}
printf_once("no more room for kmem_alloc_aligned in %p (%s)\n",
map, map->name);
return kr;
}
offset = addr - VM_MIN_KERNEL_ADDRESS;
if (entry->object.vm_object == VM_OBJECT_NULL) {
vm_object_reference(kernel_object);
entry->object.vm_object = kernel_object;
entry->offset = offset;
}
vm_map_unlock(map);
kmem_alloc_pages(kernel_object, offset,
addr, addr + size,
VM_PROT_DEFAULT);
*addrp = addr;
return KERN_SUCCESS;
}
void*
kmem_map_aligned_table(
phys_addr_t phys_address,
vm_size_t size,
int mode)
{
vm_offset_t virt_addr;
kern_return_t ret;
phys_addr_t into_page = phys_address % PAGE_SIZE;
phys_addr_t nearest_page = phys_address - into_page;
size += into_page;
ret = kmem_alloc_wired(kernel_map, &virt_addr,
round_page(size));
if (ret != KERN_SUCCESS)
return NULL;
(void) pmap_map_bd(virt_addr, nearest_page,
nearest_page + round_page(size), mode);
return (void *) (virt_addr + into_page);
}
kern_return_t
kmem_alloc_pageable(
vm_map_t map,
vm_offset_t *addrp,
vm_size_t size)
{
vm_offset_t addr;
kern_return_t kr;
addr = vm_map_min(map);
kr = vm_map_enter(map, &addr, round_page(size),
(vm_offset_t) 0, TRUE,
VM_OBJECT_NULL, (vm_offset_t) 0, FALSE,
VM_PROT_DEFAULT, VM_PROT_ALL, VM_INHERIT_DEFAULT);
if (kr != KERN_SUCCESS) {
printf_once("no more room for kmem_alloc_pageable in %p (%s)\n",
map, map->name);
return kr;
}
*addrp = addr;
return KERN_SUCCESS;
}
void
kmem_free(
vm_map_t map,
vm_offset_t addr,
vm_size_t size)
{
kern_return_t kr;
kr = vm_map_remove(map, trunc_page(addr), round_page(addr + size));
if (kr != KERN_SUCCESS)
panic("kmem_free");
}
void
kmem_alloc_pages(
vm_object_t object,
vm_offset_t offset,
vm_offset_t start,
vm_offset_t end,
vm_prot_t protection)
{
pmap_pageable(kernel_pmap, start, end, FALSE);
while (start < end) {
vm_page_t mem;
vm_object_lock(object);
while ((mem = vm_page_alloc(object, offset))
== VM_PAGE_NULL) {
vm_object_unlock(object);
VM_PAGE_WAIT((void (*)()) 0);
vm_object_lock(object);
}
vm_page_lock_queues();
vm_page_wire(mem);
vm_page_unlock_queues();
vm_object_unlock(object);
PMAP_ENTER(kernel_pmap, start, mem,
protection, TRUE);
vm_object_lock(object);
PAGE_WAKEUP_DONE(mem);
vm_object_unlock(object);
start += PAGE_SIZE;
offset += PAGE_SIZE;
}
}
void
kmem_remap_pages(
vm_object_t object,
vm_offset_t offset,
vm_offset_t start,
vm_offset_t end,
vm_prot_t protection)
{
pmap_pageable(kernel_pmap, start, end, FALSE);
while (start < end) {
vm_page_t mem;
vm_object_lock(object);
if ((mem = vm_page_lookup(object, offset)) == VM_PAGE_NULL)
panic("kmem_remap_pages");
vm_page_lock_queues();
vm_page_wire(mem);
vm_page_unlock_queues();
vm_object_unlock(object);
PMAP_ENTER(kernel_pmap, start, mem,
protection, TRUE);
start += PAGE_SIZE;
offset += PAGE_SIZE;
}
}
void
kmem_submap(
vm_map_t map,
vm_map_t parent,
vm_offset_t *min,
vm_offset_t *max,
vm_size_t size)
{
vm_offset_t addr;
kern_return_t kr;
size = round_page(size);
vm_object_reference(vm_submap_object);
addr = vm_map_min(parent);
kr = vm_map_enter(parent, &addr, size,
(vm_offset_t) 0, TRUE,
vm_submap_object, (vm_offset_t) 0, FALSE,
VM_PROT_DEFAULT, VM_PROT_ALL, VM_INHERIT_DEFAULT);
if (kr != KERN_SUCCESS)
panic("kmem_submap");
pmap_reference(vm_map_pmap(parent));
vm_map_setup(map, vm_map_pmap(parent), addr, addr + size);
kr = vm_map_submap(parent, addr, addr + size, map);
if (kr != KERN_SUCCESS)
panic("kmem_submap");
*min = addr;
*max = addr + size;
}
void kmem_init(
vm_offset_t start,
vm_offset_t end)
{
vm_map_setup(kernel_map, pmap_kernel(), VM_MIN_KERNEL_ADDRESS, end);
if (start != VM_MIN_KERNEL_ADDRESS) {
kern_return_t rc;
vm_offset_t addr = VM_MIN_KERNEL_ADDRESS;
rc = vm_map_enter(kernel_map,
&addr, start - VM_MIN_KERNEL_ADDRESS,
(vm_offset_t) 0, TRUE,
VM_OBJECT_NULL, (vm_offset_t) 0, FALSE,
VM_PROT_DEFAULT, VM_PROT_ALL,
VM_INHERIT_DEFAULT);
if (rc)
panic("vm_map_enter failed (%d)\n", rc);
}
}
kern_return_t
kmem_io_map_copyout(
vm_map_t map,
vm_offset_t *addr,
vm_offset_t *alloc_addr,
vm_size_t *alloc_size,
vm_map_copy_t copy,
vm_size_t min_size)
{
vm_offset_t myaddr, offset;
vm_size_t mysize, copy_size;
kern_return_t ret;
vm_page_t *page_list;
vm_map_copy_t new_copy;
int i;
assert(copy->type == VM_MAP_COPY_PAGE_LIST);
assert(min_size != 0);
min_size += copy->offset - trunc_page(copy->offset);
min_size = round_page(min_size);
mysize = round_page(copy->offset + copy->size) -
trunc_page(copy->offset);
copy_size = ptoa(copy->cpy_npages);
if (mysize > copy_size && copy_size > min_size)
mysize = copy_size;
myaddr = vm_map_min(map);
ret = vm_map_enter(map, &myaddr, mysize,
(vm_offset_t) 0, TRUE,
VM_OBJECT_NULL, (vm_offset_t) 0, FALSE,
VM_PROT_DEFAULT, VM_PROT_ALL, VM_INHERIT_DEFAULT);
if (ret != KERN_SUCCESS)
return(ret);
pmap_pageable(vm_map_pmap(map), myaddr, myaddr + mysize, TRUE);
*addr = myaddr + (copy->offset - trunc_page(copy->offset));
*alloc_addr = myaddr;
*alloc_size = mysize;
offset = myaddr;
page_list = &copy->cpy_page_list[0];
while (TRUE) {
for ( i = 0; i < copy->cpy_npages; i++, offset += PAGE_SIZE) {
PMAP_ENTER(vm_map_pmap(map), offset, *page_list,
VM_PROT_READ, TRUE);
page_list++;
}
if (offset == (myaddr + mysize))
break;
vm_map_copy_invoke_extend_cont(copy, &new_copy, &ret);
if (ret != KERN_SUCCESS) {
kmem_io_map_deallocate(map, myaddr, mysize);
return(ret);
}
copy->cpy_cont = vm_map_copy_discard_cont;
copy->cpy_cont_args = (vm_map_copyin_args_t)new_copy;
copy = new_copy;
page_list = &copy->cpy_page_list[0];
}
return(ret);
}
void
kmem_io_map_deallocate(
vm_map_t map,
vm_offset_t addr,
vm_size_t size)
{
pmap_remove(vm_map_pmap(map), addr, addr + size);
vm_map_remove(map, addr, addr + size);
}
int copyinmap(
vm_map_t map,
char *fromaddr,
char *toaddr,
int length)
{
if (vm_map_pmap(map) == kernel_pmap) {
memcpy(toaddr, fromaddr, length);
return 0;
}
if (current_map() == map)
return copyin( fromaddr, toaddr, length);
return 1;
}
int copyoutmap(
vm_map_t map,
char *fromaddr,
char *toaddr,
int length)
{
if (vm_map_pmap(map) == kernel_pmap) {
memcpy(toaddr, fromaddr, length);
return 0;
}
if (current_map() == map)
return copyout(fromaddr, toaddr, length);
return 1;
}