#include <kern/printf.h>
#include <string.h>
#include <mach/vm_prot.h>
#include <kern/counters.h>
#include <kern/debug.h>
#include <kern/list.h>
#include <kern/sched_prim.h>
#include <kern/task.h>
#include <kern/thread.h>
#include <mach/vm_statistics.h>
#include <machine/vm_param.h>
#include <kern/xpr.h>
#include <kern/slab.h>
#include <vm/pmap.h>
#include <vm/vm_map.h>
#include <vm/vm_page.h>
#include <vm/vm_pageout.h>
#include <vm/vm_kern.h>
#include <vm/vm_resident.h>
#if	MACH_VM_DEBUG
#include <mach/kern_return.h>
#include <mach_debug/hash_info.h>
#include <vm/vm_user.h>
#endif
#if	MACH_KDB
#include <ddb/db_output.h>
#include <vm/vm_print.h>
#endif
vm_offset_t virtual_space_start;
vm_offset_t virtual_space_end;
typedef struct {
decl_simple_lock_data(,lock)
vm_page_t pages;
} vm_page_bucket_t;
vm_page_bucket_t *vm_page_buckets;
unsigned long	vm_page_bucket_count = 0;
unsigned long	vm_page_hash_mask;
static struct list	vm_page_queue_fictitious;
def_simple_lock_data(,vm_page_queue_free_lock)
int		vm_page_fictitious_count;
int		vm_object_external_count;
int		vm_object_external_pages;
struct kmem_cache	vm_page_cache;
phys_addr_t vm_page_fictitious_addr = (phys_addr_t) -1;
def_simple_lock_data(,vm_page_queue_lock)
int	vm_page_active_count;
int	vm_page_inactive_count;
int	vm_page_wire_count;
int	vm_page_laundry_count = 0;
int	vm_page_external_laundry_count = 0;
boolean_t vm_page_deactivate_behind = TRUE;
boolean_t vm_page_deactivate_hint = TRUE;
boolean_t vm_page_readahead_enabled = TRUE;
int vm_page_readahead_max = 8;
int vm_page_readahead_min = 2;
static void vm_page_readahead_trigger(vm_object_t object, vm_offset_t offset);
void vm_page_bootstrap(
vm_offset_t *startp,
vm_offset_t *endp)
{
int i;
simple_lock_init(&vm_page_queue_free_lock);
simple_lock_init(&vm_page_queue_lock);
list_init(&vm_page_queue_fictitious);
if (vm_page_bucket_count == 0) {
unsigned long npages = vm_page_table_size();
vm_page_bucket_count = 1;
while (vm_page_bucket_count < npages)
vm_page_bucket_count <<= 1;
}
vm_page_hash_mask = vm_page_bucket_count - 1;
if (vm_page_hash_mask & vm_page_bucket_count)
printf("vm_page_bootstrap: WARNING -- strange page hash\n");
vm_page_buckets = (vm_page_bucket_t *)
pmap_steal_memory(vm_page_bucket_count *
sizeof(vm_page_bucket_t));
for (i = 0; i < vm_page_bucket_count; i++) {
vm_page_bucket_t *bucket = &vm_page_buckets[i];
bucket->pages = VM_PAGE_NULL;
simple_lock_init(&bucket->lock);
}
vm_page_setup();
virtual_space_start = round_page(virtual_space_start);
virtual_space_end = trunc_page(virtual_space_end);
*startp = virtual_space_start;
*endp = virtual_space_end;
}
#ifndef	MACHINE_PAGES
vm_offset_t pmap_steal_memory(
vm_size_t size)
{
vm_offset_t addr, vaddr;
phys_addr_t paddr;
size = round_page(size);
if (virtual_space_start == virtual_space_end) {
pmap_virtual_space(&virtual_space_start, &virtual_space_end);
virtual_space_start = round_page(virtual_space_start);
virtual_space_end = trunc_page(virtual_space_end);
}
addr = virtual_space_start;
virtual_space_start += size;
for (vaddr = round_page(addr);
vaddr < addr + size;
vaddr += PAGE_SIZE) {
paddr = vm_page_bootalloc(PAGE_SIZE);
pmap_enter(kernel_pmap, vaddr, paddr,
VM_PROT_READ|VM_PROT_WRITE, FALSE);
}
return addr;
}
#endif
void		vm_page_module_init(void)
{
kmem_cache_init(&vm_page_cache, "vm_page", sizeof(struct vm_page), 0,
NULL, 0);
}
#define vm_page_hash(object, offset) \
(((unsigned int)(vm_offset_t)object + (unsigned int)atop(offset)) \
& vm_page_hash_mask)
void vm_page_insert(
vm_page_t	mem,
vm_object_t	object,
vm_offset_t	offset)
{
vm_page_bucket_t *bucket;
assert(vm_page_locked_queues());
assert(vm_object_lock_taken(object));
VM_PAGE_CHECK(mem);
assert(!mem->active && !mem->inactive);
assert(!mem->external);
if (!object->internal) {
mem->external = TRUE;
vm_object_external_pages++;
}
if (mem->tabled)
panic("vm_page_insert");
mem->object = object;
mem->offset = offset;
bucket = &vm_page_buckets[vm_page_hash(object, offset)];
simple_lock(&bucket->lock);
mem->next = bucket->pages;
bucket->pages = mem;
simple_unlock(&bucket->lock);
queue_enter(&object->memq, mem, vm_page_t, listq);
mem->tabled = TRUE;
vm_object_increment_resident_count(object);
if (vm_page_deactivate_behind &&
(offset == object->last_alloc + PAGE_SIZE)) {
vm_page_t	last_mem;
last_mem = vm_page_lookup(object, object->last_alloc);
if ((last_mem != VM_PAGE_NULL) && !last_mem->busy)
vm_page_deactivate(last_mem);
}
object->last_alloc = offset;
vm_page_readahead_trigger(object, offset);
}
static void
vm_page_readahead_trigger(vm_object_t object, vm_offset_t offset)
{
vm_offset_t readahead_offset;
vm_page_t page;
unsigned int i, window_size;
if (!vm_page_readahead_enabled || object == VM_OBJECT_NULL)
return;
if (offset == object->readahead_next) {
object->readahead_count++;
if (object->readahead_count >= 3) {
object->readahead_window = MIN(object->readahead_window * 2,
vm_page_readahead_max);
}
} else {
object->readahead_count = 1;
object->readahead_window = vm_page_readahead_min;
}
object->readahead_next = offset + PAGE_SIZE;
if (object->readahead_count < 2)
return;
window_size = MIN(object->readahead_window, vm_page_readahead_max);
for (i = 1; i <= window_size; i++) {
readahead_offset = offset + (i * PAGE_SIZE);
if (readahead_offset >= object->size)
break;
page = vm_page_lookup(object, readahead_offset);
if (page != VM_PAGE_NULL)
continue;
}
}
void vm_page_replace(
vm_page_t	mem,
vm_object_t	object,
vm_offset_t	offset)
{
vm_page_bucket_t *bucket;
assert(vm_page_locked_queues());
assert(vm_object_lock_taken(object));
VM_PAGE_CHECK(mem);
assert(!mem->active && !mem->inactive);
assert(!mem->external);
if (!object->internal) {
mem->external = TRUE;
vm_object_external_pages++;
}
if (mem->tabled)
panic("vm_page_replace");
mem->object = object;
mem->offset = offset;
bucket = &vm_page_buckets[vm_page_hash(object, offset)];
simple_lock(&bucket->lock);
if (bucket->pages) {
vm_page_t *mp = &bucket->pages;
vm_page_t m = *mp;
do {
if (m->object == object && m->offset == offset) {
*mp = m->next;
queue_remove(&object->memq, m, vm_page_t,
listq);
m->tabled = FALSE;
vm_object_decrement_resident_count(object);
VM_PAGE_QUEUES_REMOVE(m);
if (m->external) {
m->external = FALSE;
vm_object_external_pages--;
}
vm_page_free(m);
break;
}
mp = &m->next;
} while ((m = *mp) != 0);
mem->next = bucket->pages;
} else {
mem->next = VM_PAGE_NULL;
}
bucket->pages = mem;
simple_unlock(&bucket->lock);
queue_enter(&object->memq, mem, vm_page_t, listq);
mem->tabled = TRUE;
vm_object_increment_resident_count(object);
}
void vm_page_remove(
vm_page_t		mem)
{
vm_page_bucket_t	*bucket;
vm_page_t		this;
assert(mem->tabled);
assert(vm_page_locked_queues());
assert(vm_object_lock_taken(mem->object));
VM_PAGE_CHECK(mem);
bucket = &vm_page_buckets[vm_page_hash(mem->object, mem->offset)];
simple_lock(&bucket->lock);
if ((this = bucket->pages) == mem) {
bucket->pages = mem->next;
} else {
vm_page_t	*prev;
for (prev = &this->next;
(this = *prev) != mem;
prev = &this->next)
continue;
*prev = this->next;
}
simple_unlock(&bucket->lock);
queue_remove(&mem->object->memq, mem, vm_page_t, listq);
vm_object_decrement_resident_count(mem->object);
mem->tabled = FALSE;
VM_PAGE_QUEUES_REMOVE(mem);
if (mem->external) {
mem->external = FALSE;
vm_object_external_pages--;
}
}
vm_page_t vm_page_lookup(
vm_object_t		object,
vm_offset_t		offset)
{
vm_page_t		mem;
vm_page_bucket_t 	*bucket;
assert(vm_object_lock_taken(object));
bucket = &vm_page_buckets[vm_page_hash(object, offset)];
simple_lock(&bucket->lock);
for (mem = bucket->pages; mem != VM_PAGE_NULL; mem = mem->next) {
VM_PAGE_CHECK(mem);
if ((mem->object == object) && (mem->offset == offset))
break;
}
simple_unlock(&bucket->lock);
return mem;
}
void vm_page_rename(
vm_page_t	mem,
vm_object_t	new_object,
vm_offset_t	new_offset)
{
assert(vm_object_lock_taken(new_object));
vm_page_lock_queues();
vm_page_remove(mem);
vm_page_insert(mem, new_object, new_offset);
vm_page_unlock_queues();
}
static void vm_page_init_template(vm_page_t m)
{
m->object = VM_OBJECT_NULL;
m->offset = 0;
m->wire_count = 0;
m->inactive = FALSE;
m->active = FALSE;
m->laundry = FALSE;
m->external_laundry = FALSE;
m->free = FALSE;
m->external = FALSE;
m->busy = TRUE;
m->wanted = FALSE;
m->tabled = FALSE;
m->fictitious = FALSE;
m->private = FALSE;
m->absent = FALSE;
m->error = FALSE;
m->dirty = FALSE;
m->precious = FALSE;
m->reference = FALSE;
m->page_lock = VM_PROT_NONE;
m->unlock_request = VM_PROT_NONE;
m->access_frequency = 0;
m->aging_time = 0;
}
void vm_page_init(
vm_page_t	mem)
{
vm_page_init_template(mem);
}
vm_page_t vm_page_grab_fictitious(void)
{
vm_page_t m;
simple_lock(&vm_page_queue_free_lock);
if (list_empty(&vm_page_queue_fictitious)) {
m = VM_PAGE_NULL;
} else {
m = list_first_entry(&vm_page_queue_fictitious,
struct vm_page, node);
assert(m->fictitious);
list_remove(&m->node);
m->free = FALSE;
vm_page_fictitious_count--;
}
simple_unlock(&vm_page_queue_free_lock);
return m;
}
static void vm_page_release_fictitious(
vm_page_t m)
{
simple_lock(&vm_page_queue_free_lock);
if (m->free)
panic("vm_page_release_fictitious");
m->free = TRUE;
list_insert_head(&vm_page_queue_fictitious, &m->node);
vm_page_fictitious_count++;
simple_unlock(&vm_page_queue_free_lock);
}
int vm_page_fictitious_quantum = 5;
void vm_page_more_fictitious(void)
{
vm_page_t m;
int i;
for (i = 0; i < vm_page_fictitious_quantum; i++) {
m = (vm_page_t) kmem_cache_alloc(&vm_page_cache);
if (m == VM_PAGE_NULL)
panic("vm_page_more_fictitious");
vm_page_init(m);
m->phys_addr = vm_page_fictitious_addr;
m->fictitious = TRUE;
vm_page_release_fictitious(m);
}
}
boolean_t vm_page_convert(struct vm_page **mp)
{
struct vm_page *real_m, *fict_m;
vm_object_t object;
vm_offset_t offset;
fict_m = *mp;
assert(fict_m->fictitious);
assert(fict_m->phys_addr == vm_page_fictitious_addr);
assert(!fict_m->active);
assert(!fict_m->inactive);
real_m = vm_page_grab(VM_PAGE_HIGHMEM);
if (real_m == VM_PAGE_NULL)
return FALSE;
object = fict_m->object;
assert(vm_object_lock_taken(object));
offset = fict_m->offset;
vm_page_lock_queues();
vm_page_remove(fict_m);
memcpy(&real_m->vm_page_header,
&fict_m->vm_page_header,
VM_PAGE_BODY_SIZE);
real_m->fictitious = FALSE;
vm_page_insert(real_m, object, offset);
vm_page_unlock_queues();
assert(real_m->phys_addr != vm_page_fictitious_addr);
assert(fict_m->fictitious);
assert(fict_m->phys_addr == vm_page_fictitious_addr);
vm_page_release_fictitious(fict_m);
*mp = real_m;
return TRUE;
}
vm_page_t vm_page_grab(unsigned flags)
{
unsigned selector;
vm_page_t	mem;
if (flags & VM_PAGE_HIGHMEM)
selector = VM_PAGE_SEL_HIGHMEM;
#if defined(VM_PAGE_DMA32_LIMIT) && VM_PAGE_DMA32_LIMIT > VM_PAGE_DIRECTMAP_LIMIT
else if (flags & VM_PAGE_DMA32)
selector = VM_PAGE_SEL_DMA32;
#endif
else if (flags & VM_PAGE_DIRECTMAP)
selector = VM_PAGE_SEL_DIRECTMAP;
#if defined(VM_PAGE_DMA32_LIMIT) && VM_PAGE_DMA32_LIMIT <= VM_PAGE_DIRECTMAP_LIMIT
else if (flags & VM_PAGE_DMA32)
selector = VM_PAGE_SEL_DMA32;
#endif
else
selector = VM_PAGE_SEL_DMA;
simple_lock(&vm_page_queue_free_lock);
mem = vm_page_alloc_pa(0, selector, VM_PT_KERNEL);
if (mem == NULL) {
simple_unlock(&vm_page_queue_free_lock);
return NULL;
}
mem->free = FALSE;
simple_unlock(&vm_page_queue_free_lock);
return mem;
}
phys_addr_t vm_page_grab_phys_addr(void)
{
vm_page_t p = vm_page_grab(VM_PAGE_DIRECTMAP);
if (p == VM_PAGE_NULL)
return -1;
else
return p->phys_addr;
}
void vm_page_release(
vm_page_t	mem,
boolean_t 	laundry,
boolean_t 	external_laundry)
{
simple_lock(&vm_page_queue_free_lock);
if (mem->free)
panic("vm_page_release");
mem->free = TRUE;
vm_page_free_pa(mem, 0);
if (laundry) {
vm_page_laundry_count--;
if (vm_page_laundry_count == 0) {
vm_pageout_resume();
}
}
if (external_laundry) {
if (vm_page_external_laundry_count > 0) {
vm_page_external_laundry_count--;
if (vm_page_external_laundry_count == 0) {
vm_pageout_resume();
}
}
}
simple_unlock(&vm_page_queue_free_lock);
}
vm_page_t vm_page_grab_contig(
vm_size_t size,
unsigned int selector)
{
unsigned int i, order, nr_pages;
vm_page_t mem;
order = vm_page_order(size);
nr_pages = 1 << order;
simple_lock(&vm_page_queue_free_lock);
mem = vm_page_alloc_pa(order, selector, VM_PT_KERNEL);
if (mem == NULL) {
simple_unlock(&vm_page_queue_free_lock);
return NULL;
}
for (i = 0; i < nr_pages; i++) {
mem[i].free = FALSE;
}
simple_unlock(&vm_page_queue_free_lock);
return mem;
}
void vm_page_free_contig(vm_page_t mem, vm_size_t size)
{
unsigned int i, order, nr_pages;
order = vm_page_order(size);
nr_pages = 1 << order;
simple_lock(&vm_page_queue_free_lock);
for (i = 0; i < nr_pages; i++) {
if (mem[i].free)
panic("vm_page_free_contig");
mem[i].free = TRUE;
}
vm_page_free_pa(mem, order);
simple_unlock(&vm_page_queue_free_lock);
}
vm_page_t vm_page_alloc(
vm_object_t	object,
vm_offset_t	offset)
{
vm_page_t	mem;
assert(vm_object_lock_taken(object));
mem = vm_page_grab(VM_PAGE_HIGHMEM);
if (mem == VM_PAGE_NULL)
return VM_PAGE_NULL;
vm_page_lock_queues();
vm_page_insert(mem, object, offset);
vm_page_unlock_queues();
return mem;
}
void vm_page_free(
vm_page_t	mem)
{
if (mem->free)
panic("vm_page_free");
if (mem->tabled) {
vm_page_remove(mem);
}
assert(vm_page_locked_queues());
if (mem->absent)
assert(vm_object_lock_taken(mem->object));
assert(!mem->active && !mem->inactive);
if (mem->wire_count != 0) {
if (!mem->private && !mem->fictitious)
vm_page_wire_count--;
mem->wire_count = 0;
}
PAGE_WAKEUP_DONE(mem);
if (mem->absent)
vm_object_absent_release(mem->object);
if (mem->private || mem->fictitious) {
vm_page_init(mem);
mem->phys_addr = vm_page_fictitious_addr;
mem->fictitious = TRUE;
vm_page_release_fictitious(mem);
} else {
boolean_t laundry = mem->laundry;
boolean_t external_laundry = mem->external_laundry;
vm_page_init(mem);
vm_page_release(mem, laundry, external_laundry);
}
}
void vm_page_zero_fill(
vm_page_t	m)
{
VM_PAGE_CHECK(m);
pmap_zero_page(m->phys_addr);
}
void vm_page_copy(
vm_page_t	src_m,
vm_page_t	dest_m)
{
VM_PAGE_CHECK(src_m);
VM_PAGE_CHECK(dest_m);
pmap_copy_page(src_m->phys_addr, dest_m->phys_addr);
}
#if	MACH_VM_DEBUG
unsigned int
vm_page_info(
hash_info_bucket_t *info,
unsigned int	count)
{
int i;
if (vm_page_bucket_count < count)
count = vm_page_bucket_count;
for (i = 0; i < count; i++) {
vm_page_bucket_t *bucket = &vm_page_buckets[i];
unsigned int bucket_count = 0;
vm_page_t m;
simple_lock(&bucket->lock);
for (m = bucket->pages; m != VM_PAGE_NULL; m = m->next)
bucket_count++;
simple_unlock(&bucket->lock);
info[i].hib_count = bucket_count;
}
return vm_page_bucket_count;
}
#endif
#if	MACH_KDB
#define	printf	kdbprintf
void		vm_page_print(const vm_page_t	p)
{
iprintf("Page 0x%X: object 0x%X,", (vm_offset_t) p, (vm_offset_t) p->object);
printf(" offset 0x%X", p->offset);
printf(" wire_count %d,", p->wire_count);
printf(" %s",
(p->active ? "active" : (p->inactive ? "inactive" : "loose")));
printf("%s",
(p->free ? " free" : ""));
printf("%s ",
(p->laundry ? " laundry" : ""));
printf("%s",
(p->dirty ? "dirty" : "clean"));
printf("%s",
(p->busy ? " busy" : ""));
printf("%s",
(p->absent ? " absent" : ""));
printf("%s",
(p->error ? " error" : ""));
printf("%s",
(p->fictitious ? " fictitious" : ""));
printf("%s",
(p->private ? " private" : ""));
printf("%s",
(p->wanted ? " wanted" : ""));
printf("%s,",
(p->tabled ? "" : "not_tabled"));
printf("phys_addr = 0x%X, lock = 0x%X, unlock_request = 0x%X\n",
p->phys_addr,
(vm_offset_t) p->page_lock,
(vm_offset_t) p->unlock_request);
}
#endif