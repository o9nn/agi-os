#include <kern/printf.h>
#include <mach/kern_return.h>
#include <mach/port.h>
#include <mach/vm_attributes.h>
#include <mach/vm_param.h>
#include <mach/vm_wire.h>
#include <kern/assert.h>
#include <kern/debug.h>
#include <kern/kalloc.h>
#include <kern/mach.server.h>
#include <kern/list.h>
#include <kern/rbtree.h>
#include <kern/slab.h>
#include <kern/mach4.server.h>
#include <vm/pmap.h>
#include <vm/vm_fault.h>
#include <vm/vm_map.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <vm/vm_resident.h>
#include <vm/vm_kern.h>
#include <vm/memory_object_proxy.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_kmsg.h>
#include <string.h>
#if MACH_KDB
#include <ddb/db_output.h>
#include <vm/vm_print.h>
#endif
#define vm_map_entry_copy(NEW,OLD) \
MACRO_BEGIN \
*(NEW) = *(OLD); \
(NEW)->is_shared = FALSE; \
(NEW)->needs_wakeup = FALSE; \
(NEW)->in_transition = FALSE; \
(NEW)->wired_count = 0; \
(NEW)->wired_access = VM_PROT_NONE; \
MACRO_END
#define vm_map_entry_copy_full(NEW,OLD) (*(NEW) = *(OLD))
struct kmem_cache vm_map_cache;
struct kmem_cache vm_map_entry_cache;
struct kmem_cache vm_map_copy_cache;
static struct vm_object vm_submap_object_store;
vm_object_t vm_submap_object = &vm_submap_object_store;
static vm_offset_t
vm_map_get_simple_entropy(void)
{
extern unsigned long elapsed_ticks;
static unsigned long entropy_counter = 0;
vm_offset_t entropy;
entropy_counter++;
entropy = (vm_offset_t)(elapsed_ticks ^ (entropy_counter << 8) ^
((vm_offset_t)&entropy_counter >> 3));
return entropy;
}
vm_offset_t
vm_map_get_aslr_entropy(vm_map_t map, vm_size_t size)
{
vm_offset_t entropy;
vm_offset_t max_entropy_offset;
unsigned int entropy_bits;
if (!map || !map->aslr_enabled || size == 0) {
return 0;
}
entropy_bits = map->aslr_entropy_bits;
if (entropy_bits == 0) {
entropy_bits = VM_MAP_ASLR_DEFAULT_ENTROPY_BITS;
}
if (vm_map_memory_pressure(map)) {
entropy_bits = entropy_bits / 2;
if (entropy_bits < VM_MAP_ASLR_MIN_ENTROPY_BITS) {
entropy_bits = VM_MAP_ASLR_MIN_ENTROPY_BITS;
}
}
max_entropy_offset = (1UL << entropy_bits) * PAGE_SIZE;
if (max_entropy_offset > (map->max_offset - map->min_offset) / 4) {
max_entropy_offset = (map->max_offset - map->min_offset) / 4;
max_entropy_offset &= ~(PAGE_SIZE - 1);
}
if (max_entropy_offset == 0) {
return 0;
}
entropy = vm_map_get_simple_entropy();
entropy = (entropy % (max_entropy_offset / PAGE_SIZE)) * PAGE_SIZE;
return entropy;
}
void
vm_map_set_aslr(vm_map_t map, boolean_t enabled, unsigned int entropy_bits)
{
if (!map) {
return;
}
if (entropy_bits > VM_MAP_ASLR_MAX_ENTROPY_BITS) {
entropy_bits = VM_MAP_ASLR_MAX_ENTROPY_BITS;
} else if (entropy_bits < VM_MAP_ASLR_MIN_ENTROPY_BITS && entropy_bits != 0) {
entropy_bits = VM_MAP_ASLR_MIN_ENTROPY_BITS;
}
vm_map_lock(map);
map->aslr_enabled = enabled;
map->aslr_entropy_bits = entropy_bits;
vm_map_unlock(map);
}
vm_offset_t
vm_map_optimize_placement(vm_map_t map, vm_size_t size, vm_offset_t suggested_addr)
{
vm_offset_t optimized_addr = suggested_addr;
if (!map || size == 0) {
return suggested_addr;
}
if (size >= VM_MAP_LARGE_PAGE_SIZE) {
vm_offset_t large_page_mask = VM_MAP_LARGE_PAGE_SIZE - 1;
optimized_addr = (suggested_addr + large_page_mask) & ~large_page_mask;
if (optimized_addr < map->min_offset ||
(optimized_addr + size) > map->max_offset) {
optimized_addr = suggested_addr;
}
}
if (map->prefer_high_addr && size >= VM_MAP_PREFER_HIGH_THRESHOLD) {
vm_offset_t high_region_start = map->max_offset - (map->max_offset - map->min_offset) / 4;
if (optimized_addr < high_region_start) {
vm_offset_t high_addr = high_region_start;
if (size >= VM_MAP_LARGE_PAGE_SIZE) {
vm_offset_t large_page_mask = VM_MAP_LARGE_PAGE_SIZE - 1;
high_addr = (high_addr + large_page_mask) & ~large_page_mask;
}
if ((high_addr + size) <= map->max_offset) {
optimized_addr = high_addr;
}
}
}
return optimized_addr;
}
boolean_t
vm_map_memory_pressure(vm_map_t map)
{
vm_size_t used_space;
vm_size_t total_space;
if (!map) {
return FALSE;
}
used_space = map->size;
total_space = map->max_offset - map->min_offset;
return (used_space > (total_space * 3) / 4);
}
void vm_map_init(void)
{
kmem_cache_init(&vm_map_cache, "vm_map", sizeof(struct vm_map), 0,
NULL, 0);
kmem_cache_init(&vm_map_entry_cache, "vm_map_entry",
sizeof(struct vm_map_entry), 0, NULL,
KMEM_CACHE_NOOFFSLAB | KMEM_CACHE_PHYSMEM);
kmem_cache_init(&vm_map_copy_cache, "vm_map_copy",
sizeof(struct vm_map_copy), 0, NULL, 0);
}
void vm_map_setup(
vm_map_t map,
pmap_t pmap,
vm_offset_t min,
vm_offset_t max)
{
vm_map_first_entry(map) = vm_map_to_entry(map);
vm_map_last_entry(map) = vm_map_to_entry(map);
map->hdr.nentries = 0;
rbtree_init(&map->hdr.tree);
rbtree_init(&map->hdr.gap_tree);
map->size = 0;
map->size_wired = 0;
map->ref_count = 1;
map->pmap = pmap;
map->min_offset = min;
map->max_offset = max;
map->wiring_required = FALSE;
map->wait_for_space = FALSE;
map->aslr_enabled = TRUE;
map->prefer_high_addr = FALSE;
map->aslr_entropy_bits = VM_MAP_ASLR_DEFAULT_ENTROPY_BITS;
map->first_free = vm_map_to_entry(map);
map->hint = vm_map_to_entry(map);
map->name = NULL;
vm_map_lock_init(map);
simple_lock_init(&map->ref_lock);
simple_lock_init(&map->hint_lock);
}
vm_map_t vm_map_create(
pmap_t pmap,
vm_offset_t min,
vm_offset_t max)
{
vm_map_t result;
result = (vm_map_t) kmem_cache_alloc(&vm_map_cache);
if (result == VM_MAP_NULL)
return VM_MAP_NULL;
vm_map_setup(result, pmap, min, max);
return(result);
}
void vm_map_lock(struct vm_map *map)
{
lock_write(&map->lock);
if (current_thread()) {
current_thread()->vm_privilege++;
assert(current_thread()->vm_privilege != 0);
}
map->timestamp++;
}
void vm_map_unlock(struct vm_map *map)
{
if (current_thread()) {
current_thread()->vm_privilege--;
}
lock_write_done(&map->lock);
}
#define vm_map_entry_create(map) \
_vm_map_entry_create(&(map)->hdr)
#define vm_map_copy_entry_create(copy) \
_vm_map_entry_create(&(copy)->cpy_hdr)
static vm_map_entry_t
_vm_map_entry_create(const struct vm_map_header *map_header)
{
vm_map_entry_t entry;
entry = (vm_map_entry_t) kmem_cache_alloc(&vm_map_entry_cache);
if (entry == VM_MAP_ENTRY_NULL)
panic("vm_map_entry_create");
return(entry);
}
#define vm_map_entry_dispose(map, entry) \
_vm_map_entry_dispose(&(map)->hdr, (entry))
#define vm_map_copy_entry_dispose(map, entry) \
_vm_map_entry_dispose(&(copy)->cpy_hdr, (entry))
static void
_vm_map_entry_dispose(const struct vm_map_header *map_header,
vm_map_entry_t entry)
{
(void)map_header;
kmem_cache_free(&vm_map_entry_cache, (vm_offset_t) entry);
}
static inline vm_map_entry_t
vm_map_entry_tree_next(vm_map_entry_t entry, struct vm_map_header *hdr)
{
struct rbtree_node *next_node;
next_node = rbtree_next(&entry->tree_node);
if (next_node != NULL) {
return rbtree_entry(next_node, struct vm_map_entry, tree_node);
} else {
return (vm_map_entry_t)&hdr->links;
}
}
static inline int vm_map_entry_cmp_lookup(vm_offset_t addr,
const struct rbtree_node *node)
{
struct vm_map_entry *entry;
entry = rbtree_entry(node, struct vm_map_entry, tree_node);
if (addr < entry->vme_start)
return -1;
else if (addr < entry->vme_end)
return 0;
else
return 1;
}
static inline int vm_map_entry_cmp_insert(const struct rbtree_node *a,
const struct rbtree_node *b)
{
struct vm_map_entry *entry;
entry = rbtree_entry(a, struct vm_map_entry, tree_node);
return vm_map_entry_cmp_lookup(entry->vme_start, b);
}
static inline int vm_map_entry_gap_cmp_lookup(vm_size_t gap_size,
const struct rbtree_node *node)
{
struct vm_map_entry *entry;
entry = rbtree_entry(node, struct vm_map_entry, gap_node);
if (gap_size < entry->gap_size)
return -1;
else if (gap_size == entry->gap_size)
return 0;
else
return 1;
}
static inline int vm_map_entry_gap_cmp_insert(const struct rbtree_node *a,
const struct rbtree_node *b)
{
struct vm_map_entry *entry;
entry = rbtree_entry(a, struct vm_map_entry, gap_node);
return vm_map_entry_gap_cmp_lookup(entry->gap_size, b);
}
static int
vm_map_gap_valid(struct vm_map_header *hdr, struct vm_map_entry *entry)
{
return entry != (struct vm_map_entry *)&hdr->links;
}
static void
vm_map_gap_compute(struct vm_map_header *hdr, struct vm_map_entry *entry)
{
struct vm_map_entry *next;
next = entry->vme_next;
if (vm_map_gap_valid(hdr, next)) {
entry->gap_size = next->vme_start - entry->vme_end;
} else {
entry->gap_size = hdr->vme_end - entry->vme_end;
}
}
static void
vm_map_gap_insert_single(struct vm_map_header *hdr, struct vm_map_entry *entry)
{
struct vm_map_entry *tmp;
struct rbtree_node *node;
unsigned long slot;
if (!vm_map_gap_valid(hdr, entry)) {
return;
}
vm_map_gap_compute(hdr, entry);
if (entry->gap_size == 0) {
return;
}
node = rbtree_lookup_slot(&hdr->gap_tree, entry->gap_size,
vm_map_entry_gap_cmp_lookup, slot);
if (node == NULL) {
rbtree_insert_slot(&hdr->gap_tree, slot, &entry->gap_node);
list_init(&entry->gap_list);
entry->in_gap_tree = 1;
} else {
tmp = rbtree_entry(node, struct vm_map_entry, gap_node);
list_insert_tail(&tmp->gap_list, &entry->gap_list);
entry->in_gap_tree = 0;
}
}
static void
vm_map_gap_remove_single(struct vm_map_header *hdr, struct vm_map_entry *entry)
{
struct vm_map_entry *tmp;
if (!vm_map_gap_valid(hdr, entry)) {
return;
}
if (entry->gap_size == 0) {
return;
}
if (!entry->in_gap_tree) {
list_remove(&entry->gap_list);
return;
}
rbtree_remove(&hdr->gap_tree, &entry->gap_node);
if (list_empty(&entry->gap_list)) {
return;
}
tmp = list_first_entry(&entry->gap_list, struct vm_map_entry, gap_list);
assert(tmp->gap_size == entry->gap_size);
list_remove(&tmp->gap_list);
list_set_head(&tmp->gap_list, &entry->gap_list);
assert(!tmp->in_gap_tree);
rbtree_insert(&hdr->gap_tree, &tmp->gap_node,
vm_map_entry_gap_cmp_insert);
tmp->in_gap_tree = 1;
}
static void
vm_map_gap_update(struct vm_map_header *hdr, struct vm_map_entry *entry)
{
vm_map_gap_remove_single(hdr, entry);
vm_map_gap_insert_single(hdr, entry);
}
static void
vm_map_gap_insert(struct vm_map_header *hdr, struct vm_map_entry *entry)
{
vm_map_gap_remove_single(hdr, entry->vme_prev);
vm_map_gap_insert_single(hdr, entry->vme_prev);
vm_map_gap_insert_single(hdr, entry);
}
static void
vm_map_gap_remove(struct vm_map_header *hdr, struct vm_map_entry *entry)
{
vm_map_gap_remove_single(hdr, entry);
vm_map_gap_remove_single(hdr, entry->vme_prev);
vm_map_gap_insert_single(hdr, entry->vme_prev);
}
#define vm_map_entry_link(map, after_where, entry) \
_vm_map_entry_link(&(map)->hdr, after_where, entry, 1)
#define vm_map_copy_entry_link(copy, after_where, entry) \
_vm_map_entry_link(&(copy)->cpy_hdr, after_where, entry, 0)
#define _vm_map_entry_link(hdr, after_where, entry, link_gap) \
MACRO_BEGIN \
(hdr)->nentries++; \
(entry)->vme_prev = (after_where); \
(entry)->vme_next = (after_where)->vme_next; \
(entry)->vme_prev->vme_next = \
(entry)->vme_next->vme_prev = (entry); \
rbtree_insert(&(hdr)->tree, &(entry)->tree_node, \
vm_map_entry_cmp_insert); \
if (link_gap) \
vm_map_gap_insert((hdr), (entry)); \
MACRO_END
#define vm_map_entry_unlink(map, entry) \
_vm_map_entry_unlink(&(map)->hdr, entry, 1)
#define vm_map_copy_entry_unlink(copy, entry) \
_vm_map_entry_unlink(&(copy)->cpy_hdr, entry, 0)
#define _vm_map_entry_unlink(hdr, entry, unlink_gap) \
MACRO_BEGIN \
(hdr)->nentries--; \
(entry)->vme_next->vme_prev = (entry)->vme_prev; \
(entry)->vme_prev->vme_next = (entry)->vme_next; \
rbtree_remove(&(hdr)->tree, &(entry)->tree_node); \
if (unlink_gap) \
vm_map_gap_remove((hdr), (entry)); \
MACRO_END
void vm_map_reference(vm_map_t map)
{
if (map == VM_MAP_NULL)
return;
simple_lock(&map->ref_lock);
map->ref_count++;
simple_unlock(&map->ref_lock);
}
void vm_map_deallocate(vm_map_t map)
{
int c;
if (map == VM_MAP_NULL)
return;
simple_lock(&map->ref_lock);
c = --map->ref_count;
simple_unlock(&map->ref_lock);
if (c > 0) {
return;
}
projected_buffer_collect(map);
(void) vm_map_delete(map, map->min_offset, map->max_offset);
pmap_destroy(map->pmap);
kmem_cache_free(&vm_map_cache, (vm_offset_t) map);
}
#define SAVE_HINT(map,value) \
MACRO_BEGIN \
simple_lock(&(map)->hint_lock); \
(map)->hint = (value); \
simple_unlock(&(map)->hint_lock); \
MACRO_END
boolean_t vm_map_lookup_entry(
vm_map_t map,
vm_offset_t address,
vm_map_entry_t *entry)
{
struct rbtree_node *node;
vm_map_entry_t hint;
simple_lock(&map->hint_lock);
hint = map->hint;
simple_unlock(&map->hint_lock);
if ((hint != vm_map_to_entry(map)) && (address >= hint->vme_start)) {
if (address < hint->vme_end) {
*entry = hint;
return(TRUE);
} else {
vm_map_entry_t next = hint->vme_next;
if ((next == vm_map_to_entry(map))
|| (address < next->vme_start)) {
*entry = hint;
return(FALSE);
}
}
}
node = rbtree_lookup_nearest(&map->hdr.tree, address,
vm_map_entry_cmp_lookup, RBTREE_LEFT);
if (node == NULL) {
*entry = vm_map_to_entry(map);
SAVE_HINT(map, *entry);
return(FALSE);
} else {
*entry = rbtree_entry(node, struct vm_map_entry, tree_node);
SAVE_HINT(map, *entry);
return((address < (*entry)->vme_end) ? TRUE : FALSE);
}
}
static struct vm_map_entry *
vm_map_find_entry_anywhere(struct vm_map *map,
vm_size_t size,
vm_offset_t mask,
boolean_t map_locked,
vm_offset_t *startp)
{
struct vm_map_entry *entry;
struct rbtree_node *node;
vm_size_t max_size;
vm_offset_t start, end;
vm_offset_t max;
assert(size != 0);
max = map->max_offset;
if (((mask + 1) & mask) != 0) {
int first0 = __builtin_ffs(~mask);
vm_offset_t lowmask = (1UL << (first0-1)) - 1;
vm_offset_t himask = mask - lowmask;
int second1 = __builtin_ffs(himask);
max = 1UL << (second1-1);
if (himask + max != 0) {
printf("invalid mask %zx\n", mask);
return NULL;
}
mask = lowmask;
}
if (!map_locked) {
vm_map_lock(map);
}
restart:
if (map->hdr.nentries == 0) {
entry = vm_map_to_entry(map);
start = (map->min_offset + mask) & ~mask;
if (map->aslr_enabled) {
vm_offset_t entropy = vm_map_get_aslr_entropy(map, size);
vm_offset_t randomized_start = start + entropy;
randomized_start = (randomized_start + mask) & ~mask;
if (randomized_start >= map->min_offset &&
(randomized_start + size) <= max &&
(randomized_start + size) > randomized_start) {
start = randomized_start;
}
}
start = vm_map_optimize_placement(map, size, start);
end = start + size;
if ((start < map->min_offset) || (end <= start) || (end > max)) {
goto error;
}
*startp = start;
return entry;
}
entry = map->first_free;
if (entry != vm_map_to_entry(map)) {
start = (entry->vme_end + mask) & ~mask;
if (map->aslr_enabled && entry->gap_size > (size + mask)) {
vm_offset_t available_space = entry->gap_size - (size + mask);
vm_offset_t entropy = vm_map_get_aslr_entropy(map, size);
if (available_space > 0 && entropy > 0) {
vm_offset_t max_entropy = available_space & ~(PAGE_SIZE - 1);
if (max_entropy > 0) {
entropy = (entropy % (max_entropy / PAGE_SIZE + 1)) * PAGE_SIZE;
start += entropy;
start = (start + mask) & ~mask;
}
}
}
end = start + size;
if ((start >= entry->vme_end)
&& (end > start)
&& (end <= max)
&& (end <= (entry->vme_end + entry->gap_size))) {
*startp = start;
return entry;
}
}
max_size = size + mask;
if (max_size < size) {
printf("max_size %zd got smaller than size %zd with mask %zd\n",
max_size, size, mask);
goto error;
}
node = rbtree_lookup_nearest(&map->hdr.gap_tree, max_size,
vm_map_entry_gap_cmp_lookup, RBTREE_RIGHT);
if (node == NULL) {
if (map_locked || !map->wait_for_space) {
goto error;
}
assert_wait((event_t)map, TRUE);
vm_map_unlock(map);
thread_block(NULL);
vm_map_lock(map);
goto restart;
}
entry = rbtree_entry(node, struct vm_map_entry, gap_node);
assert(entry->in_gap_tree);
if (!list_empty(&entry->gap_list)) {
entry = list_last_entry(&entry->gap_list,
struct vm_map_entry, gap_list);
}
assert(entry->gap_size >= max_size);
start = (entry->vme_end + mask) & ~mask;
if (map->aslr_enabled && entry->gap_size > max_size) {
vm_offset_t available_space = entry->gap_size - max_size;
vm_offset_t entropy = vm_map_get_aslr_entropy(map, size);
vm_offset_t max_entropy_offset;
max_entropy_offset = available_space & ~(PAGE_SIZE - 1);
if (entropy > max_entropy_offset) {
entropy = entropy % (max_entropy_offset / PAGE_SIZE + 1) * PAGE_SIZE;
}
if (entropy > 0 && entropy <= available_space) {
vm_offset_t randomized_start = start + entropy;
randomized_start = (randomized_start + mask) & ~mask;
if (randomized_start >= entry->vme_end &&
(randomized_start + size) <= (entry->vme_end + entry->gap_size)) {
start = randomized_start;
}
}
}
assert(start >= entry->vme_end);
end = start + size;
assert(end > start);
assert(end <= (entry->vme_end + entry->gap_size));
if (end > max) {
printf("%lx does not respect %lx\n", (unsigned long) end, (unsigned long) max);
return NULL;
}
*startp = start;
return entry;
error:
printf("no more room in %p (%s)\n", map, map->name);
return NULL;
}
kern_return_t vm_map_find_entry(
vm_map_t map,
vm_offset_t *address,
vm_size_t size,
vm_offset_t mask,
vm_object_t object,
vm_map_entry_t *o_entry)
{
vm_map_entry_t entry, new_entry;
vm_offset_t start;
vm_offset_t end;
entry = vm_map_find_entry_anywhere(map, size, mask, TRUE, &start);
if (entry == NULL) {
return KERN_NO_SPACE;
}
end = start + size;
*address = start;
if ((object != VM_OBJECT_NULL) &&
(entry != vm_map_to_entry(map)) &&
(entry->vme_end == start) &&
(!entry->is_shared) &&
(!entry->is_sub_map) &&
(!entry->in_transition) &&
(entry->object.vm_object == object) &&
(entry->needs_copy == FALSE) &&
(entry->inheritance == VM_INHERIT_DEFAULT) &&
(entry->protection == VM_PROT_DEFAULT) &&
(entry->max_protection == VM_PROT_ALL) &&
(entry->wired_count != 0) &&
(entry->projected_on == 0)) {
entry->vme_end = end;
vm_map_gap_update(&map->hdr, entry);
new_entry = entry;
} else {
new_entry = vm_map_entry_create(map);
new_entry->vme_start = start;
new_entry->vme_end = end;
new_entry->is_shared = FALSE;
new_entry->is_sub_map = FALSE;
new_entry->object.vm_object = VM_OBJECT_NULL;
new_entry->offset = (vm_offset_t) 0;
new_entry->needs_copy = FALSE;
new_entry->inheritance = VM_INHERIT_DEFAULT;
new_entry->protection = VM_PROT_DEFAULT;
new_entry->max_protection = VM_PROT_ALL;
new_entry->wired_count = 1;
new_entry->wired_access = VM_PROT_DEFAULT;
new_entry->in_transition = FALSE;
new_entry->needs_wakeup = FALSE;
new_entry->projected_on = 0;
vm_map_entry_link(map, entry, new_entry);
}
map->size += size;
map->first_free = new_entry;
SAVE_HINT(map, new_entry);
*o_entry = new_entry;
return(KERN_SUCCESS);
}
boolean_t vm_map_pmap_enter_print = FALSE;
boolean_t vm_map_pmap_enter_enable = FALSE;
static void
vm_map_pmap_enter(
vm_map_t map,
vm_offset_t addr,
vm_offset_t end_addr,
vm_object_t object,
vm_offset_t offset,
vm_prot_t protection)
{
while (addr < end_addr) {
vm_page_t m;
vm_object_lock(object);
vm_object_paging_begin(object);
m = vm_page_lookup(object, offset);
if (m == VM_PAGE_NULL || m->absent) {
vm_object_paging_end(object);
vm_object_unlock(object);
return;
}
if (vm_map_pmap_enter_print) {
printf("vm_map_pmap_enter:");
printf("map: %p, addr: %zx, object: %p, offset: %zx\n",
map, addr, object, offset);
}
m->busy = TRUE;
vm_object_unlock(object);
PMAP_ENTER(map->pmap, addr, m,
protection, FALSE);
vm_object_lock(object);
PAGE_WAKEUP_DONE(m);
vm_page_lock_queues();
if (!m->active && !m->inactive)
vm_page_activate(m);
vm_page_unlock_queues();
vm_object_paging_end(object);
vm_object_unlock(object);
offset += PAGE_SIZE;
addr += PAGE_SIZE;
}
}
kern_return_t vm_map_enter(
vm_map_t map,
vm_offset_t *address,
vm_size_t size,
vm_offset_t mask,
boolean_t anywhere,
vm_object_t object,
vm_offset_t offset,
boolean_t needs_copy,
vm_prot_t cur_protection,
vm_prot_t max_protection,
vm_inherit_t inheritance)
{
vm_map_entry_t entry;
vm_map_entry_t next_entry;
vm_offset_t start;
vm_offset_t end;
kern_return_t result = KERN_SUCCESS;
#define RETURN(value) \
MACRO_BEGIN \
result = value; goto BailOut; \
MACRO_END
if (size == 0)
return KERN_INVALID_ARGUMENT;
start = *address;
if (anywhere) {
entry = vm_map_find_entry_anywhere(map, size, mask, FALSE, &start);
if (entry == NULL) {
RETURN(KERN_NO_SPACE);
}
end = start + size;
*address = start;
next_entry = entry->vme_next;
} else {
vm_map_entry_t temp_entry;
if ((start & mask) != 0)
return(KERN_NO_SPACE);
vm_map_lock(map);
end = start + size;
if ((start < map->min_offset) ||
(end > map->max_offset) ||
(start >= end)) {
RETURN(KERN_INVALID_ADDRESS);
}
if (vm_map_lookup_entry(map, start, &temp_entry))
RETURN(KERN_NO_SPACE);
entry = temp_entry;
next_entry = entry->vme_next;
if ((next_entry != vm_map_to_entry(map)) &&
(next_entry->vme_start < end))
RETURN(KERN_NO_SPACE);
}
if ((entry != vm_map_to_entry(map)) &&
(entry->vme_end == start) &&
(!entry->is_shared) &&
(!entry->is_sub_map) &&
(!entry->in_transition) &&
(entry->inheritance == inheritance) &&
(entry->protection == cur_protection) &&
(entry->max_protection == max_protection) &&
(entry->wired_count == 0) &&
(entry->projected_on == 0)) {
if (vm_object_coalesce(entry->object.vm_object,
object,
entry->offset,
offset,
(vm_size_t)(entry->vme_end - entry->vme_start),
size,
&entry->object.vm_object,
&entry->offset)) {
map->size += size;
entry->vme_end = end;
vm_map_gap_update(&map->hdr, entry);
vm_map_coalesce_entries(map, next_entry);
RETURN(KERN_SUCCESS);
}
}
if ((next_entry != vm_map_to_entry(map)) &&
(next_entry->vme_start == end) &&
(!next_entry->is_shared) &&
(!next_entry->is_sub_map) &&
(!next_entry->in_transition) &&
(next_entry->inheritance == inheritance) &&
(next_entry->protection == cur_protection) &&
(next_entry->max_protection == max_protection) &&
(next_entry->wired_count == 0) &&
(next_entry->projected_on == 0)) {
if (vm_object_coalesce(object,
next_entry->object.vm_object,
offset,
next_entry->offset,
size,
(vm_size_t)(next_entry->vme_end - next_entry->vme_start),
&next_entry->object.vm_object,
&next_entry->offset)) {
map->size += size;
next_entry->vme_start = start;
vm_map_gap_update(&map->hdr, entry);
vm_map_coalesce_entries(map, next_entry);
RETURN(KERN_SUCCESS);
}
}
{
vm_map_entry_t new_entry;
new_entry = vm_map_entry_create(map);
new_entry->vme_start = start;
new_entry->vme_end = end;
new_entry->is_shared = FALSE;
new_entry->is_sub_map = FALSE;
new_entry->object.vm_object = object;
new_entry->offset = offset;
new_entry->needs_copy = needs_copy;
new_entry->inheritance = inheritance;
new_entry->protection = cur_protection;
new_entry->max_protection = max_protection;
new_entry->wired_count = 0;
new_entry->wired_access = VM_PROT_NONE;
new_entry->in_transition = FALSE;
new_entry->needs_wakeup = FALSE;
new_entry->projected_on = 0;
vm_map_entry_link(map, entry, new_entry);
map->size += size;
if ((map->first_free == entry) &&
((entry == vm_map_to_entry(map) ? map->min_offset : entry->vme_end)
>= new_entry->vme_start))
map->first_free = new_entry;
SAVE_HINT(map, new_entry);
if (map->wiring_required) {
result = vm_map_pageable(map, start, end, cur_protection, FALSE, FALSE);
if (result != KERN_SUCCESS) {
RETURN(KERN_SUCCESS);
}
}
vm_map_unlock(map);
if ((object != VM_OBJECT_NULL) &&
(vm_map_pmap_enter_enable) &&
(!anywhere) &&
(!needs_copy) &&
(size < (128*1024))) {
vm_map_pmap_enter(map, start, end,
object, offset, cur_protection);
}
return(result);
}
BailOut: ;
vm_map_unlock(map);
return(result);
#undef RETURN
}
#define vm_map_clip_start(map, entry, startaddr) \
MACRO_BEGIN \
if ((startaddr) > (entry)->vme_start) \
_vm_map_clip_start(&(map)->hdr,(entry),(startaddr),1); \
MACRO_END
#define vm_map_copy_clip_start(copy, entry, startaddr) \
MACRO_BEGIN \
if ((startaddr) > (entry)->vme_start) \
_vm_map_clip_start(&(copy)->cpy_hdr,(entry),(startaddr),0); \
MACRO_END
void _vm_map_clip_start(
struct vm_map_header *map_header,
vm_map_entry_t entry,
vm_offset_t start,
boolean_t link_gap)
{
vm_map_entry_t new_entry;
new_entry = _vm_map_entry_create(map_header);
vm_map_entry_copy_full(new_entry, entry);
new_entry->vme_end = start;
entry->offset += (start - entry->vme_start);
entry->vme_start = start;
_vm_map_entry_link(map_header, entry->vme_prev, new_entry, link_gap);
if (entry->is_sub_map)
vm_map_reference(new_entry->object.sub_map);
else
vm_object_reference(new_entry->object.vm_object);
}
#define vm_map_clip_end(map, entry, endaddr) \
MACRO_BEGIN \
if ((endaddr) < (entry)->vme_end) \
_vm_map_clip_end(&(map)->hdr,(entry),(endaddr),1); \
MACRO_END
#define vm_map_copy_clip_end(copy, entry, endaddr) \
MACRO_BEGIN \
if ((endaddr) < (entry)->vme_end) \
_vm_map_clip_end(&(copy)->cpy_hdr,(entry),(endaddr),0); \
MACRO_END
void _vm_map_clip_end(
struct vm_map_header *map_header,
vm_map_entry_t entry,
vm_offset_t end,
boolean_t link_gap)
{
vm_map_entry_t new_entry;
new_entry = _vm_map_entry_create(map_header);
vm_map_entry_copy_full(new_entry, entry);
new_entry->vme_start = entry->vme_end = end;
new_entry->offset += (end - entry->vme_start);
_vm_map_entry_link(map_header, entry, new_entry, link_gap);
if (entry->is_sub_map)
vm_map_reference(new_entry->object.sub_map);
else
vm_object_reference(new_entry->object.vm_object);
}
#define VM_MAP_RANGE_CHECK(map, start, end) \
MACRO_BEGIN \
if (start < vm_map_min(map)) \
start = vm_map_min(map); \
if (end > vm_map_max(map)) \
end = vm_map_max(map); \
if (start > end) \
start = end; \
MACRO_END
kern_return_t vm_map_submap(
vm_map_t map,
vm_offset_t start,
vm_offset_t end,
vm_map_t submap)
{
vm_map_entry_t entry;
kern_return_t result = KERN_INVALID_ARGUMENT;
vm_object_t object;
vm_map_lock(map);
VM_MAP_RANGE_CHECK(map, start, end);
if (vm_map_lookup_entry(map, start, &entry)) {
vm_map_clip_start(map, entry, start);
}
else
entry = entry->vme_next;
vm_map_clip_end(map, entry, end);
if ((entry->vme_start == start) && (entry->vme_end == end) &&
(!entry->is_sub_map) &&
((object = entry->object.vm_object) == vm_submap_object) &&
(object->resident_page_count == 0) &&
(object->copy == VM_OBJECT_NULL) &&
(object->shadow == VM_OBJECT_NULL) &&
(!object->pager_created)) {
entry->object.vm_object = VM_OBJECT_NULL;
vm_object_deallocate(object);
entry->is_sub_map = TRUE;
vm_map_reference(entry->object.sub_map = submap);
result = KERN_SUCCESS;
}
vm_map_unlock(map);
return(result);
}
static void
vm_map_entry_inc_wired(vm_map_t map, vm_map_entry_t entry)
{
if (entry->wired_count > 1) {
return;
}
if (entry->wired_count == 0) {
map->size_wired += entry->vme_end - entry->vme_start;
}
entry->wired_count++;
}
static void
vm_map_entry_reset_wired(vm_map_t map, vm_map_entry_t entry)
{
if (entry->wired_count != 0) {
map->size_wired -= entry->vme_end - entry->vme_start;
entry->wired_count = 0;
}
}
static void vm_map_pageable_scan(
vm_map_t map,
vm_map_entry_t start_entry,
vm_offset_t end)
{
vm_map_entry_t entry;
boolean_t do_wire_faults;
do_wire_faults = FALSE;
for (entry = start_entry;
(entry != vm_map_to_entry(map)) &&
(entry->vme_start < end);
) {
struct rbtree_node *next_node;
if (entry->wired_access == VM_PROT_NONE) {
if (entry->wired_count != 0) {
vm_map_entry_reset_wired(map, entry);
vm_fault_unwire(map, entry);
}
continue;
}
if (entry->protection == VM_PROT_NONE) {
if (entry->wired_count == 0) {
continue;
}
vm_map_entry_reset_wired(map, entry);
vm_fault_unwire(map, entry);
continue;
}
if (entry->wired_count == 0) {
if (entry->needs_copy &&
((entry->protection & VM_PROT_WRITE) != 0)) {
vm_object_shadow(&entry->object.vm_object,
&entry->offset,
(vm_size_t)(entry->vme_end
- entry->vme_start));
entry->needs_copy = FALSE;
}
if (entry->object.vm_object == VM_OBJECT_NULL) {
entry->object.vm_object =
vm_object_allocate(
(vm_size_t)(entry->vme_end
- entry->vme_start));
entry->offset = (vm_offset_t)0;
}
}
vm_map_entry_inc_wired(map, entry);
if (entry->wired_count == 1) {
do_wire_faults = TRUE;
}
next_node = rbtree_next(&entry->tree_node);
if (next_node != NULL) {
entry = rbtree_entry(next_node, struct vm_map_entry, tree_node);
} else {
entry = vm_map_to_entry(map);
}
}
if (!do_wire_faults) {
return;
}
if (vm_map_pmap(map) == kernel_pmap) {
for (entry = start_entry;
(entry != vm_map_to_entry(map)) &&
(entry->vme_end <= end);
) {
struct rbtree_node *next_node;
assert(!entry->in_transition);
entry->in_transition = TRUE;
entry->needs_wakeup = FALSE;
next_node = rbtree_next(&entry->tree_node);
if (next_node != NULL) {
entry = rbtree_entry(next_node, struct vm_map_entry, tree_node);
} else {
entry = vm_map_to_entry(map);
}
}
vm_map_unlock(map);
} else {
vm_map_lock_set_recursive(map);
vm_map_lock_write_to_read(map);
}
for (entry = start_entry;
(entry != vm_map_to_entry(map)) &&
(entry->vme_end <= end);
) {
struct rbtree_node *next_node;
if (entry->wired_count == 1) {
vm_fault_wire(map, entry);
}
next_node = rbtree_next(&entry->tree_node);
if (next_node != NULL) {
entry = rbtree_entry(next_node, struct vm_map_entry, tree_node);
} else {
entry = vm_map_to_entry(map);
}
}
if (vm_map_pmap(map) == kernel_pmap) {
vm_map_lock(map);
for (entry = start_entry;
(entry != vm_map_to_entry(map)) &&
(entry->vme_end <= end);
) {
struct rbtree_node *next_node;
assert(entry->in_transition);
entry->in_transition = FALSE;
assert(!entry->needs_wakeup);
next_node = rbtree_next(&entry->tree_node);
if (next_node != NULL) {
entry = rbtree_entry(next_node, struct vm_map_entry, tree_node);
} else {
entry = vm_map_to_entry(map);
}
}
} else {
vm_map_lock_clear_recursive(map);
}
}
kern_return_t vm_map_protect(
vm_map_t map,
vm_offset_t start,
vm_offset_t end,
vm_prot_t new_prot,
boolean_t set_max)
{
vm_map_entry_t current;
vm_map_entry_t entry;
vm_map_entry_t next;
vm_map_lock(map);
VM_MAP_RANGE_CHECK(map, start, end);
if (vm_map_lookup_entry(map, start, &entry)) {
vm_map_clip_start(map, entry, start);
}
else
entry = entry->vme_next;
current = entry;
while ((current != vm_map_to_entry(map)) &&
(current->vme_start < end)) {
if (current->is_sub_map) {
vm_map_unlock(map);
return(KERN_INVALID_ARGUMENT);
}
if ((new_prot & (VM_PROT_NOTIFY | current->max_protection))
!= new_prot) {
vm_map_unlock(map);
return(KERN_PROTECTION_FAILURE);
}
current = current->vme_next;
}
current = entry;
while ((current != vm_map_to_entry(map)) &&
(current->vme_start < end)) {
vm_prot_t old_prot;
vm_map_clip_end(map, current, end);
old_prot = current->protection;
if (set_max)
current->protection =
(current->max_protection = new_prot) &
old_prot;
else
current->protection = new_prot;
if ((current->protection != VM_PROT_NONE) &&
(current->wired_access != VM_PROT_NONE ||
map->wiring_required)) {
current->wired_access = current->protection;
}
if (current->protection != old_prot) {
pmap_protect(map->pmap, current->vme_start,
current->vme_end,
current->protection);
}
next = current->vme_next;
vm_map_coalesce_entries(map, current);
current = next;
}
next = current->vme_next;
if (vm_map_coalesce_entries(map, current))
current = next;
vm_map_pageable_scan(map, entry, end);
vm_map_unlock(map);
return(KERN_SUCCESS);
}
kern_return_t vm_map_inherit(
vm_map_t map,
vm_offset_t start,
vm_offset_t end,
vm_inherit_t new_inheritance)
{
vm_map_entry_t entry;
vm_map_entry_t temp_entry;
vm_map_entry_t next;
vm_map_lock(map);
VM_MAP_RANGE_CHECK(map, start, end);
if (vm_map_lookup_entry(map, start, &temp_entry)) {
entry = temp_entry;
vm_map_clip_start(map, entry, start);
}
else
entry = temp_entry->vme_next;
while ((entry != vm_map_to_entry(map)) && (entry->vme_start < end)) {
vm_map_clip_end(map, entry, end);
entry->inheritance = new_inheritance;
next = entry->vme_next;
vm_map_coalesce_entries(map, entry);
entry = next;
}
vm_map_coalesce_entries(map, entry);
vm_map_unlock(map);
return(KERN_SUCCESS);
}
kern_return_t vm_map_pageable(
vm_map_t map,
vm_offset_t start,
vm_offset_t end,
vm_prot_t access_type,
boolean_t lock_map,
boolean_t check_range)
{
vm_map_entry_t entry;
vm_map_entry_t start_entry;
vm_map_entry_t end_entry;
if (lock_map) {
vm_map_lock(map);
}
VM_MAP_RANGE_CHECK(map, start, end);
if (!vm_map_lookup_entry(map, start, &start_entry)) {
if (lock_map) {
vm_map_unlock(map);
}
return KERN_NO_SPACE;
}
vm_map_clip_start(map, start_entry, start);
for (entry = start_entry;
(entry != vm_map_to_entry(map)) &&
(entry->vme_start < end);
) {
struct rbtree_node *next_node;
vm_map_clip_end(map, entry, end);
if (check_range &&
(((entry->vme_end < end) &&
((entry->vme_next == vm_map_to_entry(map)) ||
(entry->vme_next->vme_start > entry->vme_end))) ||
((entry->protection & access_type) != access_type))) {
if (lock_map) {
vm_map_unlock(map);
}
return KERN_NO_SPACE;
}
next_node = rbtree_next(&entry->tree_node);
if (next_node != NULL) {
entry = rbtree_entry(next_node, struct vm_map_entry, tree_node);
} else {
entry = vm_map_to_entry(map);
}
}
end_entry = entry;
for (entry = start_entry; entry != end_entry; ) {
struct rbtree_node *next_node;
entry->wired_access = access_type;
next_node = rbtree_next(&entry->tree_node);
if (next_node != NULL) {
entry = rbtree_entry(next_node, struct vm_map_entry, tree_node);
} else {
break;
}
}
vm_map_pageable_scan(map, start_entry, end);
if (lock_map) {
vm_map_unlock(map);
}
return(KERN_SUCCESS);
}
static kern_return_t
vm_map_pageable_current(vm_map_t map, vm_prot_t access_type)
{
struct rbtree_node *node;
vm_offset_t min_address, max_address;
node = rbtree_first(&map->hdr.tree);
min_address = rbtree_entry(node, struct vm_map_entry,
tree_node)->vme_start;
node = rbtree_last(&map->hdr.tree);
max_address = rbtree_entry(node, struct vm_map_entry,
tree_node)->vme_end;
return vm_map_pageable(map, min_address, max_address,access_type,
FALSE, FALSE);
}
kern_return_t
vm_map_pageable_all(struct vm_map *map, vm_wire_t flags)
{
boolean_t wiring_required;
kern_return_t kr;
if ((flags & ~VM_WIRE_ALL) != 0) {
return KERN_INVALID_ARGUMENT;
}
vm_map_lock(map);
if (flags == VM_WIRE_NONE) {
map->wiring_required = FALSE;
kr = vm_map_pageable_current(map, VM_PROT_NONE);
vm_map_unlock(map);
return kr;
}
wiring_required = map->wiring_required;
if (flags & VM_WIRE_FUTURE) {
map->wiring_required = TRUE;
}
if (flags & VM_WIRE_CURRENT) {
kr = vm_map_pageable_current(map, VM_PROT_READ | VM_PROT_WRITE);
if (kr != KERN_SUCCESS) {
if (flags & VM_WIRE_FUTURE) {
map->wiring_required = wiring_required;
}
vm_map_unlock(map);
return kr;
}
}
vm_map_unlock(map);
return KERN_SUCCESS;
}
void vm_map_entry_delete(
vm_map_t map,
vm_map_entry_t entry)
{
vm_offset_t s, e;
vm_size_t size;
vm_object_t object;
extern vm_object_t kernel_object;
s = entry->vme_start;
e = entry->vme_end;
size = e - s;
if (map != kernel_map && entry->projected_on != 0) {
if (entry->projected_on->projected_on == 0)
entry->wired_count = 0;
else
return;
}
if ((object = entry->object.vm_object) != VM_OBJECT_NULL) {
if (entry->wired_count != 0) {
vm_map_entry_reset_wired(map, entry);
vm_fault_unwire(map, entry);
}
if (object == kernel_object) {
vm_object_lock(object);
vm_object_page_remove(object, entry->offset,
entry->offset + size);
vm_object_unlock(object);
} else if (entry->is_shared) {
vm_object_pmap_remove(object,
entry->offset,
entry->offset + size);
} else {
pmap_remove(map->pmap, s, e);
vm_object_lock(object);
if ((!object->pager_created) &&
(object->ref_count == 1) &&
(object->paging_in_progress == 0)) {
vm_object_page_remove(object,
entry->offset,
entry->offset + size);
}
vm_object_unlock(object);
}
}
if (entry->is_sub_map)
vm_map_deallocate(entry->object.sub_map);
else
vm_object_deallocate(entry->object.vm_object);
vm_map_entry_unlink(map, entry);
map->size -= size;
vm_map_entry_dispose(map, entry);
}
kern_return_t vm_map_delete(
vm_map_t map,
vm_offset_t start,
vm_offset_t end)
{
vm_map_entry_t entry;
vm_map_entry_t first_entry;
if (map->pmap == kernel_pmap && (start < kernel_virtual_start || end > kernel_virtual_end))
panic("vm_map_delete(%lx-%lx) falls in physical memory area!\n", (unsigned long) start, (unsigned long) end);
assert((map->ref_count > 0 && have_lock(&map->lock)) || (map->ref_count == 0));
if (!vm_map_lookup_entry(map, start, &first_entry))
entry = first_entry->vme_next;
else {
entry = first_entry;
vm_map_clip_start(map, entry, start);
SAVE_HINT(map, entry->vme_prev);
}
if (map->first_free->vme_start >= start)
map->first_free = entry->vme_prev;
while ((entry != vm_map_to_entry(map)) && (entry->vme_start < end)) {
vm_map_entry_t next;
vm_map_clip_end(map, entry, end);
if(entry->in_transition) {
entry->needs_wakeup = TRUE;
vm_map_entry_wait(map, FALSE);
vm_map_lock(map);
if(!vm_map_lookup_entry(map, start, &entry)) {
entry = entry->vme_next;
}
continue;
}
next = entry->vme_next;
vm_map_entry_delete(map, entry);
entry = next;
}
if (first_entry && first_entry->vme_prev != vm_map_to_entry(map)) {
vm_map_coalesce_entries(map, first_entry->vme_prev);
}
if (entry && entry != vm_map_to_entry(map)) {
vm_map_coalesce_entries(map, entry);
}
if (map->wait_for_space)
thread_wakeup((event_t) map);
return(KERN_SUCCESS);
}
kern_return_t vm_map_remove(
vm_map_t map,
vm_offset_t start,
vm_offset_t end)
{
kern_return_t result;
vm_map_lock(map);
VM_MAP_RANGE_CHECK(map, start, end);
result = vm_map_delete(map, start, end);
vm_map_unlock(map);
return(result);
}
static void
vm_map_copy_steal_pages(vm_map_copy_t copy)
{
vm_page_t m, new_m;
int i;
vm_object_t object;
for (i = 0; i < copy->cpy_npages; i++) {
m = copy->cpy_page_list[i];
if (!m->tabled)
continue;
while ((new_m = vm_page_grab(VM_PAGE_HIGHMEM)) == VM_PAGE_NULL) {
VM_PAGE_WAIT((void(*)()) 0);
}
vm_page_copy(m, new_m);
object = m->object;
vm_object_lock(object);
vm_page_lock_queues();
if (!m->active && !m->inactive)
vm_page_activate(m);
vm_page_unlock_queues();
PAGE_WAKEUP_DONE(m);
vm_object_paging_end(object);
vm_object_unlock(object);
copy->cpy_page_list[i] = new_m;
}
}
void vm_map_copy_page_discard(vm_map_copy_t copy)
{
while (copy->cpy_npages > 0) {
vm_page_t m;
if((m = copy->cpy_page_list[--(copy->cpy_npages)]) !=
VM_PAGE_NULL) {
if (!m->tabled) {
VM_PAGE_FREE(m);
}
else {
vm_object_t object;
object = m->object;
vm_object_lock(object);
vm_page_lock_queues();
if (!m->active && !m->inactive)
vm_page_activate(m);
vm_page_unlock_queues();
PAGE_WAKEUP_DONE(m);
vm_object_paging_end(object);
vm_object_unlock(object);
}
}
}
}
void
vm_map_copy_discard(vm_map_copy_t copy)
{
free_next_copy:
if (copy == VM_MAP_COPY_NULL)
return;
switch (copy->type) {
case VM_MAP_COPY_ENTRY_LIST:
while (vm_map_copy_first_entry(copy) !=
vm_map_copy_to_entry(copy)) {
vm_map_entry_t entry = vm_map_copy_first_entry(copy);
vm_map_copy_entry_unlink(copy, entry);
vm_object_deallocate(entry->object.vm_object);
vm_map_copy_entry_dispose(copy, entry);
}
break;
case VM_MAP_COPY_OBJECT:
vm_object_deallocate(copy->cpy_object);
break;
case VM_MAP_COPY_PAGE_LIST:
if (copy->cpy_npages > 0)
vm_map_copy_page_discard(copy);
if (vm_map_copy_has_cont(copy)) {
if (copy->cpy_cont == vm_map_copy_discard_cont) {
vm_map_copy_t new_copy;
new_copy = (vm_map_copy_t) copy->cpy_cont_args;
kmem_cache_free(&vm_map_copy_cache, (vm_offset_t) copy);
copy = new_copy;
goto free_next_copy;
}
else {
vm_map_copy_abort_cont(copy);
}
}
break;
}
kmem_cache_free(&vm_map_copy_cache, (vm_offset_t) copy);
}
vm_map_copy_t
vm_map_copy_copy(vm_map_copy_t copy)
{
vm_map_copy_t new_copy;
if (copy == VM_MAP_COPY_NULL)
return VM_MAP_COPY_NULL;
new_copy = (vm_map_copy_t) kmem_cache_alloc(&vm_map_copy_cache);
*new_copy = *copy;
if (copy->type == VM_MAP_COPY_ENTRY_LIST) {
vm_map_copy_first_entry(copy)->vme_prev
= vm_map_copy_to_entry(new_copy);
vm_map_copy_last_entry(copy)->vme_next
= vm_map_copy_to_entry(new_copy);
}
copy->type = VM_MAP_COPY_OBJECT;
copy->cpy_object = VM_OBJECT_NULL;
return new_copy;
}
kern_return_t vm_map_copy_discard_cont(
vm_map_copyin_args_t cont_args,
vm_map_copy_t *copy_result)
{
vm_map_copy_discard((vm_map_copy_t) cont_args);
if (copy_result != (vm_map_copy_t *)0)
*copy_result = VM_MAP_COPY_NULL;
return(KERN_SUCCESS);
}
kern_return_t vm_map_copy_overwrite(
vm_map_t dst_map,
vm_offset_t dst_addr,
vm_map_copy_t copy,
boolean_t interruptible)
{
vm_size_t size;
vm_offset_t start;
vm_map_entry_t tmp_entry;
vm_map_entry_t entry;
boolean_t contains_permanent_objects = FALSE;
interruptible = FALSE;
if (copy == VM_MAP_COPY_NULL)
return(KERN_SUCCESS);
assert(copy->type == VM_MAP_COPY_ENTRY_LIST);
if (!page_aligned(copy->offset) ||
!page_aligned(copy->size) ||
!page_aligned(dst_addr))
return(KERN_INVALID_ARGUMENT);
size = copy->size;
if (size == 0) {
vm_map_copy_discard(copy);
return(KERN_SUCCESS);
}
start_pass_1:
vm_map_lock(dst_map);
if (!vm_map_lookup_entry(dst_map, dst_addr, &tmp_entry)) {
vm_map_unlock(dst_map);
return(KERN_INVALID_ADDRESS);
}
vm_map_clip_start(dst_map, tmp_entry, dst_addr);
for (entry = tmp_entry;;) {
vm_size_t sub_size = (entry->vme_end - entry->vme_start);
vm_map_entry_t next;
struct rbtree_node *next_node;
if ( ! (entry->protection & VM_PROT_WRITE)) {
vm_map_unlock(dst_map);
return(KERN_PROTECTION_FAILURE);
}
if (entry->in_transition) {
entry->needs_wakeup = TRUE;
vm_map_entry_wait(dst_map, FALSE);
goto start_pass_1;
}
if (size <= sub_size)
break;
next_node = rbtree_next(&entry->tree_node);
if (next_node != NULL) {
next = rbtree_entry(next_node, struct vm_map_entry, tree_node);
} else {
next = vm_map_to_entry(dst_map);
}
if ((next == vm_map_to_entry(dst_map)) ||
(next->vme_start != entry->vme_end)) {
vm_map_unlock(dst_map);
return(KERN_INVALID_ADDRESS);
}
if ((entry->object.vm_object != VM_OBJECT_NULL) &&
!entry->object.vm_object->temporary)
contains_permanent_objects = TRUE;
size -= sub_size;
entry = next;
}
if (interruptible && contains_permanent_objects) {
vm_map_unlock(dst_map);
return(KERN_FAILURE);
}
start = dst_addr;
while (vm_map_copy_first_entry(copy) != vm_map_copy_to_entry(copy)) {
vm_map_entry_t copy_entry = vm_map_copy_first_entry(copy);
vm_size_t copy_size = (copy_entry->vme_end - copy_entry->vme_start);
vm_object_t object;
entry = tmp_entry;
size = (entry->vme_end - entry->vme_start);
if (entry->vme_start != start) {
vm_map_unlock(dst_map);
return(KERN_INVALID_ADDRESS);
}
assert(entry != vm_map_to_entry(dst_map));
if ( ! (entry->protection & VM_PROT_WRITE)) {
vm_map_unlock(dst_map);
return(KERN_PROTECTION_FAILURE);
}
if (copy_size < size) {
vm_map_clip_end(dst_map, entry, entry->vme_start + copy_size);
size = copy_size;
}
if (size < copy_size) {
vm_map_copy_clip_end(copy, copy_entry,
copy_entry->vme_start + size);
copy_size = size;
}
assert((entry->vme_end - entry->vme_start) == size);
assert((tmp_entry->vme_end - tmp_entry->vme_start) == size);
assert((copy_entry->vme_end - copy_entry->vme_start) == size);
object = entry->object.vm_object;
if (!entry->is_shared &&
((object == VM_OBJECT_NULL) || object->temporary)) {
vm_object_t old_object = entry->object.vm_object;
vm_offset_t old_offset = entry->offset;
entry->object = copy_entry->object;
entry->offset = copy_entry->offset;
entry->needs_copy = copy_entry->needs_copy;
vm_map_entry_reset_wired(dst_map, entry);
vm_map_copy_entry_unlink(copy, copy_entry);
vm_map_copy_entry_dispose(copy, copy_entry);
vm_object_pmap_protect(
old_object,
old_offset,
size,
dst_map->pmap,
tmp_entry->vme_start,
VM_PROT_NONE);
vm_object_deallocate(old_object);
start = tmp_entry->vme_end;
tmp_entry = tmp_entry->vme_next;
} else {
vm_map_version_t version;
vm_object_t dst_object = entry->object.vm_object;
vm_offset_t dst_offset = entry->offset;
kern_return_t r;
vm_object_reference(dst_object);
version.main_timestamp = dst_map->timestamp;
vm_map_unlock(dst_map);
copy_size = size;
r = vm_fault_copy(
copy_entry->object.vm_object,
copy_entry->offset,
&copy_size,
dst_object,
dst_offset,
dst_map,
&version,
FALSE );
vm_object_deallocate(dst_object);
if (r != KERN_SUCCESS)
return(r);
if (copy_size != 0) {
vm_map_copy_clip_end(copy, copy_entry,
copy_entry->vme_start + copy_size);
vm_map_copy_entry_unlink(copy, copy_entry);
vm_object_deallocate(copy_entry->object.vm_object);
vm_map_copy_entry_dispose(copy, copy_entry);
}
start += copy_size;
vm_map_lock(dst_map);
if ((version.main_timestamp + 1) == dst_map->timestamp) {
vm_map_clip_end(dst_map, tmp_entry, start);
tmp_entry = tmp_entry->vme_next;
} else {
if (!vm_map_lookup_entry(dst_map, start, &tmp_entry)) {
vm_map_unlock(dst_map);
return(KERN_INVALID_ADDRESS);
}
vm_map_clip_start(dst_map, tmp_entry, start);
}
}
}
vm_map_unlock(dst_map);
vm_map_copy_discard(copy);
return(KERN_SUCCESS);
}
static void
vm_map_copy_insert(struct vm_map *map, struct vm_map_entry *where,
struct vm_map_copy *copy)
{
struct vm_map_entry *entry;
assert(copy->type == VM_MAP_COPY_ENTRY_LIST);
for (;;) {
entry = vm_map_copy_first_entry(copy);
if (entry == vm_map_copy_to_entry(copy)) {
break;
}
vm_map_copy_entry_unlink(copy, entry);
vm_map_entry_link(map, where, entry);
where = entry;
}
kmem_cache_free(&vm_map_copy_cache, (vm_offset_t)copy);
}
kern_return_t vm_map_copyout(
vm_map_t dst_map,
vm_offset_t *dst_addr,
vm_map_copy_t copy)
{
vm_size_t size;
vm_size_t adjustment;
vm_offset_t start;
vm_offset_t vm_copy_start;
vm_map_entry_t last;
vm_map_entry_t entry;
kern_return_t kr;
if (copy == VM_MAP_COPY_NULL) {
*dst_addr = 0;
return(KERN_SUCCESS);
}
if (copy->type == VM_MAP_COPY_OBJECT) {
vm_object_t object = copy->cpy_object;
vm_size_t offset = copy->offset;
vm_size_t tmp_size = copy->size;
*dst_addr = 0;
kr = vm_map_enter(dst_map, dst_addr, tmp_size,
(vm_offset_t) 0, TRUE,
object, offset, FALSE,
VM_PROT_DEFAULT, VM_PROT_ALL,
VM_INHERIT_DEFAULT);
if (kr != KERN_SUCCESS)
return(kr);
kmem_cache_free(&vm_map_copy_cache, (vm_offset_t) copy);
return(KERN_SUCCESS);
}
if (copy->type == VM_MAP_COPY_PAGE_LIST)
return(vm_map_copyout_page_list(dst_map, dst_addr, copy));
vm_copy_start = trunc_page(copy->offset);
size = round_page(copy->offset + copy->size) - vm_copy_start;
last = vm_map_find_entry_anywhere(dst_map, size, 0, FALSE, &start);
if (last == NULL) {
vm_map_unlock(dst_map);
return KERN_NO_SPACE;
}
adjustment = start - vm_copy_start;
for (entry = vm_map_copy_first_entry(copy);
entry != vm_map_copy_to_entry(copy);
entry = entry->vme_next) {
entry->vme_start += adjustment;
entry->vme_end += adjustment;
entry->inheritance = VM_INHERIT_DEFAULT;
entry->protection = VM_PROT_DEFAULT;
entry->max_protection = VM_PROT_ALL;
entry->projected_on = 0;
if (entry->wired_count != 0) {
vm_offset_t va;
vm_offset_t offset;
vm_object_t object;
object = entry->object.vm_object;
offset = entry->offset;
va = entry->vme_start;
pmap_pageable(dst_map->pmap,
entry->vme_start,
entry->vme_end,
TRUE);
while (va < entry->vme_end) {
vm_page_t m;
vm_object_lock(object);
vm_object_paging_begin(object);
m = vm_page_lookup(object, offset);
if (m == VM_PAGE_NULL || m->wire_count == 0 ||
m->absent)
panic("vm_map_copyout: wiring %p", m);
m->busy = TRUE;
vm_object_unlock(object);
PMAP_ENTER(dst_map->pmap, va, m,
entry->protection, TRUE);
vm_object_lock(object);
PAGE_WAKEUP_DONE(m);
vm_object_paging_end(object);
vm_object_unlock(object);
offset += PAGE_SIZE;
va += PAGE_SIZE;
}
}
}
*dst_addr = start + (copy->offset - vm_copy_start);
if (dst_map->first_free == last)
dst_map->first_free = vm_map_copy_last_entry(copy);
SAVE_HINT(dst_map, vm_map_copy_last_entry(copy));
dst_map->size += size;
vm_map_copy_insert(dst_map, last, copy);
if (dst_map->wiring_required) {
kr = vm_map_pageable(dst_map, start, start + size,
VM_PROT_READ | VM_PROT_WRITE,
FALSE, FALSE);
if (kr != KERN_SUCCESS) {
vm_map_unlock(dst_map);
return kr;
}
}
vm_map_unlock(dst_map);
return(KERN_SUCCESS);
}
kern_return_t vm_map_copyout_page_list(
vm_map_t dst_map,
vm_offset_t *dst_addr,
vm_map_copy_t copy)
{
vm_size_t size;
vm_offset_t start;
vm_offset_t end;
vm_offset_t offset;
vm_map_entry_t last;
vm_object_t object;
vm_page_t *page_list, m;
vm_map_entry_t entry;
vm_offset_t old_last_offset;
boolean_t cont_invoked, needs_wakeup = FALSE;
kern_return_t result = KERN_SUCCESS;
vm_map_copy_t orig_copy;
vm_offset_t dst_offset;
boolean_t must_wire;
page_list = &copy->cpy_page_list[0];
if ((*page_list)->tabled)
vm_map_copy_steal_pages(copy);
size = round_page(copy->offset + copy->size) -
trunc_page(copy->offset);
vm_map_lock(dst_map);
last = vm_map_find_entry_anywhere(dst_map, size, 0, TRUE, &start);
if (last == NULL) {
vm_map_unlock(dst_map);
return KERN_NO_SPACE;
}
end = start + size;
must_wire = dst_map->wiring_required;
if (last == vm_map_to_entry(dst_map) ||
last->vme_end != start ||
last->is_shared != FALSE ||
last->is_sub_map != FALSE ||
last->inheritance != VM_INHERIT_DEFAULT ||
last->protection != VM_PROT_DEFAULT ||
last->max_protection != VM_PROT_ALL ||
last->in_transition ||
(must_wire ? (last->wired_count == 0)
: (last->wired_count != 0))) {
goto create_object;
}
if (last->object.vm_object == VM_OBJECT_NULL) {
object = vm_object_allocate(
(vm_size_t)(last->vme_end - last->vme_start + size));
last->object.vm_object = object;
last->offset = 0;
vm_object_lock(object);
}
else {
vm_offset_t prev_offset = last->offset;
vm_size_t prev_size = start - last->vme_start;
vm_size_t new_size;
object = last->object.vm_object;
vm_object_lock(object);
vm_object_collapse(object);
if ((object->ref_count > 1) ||
object->pager_created ||
(object->shadow != VM_OBJECT_NULL) ||
(object->copy != VM_OBJECT_NULL) ||
(object->paging_in_progress != 0)) {
vm_object_unlock(object);
goto create_object;
}
new_size = prev_offset + prev_size + size;
if (new_size > object->size)
object->size = new_size;
}
dst_map->size += size;
last->vme_end = end;
vm_map_gap_update(&dst_map->hdr, last);
SAVE_HINT(dst_map, last);
goto insert_pages;
create_object:
object = vm_object_allocate(size);
entry = vm_map_entry_create(dst_map);
entry->object.vm_object = object;
entry->offset = 0;
entry->is_shared = FALSE;
entry->is_sub_map = FALSE;
entry->needs_copy = FALSE;
entry->wired_count = 0;
if (must_wire) {
vm_map_entry_inc_wired(dst_map, entry);
entry->wired_access = VM_PROT_DEFAULT;
} else {
entry->wired_access = VM_PROT_NONE;
}
entry->in_transition = TRUE;
entry->needs_wakeup = FALSE;
entry->vme_start = start;
entry->vme_end = start + size;
entry->inheritance = VM_INHERIT_DEFAULT;
entry->protection = VM_PROT_DEFAULT;
entry->max_protection = VM_PROT_ALL;
entry->projected_on = 0;
vm_object_lock(object);
if (dst_map->first_free == last) {
dst_map->first_free = entry;
}
SAVE_HINT(dst_map, entry);
dst_map->size += size;
vm_map_entry_link(dst_map, last, entry);
last = entry;
insert_pages:
dst_offset = copy->offset & PAGE_MASK;
cont_invoked = FALSE;
orig_copy = copy;
last->in_transition = TRUE;
old_last_offset = last->offset
+ (start - last->vme_start);
vm_page_lock_queues();
for (offset = 0; offset < size; offset += PAGE_SIZE) {
m = *page_list;
assert(m && !m->tabled);
assert(!m->wanted);
m->busy = FALSE;
m->dirty = TRUE;
vm_page_replace(m, object, old_last_offset + offset);
if (must_wire) {
vm_page_wire(m);
PMAP_ENTER(dst_map->pmap,
last->vme_start + m->offset - last->offset,
m, last->protection, TRUE);
} else {
vm_page_activate(m);
}
*page_list++ = VM_PAGE_NULL;
if (--(copy->cpy_npages) == 0 &&
vm_map_copy_has_cont(copy)) {
vm_map_copy_t new_copy;
cont_invoked = TRUE;
vm_page_unlock_queues();
vm_object_unlock(object);
vm_map_unlock(dst_map);
vm_map_copy_invoke_cont(copy, &new_copy, &result);
if (result == KERN_SUCCESS) {
if (copy != orig_copy)
vm_map_copy_discard(copy);
if ((copy = new_copy) != VM_MAP_COPY_NULL) {
page_list = &copy->cpy_page_list[0];
if ((*page_list)->tabled)
vm_map_copy_steal_pages(copy);
}
}
else {
vm_map_lock(dst_map);
goto error;
}
vm_map_lock(dst_map);
vm_object_lock(object);
vm_page_lock_queues();
}
}
vm_page_unlock_queues();
vm_object_unlock(object);
*dst_addr = start + dst_offset;
error:
if (!cont_invoked) {
last->in_transition = FALSE;
assert(!last->needs_wakeup);
needs_wakeup = FALSE;
}
else {
if (!vm_map_lookup_entry(dst_map, start, &entry))
panic("vm_map_copyout_page_list: missing entry");
while((entry != vm_map_to_entry(dst_map)) &&
(entry->vme_start < end)) {
assert(entry->in_transition);
entry->in_transition = FALSE;
if(entry->needs_wakeup) {
entry->needs_wakeup = FALSE;
needs_wakeup = TRUE;
}
entry = entry->vme_next;
}
}
if (result != KERN_SUCCESS)
vm_map_delete(dst_map, start, end);
vm_map_unlock(dst_map);
if (needs_wakeup)
vm_map_entry_wakeup(dst_map);
if (copy != orig_copy) {
kmem_cache_free(&vm_map_copy_cache, (vm_offset_t) copy);
}
if (result == KERN_SUCCESS) {
kmem_cache_free(&vm_map_copy_cache, (vm_offset_t) orig_copy);
}
return(result);
}
kern_return_t vm_map_copyin(
vm_map_t src_map,
vm_offset_t src_addr,
vm_size_t len,
boolean_t src_destroy,
vm_map_copy_t *copy_result)
{
vm_map_entry_t tmp_entry;
vm_offset_t src_start;
vm_offset_t src_end;
vm_map_copy_t copy;
if (len == 0) {
*copy_result = VM_MAP_COPY_NULL;
return(KERN_SUCCESS);
}
if ((src_addr + len) <= src_addr) {
return KERN_INVALID_ADDRESS;
}
src_start = trunc_page(src_addr);
src_end = round_page(src_addr + len);
if (src_end == 0) {
return KERN_INVALID_ADDRESS;
}
copy = (vm_map_copy_t) kmem_cache_alloc(&vm_map_copy_cache);
vm_map_copy_first_entry(copy) =
vm_map_copy_last_entry(copy) = vm_map_copy_to_entry(copy);
copy->type = VM_MAP_COPY_ENTRY_LIST;
copy->cpy_hdr.nentries = 0;
rbtree_init(&copy->cpy_hdr.tree);
rbtree_init(&copy->cpy_hdr.gap_tree);
copy->offset = src_addr;
copy->size = len;
#define RETURN(x) \
MACRO_BEGIN \
vm_map_unlock(src_map); \
vm_map_copy_discard(copy); \
MACRO_RETURN(x); \
MACRO_END
vm_map_lock(src_map);
if (!vm_map_lookup_entry(src_map, src_start, &tmp_entry))
RETURN(KERN_INVALID_ADDRESS);
vm_map_clip_start(src_map, tmp_entry, src_start);
while (TRUE) {
vm_map_entry_t src_entry = tmp_entry;
vm_size_t src_size;
vm_object_t src_object;
vm_offset_t src_offset;
boolean_t src_needs_copy;
vm_map_entry_t new_entry;
boolean_t new_entry_needs_copy;
boolean_t was_wired;
vm_map_version_t version;
if (! (src_entry->protection & VM_PROT_READ))
RETURN(KERN_PROTECTION_FAILURE);
vm_map_clip_end(src_map, src_entry, src_end);
src_size = src_entry->vme_end - src_start;
src_object = src_entry->object.vm_object;
src_offset = src_entry->offset;
was_wired = (src_entry->wired_count != 0);
new_entry = vm_map_copy_entry_create(copy);
vm_map_entry_copy(new_entry, src_entry);
if (src_destroy &&
(src_object == VM_OBJECT_NULL ||
(src_object->temporary && !src_object->use_shared_copy)))
{
vm_object_reference(src_object);
goto CopySuccessful;
}
if (!was_wired && src_size >= IPC_VIRTUAL_COPY_THRESHOLD &&
src_object != VM_OBJECT_NULL &&
src_object->temporary &&
vm_object_copy_temporary(
&new_entry->object.vm_object,
&new_entry->offset,
&src_needs_copy,
&new_entry_needs_copy)) {
new_entry->needs_copy = new_entry_needs_copy;
if (src_needs_copy && !tmp_entry->needs_copy) {
vm_object_pmap_protect(
src_object,
src_offset,
src_size,
(src_entry->is_shared ? PMAP_NULL
: src_map->pmap),
src_entry->vme_start,
src_entry->protection &
~VM_PROT_WRITE);
tmp_entry->needs_copy = TRUE;
}
goto CopySuccessful;
}
if (!was_wired &&
vm_object_copy_temporary(
&new_entry->object.vm_object,
&new_entry->offset,
&src_needs_copy,
&new_entry_needs_copy)) {
new_entry->needs_copy = new_entry_needs_copy;
if (src_needs_copy && !tmp_entry->needs_copy) {
vm_object_pmap_protect(
src_object,
src_offset,
src_size,
(src_entry->is_shared ? PMAP_NULL
: src_map->pmap),
src_entry->vme_start,
src_entry->protection &
~VM_PROT_WRITE);
tmp_entry->needs_copy = TRUE;
}
goto CopySuccessful;
}
new_entry->needs_copy = FALSE;
assert(src_object != VM_OBJECT_NULL);
vm_object_reference(src_object);
version.main_timestamp = src_map->timestamp;
vm_map_unlock(src_map);
if (was_wired) {
vm_object_lock(src_object);
(void) vm_object_copy_slowly(
src_object,
src_offset,
src_size,
FALSE,
&new_entry->object.vm_object);
new_entry->offset = 0;
new_entry->needs_copy = FALSE;
} else {
kern_return_t result;
result = vm_object_copy_strategically(src_object,
src_offset,
src_size,
&new_entry->object.vm_object,
&new_entry->offset,
&new_entry_needs_copy);
new_entry->needs_copy = new_entry_needs_copy;
if (result != KERN_SUCCESS) {
vm_map_copy_entry_dispose(copy, new_entry);
vm_map_lock(src_map);
RETURN(result);
}
}
vm_object_deallocate(src_object);
vm_map_lock(src_map);
if ((version.main_timestamp + 1) == src_map->timestamp)
goto CopySuccessful;
if (!vm_map_lookup_entry(src_map, src_start, &tmp_entry)) {
vm_map_copy_entry_dispose(copy, new_entry);
RETURN(KERN_INVALID_ADDRESS);
}
src_entry = tmp_entry;
vm_map_clip_start(src_map, src_entry, src_start);
if ((src_entry->protection & VM_PROT_READ) == VM_PROT_NONE)
goto VerificationFailed;
if (src_entry->vme_end < new_entry->vme_end)
src_size = (new_entry->vme_end = src_entry->vme_end) - src_start;
if ((src_entry->object.vm_object != src_object) ||
(src_entry->offset != src_offset) ) {
VerificationFailed: ;
vm_object_deallocate(new_entry->object.vm_object);
vm_map_copy_entry_dispose(copy, new_entry);
tmp_entry = src_entry;
continue;
}
CopySuccessful: ;
vm_map_copy_entry_link(copy, vm_map_copy_last_entry(copy),
new_entry);
src_start = new_entry->vme_end;
if ((src_start >= src_end) && (src_end != 0))
break;
tmp_entry = src_entry->vme_next;
if (tmp_entry->vme_start != src_start)
RETURN(KERN_INVALID_ADDRESS);
}
if (src_destroy)
(void) vm_map_delete(src_map, trunc_page(src_addr), src_end);
vm_map_unlock(src_map);
*copy_result = copy;
return(KERN_SUCCESS);
#undef RETURN
}
kern_return_t vm_map_copyin_object(
vm_object_t object,
vm_offset_t offset,
vm_size_t size,
vm_map_copy_t *copy_result)
{
vm_map_copy_t copy;
copy = (vm_map_copy_t) kmem_cache_alloc(&vm_map_copy_cache);
vm_map_copy_first_entry(copy) =
vm_map_copy_last_entry(copy) = VM_MAP_ENTRY_NULL;
copy->type = VM_MAP_COPY_OBJECT;
copy->cpy_object = object;
copy->offset = offset;
copy->size = size;
*copy_result = copy;
return(KERN_SUCCESS);
}
static kern_return_t vm_map_copyin_page_list_cont(
vm_map_copyin_args_t cont_args,
vm_map_copy_t *copy_result)
{
kern_return_t result = 0;
boolean_t do_abort, src_destroy, src_destroy_only;
do_abort = (copy_result == (vm_map_copy_t *) 0);
src_destroy = (cont_args->destroy_len != (vm_size_t) 0);
src_destroy_only = (cont_args->src_len == (vm_size_t) 0);
if (do_abort || src_destroy_only) {
if (src_destroy)
result = vm_map_remove(cont_args->map,
cont_args->destroy_addr,
cont_args->destroy_addr + cont_args->destroy_len);
if (!do_abort)
*copy_result = VM_MAP_COPY_NULL;
}
else {
result = vm_map_copyin_page_list(cont_args->map,
cont_args->src_addr, cont_args->src_len, src_destroy,
cont_args->steal_pages, copy_result, TRUE);
if (src_destroy && !cont_args->steal_pages &&
vm_map_copy_has_cont(*copy_result)) {
vm_map_copyin_args_t new_args;
new_args = (vm_map_copyin_args_t)
(*copy_result)->cpy_cont_args;
new_args->destroy_addr = cont_args->destroy_addr;
new_args->destroy_len = cont_args->destroy_len;
}
}
vm_map_deallocate(cont_args->map);
kfree((vm_offset_t)cont_args, sizeof(vm_map_copyin_args_data_t));
return(result);
}
kern_return_t vm_map_copyin_page_list(
vm_map_t src_map,
vm_offset_t src_addr,
vm_size_t len,
boolean_t src_destroy,
boolean_t steal_pages,
vm_map_copy_t *copy_result,
boolean_t is_cont)
{
vm_map_entry_t src_entry;
vm_page_t m;
vm_offset_t src_start;
vm_offset_t src_end;
vm_size_t src_size;
vm_object_t src_object;
vm_offset_t src_offset;
vm_offset_t src_last_offset;
vm_map_copy_t copy;
kern_return_t result = KERN_SUCCESS;
boolean_t need_map_lookup;
vm_map_copyin_args_t cont_args;
if (len == 0) {
*copy_result = VM_MAP_COPY_NULL;
return(KERN_SUCCESS);
}
if ((src_addr + len) <= src_addr) {
return KERN_INVALID_ADDRESS;
}
src_start = trunc_page(src_addr);
src_end = round_page(src_addr + len);
if (src_end == 0) {
return KERN_INVALID_ADDRESS;
}
copy = (vm_map_copy_t) kmem_cache_alloc(&vm_map_copy_cache);
copy->type = VM_MAP_COPY_PAGE_LIST;
copy->cpy_npages = 0;
copy->offset = src_addr;
copy->size = len;
copy->cpy_cont = (vm_map_copy_cont_fn) 0;
copy->cpy_cont_args = VM_MAP_COPYIN_ARGS_NULL;
do_map_lookup:
vm_map_lock(src_map);
if (!vm_map_lookup_entry(src_map, src_start, &src_entry)) {
result = KERN_INVALID_ADDRESS;
goto error;
}
need_map_lookup = FALSE;
while (TRUE) {
if (! (src_entry->protection & VM_PROT_READ)) {
result = KERN_PROTECTION_FAILURE;
goto error;
}
if (src_end > src_entry->vme_end)
src_size = src_entry->vme_end - src_start;
else
src_size = src_end - src_start;
src_object = src_entry->object.vm_object;
src_offset = src_entry->offset +
(src_start - src_entry->vme_start);
if (src_object == VM_OBJECT_NULL) {
src_object = vm_object_allocate((vm_size_t)
src_entry->vme_end -
src_entry->vme_start);
src_entry->object.vm_object = src_object;
}
src_last_offset = src_offset + src_size;
for (; (src_offset < src_last_offset && !need_map_lookup);
src_offset += PAGE_SIZE, src_start += PAGE_SIZE) {
if (copy->cpy_npages == VM_MAP_COPY_PAGE_LIST_MAX) {
make_continuation:
cont_args = (vm_map_copyin_args_t)
kalloc(sizeof(vm_map_copyin_args_data_t));
cont_args->map = src_map;
vm_map_reference(src_map);
cont_args->src_addr = src_start;
cont_args->src_len = len - (src_start - src_addr);
if (src_destroy) {
cont_args->destroy_addr = cont_args->src_addr;
cont_args->destroy_len = cont_args->src_len;
}
else {
cont_args->destroy_addr = (vm_offset_t) 0;
cont_args->destroy_len = (vm_offset_t) 0;
}
cont_args->steal_pages = steal_pages;
copy->cpy_cont_args = cont_args;
copy->cpy_cont = vm_map_copyin_page_list_cont;
src_end = src_start;
vm_map_clip_end(src_map, src_entry, src_end);
break;
}
vm_object_lock(src_object);
vm_object_paging_begin(src_object);
if (((m = vm_page_lookup(src_object, src_offset)) !=
VM_PAGE_NULL) && !m->busy && !m->fictitious &&
!m->absent && !m->error) {
m->busy = TRUE;
if (!src_destroy ||
src_object->use_shared_copy)
{
pmap_page_protect(m->phys_addr,
src_entry->protection
& ~m->page_lock
& ~VM_PROT_WRITE);
}
}
else {
vm_prot_t result_prot;
vm_page_t top_page;
kern_return_t kr;
vm_map_unlock(src_map);
need_map_lookup = TRUE;
retry:
result_prot = VM_PROT_READ;
kr = vm_fault_page(src_object, src_offset,
VM_PROT_READ, FALSE, FALSE,
&result_prot, &m, &top_page,
FALSE, (void (*)()) 0);
switch (kr) {
case VM_FAULT_SUCCESS:
break;
case VM_FAULT_INTERRUPTED:
case VM_FAULT_RETRY:
vm_object_lock(src_object);
vm_object_paging_begin(src_object);
goto retry;
case VM_FAULT_MEMORY_SHORTAGE:
VM_PAGE_WAIT((void (*)()) 0);
vm_object_lock(src_object);
vm_object_paging_begin(src_object);
goto retry;
case VM_FAULT_FICTITIOUS_SHORTAGE:
vm_page_more_fictitious();
vm_object_lock(src_object);
vm_object_paging_begin(src_object);
goto retry;
case VM_FAULT_MEMORY_ERROR:
vm_map_lock(src_map);
if (is_cont &&
copy->cpy_npages != 0)
goto make_continuation;
result = KERN_MEMORY_ERROR;
goto error;
}
if (top_page != VM_PAGE_NULL) {
vm_object_lock(src_object);
VM_PAGE_FREE(top_page);
vm_object_paging_end(src_object);
vm_object_unlock(src_object);
}
}
copy->cpy_page_list[copy->cpy_npages++] = m;
vm_object_unlock(m->object);
}
if (src_start >= src_end && src_end != 0) {
if (need_map_lookup)
vm_map_lock(src_map);
break;
}
if (need_map_lookup)
goto do_map_lookup;
src_start = src_entry->vme_end;
src_entry = src_entry->vme_next;
if (src_entry->vme_start != src_start) {
result = KERN_INVALID_ADDRESS;
goto error;
}
}
src_start = trunc_page(src_addr);
if (steal_pages) {
int i;
vm_offset_t unwire_end;
unwire_end = src_start;
for (i = 0; i < copy->cpy_npages; i++) {
m = copy->cpy_page_list[i];
src_object = m->object;
vm_object_lock(src_object);
if (src_destroy &&
src_object->temporary &&
(!src_object->shadowed) &&
(!src_object->use_shared_copy) &&
!m->precious) {
vm_offset_t page_vaddr;
page_vaddr = src_start + (i * PAGE_SIZE);
if (m->wire_count > 0) {
assert(m->wire_count == 1);
vm_object_unlock(src_object);
if (page_vaddr >= unwire_end) {
if (!vm_map_lookup_entry(src_map,
page_vaddr, &src_entry))
panic("vm_map_copyin_page_list: missing wired map entry");
vm_map_clip_start(src_map, src_entry,
page_vaddr);
vm_map_clip_end(src_map, src_entry,
src_start + src_size);
assert(src_entry->wired_count > 0);
vm_map_entry_reset_wired(src_map, src_entry);
unwire_end = src_entry->vme_end;
pmap_pageable(vm_map_pmap(src_map),
page_vaddr, unwire_end, TRUE);
}
vm_object_lock(src_object);
}
vm_page_lock_queues();
vm_page_remove(m);
if (m->wire_count > 0) {
m->wire_count = 0;
vm_page_wire_count--;
} else {
VM_PAGE_QUEUES_REMOVE(m);
}
vm_page_unlock_queues();
}
else {
vm_object_unlock(src_object);
vm_map_unlock(src_map);
vm_map_copy_steal_pages(copy);
vm_map_lock(src_map);
break;
}
vm_object_paging_end(src_object);
vm_object_unlock(src_object);
}
if (src_destroy) {
(void) vm_map_delete(src_map, src_start, src_end);
}
}
else {
if (src_destroy && !vm_map_copy_has_cont(copy)) {
cont_args = (vm_map_copyin_args_t)
kalloc(sizeof(vm_map_copyin_args_data_t));
vm_map_reference(src_map);
cont_args->map = src_map;
cont_args->src_addr = (vm_offset_t) 0;
cont_args->src_len = (vm_size_t) 0;
cont_args->destroy_addr = src_start;
cont_args->destroy_len = src_end - src_start;
cont_args->steal_pages = FALSE;
copy->cpy_cont_args = cont_args;
copy->cpy_cont = vm_map_copyin_page_list_cont;
}
}
vm_map_unlock(src_map);
*copy_result = copy;
return(result);
error:
vm_map_unlock(src_map);
vm_map_copy_discard(copy);
return(result);
}
vm_map_t vm_map_fork(vm_map_t old_map)
{
vm_map_t new_map;
vm_map_entry_t old_entry;
vm_map_entry_t new_entry;
pmap_t new_pmap = pmap_create((vm_size_t) 0);
vm_size_t new_size = 0;
vm_size_t entry_size;
vm_object_t object;
if (new_pmap == PMAP_NULL)
return VM_MAP_NULL;
vm_map_lock(old_map);
new_map = vm_map_create(new_pmap,
old_map->min_offset,
old_map->max_offset);
if (new_map == VM_MAP_NULL) {
pmap_destroy(new_pmap);
return VM_MAP_NULL;
}
for (
old_entry = vm_map_first_entry(old_map);
old_entry != vm_map_to_entry(old_map);
) {
if (old_entry->is_sub_map)
panic("vm_map_fork: encountered a submap");
entry_size = (old_entry->vme_end - old_entry->vme_start);
switch (old_entry->inheritance) {
case VM_INHERIT_NONE:
break;
case VM_INHERIT_SHARE:
object = old_entry->object.vm_object;
if (object == VM_OBJECT_NULL) {
object = vm_object_allocate(
(vm_size_t)(old_entry->vme_end -
old_entry->vme_start));
old_entry->offset = 0;
old_entry->object.vm_object = object;
assert(!old_entry->needs_copy);
}
else if (old_entry->needs_copy || object->shadowed ||
(object->temporary && !old_entry->is_shared &&
object->size > (vm_size_t)(old_entry->vme_end -
old_entry->vme_start))) {
assert(object->temporary);
assert(!(object->shadowed && old_entry->is_shared));
vm_object_shadow(
&old_entry->object.vm_object,
&old_entry->offset,
(vm_size_t) (old_entry->vme_end -
old_entry->vme_start));
if (!old_entry->needs_copy &&
(old_entry->protection & VM_PROT_WRITE)) {
pmap_protect(vm_map_pmap(old_map),
old_entry->vme_start,
old_entry->vme_end,
old_entry->protection &
~VM_PROT_WRITE);
}
old_entry->needs_copy = FALSE;
object = old_entry->object.vm_object;
}
vm_object_lock(object);
object->use_shared_copy = TRUE;
object->ref_count++;
vm_object_unlock(object);
new_entry = vm_map_entry_create(new_map);
if (old_entry->projected_on != 0) {
vm_map_entry_copy_full(new_entry, old_entry);
} else {
vm_map_entry_copy(new_entry, old_entry);
old_entry->is_shared = TRUE;
new_entry->is_shared = TRUE;
}
vm_map_entry_link(
new_map,
vm_map_last_entry(new_map),
new_entry);
pmap_copy(new_map->pmap, old_map->pmap,
new_entry->vme_start,
entry_size,
old_entry->vme_start);
new_size += entry_size;
break;
case VM_INHERIT_COPY:
if (old_entry->wired_count == 0) {
boolean_t src_needs_copy;
boolean_t new_entry_needs_copy;
new_entry = vm_map_entry_create(new_map);
vm_map_entry_copy(new_entry, old_entry);
if (vm_object_copy_temporary(
&new_entry->object.vm_object,
&new_entry->offset,
&src_needs_copy,
&new_entry_needs_copy)) {
if (src_needs_copy && !old_entry->needs_copy) {
vm_object_pmap_protect(
old_entry->object.vm_object,
old_entry->offset,
entry_size,
(old_entry->is_shared ?
PMAP_NULL :
old_map->pmap),
old_entry->vme_start,
old_entry->protection &
~VM_PROT_WRITE);
old_entry->needs_copy = TRUE;
}
new_entry->needs_copy = new_entry_needs_copy;
vm_map_entry_link(new_map,
vm_map_last_entry(new_map),
new_entry);
new_size += entry_size;
break;
}
vm_map_entry_dispose(new_map, new_entry);
}
{
vm_offset_t start = old_entry->vme_start;
vm_map_copy_t copy;
vm_map_entry_t last = vm_map_last_entry(new_map);
vm_map_unlock(old_map);
if (vm_map_copyin(old_map,
start,
entry_size,
FALSE,
&copy)
!= KERN_SUCCESS) {
vm_map_lock(old_map);
if (!vm_map_lookup_entry(old_map, start, &last))
last = last->vme_next;
old_entry = last;
continue;
}
vm_map_copy_insert(new_map, last, copy);
new_size += entry_size;
vm_map_lock(old_map);
start += entry_size;
if (!vm_map_lookup_entry(old_map, start, &last))
last = last->vme_next;
else
vm_map_clip_start(old_map, last, start);
old_entry = last;
continue;
}
}
old_entry = old_entry->vme_next;
}
new_map->size = new_size;
vm_map_unlock(old_map);
return(new_map);
}
kern_return_t vm_map_lookup(
vm_map_t *var_map,
vm_offset_t vaddr,
vm_prot_t fault_type,
boolean_t keep_map_locked,
vm_map_version_t *out_version,
vm_object_t *object,
vm_offset_t *offset,
vm_prot_t *out_prot,
boolean_t *wired)
{
vm_map_entry_t entry;
vm_map_t map = *var_map;
vm_prot_t prot;
RetryLookup: ;
vm_map_lock_read(map);
#define RETURN(why) \
MACRO_BEGIN \
if (!(keep_map_locked && (why == KERN_SUCCESS))) \
vm_map_unlock_read(map); \
return(why); \
MACRO_END
simple_lock(&map->hint_lock);
entry = map->hint;
simple_unlock(&map->hint_lock);
if ((entry == vm_map_to_entry(map)) ||
(vaddr < entry->vme_start) || (vaddr >= entry->vme_end)) {
vm_map_entry_t tmp_entry;
if (!vm_map_lookup_entry(map, vaddr, &tmp_entry))
RETURN(KERN_INVALID_ADDRESS);
entry = tmp_entry;
}
if (entry->is_sub_map) {
vm_map_t old_map = map;
*var_map = map = entry->object.sub_map;
vm_map_unlock_read(old_map);
goto RetryLookup;
}
prot = entry->protection;
if ((fault_type & (prot)) != fault_type) {
if ((prot & VM_PROT_NOTIFY) && (fault_type & VM_PROT_WRITE)) {
RETURN(KERN_WRITE_PROTECTION_FAILURE);
} else {
RETURN(KERN_PROTECTION_FAILURE);
}
}
if ((*wired = (entry->wired_count != 0)))
prot = fault_type = entry->protection;
if (entry->needs_copy) {
if (fault_type & VM_PROT_WRITE) {
if (vm_map_lock_read_to_write(map)) {
goto RetryLookup;
}
map->timestamp++;
vm_object_shadow(
&entry->object.vm_object,
&entry->offset,
(vm_size_t) (entry->vme_end - entry->vme_start));
entry->needs_copy = FALSE;
vm_map_lock_write_to_read(map);
}
else {
prot &= (~VM_PROT_WRITE);
}
}
if (entry->object.vm_object == VM_OBJECT_NULL) {
if (vm_map_lock_read_to_write(map)) {
goto RetryLookup;
}
entry->object.vm_object = vm_object_allocate(
(vm_size_t)(entry->vme_end - entry->vme_start));
entry->offset = 0;
vm_map_lock_write_to_read(map);
}
*offset = (vaddr - entry->vme_start) + entry->offset;
*object = entry->object.vm_object;
*out_prot = prot;
vm_object_lock(*object);
out_version->main_timestamp = map->timestamp;
RETURN(KERN_SUCCESS);
#undef RETURN
}
boolean_t vm_map_verify(
vm_map_t map,
vm_map_version_t *version)
{
boolean_t result;
vm_map_lock_read(map);
result = (map->timestamp == version->main_timestamp);
if (!result)
vm_map_unlock_read(map);
return(result);
}
kern_return_t vm_region(
vm_map_t map,
vm_offset_t *address,
vm_size_t *size,
vm_prot_t *protection,
vm_prot_t *max_protection,
vm_inherit_t *inheritance,
boolean_t *is_shared,
ipc_port_t *object_name,
vm_offset_t *offset_in_object)
{
vm_map_entry_t tmp_entry;
vm_map_entry_t entry;
vm_offset_t tmp_offset;
vm_offset_t start;
if (map == VM_MAP_NULL)
return(KERN_INVALID_ARGUMENT);
start = *address;
vm_map_lock_read(map);
if (!vm_map_lookup_entry(map, start, &tmp_entry)) {
if ((entry = tmp_entry->vme_next) == vm_map_to_entry(map)) {
vm_map_unlock_read(map);
return(KERN_NO_SPACE);
}
} else {
entry = tmp_entry;
}
start = entry->vme_start;
*protection = entry->protection;
*max_protection = entry->max_protection;
*inheritance = entry->inheritance;
*address = start;
*size = (entry->vme_end - start);
tmp_offset = entry->offset;
if (entry->is_sub_map) {
*is_shared = FALSE;
*object_name = IP_NULL;
*offset_in_object = tmp_offset;
} else {
*is_shared = entry->is_shared;
*object_name = vm_object_name(entry->object.vm_object);
*offset_in_object = tmp_offset;
}
vm_map_unlock_read(map);
return(KERN_SUCCESS);
}
kern_return_t
vm_region_create_proxy (task_t task, vm_address_t address,
vm_prot_t max_protection, vm_size_t len,
ipc_port_t *port)
{
kern_return_t ret;
vm_map_entry_t entry, tmp_entry;
vm_object_t object;
rpc_vm_offset_t rpc_offset, rpc_start;
rpc_vm_size_t rpc_len = (rpc_vm_size_t) len;
ipc_port_t pager;
if (task == TASK_NULL)
return(KERN_INVALID_ARGUMENT);
vm_map_lock_read(task->map);
if (!vm_map_lookup_entry(task->map, address, &tmp_entry)) {
if ((entry = tmp_entry->vme_next) == vm_map_to_entry(task->map)) {
vm_map_unlock_read(task->map);
return(KERN_NO_SPACE);
}
} else {
entry = tmp_entry;
}
if (entry->is_sub_map) {
vm_map_unlock_read(task->map);
return(KERN_INVALID_ARGUMENT);
}
if (len > entry->vme_end - entry->vme_start) {
vm_map_unlock_read(task->map);
return(KERN_INVALID_ARGUMENT);
}
max_protection &= entry->max_protection;
object = entry->object.vm_object;
vm_object_lock(object);
vm_object_pager_create(object);
pager = ipc_port_copy_send(object->pager);
vm_object_unlock(object);
rpc_start = (address - entry->vme_start) + entry->offset;
rpc_offset = 0;
vm_map_unlock_read(task->map);
ret = memory_object_create_proxy(task->itk_space, max_protection,
&pager, 1,
&rpc_offset, 1,
&rpc_start, 1,
&rpc_len, 1, port);
if (ret)
ipc_port_release_send(pager);
return ret;
}
boolean_t
vm_map_coalesce_entry(
vm_map_t map,
vm_map_entry_t entry)
{
vm_map_entry_t prev = entry->vme_prev;
vm_size_t prev_size;
vm_size_t entry_size;
if ((entry == vm_map_to_entry(map)) ||
(prev == vm_map_to_entry(map)) ||
(prev->vme_end != entry->vme_start) ||
(prev->is_shared || entry->is_shared) ||
(prev->is_sub_map || entry->is_sub_map) ||
(prev->inheritance != entry->inheritance) ||
(prev->protection != entry->protection) ||
(prev->max_protection != entry->max_protection) ||
(prev->needs_copy != entry->needs_copy) ||
(prev->in_transition || entry->in_transition) ||
(prev->wired_count != entry->wired_count) ||
(prev->projected_on != 0) ||
(entry->projected_on != 0))
return FALSE;
prev_size = prev->vme_end - prev->vme_start;
entry_size = entry->vme_end - entry->vme_start;
assert(prev->gap_size == 0);
if (!vm_object_coalesce(prev->object.vm_object,
entry->object.vm_object,
prev->offset,
entry->offset,
prev_size,
entry_size,
&prev->object.vm_object,
&prev->offset))
return FALSE;
if (map->hint == entry)
SAVE_HINT(map, prev);
if (map->first_free == entry)
map->first_free = prev;
prev->vme_end = entry->vme_end;
vm_map_entry_unlink(map, entry);
vm_map_entry_dispose(map, entry);
return TRUE;
}
boolean_t
vm_map_coalesce_entry_forward(
vm_map_t map,
vm_map_entry_t entry)
{
vm_map_entry_t next = entry->vme_next;
vm_size_t entry_size;
vm_size_t next_size;
if ((entry == vm_map_to_entry(map)) ||
(next == vm_map_to_entry(map)) ||
(entry->vme_end != next->vme_start) ||
(entry->is_shared || next->is_shared) ||
(entry->is_sub_map || next->is_sub_map) ||
(entry->inheritance != next->inheritance) ||
(entry->protection != next->protection) ||
(entry->max_protection != next->max_protection) ||
(entry->needs_copy != next->needs_copy) ||
(entry->in_transition || next->in_transition) ||
(entry->wired_count != next->wired_count) ||
(entry->projected_on != 0) ||
(next->projected_on != 0))
return FALSE;
entry_size = entry->vme_end - entry->vme_start;
next_size = next->vme_end - next->vme_start;
assert(entry->gap_size == 0);
if (!vm_object_coalesce(entry->object.vm_object,
next->object.vm_object,
entry->offset,
next->offset,
entry_size,
next_size,
&entry->object.vm_object,
&entry->offset))
return FALSE;
if (map->hint == next)
SAVE_HINT(map, entry);
if (map->first_free == next)
map->first_free = entry;
entry->vme_end = next->vme_end;
vm_map_entry_unlink(map, next);
vm_map_entry_dispose(map, next);
return TRUE;
}
boolean_t
vm_map_coalesce_entries(
vm_map_t map,
vm_map_entry_t entry)
{
boolean_t coalesced = FALSE;
if (vm_map_coalesce_entry_forward(map, entry)) {
coalesced = TRUE;
}
if (vm_map_coalesce_entry(map, entry)) {
coalesced = TRUE;
}
return coalesced;
}
kern_return_t vm_map_machine_attribute(
vm_map_t map,
vm_offset_t address,
vm_size_t size,
vm_machine_attribute_t attribute,
vm_machine_attribute_val_t* value)
{
kern_return_t ret;
if (address < vm_map_min(map) ||
(address + size) > vm_map_max(map))
return KERN_INVALID_ARGUMENT;
vm_map_lock(map);
ret = pmap_attribute(map->pmap, address, size, attribute, value);
vm_map_unlock(map);
return ret;
}
kern_return_t vm_map_msync(
vm_map_t map,
vm_offset_t address,
vm_size_t size,
vm_sync_t sync_flags)
{
if (map == VM_MAP_NULL)
return KERN_INVALID_ARGUMENT;
if ((sync_flags & (VM_SYNC_ASYNCHRONOUS | VM_SYNC_SYNCHRONOUS)) ==
(VM_SYNC_ASYNCHRONOUS | VM_SYNC_SYNCHRONOUS))
return KERN_INVALID_ARGUMENT;
size = round_page(address + size) - trunc_page(address);
address = trunc_page(address);
if (size == 0)
return KERN_SUCCESS;
return KERN_INVALID_ARGUMENT;
}
#if MACH_KDB
#define printf kdbprintf
void vm_map_print(db_expr_t addr, boolean_t have_addr, db_expr_t count, const char *modif)
{
vm_map_t map;
vm_map_entry_t entry;
if (!have_addr)
map = current_thread()->task->map;
else
map = (vm_map_t)addr;
iprintf("Map 0x%X: name=\"%s\", pmap=0x%X,",
(vm_offset_t) map, map->name, (vm_offset_t) (map->pmap));
printf("ref=%d,nentries=%d\n", map->ref_count, map->hdr.nentries);
printf("size=%lu,resident:%lu,wired=%lu\n", map->size,
pmap_resident_count(map->pmap) * PAGE_SIZE, map->size_wired);
printf("version=%d\n", map->timestamp);
indent += 1;
for (entry = vm_map_first_entry(map);
entry != vm_map_to_entry(map);
entry = entry->vme_next) {
static char *inheritance_name[3] = { "share", "copy", "none"};
iprintf("map entry 0x%X: ", (vm_offset_t) entry);
printf("start=0x%X, end=0x%X\n",
(vm_offset_t) entry->vme_start, (vm_offset_t) entry->vme_end);
iprintf("prot=%X/%X/%s, ",
entry->protection,
entry->max_protection,
inheritance_name[entry->inheritance]);
if (entry->wired_count != 0) {
printf("wired, ");
}
if (entry->in_transition) {
printf("in transition");
if (entry->needs_wakeup)
printf("(wake request)");
printf(", ");
}
if (entry->is_sub_map) {
printf("submap=0x%X, offset=0x%X\n",
(vm_offset_t) entry->object.sub_map,
(vm_offset_t) entry->offset);
} else {
printf("object=0x%X, offset=0x%X",
(vm_offset_t) entry->object.vm_object,
(vm_offset_t) entry->offset);
if (entry->is_shared)
printf(", shared");
if (entry->needs_copy)
printf(", copy needed");
printf("\n");
if ((entry->vme_prev == vm_map_to_entry(map)) ||
(entry->vme_prev->object.vm_object != entry->object.vm_object)) {
indent += 1;
vm_object_print_part(entry->object.vm_object, entry->offset, entry->vme_end - entry->vme_start);
indent -= 1;
}
}
}
indent -= 1;
}
void vm_map_copy_print(const vm_map_copy_t copy)
{
int i, npages;
printf("copy object 0x%x\n", copy);
indent += 1;
iprintf("type=%d", copy->type);
switch (copy->type) {
case VM_MAP_COPY_ENTRY_LIST:
printf("[entry_list]");
break;
case VM_MAP_COPY_OBJECT:
printf("[object]");
break;
case VM_MAP_COPY_PAGE_LIST:
printf("[page_list]");
break;
default:
printf("[bad type]");
break;
}
printf(", offset=0x%x", copy->offset);
printf(", size=0x%x\n", copy->size);
switch (copy->type) {
case VM_MAP_COPY_ENTRY_LIST:
break;
case VM_MAP_COPY_OBJECT:
iprintf("object=0x%x\n", copy->cpy_object);
break;
case VM_MAP_COPY_PAGE_LIST:
iprintf("npages=%d", copy->cpy_npages);
printf(", cont=%x", copy->cpy_cont);
printf(", cont_args=%x\n", copy->cpy_cont_args);
if (copy->cpy_npages < 0) {
npages = 0;
} else if (copy->cpy_npages > VM_MAP_COPY_PAGE_LIST_MAX) {
npages = VM_MAP_COPY_PAGE_LIST_MAX;
} else {
npages = copy->cpy_npages;
}
iprintf("copy->cpy_page_list[0..%d] = {", npages);
for (i = 0; i < npages - 1; i++) {
printf("0x%x, ", copy->cpy_page_list[i]);
}
if (npages > 0) {
printf("0x%x", copy->cpy_page_list[npages - 1]);
}
printf("}\n");
break;
}
indent -= 1;
}
#endif