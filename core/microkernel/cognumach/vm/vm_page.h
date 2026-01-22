#ifndef _VM_VM_PAGE_H_
#define _VM_VM_PAGE_H_
#include <mach/boolean.h>
#include <mach/vm_prot.h>
#include <machine/vm_param.h>
#include <vm/vm_object.h>
#include <vm/vm_types.h>
#include <kern/queue.h>
#include <kern/list.h>
#include <kern/lock.h>
#include <kern/log2.h>
#include <kern/macros.h>
#include <kern/sched_prim.h>
#if MACH_VM_DEBUG
#include <mach_debug/hash_info.h>
#endif
struct vm_page {
struct list node;
void *priv;
phys_addr_t phys_addr;
queue_chain_t listq;
struct vm_page *next;
struct {} vm_page_header;
vm_object_t object;
vm_offset_t offset;
unsigned int wire_count:15,
inactive:1,
active:1,
laundry:1,
external_laundry:1,
free:1,
reference:1,
external:1,
busy:1,
wanted:1,
tabled:1,
fictitious:1,
private:1,
absent:1,
error:1,
dirty:1,
precious:1,
overwriting:1;
vm_prot_t page_lock:3;
vm_prot_t unlock_request:3;
unsigned char access_frequency:4;
unsigned char aging_time:4;
struct {} vm_page_footer;
unsigned short type:2;
unsigned short seg_index:2;
unsigned short order:4;
};
#define VM_PAGE_BODY_SIZE \
(offsetof(struct vm_page, vm_page_footer) \
- offsetof(struct vm_page, vm_page_header))
#define VM_PAGE_CHECK(mem) vm_page_check(mem)
void vm_page_check(const struct vm_page *page);
#define VM_PAGE_DMA 0x01
#if defined(VM_PAGE_DMA32_LIMIT) && VM_PAGE_DMA32_LIMIT > VM_PAGE_DIRECTMAP_LIMIT
#define VM_PAGE_DIRECTMAP 0x02
#define VM_PAGE_DMA32 0x04
#else
#define VM_PAGE_DMA32 0x02
#define VM_PAGE_DIRECTMAP 0x04
#endif
#define VM_PAGE_HIGHMEM 0x08
extern
int vm_page_fictitious_count;
extern
int vm_page_active_count;
extern
int vm_page_inactive_count;
extern
int vm_page_wire_count;
extern
int vm_page_laundry_count;
extern
int vm_page_external_laundry_count;
decl_simple_lock_data(extern,vm_page_queue_lock)
decl_simple_lock_data(extern,vm_page_queue_free_lock)
extern phys_addr_t vm_page_fictitious_addr;
extern void vm_page_bootstrap(
vm_offset_t *startp,
vm_offset_t *endp);
extern void vm_page_module_init(void);
extern vm_page_t vm_page_lookup(
vm_object_t object,
vm_offset_t offset);
extern vm_page_t vm_page_grab_fictitious(void);
extern boolean_t vm_page_convert(vm_page_t *);
extern void vm_page_more_fictitious(void);
extern vm_page_t vm_page_grab(unsigned flags);
extern void vm_page_release(vm_page_t, boolean_t, boolean_t);
extern phys_addr_t vm_page_grab_phys_addr(void);
extern vm_page_t vm_page_grab_contig(vm_size_t, unsigned int);
extern void vm_page_free_contig(vm_page_t, vm_size_t);
extern void vm_page_wait(void (*)(void));
extern vm_page_t vm_page_alloc(
vm_object_t object,
vm_offset_t offset);
extern void vm_page_init(
vm_page_t mem);
extern void vm_page_free(vm_page_t);
extern void vm_page_activate(vm_page_t);
extern void vm_page_deactivate(vm_page_t);
extern void vm_page_rename(
vm_page_t mem,
vm_object_t new_object,
vm_offset_t new_offset);
extern void vm_page_insert(
vm_page_t mem,
vm_object_t object,
vm_offset_t offset);
extern void vm_page_remove(
vm_page_t mem);
extern void vm_page_zero_fill(vm_page_t);
extern void vm_page_copy(vm_page_t src_m, vm_page_t dest_m);
extern void vm_page_wire(vm_page_t);
extern void vm_page_unwire(vm_page_t);
#if MACH_VM_DEBUG
extern unsigned int vm_page_info(
hash_info_bucket_t *info,
unsigned int count);
#endif
#define PAGE_ASSERT_WAIT(m, interruptible) \
MACRO_BEGIN \
(m)->wanted = TRUE; \
assert_wait((event_t) (m), (interruptible)); \
MACRO_END
#define PAGE_WAKEUP_DONE(m) \
MACRO_BEGIN \
(m)->busy = FALSE; \
if ((m)->wanted) { \
(m)->wanted = FALSE; \
thread_wakeup(((event_t) m)); \
} \
MACRO_END
#define PAGE_WAKEUP(m) \
MACRO_BEGIN \
if ((m)->wanted) { \
(m)->wanted = FALSE; \
thread_wakeup((event_t) (m)); \
} \
MACRO_END
#define VM_PAGE_FREE(p) \
MACRO_BEGIN \
vm_page_lock_queues(); \
vm_page_free(p); \
vm_page_unlock_queues(); \
MACRO_END
#define PMAP_ENTER(pmap, virtual_address, page, protection, wired) \
MACRO_BEGIN \
pmap_enter( \
(pmap), \
(virtual_address), \
(page)->phys_addr, \
(protection) & ~(page)->page_lock, \
(wired) \
); \
MACRO_END
#define VM_PAGE_WAIT(continuation) vm_page_wait(continuation)
#define vm_page_lock_queues() simple_lock(&vm_page_queue_lock)
#define vm_page_unlock_queues() simple_unlock(&vm_page_queue_lock)
#define vm_page_locked_queues() simple_lock_taken(&vm_page_queue_lock)
#define VM_PAGE_QUEUES_REMOVE(mem) vm_page_queues_remove(mem)
#define vm_page_atop(addr) ((addr) >> PAGE_SHIFT)
#define vm_page_ptoa(page) ((page) << PAGE_SHIFT)
#define vm_page_trunc(addr) P2ALIGN(addr, PAGE_SIZE)
#define vm_page_round(addr) P2ROUND(addr, PAGE_SIZE)
#define vm_page_aligned(addr) P2ALIGNED(addr, PAGE_SIZE)
#define VM_PAGE_SEL_DMA 0
#if defined(VM_PAGE_DMA32_LIMIT) && VM_PAGE_DMA32_LIMIT > VM_PAGE_DIRECTMAP_LIMIT
#define VM_PAGE_SEL_DIRECTMAP 1
#define VM_PAGE_SEL_DMA32 2
#else
#define VM_PAGE_SEL_DMA32 1
#define VM_PAGE_SEL_DIRECTMAP 2
#endif
#define VM_PAGE_SEL_HIGHMEM 3
#define VM_PT_FREE 0
#define VM_PT_RESERVED 1
#define VM_PT_TABLE 2
#define VM_PT_KERNEL 3
static inline unsigned short
vm_page_type(const struct vm_page *page)
{
return page->type;
}
void vm_page_set_type(struct vm_page *page, unsigned int order,
unsigned short type);
static inline unsigned int
vm_page_order(size_t size)
{
return iorder2(vm_page_atop(vm_page_round(size)));
}
static inline phys_addr_t
vm_page_to_pa(const struct vm_page *page)
{
return page->phys_addr;
}
static inline void
vm_page_set_priv(struct vm_page *page, void *priv)
{
page->priv = priv;
}
static inline void *
vm_page_get_priv(const struct vm_page *page)
{
return page->priv;
}
void vm_page_load(unsigned int seg_index, phys_addr_t start, phys_addr_t end);
void vm_page_load_heap(unsigned int seg_index, phys_addr_t start,
phys_addr_t end);
int vm_page_ready(void);
phys_addr_t vm_page_bootalloc(size_t size);
void vm_page_setup(void);
void vm_page_manage(struct vm_page *page);
struct vm_page * vm_page_lookup_pa(phys_addr_t pa);
struct vm_page * vm_page_alloc_pa(unsigned int order, unsigned int selector,
unsigned short type);
void vm_page_free_pa(struct vm_page *page, unsigned int order);
const char * vm_page_seg_name(unsigned int seg_index);
void vm_page_info_all(void);
phys_addr_t vm_page_seg_end(unsigned int selector);
unsigned long vm_page_table_size(void);
unsigned long vm_page_table_index(phys_addr_t pa);
phys_addr_t vm_page_mem_size(void);
unsigned long vm_page_mem_free(void);
void vm_page_queues_remove(struct vm_page *page);
boolean_t vm_page_balance(void);
boolean_t vm_page_evict(boolean_t *should_wait);
void vm_page_refill_inactive(void);
void db_show_vmstat(void);
#endif