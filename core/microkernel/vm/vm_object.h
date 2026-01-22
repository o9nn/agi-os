#ifndef _VM_VM_OBJECT_H_
#define _VM_VM_OBJECT_H_
#include <sys/types.h>
#include <mach/kern_return.h>
#include <mach/boolean.h>
#include <mach/memory_object.h>
#include <mach/port.h>
#include <mach/vm_prot.h>
#include <mach/machine/vm_types.h>
#include <kern/queue.h>
#include <kern/lock.h>
#include <kern/assert.h>
#include <kern/debug.h>
#include <kern/macros.h>
#include <vm/pmap.h>
#include <ipc/ipc_types.h>
typedef struct vm_object_memory_stats vm_object_memory_stats_t;
#if MACH_PAGEMAP
#include <vm/vm_external.h>
#endif
struct block_cache;
typedef struct block_cache *block_cache_t;
typedef struct ipc_port * pager_request_t;
#define PAGER_REQUEST_NULL ((pager_request_t) 0)
struct vm_object {
queue_head_t memq;
decl_simple_lock_data(, Lock)
#if VM_OBJECT_DEBUG
thread_t LockHolder;
#endif
vm_size_t size;
int ref_count;
unsigned long resident_page_count;
struct vm_object *copy;
struct vm_object *shadow;
vm_offset_t shadow_offset;
struct ipc_port *pager;
vm_offset_t paging_offset;
pager_request_t pager_request;
struct ipc_port *pager_name;
memory_object_copy_strategy_t
copy_strategy;
unsigned int
absent_count;
unsigned int
all_wanted;
unsigned int
paging_in_progress:16,
used_for_pageout:1,
pager_created:1,
pager_initialized:1,
pager_ready:1,
can_persist:1,
internal:1,
temporary:1,
alive:1,
lock_in_progress : 1,
lock_restart : 1,
use_shared_copy : 1,
shadowed: 1,
cached: 1;
queue_chain_t cached_list;
vm_offset_t last_alloc;
vm_offset_t readahead_next;
unsigned int readahead_count;
unsigned int readahead_window;
block_cache_t block_cache;
boolean_t block_cache_enabled;
#if MACH_PAGEMAP
vm_external_t existence_info;
#endif
};
extern
vm_object_t kernel_object;
extern void vm_object_bootstrap(void);
extern void vm_object_init(void);
extern void vm_object_collect(vm_object_t);
extern void vm_object_terminate(vm_object_t);
extern vm_object_t vm_object_allocate(vm_size_t);
extern void vm_object_reference(vm_object_t);
extern void vm_object_deallocate(vm_object_t);
extern void vm_object_pmap_protect(
vm_object_t object,
vm_offset_t offset,
vm_size_t size,
pmap_t pmap,
vm_offset_t pmap_start,
vm_prot_t prot);
extern void vm_object_pmap_remove(
vm_object_t object,
vm_offset_t start,
vm_offset_t end);
extern void vm_object_page_remove(
vm_object_t object,
vm_offset_t start,
vm_offset_t end);
extern void vm_object_shadow(
vm_object_t *object,
vm_offset_t *offset,
vm_size_t length);
extern void vm_object_collapse(vm_object_t);
extern vm_object_t vm_object_lookup(struct ipc_port *);
extern vm_object_t vm_object_lookup_name(struct ipc_port *);
extern struct ipc_port *vm_object_name(vm_object_t);
extern void vm_object_remove(vm_object_t);
extern boolean_t vm_object_copy_temporary(
vm_object_t *_object,
vm_offset_t *_offset,
boolean_t *_src_needs_copy,
boolean_t *_dst_needs_copy);
extern kern_return_t vm_object_copy_strategically(
vm_object_t src_object,
vm_offset_t src_offset,
vm_size_t size,
vm_object_t *dst_object,
vm_offset_t *dst_offset,
boolean_t *dst_needs_copy);
extern kern_return_t vm_object_copy_slowly(
vm_object_t src_object,
vm_offset_t src_offset,
vm_size_t size,
boolean_t interruptible,
vm_object_t *_result_object);
extern vm_object_t vm_object_enter(
struct ipc_port *pager,
vm_size_t size,
boolean_t internal);
extern void vm_object_pager_create(
vm_object_t object);
extern void vm_object_destroy(
struct ipc_port *pager);
extern kern_return_t vm_object_page_map(
vm_object_t,
vm_offset_t,
vm_size_t,
phys_addr_t (*)(void *, vm_offset_t),
void *);
extern vm_object_t vm_object_request_object(struct ipc_port *);
extern boolean_t vm_object_coalesce(
vm_object_t prev_object,
vm_object_t next_object,
vm_offset_t prev_offset,
vm_offset_t next_offset,
vm_size_t prev_size,
vm_size_t next_size,
vm_object_t *new_object,
vm_offset_t *new_offset);
extern void vm_object_pager_wakeup(ipc_port_t pager);
void memory_object_release(
ipc_port_t pager,
pager_request_t pager_request,
ipc_port_t pager_name);
void vm_object_deactivate_pages(vm_object_t);
vm_object_t vm_object_copy_delayed(
vm_object_t src_object);
boolean_t vm_object_verify_resident_count(vm_object_t object);
void vm_object_increment_resident_count(vm_object_t object);
void vm_object_decrement_resident_count(vm_object_t object);
kern_return_t vm_object_get_memory_stats(vm_object_t object,
vm_object_memory_stats_t *stats);
#define VM_OBJECT_EVENT_INITIALIZED 0
#define VM_OBJECT_EVENT_PAGER_READY 1
#define VM_OBJECT_EVENT_PAGING_IN_PROGRESS 2
#define VM_OBJECT_EVENT_ABSENT_COUNT 3
#define VM_OBJECT_EVENT_LOCK_IN_PROGRESS 4
#define vm_object_wait(object, event, interruptible) \
MACRO_BEGIN \
(object)->all_wanted |= 1 << (event); \
vm_object_sleep(((vm_offset_t) object) + (event), \
(object), \
(interruptible)); \
MACRO_END
#define vm_object_assert_wait(object, event, interruptible) \
MACRO_BEGIN \
(object)->all_wanted |= 1 << (event); \
assert_wait((event_t)(((vm_offset_t) object) + (event)), (interruptible)); \
MACRO_END
#define vm_object_wakeup(object, event) \
MACRO_BEGIN \
if ((object)->all_wanted & (1 << (event))) \
thread_wakeup((event_t)(((vm_offset_t) object) + (event))); \
(object)->all_wanted &= ~(1 << (event)); \
MACRO_END
#define vm_object_collectable(object) \
(((object)->ref_count == 0) \
&& ((object)->resident_page_count == 0))
#define vm_object_paging_begin(object) \
((object)->paging_in_progress++)
#define vm_object_paging_end(object) \
MACRO_BEGIN \
assert((object)->paging_in_progress != 0); \
if (--(object)->paging_in_progress == 0) { \
vm_object_wakeup(object, \
VM_OBJECT_EVENT_PAGING_IN_PROGRESS); \
} \
MACRO_END
#define vm_object_paging_wait(object, interruptible) \
MACRO_BEGIN \
while ((object)->paging_in_progress != 0) { \
vm_object_wait( (object), \
VM_OBJECT_EVENT_PAGING_IN_PROGRESS, \
(interruptible)); \
vm_object_lock(object); \
\
\
\
\
} \
MACRO_END
#define vm_object_absent_assert_wait(object, interruptible) \
MACRO_BEGIN \
vm_object_assert_wait( (object), \
VM_OBJECT_EVENT_ABSENT_COUNT, \
(interruptible)); \
MACRO_END
#define vm_object_absent_release(object) \
MACRO_BEGIN \
(object)->absent_count--; \
vm_object_wakeup((object), \
VM_OBJECT_EVENT_ABSENT_COUNT); \
MACRO_END
#if VM_OBJECT_DEBUG
#define vm_object_lock_init(object) \
MACRO_BEGIN \
simple_lock_init(&(object)->Lock); \
(object)->LockHolder = 0; \
MACRO_END
#define vm_object_lock(object) \
MACRO_BEGIN \
simple_lock(&(object)->Lock); \
(object)->LockHolder = current_thread(); \
MACRO_END
#define vm_object_unlock(object) \
MACRO_BEGIN \
if ((object)->LockHolder != current_thread()) \
panic("vm_object_unlock 0x%x", (object)); \
(object)->LockHolder = 0; \
simple_unlock(&(object)->Lock); \
MACRO_END
#define vm_object_lock_try(object) \
(simple_lock_try(&(object)->Lock) \
? ( ((object)->LockHolder = current_thread()) , TRUE) \
: FALSE)
#define vm_object_sleep(event, object, interruptible) \
MACRO_BEGIN \
if ((object)->LockHolder != current_thread()) \
panic("vm_object_sleep %#x", (object)); \
(object)->LockHolder = 0; \
thread_sleep((event_t)(event), simple_lock_addr((object)->Lock), \
(interruptible)); \
MACRO_END
#define vm_object_lock_taken(object) \
((object)->LockHolder == current_thread())
#else
#define vm_object_lock_init(object) simple_lock_init(&(object)->Lock)
#define vm_object_lock(object) simple_lock(&(object)->Lock)
#define vm_object_unlock(object) simple_unlock(&(object)->Lock)
#define vm_object_lock_try(object) simple_lock_try(&(object)->Lock)
#define vm_object_sleep(event, object, interruptible) \
thread_sleep((event_t)(event), simple_lock_addr((object)->Lock), \
(interruptible))
#define vm_object_lock_taken(object) simple_lock_taken(&(object)->Lock)
#endif
extern int vm_object_external_count;
extern int vm_object_external_pages;
static inline int
vm_object_reference_locked (vm_object_t obj)
{
return (++obj->ref_count);
}
static inline int
vm_object_unreference_locked (vm_object_t obj)
{
return (--obj->ref_count);
}
#endif