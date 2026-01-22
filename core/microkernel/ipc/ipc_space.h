#ifndef	_IPC_IPC_SPACE_H_
#define _IPC_IPC_SPACE_H_
#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/mach_types.h>
#include <machine/vm_param.h>
#include <kern/macros.h>
#include <kern/lock.h>
#include <kern/rdxtree.h>
#include <kern/slab.h>
#include <kern/printf.h>
#include <ipc/ipc_entry.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_types.h>
typedef unsigned int ipc_space_refs_t;
struct ipc_space {
decl_simple_lock_data(,is_ref_lock_data)
ipc_space_refs_t is_references;
struct lock is_lock_data;
boolean_t is_active;
struct rdxtree is_map;
size_t is_size;
struct rdxtree is_reverse_map;
ipc_entry_t is_free_list;
size_t is_free_list_size;
#define IS_FREE_LIST_SIZE_LIMIT	64
};
#define	IS_NULL			((ipc_space_t) 0)
extern struct kmem_cache ipc_space_cache;
#define is_alloc()		((ipc_space_t) kmem_cache_alloc(&ipc_space_cache))
#define	is_free(is)		kmem_cache_free(&ipc_space_cache, (vm_offset_t) (is))
extern struct ipc_space *ipc_space_kernel;
extern struct ipc_space *ipc_space_reply;
#define	is_ref_lock_init(is)	simple_lock_init(&(is)->is_ref_lock_data)
#define	ipc_space_reference_macro(is)					\
MACRO_BEGIN								\
simple_lock(&(is)->is_ref_lock_data);				\
assert((is)->is_references > 0);				\
(is)->is_references++;						\
simple_unlock(&(is)->is_ref_lock_data);				\
MACRO_END
#define	ipc_space_release_macro(is)					\
MACRO_BEGIN								\
ipc_space_refs_t _refs;						\
\
simple_lock(&(is)->is_ref_lock_data);				\
assert((is)->is_references > 0);				\
_refs = --(is)->is_references;					\
simple_unlock(&(is)->is_ref_lock_data);				\
\
if (_refs == 0)							\
is_free(is);						\
MACRO_END
#define	is_lock_init(is)	lock_init(&(is)->is_lock_data, TRUE)
#define	is_read_lock(is)	lock_read(&(is)->is_lock_data)
#define is_read_unlock(is)	lock_done(&(is)->is_lock_data)
#define	is_write_lock(is)	lock_write(&(is)->is_lock_data)
#define	is_write_lock_try(is)	lock_try_write(&(is)->is_lock_data)
#define is_write_unlock(is)	lock_done(&(is)->is_lock_data)
#define	is_write_to_read_lock(is) lock_write_to_read(&(is)->is_lock_data)
extern void ipc_space_reference(struct ipc_space *space);
extern void ipc_space_release(struct ipc_space *space);
#define	is_reference(is)	ipc_space_reference_macro(is)
#define	is_release(is)		ipc_space_release_macro(is)
kern_return_t	ipc_space_create(ipc_space_t *);
kern_return_t	ipc_space_create_special(struct ipc_space **);
void		ipc_space_destroy(struct ipc_space *);
static inline ipc_entry_t
ipc_entry_lookup(
ipc_space_t space,
mach_port_name_t name)
{
ipc_entry_t entry;
assert(space->is_active);
entry = rdxtree_lookup(&space->is_map, (rdxtree_key_t) name);
if (entry != IE_NULL
&& IE_BITS_TYPE(entry->ie_bits) == MACH_PORT_TYPE_NONE)
entry = NULL;
assert((entry == IE_NULL) || IE_BITS_TYPE(entry->ie_bits));
return entry;
}
extern volatile boolean_t mach_port_deallocate_debug;
#define ipc_entry_lookup_failed(msg, port_name)				\
MACRO_BEGIN								\
if (MACH_PORT_NAME_VALID(port_name)) {				\
printf("task %.*s looked up a bogus port %lu for %d, "	\
"most probably a bug.\n",			\
(int) sizeof current_task()->name, 		\
current_task()->name,				\
(unsigned long) (port_name),			\
(msg)->msgh_id);				\
if (mach_port_deallocate_debug)				\
SoftDebugger("ipc_entry_lookup");		\
}								\
MACRO_END
static inline kern_return_t
ipc_entry_get(
ipc_space_t space,
mach_port_name_t *namep,
ipc_entry_t *entryp)
{
mach_port_name_t new_name;
ipc_entry_t free_entry;
assert(space->is_active);
free_entry = space->is_free_list;
if (free_entry == IE_NULL)
return KERN_NO_SPACE;
space->is_free_list = free_entry->ie_next_free;
space->is_free_list_size -= 1;
{
mach_port_gen_t gen;
assert((free_entry->ie_bits &~ IE_BITS_GEN_MASK) == 0);
gen = free_entry->ie_bits + IE_BITS_GEN_ONE;
free_entry->ie_bits = gen;
free_entry->ie_request = 0;
new_name = MACH_PORT_MAKE(free_entry->ie_name, gen);
}
assert(MACH_PORT_NAME_VALID(new_name));
assert(free_entry->ie_object == IO_NULL);
space->is_size += 1;
*namep = new_name;
*entryp = free_entry;
return KERN_SUCCESS;
}
static inline void
ipc_entry_dealloc(
ipc_space_t	space,
mach_port_name_t	name,
ipc_entry_t	entry)
{
assert(space->is_active);
assert(entry->ie_object == IO_NULL);
assert(entry->ie_request == 0);
if (space->is_free_list_size < IS_FREE_LIST_SIZE_LIMIT) {
space->is_free_list_size += 1;
entry->ie_bits &= IE_BITS_GEN_MASK;
entry->ie_next_free = space->is_free_list;
space->is_free_list = entry;
} else {
rdxtree_remove(&space->is_map, (rdxtree_key_t) name);
ie_free(entry);
}
space->is_size -= 1;
}
#define KEY(X)								\
({								\
assert((((unsigned long) (X)) & 0x07) == 0);		\
((unsigned long long)					\
(((unsigned long) (X) - VM_MIN_KERNEL_ADDRESS) >> 3));	\
})
static inline kern_return_t
ipc_reverse_insert(ipc_space_t space,
ipc_object_t obj,
ipc_entry_t entry)
{
assert(space != IS_NULL);
assert(obj != IO_NULL);
return (kern_return_t) rdxtree_insert(&space->is_reverse_map,
KEY(obj), entry);
}
static inline ipc_entry_t
ipc_reverse_remove(ipc_space_t space,
ipc_object_t obj)
{
assert(space != IS_NULL);
assert(obj != IO_NULL);
return rdxtree_remove(&space->is_reverse_map, KEY(obj));
}
static inline void
ipc_reverse_remove_all(ipc_space_t space)
{
assert(space != IS_NULL);
rdxtree_remove_all(&space->is_reverse_map);
assert(space->is_reverse_map.height == 0);
assert(space->is_reverse_map.root == NULL);
}
static inline ipc_entry_t
ipc_reverse_lookup(ipc_space_t space,
ipc_object_t obj)
{
assert(space != IS_NULL);
assert(obj != IO_NULL);
return rdxtree_lookup(&space->is_reverse_map, KEY(obj));
}
#undef KEY
#endif