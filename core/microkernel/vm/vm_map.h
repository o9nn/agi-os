#ifndef	_VM_VM_MAP_H_
#define _VM_VM_MAP_H_
#include <mach/kern_return.h>
#include <mach/boolean.h>
#include <mach/machine/vm_types.h>
#include <mach/vm_attributes.h>
#include <mach/vm_prot.h>
#include <mach/vm_inherit.h>
#include <mach/vm_wire.h>
#include <mach/vm_sync.h>
#include <vm/pmap.h>
#include <vm/vm_object.h>
#include <vm/vm_page.h>
#include <vm/vm_types.h>
#include <kern/list.h>
#include <kern/lock.h>
#include <kern/rbtree.h>
#include <kern/macros.h>
#define KENTRY_DATA_SIZE (256*PAGE_SIZE)
#define VM_MAP_ASLR_DEFAULT_ENTROPY_BITS	8
#define VM_MAP_ASLR_MAX_ENTROPY_BITS		16
#define VM_MAP_ASLR_MIN_ENTROPY_BITS		4
#define VM_MAP_LARGE_PAGE_SIZE			(2 * 1024 * 1024)
#define VM_MAP_HUGE_PAGE_SIZE			(1024 * 1024 * 1024)
#define VM_MAP_PREFER_HIGH_THRESHOLD		(128 * 1024 * 1024)
typedef union vm_map_object {
struct vm_object	*vm_object;
struct vm_map		*sub_map;
} vm_map_object_t;
struct vm_map_links {
struct vm_map_entry	*prev;
struct vm_map_entry	*next;
vm_offset_t		start;
vm_offset_t		end;
};
struct vm_map_entry {
struct vm_map_links	links;
#define vme_prev		links.prev
#define vme_next		links.next
#define vme_start		links.start
#define vme_end			links.end
struct rbtree_node	tree_node;
struct rbtree_node	gap_node;
struct list		gap_list;
vm_size_t		gap_size;
union vm_map_object	object;
vm_offset_t		offset;
unsigned int
in_gap_tree:1,
is_shared:1,
is_sub_map:1,
in_transition:1,
needs_wakeup:1,
needs_copy:1;
vm_prot_t		protection;
vm_prot_t		max_protection;
vm_inherit_t		inheritance;
unsigned short		wired_count;
vm_prot_t		wired_access;
struct vm_map_entry     *projected_on;
};
typedef struct vm_map_entry	*vm_map_entry_t;
#define VM_MAP_ENTRY_NULL	((vm_map_entry_t) 0)
struct vm_map_header {
struct vm_map_links	links;
struct rbtree		tree;
struct rbtree		gap_tree;
int			nentries;
};
struct vm_map {
lock_data_t		lock;
struct vm_map_header	hdr;
#define min_offset		hdr.links.start
#define max_offset		hdr.links.end
pmap_t			pmap;
vm_size_t		size;
vm_size_t		size_wired;
int			ref_count;
decl_simple_lock_data(,	ref_lock)
vm_map_entry_t		hint;
decl_simple_lock_data(,	hint_lock)
vm_map_entry_t		first_free;
unsigned int	wait_for_space:1,
wiring_required:1,
aslr_enabled:1,
prefer_high_addr:1;
unsigned int		timestamp;
unsigned int		aslr_entropy_bits;
const char		*name;
};
#define vm_map_to_entry(map)	((struct vm_map_entry *) &(map)->hdr.links)
#define vm_map_first_entry(map)	((map)->hdr.links.next)
#define vm_map_last_entry(map)	((map)->hdr.links.prev)
typedef struct vm_map_version {
unsigned int	main_timestamp;
} vm_map_version_t;
#define VM_MAP_COPY_PAGE_LIST_MAX	64
struct vm_map_copy;
struct vm_map_copyin_args_data;
typedef kern_return_t (*vm_map_copy_cont_fn)(struct vm_map_copyin_args_data*, struct vm_map_copy**);
typedef struct vm_map_copy {
int			type;
#define VM_MAP_COPY_ENTRY_LIST	1
#define VM_MAP_COPY_OBJECT	2
#define VM_MAP_COPY_PAGE_LIST	3
vm_offset_t		offset;
vm_size_t		size;
union {
struct vm_map_header	hdr;
struct {
vm_object_t		object;
} c_o;
struct {
vm_page_t		page_list[VM_MAP_COPY_PAGE_LIST_MAX];
int			npages;
vm_map_copy_cont_fn cont;
struct vm_map_copyin_args_data* cont_args;
} c_p;
} c_u;
} *vm_map_copy_t;
#define cpy_hdr			c_u.hdr
#define cpy_object		c_u.c_o.object
#define cpy_page_list		c_u.c_p.page_list
#define cpy_npages		c_u.c_p.npages
#define cpy_cont		c_u.c_p.cont
#define cpy_cont_args		c_u.c_p.cont_args
#define	VM_MAP_COPY_NULL	((vm_map_copy_t) 0)
#define vm_map_copy_to_entry(copy)		\
((struct vm_map_entry *) &(copy)->cpy_hdr.links)
#define vm_map_copy_first_entry(copy)		\
((copy)->cpy_hdr.links.next)
#define vm_map_copy_last_entry(copy)		\
((copy)->cpy_hdr.links.prev)
#define	vm_map_copy_invoke_cont(old_copy, new_copy, result)		\
MACRO_BEGIN								\
vm_map_copy_page_discard(old_copy);				\
*result = (*((old_copy)->cpy_cont))((old_copy)->cpy_cont_args,	\
new_copy);			\
(old_copy)->cpy_cont = (vm_map_copy_cont_fn) 0;			\
MACRO_END
#define	vm_map_copy_invoke_extend_cont(old_copy, new_copy, result)	\
MACRO_BEGIN								\
*result = (*((old_copy)->cpy_cont))((old_copy)->cpy_cont_args,	\
new_copy);			\
(old_copy)->cpy_cont = (vm_map_copy_cont_fn) 0;			\
MACRO_END
#define vm_map_copy_abort_cont(old_copy)				\
MACRO_BEGIN								\
vm_map_copy_page_discard(old_copy);				\
(*((old_copy)->cpy_cont))((old_copy)->cpy_cont_args,		\
(vm_map_copy_t *) 0);			\
(old_copy)->cpy_cont = (vm_map_copy_cont_fn) 0;			\
(old_copy)->cpy_cont_args = VM_MAP_COPYIN_ARGS_NULL;		\
MACRO_END
#define vm_map_copy_has_cont(copy)					\
(((copy)->cpy_cont) != (vm_map_copy_cont_fn) 0)
typedef	struct vm_map_copyin_args_data {
vm_map_t	map;
vm_offset_t	src_addr;
vm_size_t	src_len;
vm_offset_t	destroy_addr;
vm_size_t	destroy_len;
boolean_t	steal_pages;
} vm_map_copyin_args_data_t, *vm_map_copyin_args_t;
#define	VM_MAP_COPYIN_ARGS_NULL	((vm_map_copyin_args_t) 0)
#define vm_map_lock_init(map)			\
MACRO_BEGIN					\
lock_init(&(map)->lock, TRUE);		\
(map)->timestamp = 0;			\
MACRO_END
void vm_map_lock(struct vm_map *map);
void vm_map_unlock(struct vm_map *map);
#define vm_map_lock_read(map)	lock_read(&(map)->lock)
#define vm_map_unlock_read(map)	lock_read_done(&(map)->lock)
#define vm_map_lock_write_to_read(map) \
lock_write_to_read(&(map)->lock)
#define vm_map_lock_read_to_write(map) \
(lock_read_to_write(&(map)->lock) || (((map)->timestamp++), 0))
#define vm_map_lock_set_recursive(map) \
lock_set_recursive(&(map)->lock)
#define vm_map_lock_clear_recursive(map) \
lock_clear_recursive(&(map)->lock)
extern void		vm_map_init(void);
extern void		vm_map_setup(vm_map_t, pmap_t, vm_offset_t, vm_offset_t);
extern vm_map_t		vm_map_create(pmap_t, vm_offset_t, vm_offset_t);
extern vm_map_t		vm_map_fork(vm_map_t);
extern void		vm_map_reference(vm_map_t);
extern void		vm_map_deallocate(vm_map_t);
extern kern_return_t	vm_map_enter(vm_map_t, vm_offset_t *, vm_size_t,
vm_offset_t, boolean_t, vm_object_t,
vm_offset_t, boolean_t, vm_prot_t,
vm_prot_t, vm_inherit_t);
extern kern_return_t	vm_map_find_entry(vm_map_t, vm_offset_t *, vm_size_t,
vm_offset_t, vm_object_t,
vm_map_entry_t *);
extern kern_return_t	vm_map_remove(vm_map_t, vm_offset_t, vm_offset_t);
extern kern_return_t	vm_map_protect(vm_map_t, vm_offset_t, vm_offset_t,
vm_prot_t, boolean_t);
extern kern_return_t	vm_map_inherit(vm_map_t, vm_offset_t, vm_offset_t,
vm_inherit_t);
extern kern_return_t	vm_map_lookup(vm_map_t *, vm_offset_t, vm_prot_t, boolean_t,
vm_map_version_t *, vm_object_t *,
vm_offset_t *, vm_prot_t *, boolean_t *);
extern boolean_t	vm_map_lookup_entry(vm_map_t, vm_offset_t,
vm_map_entry_t *);
extern boolean_t	vm_map_verify(vm_map_t, vm_map_version_t *);
extern kern_return_t	vm_map_copyin(vm_map_t, vm_offset_t, vm_size_t,
boolean_t, vm_map_copy_t *);
extern kern_return_t	vm_map_copyin_page_list(vm_map_t, vm_offset_t,
vm_size_t, boolean_t,
boolean_t, vm_map_copy_t *,
boolean_t);
extern kern_return_t	vm_map_copyout(vm_map_t, vm_offset_t *, vm_map_copy_t);
extern kern_return_t	vm_map_copy_overwrite(vm_map_t, vm_offset_t,
vm_map_copy_t, boolean_t);
extern void		vm_map_copy_discard(vm_map_copy_t);
extern void		vm_map_copy_page_discard(vm_map_copy_t);
extern vm_map_copy_t	vm_map_copy_copy(vm_map_copy_t);
extern kern_return_t	vm_map_copy_discard_cont(vm_map_copyin_args_t,
vm_map_copy_t *);
extern boolean_t	vm_map_coalesce_entry(vm_map_t, vm_map_entry_t);
extern boolean_t	vm_map_coalesce_entry_forward(vm_map_t, vm_map_entry_t);
extern boolean_t	vm_map_coalesce_entries(vm_map_t, vm_map_entry_t);
extern kern_return_t	vm_map_machine_attribute(vm_map_t, vm_offset_t,
vm_size_t,
vm_machine_attribute_t,
vm_machine_attribute_val_t *);
extern kern_return_t	vm_map_msync(vm_map_t,
vm_offset_t, vm_size_t, vm_sync_t);
extern void		vm_map_entry_delete(vm_map_t, vm_map_entry_t);
kern_return_t vm_map_delete(
vm_map_t   	map,
vm_offset_t    	start,
vm_offset_t    	end);
kern_return_t vm_map_copyout_page_list(
vm_map_t    	dst_map,
vm_offset_t 	*dst_addr,
vm_map_copy_t   	copy);
static inline void vm_map_set_name(vm_map_t map, const char *name)
{
map->name = name;
}
#define		vm_map_min(map)		((map)->min_offset)
#define		vm_map_max(map)		((map)->max_offset)
#define		vm_map_pmap(map)	((map)->pmap)
#define		vm_map_verify_done(map, version)    (vm_map_unlock_read(map))
extern kern_return_t	vm_map_pageable(vm_map_t, vm_offset_t, vm_offset_t,
vm_prot_t, boolean_t, boolean_t);
extern kern_return_t	vm_map_pageable_all(vm_map_t, vm_wire_t);
extern vm_object_t	vm_submap_object;
extern kern_return_t vm_map_copyin_object(
vm_object_t object,
vm_offset_t offset,
vm_size_t   size,
vm_map_copy_t   *copy_result);
extern kern_return_t vm_map_submap(
vm_map_t   map,
vm_offset_t    start,
vm_offset_t    end,
vm_map_t        submap);
#define vm_map_entry_wait(map, interruptible)    	\
MACRO_BEGIN                                     \
assert_wait((event_t)&(map)->hdr, interruptible);	\
vm_map_unlock(map);                             \
thread_block((void (*)()) 0);			\
MACRO_END
#define vm_map_entry_wakeup(map)        thread_wakeup((event_t)&(map)->hdr)
extern void _vm_map_clip_start(
struct vm_map_header *map_header,
vm_map_entry_t entry,
vm_offset_t	start,
boolean_t	link_gap);
void _vm_map_clip_end(
struct vm_map_header 	*map_header,
vm_map_entry_t		entry,
vm_offset_t		end,
boolean_t		link_gap);
extern void		vm_map_set_aslr(vm_map_t, boolean_t, unsigned int);
extern vm_offset_t	vm_map_get_aslr_entropy(vm_map_t, vm_size_t);
extern vm_offset_t	vm_map_optimize_placement(vm_map_t, vm_size_t, vm_offset_t);
extern boolean_t	vm_map_memory_pressure(vm_map_t);
#endif