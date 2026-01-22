#include <string.h>
#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/port.h>
#include <kern/assert.h>
#include <kern/sched_prim.h>
#include <kern/slab.h>
#include <ipc/port.h>
#include <ipc/ipc_entry.h>
#include <ipc/ipc_table.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_right.h>
struct kmem_cache ipc_space_cache;
ipc_space_t ipc_space_kernel;
ipc_space_t ipc_space_reply;
void
ipc_space_reference(
ipc_space_t	space)
{
ipc_space_reference_macro(space);
}
void
ipc_space_release(
ipc_space_t	space)
{
ipc_space_release_macro(space);
}
struct ipc_entry zero_entry;
kern_return_t
ipc_space_create(
ipc_space_t		*spacep)
{
ipc_space_t space;
space = is_alloc();
if (space == IS_NULL)
return KERN_RESOURCE_SHORTAGE;
is_ref_lock_init(space);
space->is_references = 2;
is_lock_init(space);
space->is_active = TRUE;
rdxtree_init(&space->is_map);
rdxtree_init(&space->is_reverse_map);
rdxtree_insert(&space->is_map, 0, &zero_entry);
space->is_size = 1;
space->is_free_list = NULL;
space->is_free_list_size = 0;
*spacep = space;
return KERN_SUCCESS;
}
kern_return_t
ipc_space_create_special(
ipc_space_t	*spacep)
{
ipc_space_t space;
space = is_alloc();
if (space == IS_NULL)
return KERN_RESOURCE_SHORTAGE;
is_ref_lock_init(space);
space->is_references = 1;
is_lock_init(space);
space->is_active = FALSE;
*spacep = space;
return KERN_SUCCESS;
}
void
ipc_space_destroy(
ipc_space_t	space)
{
boolean_t active;
assert(space != IS_NULL);
is_write_lock(space);
active = space->is_active;
space->is_active = FALSE;
is_write_unlock(space);
if (!active)
return;
ipc_entry_t entry;
struct rdxtree_iter iter;
rdxtree_for_each(&space->is_map, &iter, entry) {
if (entry->ie_name == MACH_PORT_NULL)
continue;
mach_port_type_t type = IE_BITS_TYPE(entry->ie_bits);
if (type != MACH_PORT_TYPE_NONE) {
mach_port_name_t name =
MACH_PORT_MAKEB(entry->ie_name, entry->ie_bits);
ipc_right_clean(space, name, entry);
}
ie_free(entry);
}
rdxtree_remove_all(&space->is_map);
rdxtree_remove_all(&space->is_reverse_map);
is_release(space);
}