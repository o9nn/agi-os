#include <mach/message.h>
#include <mach/port.h>
#include <kern/lock.h>
#include <kern/kalloc.h>
#include <kern/slab.h>
#include <ipc/port.h>
#include <ipc/ipc_init.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_entry.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_right.h>
#include <ipc/ipc_marequest.h>
#include <ipc/ipc_notify.h>
#if MACH_IPC_DEBUG
#include <mach/kern_return.h>
#include <mach_debug/hash_info.h>
#include <vm/vm_map.h>
#include <vm/vm_kern.h>
#include <vm/vm_user.h>
#endif
struct kmem_cache ipc_marequest_cache;
#define imar_alloc() ((ipc_marequest_t) kmem_cache_alloc(&ipc_marequest_cache))
#define imar_free(imar) kmem_cache_free(&ipc_marequest_cache, (vm_offset_t) (imar))
typedef unsigned int ipc_marequest_index_t;
ipc_marequest_index_t ipc_marequest_size;
ipc_marequest_index_t ipc_marequest_mask;
#define IMAR_HASH(space, name) \
((((ipc_marequest_index_t)((vm_offset_t)space) >> 4) + \
MACH_PORT_INDEX(name) + MACH_PORT_NGEN(name)) & \
ipc_marequest_mask)
typedef struct ipc_marequest_bucket {
decl_simple_lock_data(, imarb_lock_data)
ipc_marequest_t imarb_head;
} *ipc_marequest_bucket_t;
#define IMARB_NULL ((ipc_marequest_bucket_t) 0)
#define imarb_lock_init(imarb) simple_lock_init(&(imarb)->imarb_lock_data)
#define imarb_lock(imarb) simple_lock(&(imarb)->imarb_lock_data)
#define imarb_unlock(imarb) simple_unlock(&(imarb)->imarb_lock_data)
ipc_marequest_bucket_t ipc_marequest_table;
void
ipc_marequest_init(void)
{
ipc_marequest_index_t i;
ipc_marequest_size = IPC_MAREQUEST_SIZE;
ipc_marequest_mask = ipc_marequest_size - 1;
if ((ipc_marequest_size & ipc_marequest_mask) != 0) {
unsigned int bit;
for (bit = 1;; bit <<= 1) {
ipc_marequest_mask |= bit;
ipc_marequest_size = ipc_marequest_mask + 1;
if ((ipc_marequest_size & ipc_marequest_mask) == 0)
break;
}
}
ipc_marequest_table = (ipc_marequest_bucket_t)
kalloc((vm_size_t) (ipc_marequest_size *
sizeof(struct ipc_marequest_bucket)));
assert(ipc_marequest_table != IMARB_NULL);
for (i = 0; i < ipc_marequest_size; i++) {
ipc_marequest_bucket_t bucket;
bucket = &ipc_marequest_table[i];
imarb_lock_init(bucket);
bucket->imarb_head = IMAR_NULL;
}
kmem_cache_init(&ipc_marequest_cache, "ipc_marequest",
sizeof(struct ipc_marequest), 0, NULL, 0);
}
mach_msg_return_t
ipc_marequest_create(
ipc_space_t space,
ipc_port_t port,
mach_port_name_t notify,
ipc_marequest_t *marequestp)
{
mach_port_name_t name;
ipc_entry_t entry;
ipc_port_t soright;
ipc_marequest_t marequest;
ipc_marequest_bucket_t bucket;
marequest = imar_alloc();
if (marequest == IMAR_NULL)
return MACH_SEND_NO_NOTIFY;
is_write_lock(space);
if (!space->is_active) {
is_write_unlock(space);
imar_free(marequest);
return MACH_SEND_INVALID_NOTIFY;
}
if (ipc_right_reverse(space, (ipc_object_t) port, &name, &entry)) {
ipc_entry_bits_t bits;
ip_unlock(port);
bits = entry->ie_bits;
assert(port == (ipc_port_t) entry->ie_object);
assert(bits & MACH_PORT_TYPE_SEND_RECEIVE);
if (bits & IE_BITS_MAREQUEST) {
is_write_unlock(space);
imar_free(marequest);
return MACH_SEND_NOTIFY_IN_PROGRESS;
}
if ((soright = ipc_port_lookup_notify(space, notify))
== IP_NULL) {
is_write_unlock(space);
imar_free(marequest);
return MACH_SEND_INVALID_NOTIFY;
}
entry->ie_bits = bits | IE_BITS_MAREQUEST;
is_reference(space);
marequest->imar_space = space;
marequest->imar_name = name;
marequest->imar_soright = soright;
bucket = &ipc_marequest_table[IMAR_HASH(space, name)];
imarb_lock(bucket);
marequest->imar_next = bucket->imarb_head;
bucket->imarb_head = marequest;
imarb_unlock(bucket);
} else {
if ((soright = ipc_port_lookup_notify(space, notify))
== IP_NULL) {
is_write_unlock(space);
imar_free(marequest);
return MACH_SEND_INVALID_NOTIFY;
}
is_reference(space);
marequest->imar_space = space;
marequest->imar_name = MACH_PORT_NULL;
marequest->imar_soright = soright;
}
is_write_unlock(space);
*marequestp = marequest;
return MACH_MSG_SUCCESS;
}
void
ipc_marequest_cancel(
ipc_space_t space,
mach_port_name_t name)
{
ipc_marequest_bucket_t bucket;
ipc_marequest_t marequest, *last;
assert(space->is_active);
bucket = &ipc_marequest_table[IMAR_HASH(space, name)];
imarb_lock(bucket);
for (last = &bucket->imarb_head;
(marequest = *last) != IMAR_NULL;
last = &marequest->imar_next)
if ((marequest->imar_space == space) &&
(marequest->imar_name == name))
break;
assert(marequest != IMAR_NULL);
*last = marequest->imar_next;
imarb_unlock(bucket);
marequest->imar_name = MACH_PORT_NAME_NULL;
}
void
ipc_marequest_rename(
ipc_space_t space,
mach_port_name_t old,
mach_port_name_t new)
{
ipc_marequest_bucket_t bucket;
ipc_marequest_t marequest, *last;
assert(space->is_active);
bucket = &ipc_marequest_table[IMAR_HASH(space, old)];
imarb_lock(bucket);
for (last = &bucket->imarb_head;
(marequest = *last) != IMAR_NULL;
last = &marequest->imar_next)
if ((marequest->imar_space == space) &&
(marequest->imar_name == old))
break;
assert(marequest != IMAR_NULL);
*last = marequest->imar_next;
imarb_unlock(bucket);
marequest->imar_name = new;
bucket = &ipc_marequest_table[IMAR_HASH(space, new)];
imarb_lock(bucket);
marequest->imar_next = bucket->imarb_head;
bucket->imarb_head = marequest;
imarb_unlock(bucket);
}
void
ipc_marequest_destroy(ipc_marequest_t marequest)
{
ipc_space_t space = marequest->imar_space;
mach_port_name_t name;
ipc_port_t soright;
is_write_lock(space);
name = marequest->imar_name;
soright = marequest->imar_soright;
if (name != MACH_PORT_NULL) {
ipc_marequest_bucket_t bucket;
ipc_marequest_t this, *last;
bucket = &ipc_marequest_table[IMAR_HASH(space, name)];
imarb_lock(bucket);
for (last = &bucket->imarb_head;
(this = *last) != IMAR_NULL;
last = &this->imar_next)
if ((this->imar_space == space) &&
(this->imar_name == name))
break;
assert(this == marequest);
*last = this->imar_next;
imarb_unlock(bucket);
if (space->is_active) {
ipc_entry_t entry;
entry = ipc_entry_lookup(space, name);
assert(entry != IE_NULL);
assert(entry->ie_bits & IE_BITS_MAREQUEST);
assert(entry->ie_bits & MACH_PORT_TYPE_SEND_RECEIVE);
entry->ie_bits &= ~IE_BITS_MAREQUEST;
} else
name = MACH_PORT_NAME_NULL;
}
is_write_unlock(space);
is_release(space);
imar_free(marequest);
assert(soright != IP_NULL);
ipc_notify_msg_accepted(soright, name);
}
#if MACH_IPC_DEBUG
unsigned int
ipc_marequest_info(
unsigned int *maxp,
hash_info_bucket_t *info,
unsigned int count)
{
ipc_marequest_index_t i;
if (ipc_marequest_size < count)
count = ipc_marequest_size;
for (i = 0; i < count; i++) {
ipc_marequest_bucket_t bucket = &ipc_marequest_table[i];
unsigned int bucket_count = 0;
ipc_marequest_t marequest;
imarb_lock(bucket);
for (marequest = bucket->imarb_head;
marequest != IMAR_NULL;
marequest = marequest->imar_next)
bucket_count++;
imarb_unlock(bucket);
info[i].hib_count = bucket_count;
}
*maxp = (unsigned int)-1;
return ipc_marequest_size;
}
#endif