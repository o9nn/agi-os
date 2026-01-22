#include <string.h>
#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/port.h>
#include <mach/message.h>
#include <ipc/port.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_entry.h>
#include <ipc/ipc_object.h>
#include <ipc/ipc_right.h>
#include <ipc/ipc_notify.h>
#include <ipc/ipc_pset.h>
#include <kern/debug.h>
#include <kern/printf.h>
#include <kern/slab.h>
#if	MACH_KDB
#include <ddb/db_output.h>
#endif
struct kmem_cache ipc_object_caches[IOT_NUMBER];
void
ipc_object_reference(
ipc_object_t	object)
{
io_lock(object);
assert(object->io_references > 0);
io_reference(object);
io_unlock(object);
}
void
ipc_object_release(
ipc_object_t	object)
{
io_lock(object);
assert(object->io_references > 0);
io_release(object);
io_check_unlock(object);
}
kern_return_t
ipc_object_translate(
ipc_space_t		space,
mach_port_name_t	name,
mach_port_right_t	right,
ipc_object_t		*objectp)
{
ipc_entry_t entry;
ipc_object_t object;
kern_return_t kr;
kr = ipc_right_lookup_read(space, name, &entry);
if (kr != KERN_SUCCESS)
return kr;
if ((entry->ie_bits & MACH_PORT_TYPE(right)) == (mach_port_right_t) 0) {
is_read_unlock(space);
return KERN_INVALID_RIGHT;
}
object = entry->ie_object;
assert(object != IO_NULL);
io_lock(object);
is_read_unlock(space);
*objectp = object;
return KERN_SUCCESS;
}
kern_return_t
ipc_object_alloc_dead(
ipc_space_t	space,
mach_port_name_t	*namep)
{
ipc_entry_t entry;
kern_return_t kr;
is_write_lock(space);
kr = ipc_entry_alloc(space, namep, &entry);
if (kr != KERN_SUCCESS) {
is_write_unlock(space);
return kr;
}
assert(entry->ie_object == IO_NULL);
entry->ie_bits |= MACH_PORT_TYPE_DEAD_NAME | 1;
is_write_unlock(space);
return KERN_SUCCESS;
}
kern_return_t
ipc_object_alloc_dead_name(
ipc_space_t	space,
mach_port_name_t	name)
{
ipc_entry_t entry;
kern_return_t kr;
is_write_lock(space);
kr = ipc_entry_alloc_name(space, name, &entry);
if (kr != KERN_SUCCESS) {
is_write_unlock(space);
return kr;
}
if (ipc_right_inuse(space, name, entry))
return KERN_NAME_EXISTS;
assert(entry->ie_object == IO_NULL);
entry->ie_bits |= MACH_PORT_TYPE_DEAD_NAME | 1;
is_write_unlock(space);
return KERN_SUCCESS;
}
kern_return_t
ipc_object_alloc(
ipc_space_t		space,
ipc_object_type_t	otype,
mach_port_type_t	type,
mach_port_urefs_t	urefs,
mach_port_name_t	*namep,
ipc_object_t		*objectp)
{
ipc_object_t object;
ipc_entry_t entry;
kern_return_t kr;
assert(otype < IOT_NUMBER);
assert((type & MACH_PORT_TYPE_ALL_RIGHTS) == type);
assert(type != MACH_PORT_TYPE_NONE);
assert(urefs <= MACH_PORT_UREFS_MAX);
object = io_alloc(otype);
if (object == IO_NULL)
return KERN_RESOURCE_SHORTAGE;
if (otype == IOT_PORT) {
ipc_port_t port = (ipc_port_t)object;
memset(port, 0, sizeof(*port));
} else if (otype == IOT_PORT_SET) {
ipc_pset_t pset = (ipc_pset_t)object;
memset(pset, 0, sizeof(*pset));
}
is_write_lock(space);
kr = ipc_entry_alloc(space, namep, &entry);
if (kr != KERN_SUCCESS) {
is_write_unlock(space);
io_free(otype, object);
return kr;
}
entry->ie_bits |= type | urefs;
entry->ie_object = object;
io_lock_init(object);
io_lock(object);
is_write_unlock(space);
object->io_references = 1;
object->io_bits = io_makebits(TRUE, otype, 0);
*objectp = object;
return KERN_SUCCESS;
}
kern_return_t
ipc_object_alloc_name(
ipc_space_t		space,
ipc_object_type_t	otype,
mach_port_type_t	type,
mach_port_urefs_t	urefs,
mach_port_name_t	name,
ipc_object_t		*objectp)
{
ipc_object_t object;
ipc_entry_t entry;
kern_return_t kr;
assert(otype < IOT_NUMBER);
assert((type & MACH_PORT_TYPE_ALL_RIGHTS) == type);
assert(type != MACH_PORT_TYPE_NONE);
assert(urefs <= MACH_PORT_UREFS_MAX);
object = io_alloc(otype);
if (object == IO_NULL)
return KERN_RESOURCE_SHORTAGE;
if (otype == IOT_PORT) {
ipc_port_t port = (ipc_port_t)object;
memset(port, 0, sizeof(*port));
} else if (otype == IOT_PORT_SET) {
ipc_pset_t pset = (ipc_pset_t)object;
memset(pset, 0, sizeof(*pset));
}
is_write_lock(space);
kr = ipc_entry_alloc_name(space, name, &entry);
if (kr != KERN_SUCCESS) {
is_write_unlock(space);
io_free(otype, object);
return kr;
}
if (ipc_right_inuse(space, name, entry)) {
io_free(otype, object);
return KERN_NAME_EXISTS;
}
entry->ie_bits |= type | urefs;
entry->ie_object = object;
io_lock_init(object);
io_lock(object);
is_write_unlock(space);
object->io_references = 1;
object->io_bits = io_makebits(TRUE, otype, 0);
*objectp = object;
return KERN_SUCCESS;
}
mach_msg_type_name_t
ipc_object_copyin_type(
mach_msg_type_name_t	msgt_name)
{
switch (msgt_name) {
case 0:
return 0;
case MACH_MSG_TYPE_MOVE_RECEIVE:
return MACH_MSG_TYPE_PORT_RECEIVE;
case MACH_MSG_TYPE_MOVE_SEND_ONCE:
case MACH_MSG_TYPE_MAKE_SEND_ONCE:
return MACH_MSG_TYPE_PORT_SEND_ONCE;
case MACH_MSG_TYPE_MOVE_SEND:
case MACH_MSG_TYPE_MAKE_SEND:
case MACH_MSG_TYPE_COPY_SEND:
return MACH_MSG_TYPE_PORT_SEND;
default:
#if MACH_ASSERT
assert(!"ipc_object_copyin_type: strange rights");
#else
panic("ipc_object_copyin_type: strange rights");
#endif
return 0;
}
}
kern_return_t
ipc_object_copyin(
ipc_space_t		space,
mach_port_name_t	name,
mach_msg_type_name_t	msgt_name,
ipc_object_t		*objectp)
{
ipc_entry_t entry;
ipc_port_t soright;
kern_return_t kr;
kr = ipc_right_lookup_write(space, name, &entry);
if (kr != KERN_SUCCESS)
return kr;
kr = ipc_right_copyin(space, name, entry,
msgt_name, TRUE,
objectp, &soright);
if (IE_BITS_TYPE(entry->ie_bits) == MACH_PORT_TYPE_NONE)
ipc_entry_dealloc(space, name, entry);
is_write_unlock(space);
if ((kr == KERN_SUCCESS) && (soright != IP_NULL))
ipc_notify_port_deleted(soright, name);
return kr;
}
void
ipc_object_copyin_from_kernel(
ipc_object_t		object,
mach_msg_type_name_t	msgt_name)
{
assert(IO_VALID(object));
switch (msgt_name) {
case MACH_MSG_TYPE_MOVE_RECEIVE: {
ipc_port_t port = (ipc_port_t) object;
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name != MACH_PORT_NULL);
assert(port->ip_receiver == ipc_space_kernel);
ipc_port_set_mscount(port, 0);
port->ip_receiver_name = MACH_PORT_NULL;
port->ip_destination = IP_NULL;
ipc_port_flag_protected_payload_clear(port);
ip_unlock(port);
break;
}
case MACH_MSG_TYPE_COPY_SEND: {
ipc_port_t port = (ipc_port_t) object;
ip_lock(port);
if (ip_active(port)) {
assert(port->ip_srights > 0);
port->ip_srights++;
}
ip_reference(port);
ip_unlock(port);
break;
}
case MACH_MSG_TYPE_MAKE_SEND: {
ipc_port_t port = (ipc_port_t) object;
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name != MACH_PORT_NULL);
assert(port->ip_receiver == ipc_space_kernel);
ip_reference(port);
port->ip_mscount++;
port->ip_srights++;
ip_unlock(port);
break;
}
case MACH_MSG_TYPE_MOVE_SEND:
break;
case MACH_MSG_TYPE_MAKE_SEND_ONCE: {
ipc_port_t port = (ipc_port_t) object;
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name != MACH_PORT_NULL);
assert(port->ip_receiver == ipc_space_kernel);
ip_reference(port);
port->ip_sorights++;
ip_unlock(port);
break;
}
case MACH_MSG_TYPE_MOVE_SEND_ONCE:
break;
default:
#if MACH_ASSERT
assert(!"ipc_object_copyin_from_kernel: strange rights");
#else
panic("ipc_object_copyin_from_kernel: strange rights");
#endif
}
}
void
ipc_object_destroy(
ipc_object_t		object,
mach_msg_type_name_t	msgt_name)
{
assert(IO_VALID(object));
assert(io_otype(object) == IOT_PORT);
switch (msgt_name) {
case MACH_MSG_TYPE_PORT_SEND:
ipc_port_release_send((ipc_port_t) object);
break;
case MACH_MSG_TYPE_PORT_SEND_ONCE:
ipc_notify_send_once((ipc_port_t) object);
break;
case MACH_MSG_TYPE_PORT_RECEIVE:
ipc_port_release_receive((ipc_port_t) object);
break;
default:
panic("ipc_object_destroy: strange rights");
}
}
kern_return_t
ipc_object_copyout(
ipc_space_t		space,
ipc_object_t		object,
mach_msg_type_name_t	msgt_name,
boolean_t		overflow,
mach_port_name_t	*namep)
{
mach_port_name_t name;
ipc_entry_t entry;
kern_return_t kr;
assert(IO_VALID(object));
assert(io_otype(object) == IOT_PORT);
is_write_lock(space);
for (;;) {
if (!space->is_active) {
is_write_unlock(space);
return KERN_INVALID_TASK;
}
if ((msgt_name != MACH_MSG_TYPE_PORT_SEND_ONCE) &&
ipc_right_reverse(space, object, &name, &entry)) {
assert(entry->ie_bits & MACH_PORT_TYPE_SEND_RECEIVE);
break;
}
kr = ipc_entry_alloc(space, &name, &entry);
if (kr != KERN_SUCCESS) {
is_write_unlock(space);
return kr;
}
assert(IE_BITS_TYPE(entry->ie_bits) == MACH_PORT_TYPE_NONE);
assert(entry->ie_object == IO_NULL);
io_lock(object);
if (!io_active(object)) {
io_unlock(object);
ipc_entry_dealloc(space, name, entry);
is_write_unlock(space);
return KERN_INVALID_CAPABILITY;
}
entry->ie_object = object;
break;
}
kr = ipc_right_copyout(space, name, entry,
msgt_name, overflow, object);
is_write_unlock(space);
if (kr == KERN_SUCCESS)
*namep = name;
return kr;
}
kern_return_t
ipc_object_copyout_name(
ipc_space_t		space,
ipc_object_t		object,
mach_msg_type_name_t	msgt_name,
boolean_t		overflow,
mach_port_name_t	name)
{
mach_port_name_t oname;
ipc_entry_t oentry;
ipc_entry_t entry;
kern_return_t kr;
assert(IO_VALID(object));
assert(io_otype(object) == IOT_PORT);
is_write_lock(space);
kr = ipc_entry_alloc_name(space, name, &entry);
if (kr != KERN_SUCCESS) {
is_write_unlock(space);
return kr;
}
if ((msgt_name != MACH_MSG_TYPE_PORT_SEND_ONCE) &&
ipc_right_reverse(space, object, &oname, &oentry)) {
if (name != oname) {
io_unlock(object);
if (IE_BITS_TYPE(entry->ie_bits)
== MACH_PORT_TYPE_NONE)
ipc_entry_dealloc(space, name, entry);
is_write_unlock(space);
return KERN_RIGHT_EXISTS;
}
assert(entry == oentry);
assert(entry->ie_bits & MACH_PORT_TYPE_SEND_RECEIVE);
} else {
if (ipc_right_inuse(space, name, entry))
return KERN_NAME_EXISTS;
assert(IE_BITS_TYPE(entry->ie_bits) == MACH_PORT_TYPE_NONE);
assert(entry->ie_object == IO_NULL);
io_lock(object);
if (!io_active(object)) {
io_unlock(object);
ipc_entry_dealloc(space, name, entry);
is_write_unlock(space);
return KERN_INVALID_CAPABILITY;
}
entry->ie_object = object;
}
kr = ipc_right_copyout(space, name, entry,
msgt_name, overflow, object);
is_write_unlock(space);
return kr;
}
void
ipc_object_copyout_dest(
ipc_space_t		space,
ipc_object_t		object,
mach_msg_type_name_t	msgt_name,
mach_port_name_t	*namep)
{
mach_port_name_t name;
assert(IO_VALID(object));
assert(io_active(object));
io_release(object);
switch (msgt_name) {
case MACH_MSG_TYPE_PORT_SEND: {
ipc_port_t port = (ipc_port_t) object;
ipc_port_t nsrequest = IP_NULL;
mach_port_mscount_t mscount = 0;
assert(port->ip_srights > 0);
if (--port->ip_srights == 0) {
nsrequest = port->ip_nsrequest;
if (nsrequest != IP_NULL) {
port->ip_nsrequest = IP_NULL;
mscount = port->ip_mscount;
}
}
if (port->ip_receiver == space)
name = port->ip_receiver_name;
else
name = MACH_PORT_NAME_NULL;
ip_unlock(port);
if (nsrequest != IP_NULL)
ipc_notify_no_senders(nsrequest, mscount);
break;
}
case MACH_MSG_TYPE_PORT_SEND_ONCE: {
ipc_port_t port = (ipc_port_t) object;
assert(port->ip_sorights > 0);
if (port->ip_receiver == space) {
port->ip_sorights--;
name = port->ip_receiver_name;
ip_unlock(port);
} else {
ip_reference(port);
ip_unlock(port);
ipc_notify_send_once(port);
name = MACH_PORT_NAME_NULL;
}
break;
}
default:
#if MACH_ASSERT
assert(!"ipc_object_copyout_dest: strange rights");
#else
panic("ipc_object_copyout_dest: strange rights");
#endif
}
*namep = name;
}
kern_return_t
ipc_object_rename(
ipc_space_t	space,
mach_port_name_t	oname,
mach_port_name_t	nname)
{
ipc_entry_t oentry, nentry;
kern_return_t kr;
is_write_lock(space);
kr = ipc_entry_alloc_name(space, nname, &nentry);
if (kr != KERN_SUCCESS) {
is_write_unlock(space);
return kr;
}
if (ipc_right_inuse(space, nname, nentry)) {
return KERN_NAME_EXISTS;
}
if ((oname == nname) ||
((oentry = ipc_entry_lookup(space, oname)) == IE_NULL)) {
ipc_entry_dealloc(space, nname, nentry);
is_write_unlock(space);
return KERN_INVALID_NAME;
}
kr = ipc_right_rename(space, oname, oentry, nname, nentry);
return kr;
}
#if	MACH_KDB
#define	printf	kdbprintf
char *ikot_print_array[IKOT_MAX_TYPE] = {
"(NONE)             ",
"(THREAD)           ",
"(TASK)             ",
"(HOST)             ",
"(HOST_PRIV)        ",
"(PROCESSOR)        ",
"(PSET)             ",
"(PSET_NAME)        ",
"(PAGER)            ",
"(PAGER_REQUEST)    ",
"(DEVICE)           ",
"(XMM_OBJECT)       ",
"(XMM_PAGER)        ",
"(XMM_KERNEL)       ",
"(XMM_REPLY)        ",
"(PAGER_TERMINATING)",
"(PAGING_NAME)      ",
"(HOST_SECURITY)    ",
"(LEDGER)           ",
"(MASTER_DEVICE)    ",
"(ACTIVATION)       ",
"(SUBSYSTEM)        ",
"(IO_DONE_QUEUE)    ",
"(SEMAPHORE)        ",
"(LOCK_SET)         ",
"(CLOCK)            ",
"(CLOCK_CTRL)       ",
"(PAGER_PROXY)      ",
"(UNKNOWN)     "
};
void
ipc_object_print(
const ipc_object_t object)
{
int kotype;
iprintf("%s", io_active(object) ? "active" : "dead");
printf(", refs=%d", object->io_references);
printf(", otype=%d", io_otype(object));
kotype = io_kotype(object);
if (kotype >= 0 && kotype < IKOT_MAX_TYPE)
printf(", kotype=%d %s\n", io_kotype(object),
ikot_print_array[kotype]);
else
printf(", kotype=0x%x %s\n", io_kotype(object),
ikot_print_array[IKOT_UNKNOWN]);
}
#endif