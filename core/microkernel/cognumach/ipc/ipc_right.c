#include <mach/boolean.h>
#include <mach/kern_return.h>
#include <mach/port.h>
#include <mach/message.h>
#include <kern/assert.h>
#include <kern/debug.h>
#include <ipc/port.h>
#include <ipc/ipc_entry.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_object.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_pset.h>
#include <ipc/ipc_marequest.h>
#include <ipc/ipc_right.h>
#include <ipc/ipc_notify.h>
kern_return_t
ipc_right_lookup_write(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t *entryp)
{
ipc_entry_t entry;
assert(space != IS_NULL);
is_write_lock(space);
if (!space->is_active) {
is_write_unlock(space);
return KERN_INVALID_TASK;
}
if ((entry = ipc_entry_lookup(space, name)) == IE_NULL) {
is_write_unlock(space);
return KERN_INVALID_NAME;
}
*entryp = entry;
return KERN_SUCCESS;
}
boolean_t
ipc_right_reverse(
ipc_space_t space,
ipc_object_t object,
mach_port_name_t *namep,
ipc_entry_t *entryp)
{
ipc_port_t port;
mach_port_name_t name;
ipc_entry_t entry;
assert(space->is_active);
assert(io_otype(object) == IOT_PORT);
port = (ipc_port_t) object;
ip_lock(port);
if (!ip_active(port)) {
ip_unlock(port);
return FALSE;
}
if (port->ip_receiver == space) {
name = port->ip_receiver_name;
assert(name != MACH_PORT_NULL);
entry = ipc_entry_lookup(space, name);
assert(entry != IE_NULL);
assert(entry->ie_bits & MACH_PORT_TYPE_RECEIVE);
assert(port == (ipc_port_t) entry->ie_object);
*namep = name;
*entryp = entry;
return TRUE;
}
if ((*entryp = ipc_reverse_lookup(space, (ipc_object_t) port))) {
*namep = (*entryp)->ie_name;
assert((entry = *entryp) != IE_NULL);
assert(IE_BITS_TYPE(entry->ie_bits) == MACH_PORT_TYPE_SEND);
assert(port == (ipc_port_t) entry->ie_object);
return TRUE;
}
ip_unlock(port);
return FALSE;
}
kern_return_t
ipc_right_dnrequest(
ipc_space_t space,
mach_port_name_t name,
boolean_t immediate,
ipc_port_t notify,
ipc_port_t *previousp)
{
ipc_port_t previous;
for (;;) {
ipc_entry_t entry;
ipc_entry_bits_t bits;
kern_return_t kr;
kr = ipc_right_lookup_write(space, name, &entry);
if (kr != KERN_SUCCESS)
return kr;
bits = entry->ie_bits;
if (bits & MACH_PORT_TYPE_PORT_RIGHTS) {
ipc_port_t port;
ipc_port_request_index_t request;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (!ipc_right_check(space, port, name, entry)) {
if (notify == IP_NULL) {
previous = ipc_right_dncancel_macro(
space, port, name, entry);
ip_unlock(port);
is_write_unlock(space);
break;
}
previous = ipc_right_dncancel_macro(space,
port, name, entry);
kr = ipc_port_dnrequest(port, name, notify,
&request);
if (kr != KERN_SUCCESS) {
assert(previous == IP_NULL);
is_write_unlock(space);
kr = ipc_port_dngrow(port);
if (kr != KERN_SUCCESS)
return kr;
continue;
}
assert(request != 0);
ip_unlock(port);
entry->ie_request = request;
is_write_unlock(space);
break;
}
bits = entry->ie_bits;
assert(bits & MACH_PORT_TYPE_DEAD_NAME);
}
if ((bits & MACH_PORT_TYPE_DEAD_NAME) &&
immediate && (notify != IP_NULL)) {
mach_port_urefs_t urefs = IE_BITS_UREFS(bits);
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_DEAD_NAME);
assert(urefs > 0);
if (MACH_PORT_UREFS_OVERFLOW(urefs, 1)) {
is_write_unlock(space);
return KERN_UREFS_OVERFLOW;
}
entry->ie_bits = bits + 1;
is_write_unlock(space);
ipc_notify_dead_name(notify, name);
previous = IP_NULL;
break;
}
is_write_unlock(space);
if (bits & MACH_PORT_TYPE_PORT_OR_DEAD)
return KERN_INVALID_ARGUMENT;
else
return KERN_INVALID_RIGHT;
}
*previousp = previous;
return KERN_SUCCESS;
}
ipc_port_t
ipc_right_dncancel(
ipc_space_t space,
ipc_port_t port,
mach_port_name_t name,
ipc_entry_t entry)
{
ipc_port_t dnrequest;
assert(ip_active(port));
assert(port == (ipc_port_t) entry->ie_object);
dnrequest = ipc_port_dncancel(port, name, entry->ie_request);
entry->ie_request = 0;
return dnrequest;
}
boolean_t
ipc_right_inuse(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry)
{
ipc_entry_bits_t bits = entry->ie_bits;
if (IE_BITS_TYPE(bits) != MACH_PORT_TYPE_NONE) {
is_write_unlock(space);
return TRUE;
}
return FALSE;
}
boolean_t
ipc_right_check(
ipc_space_t space,
ipc_port_t port,
mach_port_name_t name,
ipc_entry_t entry)
{
ipc_entry_bits_t bits;
assert(space->is_active);
assert(port == (ipc_port_t) entry->ie_object);
ip_lock(port);
if (ip_active(port))
return FALSE;
ip_unlock(port);
bits = entry->ie_bits;
assert((bits & MACH_PORT_TYPE_RECEIVE) == 0);
assert(IE_BITS_UREFS(bits) > 0);
if (bits & MACH_PORT_TYPE_SEND) {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_SEND);
if (bits & IE_BITS_MAREQUEST) {
bits &= ~IE_BITS_MAREQUEST;
ipc_marequest_cancel(space, name);
}
ipc_reverse_remove(space, (ipc_object_t) port);
} else {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_SEND_ONCE);
assert(IE_BITS_UREFS(bits) == 1);
assert((bits & IE_BITS_MAREQUEST) == 0);
}
ipc_port_release(port);
bits = (bits &~ IE_BITS_TYPE_MASK) | MACH_PORT_TYPE_DEAD_NAME;
if (entry->ie_request != 0) {
assert(IE_BITS_UREFS(bits) < MACH_PORT_UREFS_MAX);
entry->ie_request = 0;
bits++;
}
entry->ie_bits = bits;
entry->ie_object = IO_NULL;
return TRUE;
}
void
ipc_right_clean(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry)
{
ipc_entry_bits_t bits = entry->ie_bits;
mach_port_type_t type = IE_BITS_TYPE(bits);
assert(!space->is_active);
switch (type) {
case MACH_PORT_TYPE_DEAD_NAME:
assert(entry->ie_request == 0);
assert(entry->ie_object == IO_NULL);
assert((bits & IE_BITS_MAREQUEST) == 0);
break;
case MACH_PORT_TYPE_PORT_SET: {
ipc_pset_t pset = (ipc_pset_t) entry->ie_object;
assert(entry->ie_request == 0);
assert((bits & IE_BITS_MAREQUEST) == 0);
assert(pset != IPS_NULL);
ips_lock(pset);
assert(ips_active(pset));
ipc_pset_destroy(pset);
break;
}
case MACH_PORT_TYPE_SEND:
case MACH_PORT_TYPE_RECEIVE:
case MACH_PORT_TYPE_SEND_RECEIVE:
case MACH_PORT_TYPE_SEND_ONCE: {
ipc_port_t port = (ipc_port_t) entry->ie_object;
ipc_port_t dnrequest;
ipc_port_t nsrequest = IP_NULL;
mach_port_mscount_t mscount = 0;
assert(port != IP_NULL);
ip_lock(port);
if (!ip_active(port)) {
ip_release(port);
ip_check_unlock(port);
break;
}
dnrequest = ipc_right_dncancel_macro(space, port, name, entry);
if (type & MACH_PORT_TYPE_SEND) {
assert(port->ip_srights > 0);
if (--port->ip_srights == 0) {
nsrequest = port->ip_nsrequest;
if (nsrequest != IP_NULL) {
port->ip_nsrequest = IP_NULL;
mscount = port->ip_mscount;
}
}
}
if (type & MACH_PORT_TYPE_RECEIVE) {
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
ipc_port_clear_receiver(port);
ipc_port_destroy(port);
} else if (type & MACH_PORT_TYPE_SEND_ONCE) {
assert(port->ip_sorights > 0);
ip_unlock(port);
ipc_notify_send_once(port);
} else {
assert(port->ip_receiver != space);
ip_release(port);
ip_unlock(port);
}
if (nsrequest != IP_NULL)
ipc_notify_no_senders(nsrequest, mscount);
if (dnrequest != IP_NULL)
ipc_notify_port_deleted(dnrequest, name);
break;
}
default:
#if MACH_ASSERT
assert(!"ipc_right_clean: strange type");
#else
panic("ipc_right_clean: strange type");
#endif
}
}
kern_return_t
ipc_right_destroy(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry)
{
ipc_entry_bits_t bits = entry->ie_bits;
mach_port_type_t type = IE_BITS_TYPE(bits);
assert(space->is_active);
switch (type) {
case MACH_PORT_TYPE_DEAD_NAME:
assert(entry->ie_request == 0);
assert(entry->ie_object == IO_NULL);
assert((bits & IE_BITS_MAREQUEST) == 0);
ipc_entry_dealloc(space, name, entry);
is_write_unlock(space);
break;
case MACH_PORT_TYPE_PORT_SET: {
ipc_pset_t pset = (ipc_pset_t) entry->ie_object;
assert(entry->ie_request == 0);
assert(pset != IPS_NULL);
entry->ie_object = IO_NULL;
ipc_entry_dealloc(space, name, entry);
ips_lock(pset);
assert(ips_active(pset));
is_write_unlock(space);
ipc_pset_destroy(pset);
break;
}
case MACH_PORT_TYPE_SEND:
case MACH_PORT_TYPE_RECEIVE:
case MACH_PORT_TYPE_SEND_RECEIVE:
case MACH_PORT_TYPE_SEND_ONCE: {
ipc_port_t port = (ipc_port_t) entry->ie_object;
ipc_port_t nsrequest = IP_NULL;
mach_port_mscount_t mscount = 0;
ipc_port_t dnrequest;
assert(port != IP_NULL);
if (bits & IE_BITS_MAREQUEST) {
assert(type & MACH_PORT_TYPE_SEND_RECEIVE);
ipc_marequest_cancel(space, name);
}
if (type == MACH_PORT_TYPE_SEND)
ipc_reverse_remove(space, (ipc_object_t) port);
ip_lock(port);
if (!ip_active(port)) {
assert((type & MACH_PORT_TYPE_RECEIVE) == 0);
ip_release(port);
ip_check_unlock(port);
entry->ie_request = 0;
entry->ie_object = IO_NULL;
ipc_entry_dealloc(space, name, entry);
is_write_unlock(space);
break;
}
dnrequest = ipc_right_dncancel_macro(space, port, name, entry);
entry->ie_object = IO_NULL;
ipc_entry_dealloc(space, name, entry);
is_write_unlock(space);
if (type & MACH_PORT_TYPE_SEND) {
assert(port->ip_srights > 0);
if (--port->ip_srights == 0) {
nsrequest = port->ip_nsrequest;
if (nsrequest != IP_NULL) {
port->ip_nsrequest = IP_NULL;
mscount = port->ip_mscount;
}
}
}
if (type & MACH_PORT_TYPE_RECEIVE) {
assert(ip_active(port));
assert(port->ip_receiver == space);
ipc_port_clear_receiver(port);
ipc_port_destroy(port);
} else if (type & MACH_PORT_TYPE_SEND_ONCE) {
assert(port->ip_sorights > 0);
ip_unlock(port);
ipc_notify_send_once(port);
} else {
assert(port->ip_receiver != space);
ip_release(port);
ip_unlock(port);
}
if (nsrequest != IP_NULL)
ipc_notify_no_senders(nsrequest, mscount);
if (dnrequest != IP_NULL)
ipc_notify_port_deleted(dnrequest, name);
break;
}
default:
#if MACH_ASSERT
assert(!"ipc_right_destroy: strange type");
#else
panic("ipc_right_destroy: strange type");
#endif
}
return KERN_SUCCESS;
}
kern_return_t
ipc_right_dealloc(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry)
{
ipc_entry_bits_t bits = entry->ie_bits;
mach_port_type_t type = IE_BITS_TYPE(bits);
assert(space->is_active);
switch (type) {
case MACH_PORT_TYPE_DEAD_NAME: {
dead_name:
assert(IE_BITS_UREFS(bits) > 0);
assert(entry->ie_request == 0);
assert(entry->ie_object == IO_NULL);
assert((bits & IE_BITS_MAREQUEST) == 0);
if (IE_BITS_UREFS(bits) == 1)
ipc_entry_dealloc(space, name, entry);
else
entry->ie_bits = bits-1;
is_write_unlock(space);
break;
}
case MACH_PORT_TYPE_SEND_ONCE: {
ipc_port_t port, dnrequest;
assert(IE_BITS_UREFS(bits) == 1);
assert((bits & IE_BITS_MAREQUEST) == 0);
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (ipc_right_check(space, port, name, entry)) {
bits = entry->ie_bits;
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_DEAD_NAME);
goto dead_name;
}
assert(port->ip_sorights > 0);
dnrequest = ipc_right_dncancel_macro(space, port, name, entry);
ip_unlock(port);
entry->ie_object = IO_NULL;
ipc_entry_dealloc(space, name, entry);
is_write_unlock(space);
ipc_notify_send_once(port);
if (dnrequest != IP_NULL)
ipc_notify_port_deleted(dnrequest, name);
break;
}
case MACH_PORT_TYPE_SEND: {
ipc_port_t port;
ipc_port_t dnrequest = IP_NULL;
ipc_port_t nsrequest = IP_NULL;
mach_port_mscount_t mscount = 0;
assert(IE_BITS_UREFS(bits) > 0);
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (ipc_right_check(space, port, name, entry)) {
bits = entry->ie_bits;
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_DEAD_NAME);
goto dead_name;
}
assert(port->ip_srights > 0);
if (IE_BITS_UREFS(bits) == 1) {
if (--port->ip_srights == 0) {
nsrequest = port->ip_nsrequest;
if (nsrequest != IP_NULL) {
port->ip_nsrequest = IP_NULL;
mscount = port->ip_mscount;
}
}
dnrequest = ipc_right_dncancel_macro(space, port,
name, entry);
ipc_reverse_remove(space, (ipc_object_t) port);
if (bits & IE_BITS_MAREQUEST)
ipc_marequest_cancel(space, name);
ip_release(port);
entry->ie_object = IO_NULL;
ipc_entry_dealloc(space, name, entry);
} else
entry->ie_bits = bits-1;
ip_unlock(port);
is_write_unlock(space);
if (nsrequest != IP_NULL)
ipc_notify_no_senders(nsrequest, mscount);
if (dnrequest != IP_NULL)
ipc_notify_port_deleted(dnrequest, name);
break;
}
case MACH_PORT_TYPE_SEND_RECEIVE: {
ipc_port_t port;
ipc_port_t nsrequest = IP_NULL;
mach_port_mscount_t mscount = 0;
assert(IE_BITS_UREFS(bits) > 0);
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
assert(port->ip_srights > 0);
if (IE_BITS_UREFS(bits) == 1) {
if (--port->ip_srights == 0) {
nsrequest = port->ip_nsrequest;
if (nsrequest != IP_NULL) {
port->ip_nsrequest = IP_NULL;
mscount = port->ip_mscount;
}
}
entry->ie_bits = bits &~ (IE_BITS_UREFS_MASK|
MACH_PORT_TYPE_SEND);
} else
entry->ie_bits = bits-1;
ip_unlock(port);
is_write_unlock(space);
if (nsrequest != IP_NULL)
ipc_notify_no_senders(nsrequest, mscount);
break;
}
default:
is_write_unlock(space);
return KERN_INVALID_RIGHT;
}
return KERN_SUCCESS;
}
kern_return_t
ipc_right_delta(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry,
mach_port_right_t right,
mach_port_delta_t delta)
{
ipc_entry_bits_t bits = entry->ie_bits;
assert(space->is_active);
assert(right < MACH_PORT_RIGHT_NUMBER);
switch (right) {
case MACH_PORT_RIGHT_PORT_SET: {
ipc_pset_t pset;
if ((bits & MACH_PORT_TYPE_PORT_SET) == 0)
goto invalid_right;
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_PORT_SET);
assert(IE_BITS_UREFS(bits) == 0);
assert((bits & IE_BITS_MAREQUEST) == 0);
assert(entry->ie_request == 0);
if (delta == 0)
goto success;
if (delta != -1)
goto invalid_value;
pset = (ipc_pset_t) entry->ie_object;
assert(pset != IPS_NULL);
entry->ie_object = IO_NULL;
ipc_entry_dealloc(space, name, entry);
ips_lock(pset);
assert(ips_active(pset));
is_write_unlock(space);
ipc_pset_destroy(pset);
break;
}
case MACH_PORT_RIGHT_RECEIVE: {
ipc_port_t port;
ipc_port_t dnrequest = IP_NULL;
if ((bits & MACH_PORT_TYPE_RECEIVE) == 0)
goto invalid_right;
if (delta == 0)
goto success;
if (delta != -1)
goto invalid_value;
if (bits & IE_BITS_MAREQUEST) {
bits &= ~IE_BITS_MAREQUEST;
ipc_marequest_cancel(space, name);
}
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
if (bits & MACH_PORT_TYPE_SEND) {
assert(IE_BITS_TYPE(bits) ==
MACH_PORT_TYPE_SEND_RECEIVE);
assert(IE_BITS_UREFS(bits) > 0);
assert(IE_BITS_UREFS(bits) < MACH_PORT_UREFS_MAX);
assert(port->ip_srights > 0);
bits &= ~IE_BITS_TYPE_MASK;
bits |= MACH_PORT_TYPE_DEAD_NAME;
if (entry->ie_request != 0) {
entry->ie_request = 0;
bits++;
}
entry->ie_bits = bits;
entry->ie_object = IO_NULL;
} else {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_RECEIVE);
assert(IE_BITS_UREFS(bits) == 0);
dnrequest = ipc_right_dncancel_macro(space, port,
name, entry);
entry->ie_object = IO_NULL;
ipc_entry_dealloc(space, name, entry);
}
is_write_unlock(space);
ipc_port_clear_receiver(port);
ipc_port_destroy(port);
if (dnrequest != IP_NULL)
ipc_notify_port_deleted(dnrequest, name);
break;
}
case MACH_PORT_RIGHT_SEND_ONCE: {
ipc_port_t port, dnrequest;
if ((bits & MACH_PORT_TYPE_SEND_ONCE) == 0)
goto invalid_right;
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_SEND_ONCE);
assert(IE_BITS_UREFS(bits) == 1);
assert((bits & IE_BITS_MAREQUEST) == 0);
if ((delta > 0) || (delta < -1))
goto invalid_value;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (ipc_right_check(space, port, name, entry)) {
assert(!(entry->ie_bits & MACH_PORT_TYPE_SEND_ONCE));
goto invalid_right;
}
assert(port->ip_sorights > 0);
if (delta == 0) {
ip_unlock(port);
goto success;
}
dnrequest = ipc_right_dncancel_macro(space, port, name, entry);
ip_unlock(port);
entry->ie_object = IO_NULL;
ipc_entry_dealloc(space, name, entry);
is_write_unlock(space);
ipc_notify_send_once(port);
if (dnrequest != IP_NULL)
ipc_notify_port_deleted(dnrequest, name);
break;
}
case MACH_PORT_RIGHT_DEAD_NAME: {
mach_port_urefs_t urefs;
if (bits & MACH_PORT_TYPE_SEND_RIGHTS) {
ipc_port_t port;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (!ipc_right_check(space, port, name, entry)) {
ip_unlock(port);
goto invalid_right;
}
bits = entry->ie_bits;
} else if ((bits & MACH_PORT_TYPE_DEAD_NAME) == 0)
goto invalid_right;
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_DEAD_NAME);
assert(IE_BITS_UREFS(bits) > 0);
assert((bits & IE_BITS_MAREQUEST) == 0);
assert(entry->ie_object == IO_NULL);
assert(entry->ie_request == 0);
urefs = IE_BITS_UREFS(bits);
if (MACH_PORT_UREFS_UNDERFLOW(urefs, delta))
goto invalid_value;
if (MACH_PORT_UREFS_OVERFLOW(urefs, delta))
goto urefs_overflow;
if ((urefs + delta) == 0)
ipc_entry_dealloc(space, name, entry);
else
entry->ie_bits = bits + delta;
is_write_unlock(space);
break;
}
case MACH_PORT_RIGHT_SEND: {
mach_port_urefs_t urefs;
ipc_port_t port;
ipc_port_t dnrequest = IP_NULL;
ipc_port_t nsrequest = IP_NULL;
mach_port_mscount_t mscount = 0;
if ((bits & MACH_PORT_TYPE_SEND) == 0)
goto invalid_right;
urefs = IE_BITS_UREFS(bits);
if (MACH_PORT_UREFS_UNDERFLOW(urefs, delta))
goto invalid_value;
if (MACH_PORT_UREFS_OVERFLOW(urefs+1, delta))
goto urefs_overflow;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (ipc_right_check(space, port, name, entry)) {
assert((entry->ie_bits & MACH_PORT_TYPE_SEND) == 0);
goto invalid_right;
}
assert(port->ip_srights > 0);
if ((urefs + delta) == 0) {
if (--port->ip_srights == 0) {
nsrequest = port->ip_nsrequest;
if (nsrequest != IP_NULL) {
port->ip_nsrequest = IP_NULL;
mscount = port->ip_mscount;
}
}
if (bits & MACH_PORT_TYPE_RECEIVE) {
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
assert(IE_BITS_TYPE(bits) ==
MACH_PORT_TYPE_SEND_RECEIVE);
entry->ie_bits = bits &~ (IE_BITS_UREFS_MASK|
MACH_PORT_TYPE_SEND);
} else {
assert(IE_BITS_TYPE(bits) ==
MACH_PORT_TYPE_SEND);
dnrequest = ipc_right_dncancel_macro(
space, port, name, entry);
ipc_reverse_remove(space, (ipc_object_t) port);
if (bits & IE_BITS_MAREQUEST)
ipc_marequest_cancel(space, name);
ip_release(port);
entry->ie_object = IO_NULL;
ipc_entry_dealloc(space, name, entry);
}
} else
entry->ie_bits = bits + delta;
ip_unlock(port);
is_write_unlock(space);
if (nsrequest != IP_NULL)
ipc_notify_no_senders(nsrequest, mscount);
if (dnrequest != IP_NULL)
ipc_notify_port_deleted(dnrequest, name);
break;
}
default:
#if MACH_ASSERT
assert(!"ipc_right_delta: strange right");
#else
panic("ipc_right_delta: strange right");
#endif
}
return KERN_SUCCESS;
success:
is_write_unlock(space);
return KERN_SUCCESS;
invalid_right:
is_write_unlock(space);
return KERN_INVALID_RIGHT;
invalid_value:
is_write_unlock(space);
return KERN_INVALID_VALUE;
urefs_overflow:
is_write_unlock(space);
return KERN_UREFS_OVERFLOW;
}
kern_return_t
ipc_right_info(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry,
mach_port_type_t *typep,
mach_port_urefs_t *urefsp)
{
ipc_entry_bits_t bits = entry->ie_bits;
ipc_port_request_index_t request;
mach_port_type_t type;
if (bits & MACH_PORT_TYPE_SEND_RIGHTS) {
ipc_port_t port = (ipc_port_t) entry->ie_object;
if (ipc_right_check(space, port, name, entry)) {
bits = entry->ie_bits;
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_DEAD_NAME);
} else
ip_unlock(port);
}
type = IE_BITS_TYPE(bits);
request = entry->ie_request;
if (request != 0)
type |= MACH_PORT_TYPE_DNREQUEST;
if (bits & IE_BITS_MAREQUEST)
type |= MACH_PORT_TYPE_MAREQUEST;
*typep = type;
*urefsp = IE_BITS_UREFS(bits);
return KERN_SUCCESS;
}
boolean_t
ipc_right_copyin_check(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry,
mach_msg_type_name_t msgt_name)
{
ipc_entry_bits_t bits = entry->ie_bits;
assert(space->is_active);
switch (msgt_name) {
case MACH_MSG_TYPE_MAKE_SEND:
case MACH_MSG_TYPE_MAKE_SEND_ONCE:
case MACH_MSG_TYPE_MOVE_RECEIVE:
if ((bits & MACH_PORT_TYPE_RECEIVE) == 0)
return FALSE;
break;
case MACH_MSG_TYPE_COPY_SEND:
case MACH_MSG_TYPE_MOVE_SEND:
case MACH_MSG_TYPE_MOVE_SEND_ONCE: {
ipc_port_t port;
boolean_t active;
if (bits & MACH_PORT_TYPE_DEAD_NAME)
break;
if ((bits & MACH_PORT_TYPE_SEND_RIGHTS) == 0)
return FALSE;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
ip_lock(port);
active = ip_active(port);
ip_unlock(port);
if (!active) {
break;
}
if (msgt_name == MACH_MSG_TYPE_MOVE_SEND_ONCE) {
if ((bits & MACH_PORT_TYPE_SEND_ONCE) == 0)
return FALSE;
} else {
if ((bits & MACH_PORT_TYPE_SEND) == 0)
return FALSE;
}
break;
}
default:
#if MACH_ASSERT
assert(!"ipc_right_copyin_check: strange rights");
#else
panic("ipc_right_copyin_check: strange rights");
#endif
}
return TRUE;
}
kern_return_t
ipc_right_copyin(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry,
mach_msg_type_name_t msgt_name,
boolean_t deadok,
ipc_object_t *objectp,
ipc_port_t *sorightp)
{
ipc_entry_bits_t bits = entry->ie_bits;
assert(space->is_active);
switch (msgt_name) {
case MACH_MSG_TYPE_MAKE_SEND: {
ipc_port_t port;
if ((bits & MACH_PORT_TYPE_RECEIVE) == 0)
goto invalid_right;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
port->ip_mscount++;
port->ip_srights++;
ip_reference(port);
ip_unlock(port);
*objectp = (ipc_object_t) port;
*sorightp = IP_NULL;
break;
}
case MACH_MSG_TYPE_MAKE_SEND_ONCE: {
ipc_port_t port;
if ((bits & MACH_PORT_TYPE_RECEIVE) == 0)
goto invalid_right;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
port->ip_sorights++;
ip_reference(port);
ip_unlock(port);
*objectp = (ipc_object_t) port;
*sorightp = IP_NULL;
break;
}
case MACH_MSG_TYPE_MOVE_RECEIVE: {
ipc_port_t port;
ipc_port_t dnrequest = IP_NULL;
if ((bits & MACH_PORT_TYPE_RECEIVE) == 0)
goto invalid_right;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
if (bits & MACH_PORT_TYPE_SEND) {
assert(IE_BITS_TYPE(bits) ==
MACH_PORT_TYPE_SEND_RECEIVE);
assert(IE_BITS_UREFS(bits) > 0);
assert(port->ip_srights > 0);
entry->ie_name = name;
ipc_reverse_insert(space, (ipc_object_t) port, entry);
ip_reference(port);
} else {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_RECEIVE);
assert(IE_BITS_UREFS(bits) == 0);
dnrequest = ipc_right_dncancel_macro(space, port,
name, entry);
if (bits & IE_BITS_MAREQUEST)
ipc_marequest_cancel(space, name);
entry->ie_object = IO_NULL;
}
entry->ie_bits = bits &~ MACH_PORT_TYPE_RECEIVE;
ipc_port_clear_receiver(port);
port->ip_receiver_name = MACH_PORT_NULL;
port->ip_destination = IP_NULL;
ipc_port_flag_protected_payload_clear(port);
ip_unlock(port);
*objectp = (ipc_object_t) port;
*sorightp = dnrequest;
break;
}
case MACH_MSG_TYPE_COPY_SEND: {
ipc_port_t port;
if (bits & MACH_PORT_TYPE_DEAD_NAME)
goto copy_dead;
if ((bits & MACH_PORT_TYPE_SEND_RIGHTS) == 0)
goto invalid_right;
assert(IE_BITS_UREFS(bits) > 0);
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (ipc_right_check(space, port, name, entry)) {
bits = entry->ie_bits;
goto copy_dead;
}
if ((bits & MACH_PORT_TYPE_SEND) == 0) {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_SEND_ONCE);
assert(port->ip_sorights > 0);
ip_unlock(port);
goto invalid_right;
}
assert(port->ip_srights > 0);
port->ip_srights++;
ip_reference(port);
ip_unlock(port);
*objectp = (ipc_object_t) port;
*sorightp = IP_NULL;
break;
}
case MACH_MSG_TYPE_MOVE_SEND: {
ipc_port_t port;
ipc_port_t dnrequest = IP_NULL;
if (bits & MACH_PORT_TYPE_DEAD_NAME)
goto move_dead;
if ((bits & MACH_PORT_TYPE_SEND_RIGHTS) == 0)
goto invalid_right;
assert(IE_BITS_UREFS(bits) > 0);
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (ipc_right_check(space, port, name, entry)) {
bits = entry->ie_bits;
goto move_dead;
}
if ((bits & MACH_PORT_TYPE_SEND) == 0) {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_SEND_ONCE);
assert(port->ip_sorights > 0);
ip_unlock(port);
goto invalid_right;
}
assert(port->ip_srights > 0);
if (IE_BITS_UREFS(bits) == 1) {
if (bits & MACH_PORT_TYPE_RECEIVE) {
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
assert(IE_BITS_TYPE(bits) ==
MACH_PORT_TYPE_SEND_RECEIVE);
ip_reference(port);
} else {
assert(IE_BITS_TYPE(bits) ==
MACH_PORT_TYPE_SEND);
dnrequest = ipc_right_dncancel_macro(
space, port, name, entry);
ipc_reverse_remove(space, (ipc_object_t) port);
if (bits & IE_BITS_MAREQUEST)
ipc_marequest_cancel(space, name);
entry->ie_object = IO_NULL;
}
entry->ie_bits = bits &~
(IE_BITS_UREFS_MASK|MACH_PORT_TYPE_SEND);
} else {
port->ip_srights++;
ip_reference(port);
entry->ie_bits = bits-1;
}
ip_unlock(port);
*objectp = (ipc_object_t) port;
*sorightp = dnrequest;
break;
}
case MACH_MSG_TYPE_MOVE_SEND_ONCE: {
ipc_port_t port;
ipc_port_t dnrequest;
if (bits & MACH_PORT_TYPE_DEAD_NAME)
goto move_dead;
if ((bits & MACH_PORT_TYPE_SEND_RIGHTS) == 0)
goto invalid_right;
assert(IE_BITS_UREFS(bits) > 0);
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (ipc_right_check(space, port, name, entry)) {
bits = entry->ie_bits;
goto move_dead;
}
if ((bits & MACH_PORT_TYPE_SEND_ONCE) == 0) {
assert(bits & MACH_PORT_TYPE_SEND);
assert(port->ip_srights > 0);
ip_unlock(port);
goto invalid_right;
}
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_SEND_ONCE);
assert(IE_BITS_UREFS(bits) == 1);
assert((bits & IE_BITS_MAREQUEST) == 0);
assert(port->ip_sorights > 0);
dnrequest = ipc_right_dncancel_macro(space, port, name, entry);
ip_unlock(port);
entry->ie_object = IO_NULL;
entry->ie_bits = bits &~ MACH_PORT_TYPE_SEND_ONCE;
*objectp = (ipc_object_t) port;
*sorightp = dnrequest;
break;
}
default:
#if MACH_ASSERT
assert(!"ipc_right_copyin: strange rights");
#else
panic("ipc_right_copyin: strange rights");
#endif
}
return KERN_SUCCESS;
copy_dead:
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_DEAD_NAME);
assert(IE_BITS_UREFS(bits) > 0);
assert((bits & IE_BITS_MAREQUEST) == 0);
assert(entry->ie_request == 0);
assert(entry->ie_object == 0);
if (!deadok)
goto invalid_right;
*objectp = IO_DEAD;
*sorightp = IP_NULL;
return KERN_SUCCESS;
move_dead:
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_DEAD_NAME);
assert(IE_BITS_UREFS(bits) > 0);
assert((bits & IE_BITS_MAREQUEST) == 0);
assert(entry->ie_request == 0);
assert(entry->ie_object == 0);
if (!deadok)
goto invalid_right;
if (IE_BITS_UREFS(bits) == 1)
entry->ie_bits = bits &~ MACH_PORT_TYPE_DEAD_NAME;
else
entry->ie_bits = bits-1;
*objectp = IO_DEAD;
*sorightp = IP_NULL;
return KERN_SUCCESS;
invalid_right:
return KERN_INVALID_RIGHT;
}
void
ipc_right_copyin_undo(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry,
mach_msg_type_name_t msgt_name,
ipc_object_t object,
ipc_port_t soright)
{
ipc_entry_bits_t bits = entry->ie_bits;
assert(space->is_active);
assert((msgt_name == MACH_MSG_TYPE_MOVE_SEND) ||
(msgt_name == MACH_MSG_TYPE_COPY_SEND) ||
(msgt_name == MACH_MSG_TYPE_MOVE_SEND_ONCE));
if (soright != IP_NULL) {
assert((msgt_name == MACH_MSG_TYPE_MOVE_SEND) ||
(msgt_name == MACH_MSG_TYPE_MOVE_SEND_ONCE));
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_NONE);
assert(entry->ie_object == IO_NULL);
assert(object != IO_DEAD);
entry->ie_bits = ((bits &~ IE_BITS_RIGHT_MASK) |
MACH_PORT_TYPE_DEAD_NAME | 2);
} else if (IE_BITS_TYPE(bits) == MACH_PORT_TYPE_NONE) {
assert((msgt_name == MACH_MSG_TYPE_MOVE_SEND) ||
(msgt_name == MACH_MSG_TYPE_MOVE_SEND_ONCE));
assert(entry->ie_object == IO_NULL);
entry->ie_bits = ((bits &~ IE_BITS_RIGHT_MASK) |
MACH_PORT_TYPE_DEAD_NAME | 1);
} else if (IE_BITS_TYPE(bits) == MACH_PORT_TYPE_DEAD_NAME) {
assert(entry->ie_object == IO_NULL);
assert(object == IO_DEAD);
assert(IE_BITS_UREFS(bits) > 0);
if (msgt_name != MACH_MSG_TYPE_COPY_SEND) {
assert(IE_BITS_UREFS(bits) < MACH_PORT_UREFS_MAX);
entry->ie_bits = bits+1;
}
} else {
assert((msgt_name == MACH_MSG_TYPE_MOVE_SEND) ||
(msgt_name == MACH_MSG_TYPE_COPY_SEND));
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_SEND);
assert(object != IO_DEAD);
assert(entry->ie_object == object);
assert(IE_BITS_UREFS(bits) > 0);
if (msgt_name != MACH_MSG_TYPE_COPY_SEND) {
assert(IE_BITS_UREFS(bits) < MACH_PORT_UREFS_MAX-1);
entry->ie_bits = bits+1;
}
(void) ipc_right_check(space, (ipc_port_t) object,
name, entry);
}
if (object != IO_DEAD)
ipc_object_release(object);
}
kern_return_t
ipc_right_copyin_two(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry,
ipc_object_t *objectp,
ipc_port_t *sorightp)
{
ipc_entry_bits_t bits = entry->ie_bits;
mach_port_urefs_t urefs;
ipc_port_t port;
ipc_port_t dnrequest = IP_NULL;
assert(space->is_active);
if ((bits & MACH_PORT_TYPE_SEND) == 0)
goto invalid_right;
urefs = IE_BITS_UREFS(bits);
if (urefs < 2)
goto invalid_right;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
if (ipc_right_check(space, port, name, entry)) {
goto invalid_right;
}
assert(port->ip_srights > 0);
if (urefs == 2) {
if (bits & MACH_PORT_TYPE_RECEIVE) {
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
assert(IE_BITS_TYPE(bits) ==
MACH_PORT_TYPE_SEND_RECEIVE);
port->ip_srights++;
ip_reference(port);
ip_reference(port);
} else {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_SEND);
dnrequest = ipc_right_dncancel_macro(space, port,
name, entry);
ipc_reverse_remove(space, (ipc_object_t) port);
if (bits & IE_BITS_MAREQUEST)
ipc_marequest_cancel(space, name);
port->ip_srights++;
ip_reference(port);
entry->ie_object = IO_NULL;
}
entry->ie_bits = bits &~
(IE_BITS_UREFS_MASK|MACH_PORT_TYPE_SEND);
} else {
port->ip_srights += 2;
ip_reference(port);
ip_reference(port);
entry->ie_bits = bits-2;
}
ip_unlock(port);
*objectp = (ipc_object_t) port;
*sorightp = dnrequest;
return KERN_SUCCESS;
invalid_right:
return KERN_INVALID_RIGHT;
}
kern_return_t
ipc_right_copyout(
ipc_space_t space,
mach_port_name_t name,
ipc_entry_t entry,
mach_msg_type_name_t msgt_name,
boolean_t overflow,
ipc_object_t object)
{
ipc_entry_bits_t bits = entry->ie_bits;
ipc_port_t port;
assert(IO_VALID(object));
assert(io_otype(object) == IOT_PORT);
assert(io_active(object));
assert(entry->ie_object == object);
port = (ipc_port_t) object;
switch (msgt_name) {
case MACH_MSG_TYPE_PORT_SEND_ONCE:
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_NONE);
assert(port->ip_sorights > 0);
ip_unlock(port);
entry->ie_bits = bits | (MACH_PORT_TYPE_SEND_ONCE | 1);
break;
case MACH_MSG_TYPE_PORT_SEND:
assert(port->ip_srights > 0);
if (bits & MACH_PORT_TYPE_SEND) {
mach_port_urefs_t urefs = IE_BITS_UREFS(bits);
assert(port->ip_srights > 1);
assert(urefs > 0);
assert(urefs < MACH_PORT_UREFS_MAX);
if (urefs+1 == MACH_PORT_UREFS_MAX) {
if (overflow) {
port->ip_srights--;
ip_release(port);
ip_unlock(port);
return KERN_SUCCESS;
}
ip_unlock(port);
return KERN_UREFS_OVERFLOW;
}
port->ip_srights--;
ip_release(port);
ip_unlock(port);
} else if (bits & MACH_PORT_TYPE_RECEIVE) {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_RECEIVE);
assert(IE_BITS_UREFS(bits) == 0);
ip_release(port);
ip_unlock(port);
} else {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_NONE);
assert(IE_BITS_UREFS(bits) == 0);
ip_unlock(port);
entry->ie_name = name;
ipc_reverse_insert(space, (ipc_object_t) port, entry);
}
entry->ie_bits = (bits | MACH_PORT_TYPE_SEND) + 1;
break;
case MACH_MSG_TYPE_PORT_RECEIVE: {
ipc_port_t dest;
assert(port->ip_mscount == 0);
assert(port->ip_receiver_name == MACH_PORT_NULL);
dest = port->ip_destination;
port->ip_receiver_name = name;
port->ip_receiver = space;
ipc_port_flag_protected_payload_clear(port);
assert((bits & MACH_PORT_TYPE_RECEIVE) == 0);
if (bits & MACH_PORT_TYPE_SEND) {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_SEND);
assert(IE_BITS_UREFS(bits) > 0);
assert(port->ip_srights > 0);
ip_release(port);
ip_unlock(port);
ipc_reverse_remove(space, (ipc_object_t) port);
} else {
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_NONE);
assert(IE_BITS_UREFS(bits) == 0);
ip_unlock(port);
}
entry->ie_bits = bits | MACH_PORT_TYPE_RECEIVE;
if (dest != IP_NULL)
ipc_port_release(dest);
break;
}
default:
#if MACH_ASSERT
assert(!"ipc_right_copyout: strange rights");
#else
panic("ipc_right_copyout: strange rights");
#endif
}
return KERN_SUCCESS;
}
kern_return_t
ipc_right_rename(
ipc_space_t space,
mach_port_name_t oname,
ipc_entry_t oentry,
mach_port_name_t nname,
ipc_entry_t nentry)
{
ipc_entry_bits_t bits = oentry->ie_bits;
ipc_port_request_index_t request = oentry->ie_request;
ipc_object_t object = oentry->ie_object;
assert(space->is_active);
assert(oname != nname);
if (request != 0) {
ipc_port_t port;
assert(bits & MACH_PORT_TYPE_PORT_RIGHTS);
port = (ipc_port_t) object;
assert(port != IP_NULL);
if (ipc_right_check(space, port, oname, oentry)) {
bits = oentry->ie_bits;
assert(IE_BITS_TYPE(bits) == MACH_PORT_TYPE_DEAD_NAME);
assert(oentry->ie_request == 0);
request = 0;
assert(oentry->ie_object == IO_NULL);
object = IO_NULL;
} else {
ipc_port_dnrename(port, request, oname, nname);
ip_unlock(port);
oentry->ie_request = 0;
}
}
if (bits & IE_BITS_MAREQUEST) {
assert(bits & MACH_PORT_TYPE_SEND_RECEIVE);
ipc_marequest_rename(space, oname, nname);
}
assert((nentry->ie_bits & IE_BITS_RIGHT_MASK) == 0);
nentry->ie_bits |= bits & IE_BITS_RIGHT_MASK;
nentry->ie_request = request;
nentry->ie_object = object;
switch (IE_BITS_TYPE(bits)) {
case MACH_PORT_TYPE_SEND: {
ipc_port_t port;
port = (ipc_port_t) object;
assert(port != IP_NULL);
ipc_reverse_remove(space, (ipc_object_t) port);
nentry->ie_name = nname;
ipc_reverse_insert(space, (ipc_object_t) port, nentry);
break;
}
case MACH_PORT_TYPE_RECEIVE:
case MACH_PORT_TYPE_SEND_RECEIVE: {
ipc_port_t port;
port = (ipc_port_t) object;
assert(port != IP_NULL);
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name == oname);
assert(port->ip_receiver == space);
port->ip_receiver_name = nname;
ip_unlock(port);
break;
}
case MACH_PORT_TYPE_PORT_SET: {
ipc_pset_t pset;
pset = (ipc_pset_t) object;
assert(pset != IPS_NULL);
ips_lock(pset);
assert(ips_active(pset));
assert(pset->ips_local_name == oname);
pset->ips_local_name = nname;
ips_unlock(pset);
break;
}
case MACH_PORT_TYPE_SEND_ONCE:
case MACH_PORT_TYPE_DEAD_NAME:
break;
default:
#if MACH_ASSERT
assert(!"ipc_right_rename: strange rights");
#else
panic("ipc_right_rename: strange rights");
#endif
}
assert(oentry->ie_request == 0);
oentry->ie_object = IO_NULL;
ipc_entry_dealloc(space, oname, oentry);
is_write_unlock(space);
return KERN_SUCCESS;
}