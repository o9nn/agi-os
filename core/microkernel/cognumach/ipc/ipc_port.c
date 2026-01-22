#include <kern/printf.h>
#include <string.h>
#include <mach/port.h>
#include <mach/kern_return.h>
#include <kern/lock.h>
#include <kern/ipc_sched.h>
#include <kern/ipc_kobject.h>
#include <ipc/ipc_entry.h>
#include <ipc/ipc_space.h>
#include <ipc/ipc_object.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_pset.h>
#include <ipc/ipc_thread.h>
#include <ipc/ipc_mqueue.h>
#include <ipc/ipc_notify.h>
#if MACH_KDB
#include <ddb/db_output.h>
#include <ipc/ipc_print.h>
#endif
def_simple_lock_data(, ipc_port_multiple_lock_data)
def_simple_lock_data(, ipc_port_timestamp_lock_data)
ipc_port_timestamp_t ipc_port_timestamp_data;
ipc_port_timestamp_t
ipc_port_timestamp(void)
{
ipc_port_timestamp_t timestamp;
ipc_port_timestamp_lock();
timestamp = ipc_port_timestamp_data++;
ipc_port_timestamp_unlock();
return timestamp;
}
kern_return_t
ipc_port_dnrequest(
ipc_port_t port,
mach_port_name_t name,
ipc_port_t soright,
ipc_port_request_index_t *indexp)
{
ipc_port_request_t ipr, table;
ipc_port_request_index_t index;
assert(ip_active(port));
assert(name != MACH_PORT_NULL);
assert(soright != IP_NULL);
table = port->ip_dnrequests;
if (table == IPR_NULL)
return KERN_NO_SPACE;
index = table->ipr_next;
if (index == 0)
return KERN_NO_SPACE;
ipr = &table[index];
assert(ipr->ipr_name == MACH_PORT_NULL);
table->ipr_next = ipr->ipr_next;
ipr->ipr_name = name;
ipr->ipr_soright = soright;
*indexp = index;
return KERN_SUCCESS;
}
kern_return_t
ipc_port_dngrow(ipc_port_t port)
{
ipc_table_size_t its;
ipc_port_request_t otable, ntable;
assert(ip_active(port));
otable = port->ip_dnrequests;
if (otable == IPR_NULL)
its = &ipc_table_dnrequests[0];
else
its = otable->ipr_size + 1;
ip_reference(port);
ip_unlock(port);
if ((its->its_size == 0) ||
((ntable = it_dnrequests_alloc(its)) == IPR_NULL)) {
ipc_port_release(port);
return KERN_RESOURCE_SHORTAGE;
}
ip_lock(port);
ip_release(port);
if (ip_active(port) &&
(port->ip_dnrequests == otable) &&
((otable == IPR_NULL) || (otable->ipr_size+1 == its))) {
ipc_table_size_t oits = 0;
ipc_table_elems_t osize, nsize;
ipc_port_request_index_t free, i;
if (otable != IPR_NULL) {
oits = otable->ipr_size;
osize = oits->its_size;
free = otable->ipr_next;
memcpy((ntable + 1), (otable + 1),
(osize - 1) * sizeof(struct ipc_port_request));
} else {
osize = 1;
free = 0;
}
nsize = its->its_size;
assert(nsize > osize);
for (i = osize; i < nsize; i++) {
ipc_port_request_t ipr = &ntable[i];
ipr->ipr_name = MACH_PORT_NULL;
ipr->ipr_next = free;
free = i;
}
ntable->ipr_next = free;
ntable->ipr_size = its;
port->ip_dnrequests = ntable;
ip_unlock(port);
if (otable != IPR_NULL)
it_dnrequests_free(oits, otable);
} else {
ip_check_unlock(port);
it_dnrequests_free(its, ntable);
}
return KERN_SUCCESS;
}
ipc_port_t
ipc_port_dncancel(
ipc_port_t port,
mach_port_name_t name,
ipc_port_request_index_t index)
{
ipc_port_request_t ipr, table;
ipc_port_t dnrequest;
assert(ip_active(port));
assert(name != MACH_PORT_NULL);
assert(index != 0);
table = port->ip_dnrequests;
assert(table != IPR_NULL);
ipr = &table[index];
dnrequest = ipr->ipr_soright;
assert(ipr->ipr_name == name);
ipr->ipr_name = MACH_PORT_NULL;
ipr->ipr_next = table->ipr_next;
table->ipr_next = index;
return dnrequest;
}
void
ipc_port_pdrequest(
ipc_port_t port,
const ipc_port_t notify,
ipc_port_t *previousp)
{
ipc_port_t previous;
assert(ip_active(port));
previous = port->ip_pdrequest;
port->ip_pdrequest = notify;
ip_unlock(port);
*previousp = previous;
}
void
ipc_port_nsrequest(
ipc_port_t port,
mach_port_mscount_t sync,
ipc_port_t notify,
ipc_port_t *previousp)
{
ipc_port_t previous;
mach_port_mscount_t mscount;
assert(ip_active(port));
previous = port->ip_nsrequest;
mscount = port->ip_mscount;
if ((port->ip_srights == 0) &&
(sync <= mscount) &&
(notify != IP_NULL)) {
port->ip_nsrequest = IP_NULL;
ip_unlock(port);
ipc_notify_no_senders(notify, mscount);
} else {
port->ip_nsrequest = notify;
ip_unlock(port);
}
*previousp = previous;
}
void
ipc_port_set_qlimit(
ipc_port_t port,
mach_port_msgcount_t qlimit)
{
assert(ip_active(port));
if (qlimit > port->ip_qlimit) {
mach_port_msgcount_t i, wakeup;
wakeup = qlimit - port->ip_qlimit;
for (i = 0; i < wakeup; i++) {
ipc_thread_t th;
th = ipc_thread_dequeue(&port->ip_blocked);
if (th == ITH_NULL)
break;
th->ith_state = MACH_MSG_SUCCESS;
thread_go(th);
}
}
port->ip_qlimit = qlimit;
}
ipc_mqueue_t
ipc_port_lock_mqueue(ipc_port_t port)
{
if (port->ip_pset != IPS_NULL) {
ipc_pset_t pset = port->ip_pset;
ips_lock(pset);
if (ips_active(pset)) {
imq_lock(&pset->ips_messages);
ips_unlock(pset);
return &pset->ips_messages;
}
ipc_pset_remove(pset, port);
ips_check_unlock(pset);
}
imq_lock(&port->ip_messages);
return &port->ip_messages;
}
void
ipc_port_set_seqno(
ipc_port_t port,
mach_port_seqno_t seqno)
{
ipc_mqueue_t mqueue;
mqueue = ipc_port_lock_mqueue(port);
port->ip_seqno = seqno;
imq_unlock(mqueue);
}
void
ipc_port_set_protected_payload(ipc_port_t port, rpc_uintptr_t payload)
{
ipc_mqueue_t mqueue;
mqueue = ipc_port_lock_mqueue(port);
port->ip_protected_payload = payload;
ipc_port_flag_protected_payload_set(port);
imq_unlock(mqueue);
}
void
ipc_port_clear_protected_payload(ipc_port_t port)
{
ipc_mqueue_t mqueue;
mqueue = ipc_port_lock_mqueue(port);
ipc_port_flag_protected_payload_clear(port);
imq_unlock(mqueue);
}
void
ipc_port_clear_receiver(
ipc_port_t port)
{
ipc_pset_t pset;
assert(ip_active(port));
pset = port->ip_pset;
if (pset != IPS_NULL) {
ips_lock(pset);
ipc_pset_remove(pset, port);
ips_check_unlock(pset);
} else {
imq_lock(&port->ip_messages);
ipc_mqueue_changed(&port->ip_messages, MACH_RCV_PORT_DIED);
imq_unlock(&port->ip_messages);
}
ipc_port_set_mscount(port, 0);
imq_lock(&port->ip_messages);
port->ip_seqno = 0;
imq_unlock(&port->ip_messages);
}
void
ipc_port_init(
ipc_port_t port,
ipc_space_t space,
mach_port_name_t name)
{
ipc_target_init(&port->ip_target, name);
port->ip_receiver = space;
port->ip_mscount = 0;
port->ip_srights = 0;
port->ip_sorights = 0;
port->ip_nsrequest = IP_NULL;
port->ip_pdrequest = IP_NULL;
port->ip_dnrequests = IPR_NULL;
port->ip_pset = IPS_NULL;
port->ip_cur_target = &port->ip_target;
port->ip_seqno = 0;
port->ip_msgcount = 0;
port->ip_qlimit = MACH_PORT_QLIMIT_DEFAULT;
ipc_port_flag_protected_payload_clear(port);
port->ip_protected_payload = 0;
ipc_mqueue_init(&port->ip_messages);
ipc_thread_queue_init(&port->ip_blocked);
}
kern_return_t
ipc_port_alloc(
ipc_space_t space,
mach_port_name_t *namep,
ipc_port_t *portp)
{
ipc_port_t port;
mach_port_name_t name;
kern_return_t kr;
kr = ipc_object_alloc(space, IOT_PORT,
MACH_PORT_TYPE_RECEIVE, 0,
&name, (ipc_object_t *) &port);
if (kr != KERN_SUCCESS)
return kr;
ipc_port_init(port, space, name);
*namep = name;
*portp = port;
return KERN_SUCCESS;
}
kern_return_t
ipc_port_alloc_name(
ipc_space_t space,
mach_port_name_t name,
ipc_port_t *portp)
{
ipc_port_t port;
kern_return_t kr;
kr = ipc_object_alloc_name(space, IOT_PORT,
MACH_PORT_TYPE_RECEIVE, 0,
name, (ipc_object_t *) &port);
if (kr != KERN_SUCCESS)
return kr;
ipc_port_init(port, space, name);
*portp = port;
return KERN_SUCCESS;
}
void
ipc_port_destroy(
ipc_port_t port)
{
ipc_port_t pdrequest, nsrequest;
ipc_mqueue_t mqueue;
ipc_kmsg_queue_t kmqueue;
ipc_kmsg_t kmsg;
ipc_thread_t sender;
ipc_port_request_t dnrequests;
assert(ip_active(port));
assert(port->ip_pset == IPS_NULL);
assert(port->ip_mscount == 0);
assert(port->ip_seqno == 0);
pdrequest = port->ip_pdrequest;
if (pdrequest != IP_NULL) {
port->ip_pdrequest = IP_NULL;
port->ip_receiver_name = MACH_PORT_NULL;
port->ip_destination = IP_NULL;
ipc_port_flag_protected_payload_clear(port);
ip_unlock(port);
if (!ipc_port_check_circularity(port, pdrequest)) {
ipc_notify_port_destroyed(pdrequest, port);
return;
} else {
ipc_port_release_sonce(pdrequest);
}
ip_lock(port);
assert(ip_active(port));
assert(port->ip_pset == IPS_NULL);
assert(port->ip_mscount == 0);
assert(port->ip_seqno == 0);
assert(port->ip_pdrequest == IP_NULL);
assert(port->ip_receiver_name == MACH_PORT_NULL);
assert(port->ip_destination == IP_NULL);
}
while ((sender = ipc_thread_dequeue(&port->ip_blocked)) != ITH_NULL) {
sender->ith_state = MACH_MSG_SUCCESS;
thread_go(sender);
}
port->ip_object.io_bits &= ~IO_BITS_ACTIVE;
port->ip_timestamp = ipc_port_timestamp();
ip_unlock(port);
nsrequest = port->ip_nsrequest;
if (nsrequest != IP_NULL)
ipc_notify_send_once(nsrequest);
mqueue = &port->ip_messages;
imq_lock(mqueue);
assert(ipc_thread_queue_empty(&mqueue->imq_threads));
kmqueue = &mqueue->imq_messages;
while ((kmsg = ipc_kmsg_dequeue(kmqueue)) != IKM_NULL) {
imq_unlock(mqueue);
assert(kmsg->ikm_header.msgh_remote_port ==
(mach_port_t) port);
ipc_port_release(port);
kmsg->ikm_header.msgh_remote_port = MACH_PORT_NULL;
ipc_kmsg_destroy(kmsg);
imq_lock(mqueue);
}
imq_unlock(mqueue);
dnrequests = port->ip_dnrequests;
if (dnrequests != IPR_NULL) {
ipc_table_size_t its = dnrequests->ipr_size;
ipc_table_elems_t size = its->its_size;
ipc_port_request_index_t index;
for (index = 1; index < size; index++) {
ipc_port_request_t ipr = &dnrequests[index];
mach_port_name_t name = ipr->ipr_name;
ipc_port_t soright;
if (name == MACH_PORT_NULL)
continue;
soright = ipr->ipr_soright;
assert(soright != IP_NULL);
ipc_notify_dead_name(soright, name);
}
it_dnrequests_free(its, dnrequests);
}
if (ip_kotype(port) != IKOT_NONE)
ipc_kobject_destroy(port);
ipc_target_terminate(&port->ip_target);
ipc_port_release(port);
}
boolean_t
ipc_port_check_circularity(
ipc_port_t port,
ipc_port_t dest)
{
ipc_port_t base;
assert(port != IP_NULL);
assert(dest != IP_NULL);
if (port == dest)
return TRUE;
base = dest;
ip_lock(port);
if (ip_lock_try(dest)) {
if (!ip_active(dest) ||
(dest->ip_receiver_name != MACH_PORT_NULL) ||
(dest->ip_destination == IP_NULL))
goto not_circular;
ip_unlock(dest);
}
ip_unlock(port);
ipc_port_multiple_lock();
for (;;) {
ip_lock(base);
if (!ip_active(base) ||
(base->ip_receiver_name != MACH_PORT_NULL) ||
(base->ip_destination == IP_NULL))
break;
base = base->ip_destination;
}
if (port == base) {
ipc_port_multiple_unlock();
assert(ip_active(port));
assert(port->ip_receiver_name == MACH_PORT_NULL);
assert(port->ip_destination == IP_NULL);
while (dest != IP_NULL) {
ipc_port_t next;
assert(ip_active(dest));
assert(dest->ip_receiver_name == MACH_PORT_NULL);
next = dest->ip_destination;
ip_unlock(dest);
dest = next;
}
return TRUE;
}
ip_lock(port);
ipc_port_multiple_unlock();
not_circular:
assert(ip_active(port));
assert(port->ip_receiver_name == MACH_PORT_NULL);
assert(port->ip_destination == IP_NULL);
ip_reference(dest);
port->ip_destination = dest;
while (port != base) {
ipc_port_t next;
assert(ip_active(port));
assert(port->ip_receiver_name == MACH_PORT_NULL);
assert(port->ip_destination != IP_NULL);
next = port->ip_destination;
ip_unlock(port);
port = next;
}
assert(!ip_active(base) ||
(base->ip_receiver_name != MACH_PORT_NULL) ||
(base->ip_destination == IP_NULL));
ip_unlock(base);
return FALSE;
}
ipc_port_t
ipc_port_lookup_notify(
ipc_space_t space,
mach_port_name_t name)
{
ipc_port_t port;
ipc_entry_t entry;
assert(space->is_active);
entry = ipc_entry_lookup(space, name);
if (entry == IE_NULL)
return IP_NULL;
if ((entry->ie_bits & MACH_PORT_TYPE_RECEIVE) == 0)
return IP_NULL;
port = (ipc_port_t) entry->ie_object;
assert(port != IP_NULL);
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name == name);
assert(port->ip_receiver == space);
ip_reference(port);
port->ip_sorights++;
ip_unlock(port);
return port;
}
ipc_port_t
ipc_port_make_send(
ipc_port_t port)
{
assert(IP_VALID(port));
ip_lock(port);
assert(ip_active(port));
port->ip_mscount++;
port->ip_srights++;
ip_reference(port);
ip_unlock(port);
return port;
}
ipc_port_t
ipc_port_copy_send(
ipc_port_t port)
{
ipc_port_t sright;
if (!IP_VALID(port))
return port;
ip_lock(port);
if (ip_active(port)) {
assert(port->ip_srights > 0);
ip_reference(port);
port->ip_srights++;
sright = port;
} else
sright = IP_DEAD;
ip_unlock(port);
return sright;
}
mach_port_name_t
ipc_port_copyout_send(
ipc_port_t sright,
ipc_space_t space)
{
mach_port_name_t name;
if (IP_VALID(sright)) {
kern_return_t kr;
kr = ipc_object_copyout(space, (ipc_object_t) sright,
MACH_MSG_TYPE_PORT_SEND, TRUE, &name);
if (kr != KERN_SUCCESS) {
ipc_port_release_send(sright);
if (kr == KERN_INVALID_CAPABILITY)
name = MACH_PORT_NAME_DEAD;
else
name = MACH_PORT_NAME_NULL;
}
} else
name = invalid_port_to_name((mach_port_t)sright);
return name;
}
void
ipc_port_release_send(
ipc_port_t port)
{
ipc_port_t nsrequest = IP_NULL;
mach_port_mscount_t mscount;
assert(IP_VALID(port));
ip_lock(port);
ip_release(port);
if (!ip_active(port)) {
ip_check_unlock(port);
return;
}
assert(port->ip_srights > 0);
if (--port->ip_srights == 0) {
nsrequest = port->ip_nsrequest;
if (nsrequest != IP_NULL) {
port->ip_nsrequest = IP_NULL;
mscount = port->ip_mscount;
}
}
ip_unlock(port);
if (nsrequest != IP_NULL)
ipc_notify_no_senders(nsrequest, mscount);
}
ipc_port_t
ipc_port_make_sonce(
ipc_port_t port)
{
assert(IP_VALID(port));
ip_lock(port);
assert(ip_active(port));
port->ip_sorights++;
ip_reference(port);
ip_unlock(port);
return port;
}
void
ipc_port_release_sonce(
ipc_port_t port)
{
assert(IP_VALID(port));
ip_lock(port);
assert(port->ip_sorights > 0);
port->ip_sorights--;
ip_release(port);
if (!ip_active(port)) {
ip_check_unlock(port);
return;
}
ip_unlock(port);
}
void
ipc_port_release_receive(
ipc_port_t port)
{
ipc_port_t dest;
assert(IP_VALID(port));
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name == MACH_PORT_NULL);
dest = port->ip_destination;
ipc_port_destroy(port);
if (dest != IP_NULL)
ipc_port_release(dest);
}
ipc_port_t
ipc_port_alloc_special(ipc_space_t space)
{
ipc_port_t port;
port = ip_alloc();
if (port == IP_NULL)
return IP_NULL;
ip_lock_init(port);
port->ip_references = 1;
port->ip_object.io_bits = io_makebits(TRUE, IOT_PORT, 0);
ipc_port_init(port, space, (mach_port_name_t)port);
return port;
}
void
ipc_port_dealloc_special(
ipc_port_t port,
ipc_space_t space)
{
ip_lock(port);
assert(ip_active(port));
assert(port->ip_receiver_name != MACH_PORT_NULL);
assert(port->ip_receiver == space);
port->ip_receiver_name = MACH_PORT_NULL;
port->ip_receiver = IS_NULL;
ipc_port_clear_receiver(port);
ipc_port_destroy(port);
}
#if MACH_KDB
#define printf kdbprintf
void
ipc_port_print(const ipc_port_t port)
{
printf("port 0x%x\n", port);
indent += 2;
iprintf("flags ");
printf("has_protected_payload=%d",
ipc_port_flag_protected_payload(port));
printf("\n");
ipc_object_print(&port->ip_object);
iprintf("receiver=0x%x", port->ip_receiver);
printf(", receiver_name=0x%x\n", port->ip_receiver_name);
iprintf("mscount=%d", port->ip_mscount);
printf(", srights=%d", port->ip_srights);
printf(", sorights=%d\n", port->ip_sorights);
iprintf("nsrequest=0x%x", port->ip_nsrequest);
printf(", pdrequest=0x%x", port->ip_pdrequest);
printf(", dnrequests=0x%x\n", port->ip_dnrequests);
iprintf("pset=0x%x", port->ip_pset);
printf(", seqno=%d", port->ip_seqno);
printf(", msgcount=%d", port->ip_msgcount);
printf(", qlimit=%d\n", port->ip_qlimit);
iprintf("kmsgs=0x%x", port->ip_messages.imq_messages.ikmq_base);
printf(", rcvrs=0x%x", port->ip_messages.imq_threads.ithq_base);
printf(", sndrs=0x%x", port->ip_blocked.ithq_base);
printf(", kobj=0x%x\n", port->ip_kobject);
iprintf("protected_payload=%p\n", (void *) (vm_offset_t) port->ip_protected_payload);
indent -= 2;
}
#endif