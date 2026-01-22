#include <sys/types.h>
#include <string.h>
#include <device/net_status.h>
#include <machine/spl.h>
#include <device/net_io.h>
#include <device/if_hdr.h>
#include <device/io_req.h>
#include <device/ds_routines.h>
#include <mach/boolean.h>
#include <mach/vm_param.h>
#include <ipc/ipc_port.h>
#include <ipc/ipc_kmsg.h>
#include <ipc/ipc_mqueue.h>
#include <kern/counters.h>
#include <kern/debug.h>
#include <kern/lock.h>
#include <kern/printf.h>
#include <kern/queue.h>
#include <kern/sched_prim.h>
#include <kern/slab.h>
#include <kern/thread.h>
#include <machine/spl.h>
#if MACH_TTD
#include <ttd/ttd_stub.h>
#endif
#if MACH_TTD
int kttd_async_counter= 0;
#endif
def_simple_lock_data(static,net_queue_lock)
boolean_t net_thread_awake = FALSE;
struct ipc_kmsg_queue net_queue_high;
int net_queue_high_size = 0;
int net_queue_high_max = 0;
struct ipc_kmsg_queue net_queue_low;
int net_queue_low_size = 0;
int net_queue_low_max = 0;
def_simple_lock_data(static,net_queue_free_lock)
struct ipc_kmsg_queue net_queue_free;
int net_queue_free_size = 0;
int net_queue_free_max = 0;
int net_queue_free_min = 3;
int net_queue_free_hits = 0;
int net_queue_free_steals = 0;
int net_queue_free_misses = 0;
int net_kmsg_send_high_hits = 0;
int net_kmsg_send_low_hits = 0;
int net_kmsg_send_high_misses = 0;
int net_kmsg_send_low_misses = 0;
int net_thread_awaken = 0;
int net_ast_taken = 0;
def_simple_lock_data(static,net_kmsg_total_lock)
int net_kmsg_total = 0;
int net_kmsg_max;
vm_size_t net_kmsg_size;
#define net_kmsg_want_more() \
(((net_queue_free_size + net_queue_low_size) < net_queue_free_min) && \
(net_kmsg_total < net_kmsg_max))
ipc_kmsg_t
net_kmsg_get(void)
{
ipc_kmsg_t kmsg;
spl_t s;
s = splimp();
simple_lock(&net_queue_free_lock);
kmsg = ipc_kmsg_queue_first(&net_queue_free);
if (kmsg != IKM_NULL) {
ipc_kmsg_rmqueue_first_macro(&net_queue_free, kmsg);
net_queue_free_size--;
net_queue_free_hits++;
}
simple_unlock(&net_queue_free_lock);
if (kmsg == IKM_NULL) {
simple_lock(&net_queue_lock);
kmsg = ipc_kmsg_queue_first(&net_queue_low);
if (kmsg != IKM_NULL) {
ipc_kmsg_rmqueue_first_macro(&net_queue_low, kmsg);
net_queue_low_size--;
net_queue_free_steals++;
}
simple_unlock(&net_queue_lock);
}
if (kmsg == IKM_NULL)
net_queue_free_misses++;
(void) splx(s);
if (net_kmsg_want_more() || (kmsg == IKM_NULL)) {
boolean_t awake;
s = splimp();
simple_lock(&net_queue_lock);
awake = net_thread_awake;
net_thread_awake = TRUE;
simple_unlock(&net_queue_lock);
(void) splx(s);
if (!awake)
thread_wakeup((event_t) &net_thread_awake);
}
return kmsg;
}
void
net_kmsg_put(const ipc_kmsg_t kmsg)
{
spl_t s;
s = splimp();
simple_lock(&net_queue_free_lock);
ipc_kmsg_enqueue_macro(&net_queue_free, kmsg);
if (++net_queue_free_size > net_queue_free_max)
net_queue_free_max = net_queue_free_size;
simple_unlock(&net_queue_free_lock);
(void) splx(s);
}
void
net_kmsg_collect(void)
{
ipc_kmsg_t kmsg;
spl_t s;
s = splimp();
simple_lock(&net_queue_free_lock);
while (net_queue_free_size > net_queue_free_min) {
kmsg = ipc_kmsg_dequeue(&net_queue_free);
net_queue_free_size--;
simple_unlock(&net_queue_free_lock);
(void) splx(s);
net_kmsg_free(kmsg);
simple_lock(&net_kmsg_total_lock);
net_kmsg_total--;
simple_unlock(&net_kmsg_total_lock);
s = splimp();
simple_lock(&net_queue_free_lock);
}
simple_unlock(&net_queue_free_lock);
(void) splx(s);
}
static void
net_kmsg_more(void)
{
ipc_kmsg_t kmsg;
while (net_kmsg_want_more()) {
simple_lock(&net_kmsg_total_lock);
net_kmsg_total++;
simple_unlock(&net_kmsg_total_lock);
kmsg = net_kmsg_alloc();
net_kmsg_put(kmsg);
}
}
struct net_rcv_port {
queue_chain_t input;
queue_chain_t output;
ipc_port_t rcv_port;
int rcv_qlimit;
int rcv_count;
int priority;
filter_t *filter_end;
filter_t filter[NET_MAX_FILTER];
};
struct kmem_cache net_rcv_cache;
#define NET_HASH_SIZE 256
#define N_NET_HASH 4
#define N_NET_HASH_KEYS 4
struct net_hash_entry {
queue_chain_t chain;
#define he_next chain.next
#define he_prev chain.prev
ipc_port_t rcv_port;
int rcv_qlimit;
unsigned int keys[N_NET_HASH_KEYS];
};
struct kmem_cache net_hash_entry_cache;
struct net_hash_header {
struct net_rcv_port rcv;
int n_keys;
int ref_count;
net_hash_entry_t table[NET_HASH_SIZE];
} filter_hash_header[N_NET_HASH];
def_simple_lock_data(static,net_hash_header_lock)
#define HASH_ITERATE(head, elt) (elt) = (net_hash_entry_t) (head); do {
#define HASH_ITERATE_END(head, elt) \
(elt) = (net_hash_entry_t) queue_next((queue_entry_t) (elt)); \
} while ((elt) != (head));
#define FILTER_ITERATE(if_port_list, fp, nextfp, chain) \
for ((fp) = (net_rcv_port_t) queue_first(if_port_list); \
!queue_end(if_port_list, (queue_entry_t)(fp)); \
(fp) = (nextfp)) { \
(nextfp) = (net_rcv_port_t) queue_next(chain);
#define FILTER_ITERATE_END }
#define ENQUEUE_DEAD(dead, entry_p, chain) \
MACRO_BEGIN \
(entry_p)->chain.next = (queue_entry_t) (dead); \
(dead) = (queue_entry_t)(entry_p); \
MACRO_END
boolean_t ethernet_priority(const ipc_kmsg_t kmsg)
{
unsigned char *addr =
(unsigned char *) net_kmsg(kmsg)->header;
if ((addr[0] == 0xff) && (addr[1] == 0xff) &&
(addr[2] == 0xff) && (addr[3] == 0xff) &&
(addr[4] == 0xff) && (addr[5] == 0xff))
return FALSE;
else
return TRUE;
}
mach_msg_type_t header_type = {
.msgt_name = MACH_MSG_TYPE_BYTE,
.msgt_size = 8,
.msgt_number = NET_HDW_HDR_MAX,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
mach_msg_type_t packet_type = {
.msgt_name = MACH_MSG_TYPE_BYTE,
.msgt_size = 8,
.msgt_number = 0,
.msgt_inline = TRUE,
.msgt_longform = FALSE,
.msgt_deallocate = FALSE,
.msgt_unused = 0
};
static boolean_t net_deliver(boolean_t nonblocking)
{
ipc_kmsg_t kmsg;
boolean_t high_priority;
struct ipc_kmsg_queue send_list;
if ((kmsg = ipc_kmsg_dequeue(&net_queue_high)) != IKM_NULL) {
net_queue_high_size--;
high_priority = TRUE;
} else if ((kmsg = ipc_kmsg_dequeue(&net_queue_low)) != IKM_NULL) {
net_queue_low_size--;
high_priority = FALSE;
} else
return FALSE;
simple_unlock(&net_queue_lock);
(void) spl0();
net_filter(kmsg, &send_list);
if (!nonblocking) {
net_kmsg_more();
}
while ((kmsg = ipc_kmsg_dequeue(&send_list)) != IKM_NULL) {
int count;
count = net_kmsg(kmsg)->net_rcv_msg_packet_count;
ikm_init_special(kmsg, IKM_SIZE_NETWORK);
kmsg->ikm_header.msgh_bits =
MACH_MSGH_BITS(MACH_MSG_TYPE_PORT_SEND, 0);
kmsg->ikm_header.msgh_size =
(mach_msg_size_t) P2ROUND(sizeof(struct net_rcv_msg)
- sizeof net_kmsg(kmsg)->sent
- NET_RCV_MAX + count,
__alignof__ (uintptr_t));
kmsg->ikm_header.msgh_local_port = MACH_PORT_NULL;
kmsg->ikm_header.msgh_kind = MACH_MSGH_KIND_NORMAL;
kmsg->ikm_header.msgh_id = NET_RCV_MSG_ID;
net_kmsg(kmsg)->header_type = header_type;
net_kmsg(kmsg)->packet_type = packet_type;
net_kmsg(kmsg)->net_rcv_msg_packet_count = count;
if (ipc_mqueue_send(kmsg, MACH_SEND_TIMEOUT, 0) ==
MACH_MSG_SUCCESS) {
if (high_priority)
net_kmsg_send_high_hits++;
else
net_kmsg_send_low_hits++;
} else {
if (high_priority)
net_kmsg_send_high_misses++;
else
net_kmsg_send_low_misses++;
ipc_kmsg_destroy(kmsg);
}
}
(void) splimp();
simple_lock(&net_queue_lock);
return TRUE;
}
void net_ast(void)
{
spl_t s;
net_ast_taken++;
s = splimp();
simple_lock(&net_queue_lock);
while (!net_thread_awake && net_deliver(TRUE))
continue;
simple_unlock(&net_queue_lock);
(void) splsched();
ast_off(cpu_number(), AST_NETWORK);
(void) splx(s);
}
static void __attribute__ ((noreturn)) net_thread_continue(void)
{
for (;;) {
spl_t s;
net_thread_awaken++;
net_kmsg_more();
s = splimp();
simple_lock(&net_queue_lock);
while (net_deliver(FALSE))
continue;
net_thread_awake = FALSE;
assert_wait(&net_thread_awake, FALSE);
simple_unlock(&net_queue_lock);
(void) splx(s);
counter(c_net_thread_block++);
thread_block(net_thread_continue);
}
}
void net_thread(void)
{
spl_t s;
thread_set_own_priority(0);
s = splimp();
simple_lock(&net_queue_lock);
net_thread_awake = FALSE;
assert_wait(&net_thread_awake, FALSE);
simple_unlock(&net_queue_lock);
(void) splx(s);
counter(c_net_thread_block++);
thread_block(net_thread_continue);
net_thread_continue();
}
static void
reorder_queue(
queue_t first,
queue_t last)
{
queue_entry_t prev, next;
prev = first->prev;
next = last->next;
prev->next = last;
next->prev = first;
last->prev = prev;
last->next = first;
first->next = next;
first->prev = last;
}
void
net_packet(
struct ifnet *ifp,
ipc_kmsg_t kmsg,
unsigned int count,
boolean_t priority)
{
boolean_t awake;
#if MACH_TTD
if (kttd_enabled && kttd_handle_async(kmsg)) {
if (kttd_debug)
printf("**%x**", kttd_async_counter++);
return;
}
#endif
kmsg->ikm_header.msgh_remote_port = (mach_port_t) ifp;
net_kmsg(kmsg)->net_rcv_msg_packet_count = count;
simple_lock(&net_queue_lock);
if (priority) {
ipc_kmsg_enqueue(&net_queue_high, kmsg);
if (++net_queue_high_size > net_queue_high_max)
net_queue_high_max = net_queue_high_size;
} else {
ipc_kmsg_enqueue(&net_queue_low, kmsg);
if (++net_queue_low_size > net_queue_low_max)
net_queue_low_max = net_queue_low_size;
}
awake = net_thread_awake;
simple_unlock(&net_queue_lock);
if (!awake) {
spl_t s = splsched();
ast_on(cpu_number(), AST_NETWORK);
(void) splx(s);
}
}
int net_filter_queue_reorder = 0;
void
net_filter(const ipc_kmsg_t kmsg,
ipc_kmsg_queue_t send_list)
{
struct ifnet *ifp;
net_rcv_port_t infp, nextfp;
ipc_kmsg_t new_kmsg;
net_hash_entry_t entp, *hash_headp;
ipc_port_t dest;
queue_entry_t dead_infp = (queue_entry_t) 0;
queue_entry_t dead_entp = (queue_entry_t) 0;
unsigned int ret_count;
queue_head_t *if_port_list;
int count = net_kmsg(kmsg)->net_rcv_msg_packet_count;
ifp = (struct ifnet *) kmsg->ikm_header.msgh_remote_port;
ipc_kmsg_queue_init(send_list);
if (net_kmsg(kmsg)->sent)
if_port_list = &ifp->if_snd_port_list;
else
if_port_list = &ifp->if_rcv_port_list;
simple_lock(&ifp->if_rcv_port_list_lock);
simple_lock(&ifp->if_snd_port_list_lock);
FILTER_ITERATE(if_port_list, infp, nextfp,
net_kmsg(kmsg)->sent ? &infp->output : &infp->input)
{
entp = (net_hash_entry_t) 0;
if ((infp->filter[0] & NETF_TYPE_MASK) == NETF_BPF) {
ret_count = bpf_do_filter(infp, net_kmsg(kmsg)->packet
+ sizeof(struct packet_header),
count - sizeof(struct packet_header),
net_kmsg(kmsg)->header,
ifp->if_header_size, &hash_headp,
&entp);
if (entp == (net_hash_entry_t) 0)
dest = infp->rcv_port;
else
dest = entp->rcv_port;
if (ret_count)
ret_count += sizeof(struct packet_header);
} else {
ret_count = net_do_filter(infp, net_kmsg(kmsg)->packet, count,
net_kmsg(kmsg)->header);
if (ret_count)
ret_count = count;
dest = infp->rcv_port;
}
if (ret_count) {
dest = ipc_port_copy_send(dest);
if (!IP_VALID(dest)) {
if (entp == (net_hash_entry_t) 0) {
if (infp->filter[0] & NETF_IN)
queue_remove(&ifp->if_rcv_port_list, infp,
net_rcv_port_t, input);
if (infp->filter[0] & NETF_OUT)
queue_remove(&ifp->if_snd_port_list, infp,
net_rcv_port_t, output);
ENQUEUE_DEAD(dead_infp, infp, input);
continue;
} else {
hash_ent_remove (ifp,
(net_hash_header_t)infp,
FALSE,
hash_headp,
entp,
&dead_entp);
continue;
}
}
if (ipc_kmsg_queue_empty(send_list)) {
new_kmsg = kmsg;
} else {
new_kmsg = net_kmsg_get();
if (new_kmsg == IKM_NULL) {
ipc_port_release_send(dest);
break;
}
memcpy(
net_kmsg(new_kmsg)->packet,
net_kmsg(kmsg)->packet,
ret_count);
memcpy(
net_kmsg(new_kmsg)->header,
net_kmsg(kmsg)->header,
NET_HDW_HDR_MAX);
}
net_kmsg(new_kmsg)->net_rcv_msg_packet_count = ret_count;
new_kmsg->ikm_header.msgh_remote_port = (mach_port_t) dest;
ipc_kmsg_enqueue(send_list, new_kmsg);
{
net_rcv_port_t prevfp;
int rcount = ++infp->rcv_count;
if (infp->priority >= NET_HI_PRI) {
#define REORDER_PRIO(chain) \
prevfp = (net_rcv_port_t) queue_prev(&infp->chain); \
\
if ((queue_t)prevfp != if_port_list && \
infp->priority == prevfp->priority) { \
\
if (net_filter_queue_reorder \
&& (100 + prevfp->rcv_count < rcount)) \
reorder_queue(&prevfp->chain, &infp->chain);\
}
REORDER_PRIO(input);
REORDER_PRIO(output);
break;
}
}
}
}
FILTER_ITERATE_END
simple_unlock(&ifp->if_snd_port_list_lock);
simple_unlock(&ifp->if_rcv_port_list_lock);
if (dead_infp != 0)
net_free_dead_infp(dead_infp);
if (dead_entp != 0)
net_free_dead_entp(dead_entp);
if (ipc_kmsg_queue_empty(send_list)) {
net_kmsg_put(kmsg);
}
}
boolean_t
net_do_filter(net_rcv_port_t infp,
const char * data,
unsigned int data_count,
const char * header)
{
int stack[NET_FILTER_STACK_DEPTH+1];
int *sp;
filter_t *fp, *fpe;
unsigned int op, arg;
data_count /= sizeof(unsigned short);
#define data_word ((unsigned short *)data)
#define header_word ((unsigned short *)header)
sp = &stack[NET_FILTER_STACK_DEPTH];
fp = &infp->filter[1];
fpe = infp->filter_end;
*sp = TRUE;
while (fp < fpe) {
arg = *fp++;
op = NETF_OP(arg);
arg = NETF_ARG(arg);
switch (arg) {
case NETF_NOPUSH:
arg = *sp++;
break;
case NETF_PUSHZERO:
arg = 0;
break;
case NETF_PUSHLIT:
arg = *fp++;
break;
case NETF_PUSHIND:
arg = *sp++;
if (arg >= data_count)
return FALSE;
arg = data_word[arg];
break;
case NETF_PUSHHDRIND:
arg = *sp++;
if (arg >= NET_HDW_HDR_MAX/sizeof(unsigned short))
return FALSE;
arg = header_word[arg];
break;
default:
if (arg >= NETF_PUSHSTK) {
arg = sp[arg - NETF_PUSHSTK];
}
else if (arg >= NETF_PUSHHDR) {
arg = header_word[arg - NETF_PUSHHDR];
}
else {
arg -= NETF_PUSHWORD;
if (arg >= data_count)
return FALSE;
arg = data_word[arg];
}
break;
}
switch (op) {
case NETF_OP(NETF_NOP):
*--sp = arg;
break;
case NETF_OP(NETF_AND):
*sp &= arg;
break;
case NETF_OP(NETF_OR):
*sp |= arg;
break;
case NETF_OP(NETF_XOR):
*sp ^= arg;
break;
case NETF_OP(NETF_EQ):
*sp = (*sp == arg);
break;
case NETF_OP(NETF_NEQ):
*sp = (*sp != arg);
break;
case NETF_OP(NETF_LT):
*sp = (*sp < arg);
break;
case NETF_OP(NETF_LE):
*sp = (*sp <= arg);
break;
case NETF_OP(NETF_GT):
*sp = (*sp > arg);
break;
case NETF_OP(NETF_GE):
*sp = (*sp >= arg);
break;
case NETF_OP(NETF_COR):
if (*sp++ == arg)
return (TRUE);
break;
case NETF_OP(NETF_CAND):
if (*sp++ != arg)
return (FALSE);
break;
case NETF_OP(NETF_CNOR):
if (*sp++ == arg)
return (FALSE);
break;
case NETF_OP(NETF_CNAND):
if (*sp++ != arg)
return (TRUE);
break;
case NETF_OP(NETF_LSH):
*sp <<= arg;
break;
case NETF_OP(NETF_RSH):
*sp >>= arg;
break;
case NETF_OP(NETF_ADD):
*sp += arg;
break;
case NETF_OP(NETF_SUB):
*sp -= arg;
break;
}
}
return ((*sp) ? TRUE : FALSE);
#undef data_word
#undef header_word
}
static boolean_t
parse_net_filter(
filter_t *filter,
unsigned int count)
{
int sp;
filter_t *fpe = &filter[count];
filter_t op, arg;
filter++;
sp = NET_FILTER_STACK_DEPTH;
for (; filter < fpe; filter++) {
op = NETF_OP(*filter);
arg = NETF_ARG(*filter);
switch (arg) {
case NETF_NOPUSH:
break;
case NETF_PUSHZERO:
sp--;
break;
case NETF_PUSHLIT:
filter++;
if (filter >= fpe)
return (FALSE);
sp--;
break;
case NETF_PUSHIND:
case NETF_PUSHHDRIND:
break;
default:
if (arg >= NETF_PUSHSTK) {
if (arg - NETF_PUSHSTK + sp > NET_FILTER_STACK_DEPTH)
return FALSE;
}
else if (arg >= NETF_PUSHHDR) {
if (arg - NETF_PUSHHDR >=
NET_HDW_HDR_MAX/sizeof(unsigned short))
return FALSE;
}
sp--;
break;
}
if (sp < 2) {
return (FALSE);
}
if (op == NETF_OP(NETF_NOP))
continue;
if (sp > NET_MAX_FILTER-2)
return (FALSE);
sp++;
switch (op) {
case NETF_OP(NETF_AND):
case NETF_OP(NETF_OR):
case NETF_OP(NETF_XOR):
case NETF_OP(NETF_EQ):
case NETF_OP(NETF_NEQ):
case NETF_OP(NETF_LT):
case NETF_OP(NETF_LE):
case NETF_OP(NETF_GT):
case NETF_OP(NETF_GE):
case NETF_OP(NETF_COR):
case NETF_OP(NETF_CAND):
case NETF_OP(NETF_CNOR):
case NETF_OP(NETF_CNAND):
case NETF_OP(NETF_LSH):
case NETF_OP(NETF_RSH):
case NETF_OP(NETF_ADD):
case NETF_OP(NETF_SUB):
break;
default:
return (FALSE);
}
}
return (TRUE);
}
io_return_t
net_set_filter(
struct ifnet *ifp,
ipc_port_t rcv_port,
int priority,
filter_t *filter,
unsigned int filter_count)
{
int filter_bytes;
bpf_insn_t match;
net_rcv_port_t infp, my_infp;
net_rcv_port_t nextfp;
net_hash_header_t hhp;
net_hash_entry_t entp;
net_hash_entry_t *head, nextentp;
queue_entry_t dead_infp, dead_entp;
int i;
int ret, is_new_infp;
io_return_t rval;
boolean_t in, out;
net_hash_entry_t hash_entp = NULL;
filter_bytes = CSPF_BYTES(filter_count);
match = (bpf_insn_t) 0;
if (filter_count == 0) {
return (D_INVALID_OPERATION);
} else if (!((filter[0] & NETF_IN) || (filter[0] & NETF_OUT))) {
return (D_INVALID_OPERATION);
} else if ((filter[0] & NETF_TYPE_MASK) == NETF_BPF) {
ret = bpf_validate((bpf_insn_t)filter, filter_bytes, &match);
if (!ret)
return (D_INVALID_OPERATION);
} else if ((filter[0] & NETF_TYPE_MASK) == 0) {
if (!parse_net_filter(filter, filter_count))
return (D_INVALID_OPERATION);
} else {
return (D_INVALID_OPERATION);
}
rval = D_SUCCESS;
dead_infp = dead_entp = 0;
if (match == (bpf_insn_t) 0) {
my_infp = (net_rcv_port_t) kmem_cache_alloc(&net_rcv_cache);
my_infp->rcv_port = rcv_port;
is_new_infp = TRUE;
} else {
my_infp = 0;
hash_entp = (net_hash_entry_t) kmem_cache_alloc(&net_hash_entry_cache);
is_new_infp = FALSE;
}
void check_filter_list(queue_head_t *if_port_list)
{
FILTER_ITERATE(if_port_list, infp, nextfp,
(if_port_list == &ifp->if_rcv_port_list)
? &infp->input : &infp->output)
{
if (infp->rcv_port == MACH_PORT_NULL) {
if (match != 0
&& infp->priority == priority
&& my_infp == 0
&& (infp->filter_end - infp->filter) == filter_count
&& bpf_eq((bpf_insn_t)infp->filter,
(bpf_insn_t)filter, filter_bytes))
my_infp = infp;
for (i = 0; i < NET_HASH_SIZE; i++) {
head = &((net_hash_header_t) infp)->table[i];
if (*head == 0)
continue;
entp = *head;
do {
nextentp = (net_hash_entry_t) entp->he_next;
if (entp->rcv_port == rcv_port
|| !IP_VALID(entp->rcv_port)
|| !ip_active(entp->rcv_port)) {
ret = hash_ent_remove (ifp,
(net_hash_header_t)infp,
(my_infp == infp),
head,
entp,
&dead_entp);
if (ret)
goto hash_loop_end;
}
entp = nextentp;
} while (*head != 0 && entp != *head);
}
hash_loop_end:
;
} else if (infp->rcv_port == rcv_port
|| !IP_VALID(infp->rcv_port)
|| !ip_active(infp->rcv_port)) {
if (infp->filter[0] & NETF_IN)
queue_remove(&ifp->if_rcv_port_list, infp,
net_rcv_port_t, input);
if (infp->filter[0] & NETF_OUT)
queue_remove(&ifp->if_snd_port_list, infp,
net_rcv_port_t, output);
ENQUEUE_DEAD(dead_infp, infp, input);
}
}
FILTER_ITERATE_END
}
in = (filter[0] & NETF_IN) != 0;
out = (filter[0] & NETF_OUT) != 0;
simple_lock(&ifp->if_rcv_port_list_lock);
simple_lock(&ifp->if_snd_port_list_lock);
if (in)
check_filter_list(&ifp->if_rcv_port_list);
if (out)
check_filter_list(&ifp->if_snd_port_list);
if (my_infp == 0) {
simple_lock(&net_hash_header_lock);
for (i = 0; i < N_NET_HASH; i++) {
if (filter_hash_header[i].n_keys == 0)
break;
}
if (i == N_NET_HASH) {
simple_unlock(&net_hash_header_lock);
simple_unlock(&ifp->if_snd_port_list_lock);
simple_unlock(&ifp->if_rcv_port_list_lock);
ipc_port_release_send(rcv_port);
if (match != 0)
kmem_cache_free(&net_hash_entry_cache,
(vm_offset_t)hash_entp);
rval = D_NO_MEMORY;
goto clean_and_return;
}
hhp = &filter_hash_header[i];
hhp->n_keys = match->jt;
simple_unlock(&net_hash_header_lock);
hhp->ref_count = 0;
for (i = 0; i < NET_HASH_SIZE; i++)
hhp->table[i] = 0;
my_infp = (net_rcv_port_t)hhp;
my_infp->rcv_port = MACH_PORT_NULL;
is_new_infp = TRUE;
}
if (is_new_infp) {
my_infp->priority = priority;
my_infp->rcv_count = 0;
memcpy (my_infp->filter, filter, filter_bytes);
my_infp->filter_end =
(filter_t *)((char *)my_infp->filter + filter_bytes);
if (match == 0) {
my_infp->rcv_qlimit = net_add_q_info(rcv_port);
} else {
my_infp->rcv_qlimit = 0;
}
if (in) {
queue_iterate(&ifp->if_rcv_port_list, infp, net_rcv_port_t, input)
if (priority > infp->priority)
break;
queue_enter(&ifp->if_rcv_port_list, my_infp, net_rcv_port_t, input);
}
if (out) {
queue_iterate(&ifp->if_snd_port_list, infp, net_rcv_port_t, output)
if (priority > infp->priority)
break;
queue_enter(&ifp->if_snd_port_list, my_infp, net_rcv_port_t, output);
}
}
if (match != 0)
{
net_hash_entry_t *p;
hash_entp->rcv_port = rcv_port;
for (i = 0; i < match->jt; i++)
hash_entp->keys[i] = match[i+1].k;
p = &((net_hash_header_t)my_infp)->
table[bpf_hash(match->jt, hash_entp->keys)];
if (*p == 0) {
queue_init (&hash_entp->chain);
*p = hash_entp;
} else {
enqueue_tail(&(*p)->chain, &hash_entp->chain);
}
((net_hash_header_t)my_infp)->ref_count++;
hash_entp->rcv_qlimit = net_add_q_info(rcv_port);
}
simple_unlock(&ifp->if_snd_port_list_lock);
simple_unlock(&ifp->if_rcv_port_list_lock);
clean_and_return:
if (dead_infp != 0)
net_free_dead_infp(dead_infp);
if (dead_entp != 0)
net_free_dead_entp(dead_entp);
return (rval);
}
io_return_t
net_getstat(
struct ifnet *ifp,
dev_flavor_t flavor,
dev_status_t status,
mach_msg_type_number_t *count)
{
switch (flavor) {
case NET_STATUS:
{
struct net_status *ns = (struct net_status *)status;
if (*count < NET_STATUS_COUNT)
return (D_INVALID_OPERATION);
ns->min_packet_size = ifp->if_header_size;
ns->max_packet_size = ifp->if_header_size + ifp->if_mtu;
ns->header_format = ifp->if_header_format;
ns->header_size = ifp->if_header_size;
ns->address_size = ifp->if_address_size;
ns->flags = ifp->if_flags;
ns->mapped_size = 0;
*count = NET_STATUS_COUNT;
break;
}
case NET_ADDRESS:
{
int addr_byte_count;
int addr_int_count;
int i;
addr_byte_count = ifp->if_address_size;
addr_int_count = (addr_byte_count + (sizeof(int)-1))
/ sizeof(int);
if (*count < addr_int_count)
{
printf ("net_getstat: count: %d, addr_int_count: %d\n",
*count, addr_int_count);
return (D_INVALID_OPERATION);
}
memcpy(status, ifp->if_address, addr_byte_count);
if (addr_byte_count < addr_int_count * sizeof(int))
memset((char *)status + addr_byte_count, 0,
(addr_int_count * sizeof(int)
- addr_byte_count));
for (i = 0; i < addr_int_count; i++) {
int word;
word = status[i];
status[i] = htonl(word);
}
*count = addr_int_count;
break;
}
default:
return (D_INVALID_OPERATION);
}
return (D_SUCCESS);
}
io_return_t
net_write(
struct ifnet *ifp,
net_write_start_device_fn start,
io_req_t ior)
{
spl_t s;
kern_return_t rc;
boolean_t wait;
if ((ifp->if_flags & (IFF_UP|IFF_RUNNING)) != (IFF_UP|IFF_RUNNING))
return (D_DEVICE_DOWN);
if (ior->io_count < ifp->if_header_size ||
ior->io_count > ifp->if_header_size + ifp->if_mtu)
return (D_INVALID_SIZE);
rc = device_write_get(ior, &wait);
if (rc != KERN_SUCCESS)
return (rc);
if (wait) {
panic("net_write: VM continuation");
}
s = splimp();
IF_ENQUEUE(&ifp->if_snd, ior);
(*start)(ifp->if_unit);
splx(s);
return (D_IO_QUEUED);
}
void
net_io_init(void)
{
vm_size_t size;
size = sizeof(struct net_rcv_port);
kmem_cache_init(&net_rcv_cache, "net_rcv_port", size, 0,
NULL, 0);
size = sizeof(struct net_hash_entry);
kmem_cache_init(&net_hash_entry_cache, "net_hash_entry", size, 0,
NULL, 0);
size = ikm_plus_overhead(sizeof(struct net_rcv_msg));
net_kmsg_size = round_page(size);
simple_lock_init(&net_kmsg_total_lock);
if (net_kmsg_max == 0)
net_kmsg_max = net_queue_free_min;
simple_lock_init(&net_queue_free_lock);
ipc_kmsg_queue_init(&net_queue_free);
simple_lock_init(&net_queue_lock);
ipc_kmsg_queue_init(&net_queue_high);
ipc_kmsg_queue_init(&net_queue_low);
simple_lock_init(&net_hash_header_lock);
}
#if defined(sparc) || defined(mips) || defined(ibm032) || defined(alpha)
#define BPF_ALIGN
#endif
#ifndef BPF_ALIGN
#define EXTRACT_SHORT(p) ((u_short)ntohs(*(u_short *)p))
#define EXTRACT_LONG(p) (ntohl(*(u_int *)p))
#else
#define EXTRACT_SHORT(p)\
((u_short)\
((u_short)*((u_char *)p+0)<<8|\
(u_short)*((u_char *)p+1)<<0))
#define EXTRACT_LONG(p)\
((u_int)*((u_char *)p+0)<<24|\
(u_int)*((u_char *)p+1)<<16|\
(u_int)*((u_char *)p+2)<<8|\
(u_int)*((u_char *)p+3)<<0)
#endif
int
bpf_do_filter(
net_rcv_port_t infp,
char * p,
unsigned int wirelen,
char * header,
unsigned int hlen,
net_hash_entry_t **hash_headpp,
net_hash_entry_t *entpp)
{
bpf_insn_t pc, pc_end;
unsigned int buflen;
unsigned int A, X;
int k;
unsigned int mem[BPF_MEMWORDS];
char *data = NULL;
pc = ((bpf_insn_t) infp->filter) + 1;
pc_end = (bpf_insn_t)infp->filter_end;
buflen = NET_RCV_MAX;
*entpp = 0;
A = 0;
X = 0;
for (; pc < pc_end; ++pc) {
switch (pc->code) {
default:
#ifdef KERNEL
return 0;
#else
abort();
#endif
case BPF_RET|BPF_K:
if (infp->rcv_port == MACH_PORT_NULL &&
*entpp == 0) {
return 0;
}
return ((u_int)pc->k <= wirelen) ?
pc->k : wirelen;
case BPF_RET|BPF_A:
if (infp->rcv_port == MACH_PORT_NULL &&
*entpp == 0) {
return 0;
}
return ((u_int)A <= wirelen) ?
A : wirelen;
case BPF_RET|BPF_MATCH_IMM:
if (bpf_match ((net_hash_header_t)infp, pc->jt, mem,
hash_headpp, entpp)) {
return ((u_int)pc->k <= wirelen) ?
pc->k : wirelen;
}
return 0;
case BPF_LD|BPF_W|BPF_ABS:
k = pc->k;
load_word:
if ((u_int)k + sizeof(int) <= hlen)
data = header;
else if ((u_int)k + sizeof(int) <= buflen) {
k -= hlen;
data = p;
} else
return 0;
#ifdef BPF_ALIGN
if (((int)(data + k) & 3) != 0)
A = EXTRACT_LONG(&data[k]);
else
#endif
A = ntohl(*(int *)(data + k));
continue;
case BPF_LD|BPF_H|BPF_ABS:
k = pc->k;
load_half:
if ((u_int)k + sizeof(short) <= hlen)
data = header;
else if ((u_int)k + sizeof(short) <= buflen) {
k -= hlen;
data = p;
} else
return 0;
A = EXTRACT_SHORT(&data[k]);
continue;
case BPF_LD|BPF_B|BPF_ABS:
k = pc->k;
load_byte:
if ((u_int)k < hlen)
data = header;
else if ((u_int)k < buflen) {
data = p;
k -= hlen;
} else
return 0;
A = data[k];
continue;
case BPF_LD|BPF_W|BPF_LEN:
A = wirelen;
continue;
case BPF_LDX|BPF_W|BPF_LEN:
X = wirelen;
continue;
case BPF_LD|BPF_W|BPF_IND:
k = X + pc->k;
goto load_word;
case BPF_LD|BPF_H|BPF_IND:
k = X + pc->k;
goto load_half;
case BPF_LD|BPF_B|BPF_IND:
k = X + pc->k;
goto load_byte;
case BPF_LDX|BPF_MSH|BPF_B:
k = pc->k;
if (k < hlen)
data = header;
else if (k < buflen) {
data = p;
k -= hlen;
} else
return 0;
X = (data[k] & 0xf) << 2;
continue;
case BPF_LD|BPF_IMM:
A = pc->k;
continue;
case BPF_LDX|BPF_IMM:
X = pc->k;
continue;
case BPF_LD|BPF_MEM:
A = mem[pc->k];
continue;
case BPF_LDX|BPF_MEM:
X = mem[pc->k];
continue;
case BPF_ST:
mem[pc->k] = A;
continue;
case BPF_STX:
mem[pc->k] = X;
continue;
case BPF_JMP|BPF_JA:
pc += pc->k;
continue;
case BPF_JMP|BPF_JGT|BPF_K:
pc += (A > pc->k) ? pc->jt : pc->jf;
continue;
case BPF_JMP|BPF_JGE|BPF_K:
pc += (A >= pc->k) ? pc->jt : pc->jf;
continue;
case BPF_JMP|BPF_JEQ|BPF_K:
pc += (A == pc->k) ? pc->jt : pc->jf;
continue;
case BPF_JMP|BPF_JSET|BPF_K:
pc += (A & pc->k) ? pc->jt : pc->jf;
continue;
case BPF_JMP|BPF_JGT|BPF_X:
pc += (A > X) ? pc->jt : pc->jf;
continue;
case BPF_JMP|BPF_JGE|BPF_X:
pc += (A >= X) ? pc->jt : pc->jf;
continue;
case BPF_JMP|BPF_JEQ|BPF_X:
pc += (A == X) ? pc->jt : pc->jf;
continue;
case BPF_JMP|BPF_JSET|BPF_X:
pc += (A & X) ? pc->jt : pc->jf;
continue;
case BPF_ALU|BPF_ADD|BPF_X:
A += X;
continue;
case BPF_ALU|BPF_SUB|BPF_X:
A -= X;
continue;
case BPF_ALU|BPF_MUL|BPF_X:
A *= X;
continue;
case BPF_ALU|BPF_DIV|BPF_X:
if (X == 0)
return 0;
A /= X;
continue;
case BPF_ALU|BPF_AND|BPF_X:
A &= X;
continue;
case BPF_ALU|BPF_OR|BPF_X:
A |= X;
continue;
case BPF_ALU|BPF_LSH|BPF_X:
A <<= X;
continue;
case BPF_ALU|BPF_RSH|BPF_X:
A >>= X;
continue;
case BPF_ALU|BPF_ADD|BPF_K:
A += pc->k;
continue;
case BPF_ALU|BPF_SUB|BPF_K:
A -= pc->k;
continue;
case BPF_ALU|BPF_MUL|BPF_K:
A *= pc->k;
continue;
case BPF_ALU|BPF_DIV|BPF_K:
A /= pc->k;
continue;
case BPF_ALU|BPF_AND|BPF_K:
A &= pc->k;
continue;
case BPF_ALU|BPF_OR|BPF_K:
A |= pc->k;
continue;
case BPF_ALU|BPF_LSH|BPF_K:
A <<= pc->k;
continue;
case BPF_ALU|BPF_RSH|BPF_K:
A >>= pc->k;
continue;
case BPF_ALU|BPF_NEG:
A = -A;
continue;
case BPF_MISC|BPF_TAX:
X = A;
continue;
case BPF_MISC|BPF_TXA:
A = X;
continue;
}
}
return 0;
}
int
bpf_validate(
bpf_insn_t f,
int bytes,
bpf_insn_t *match)
{
int i, j, len;
bpf_insn_t p;
len = BPF_BYTES2LEN(bytes);
for (i = 1; i < len; ++i) {
p = &f[i];
if (BPF_CLASS(p->code) == BPF_JMP) {
int from = i + 1;
if (BPF_OP(p->code) == BPF_JA) {
if (from + p->k >= len)
return 0;
}
else if (from + p->jt >= len || from + p->jf >= len)
return 0;
}
if ((BPF_CLASS(p->code) == BPF_ST ||
(BPF_CLASS(p->code) == BPF_LD &&
(p->code & 0xe0) == BPF_MEM)) &&
(p->k >= BPF_MEMWORDS || p->k < 0))
return 0;
if (p->code == (BPF_ALU|BPF_DIV|BPF_K) && p->k == 0)
return 0;
if (p->code == (BPF_RET|BPF_MATCH_IMM)) {
if (*match != 0 ||
p->jt == 0 ||
p->jt > N_NET_HASH_KEYS)
return 0;
i += p->jt;
if (i + 1 > len)
return 0;
for (j = 1; j <= p->jt; j++) {
if (p[j].code != (BPF_MISC|BPF_KEY))
return 0;
}
*match = p;
}
}
if (BPF_CLASS(f[len - 1].code) == BPF_RET)
return ((*match == 0) ? 1 : 2);
else
return 0;
}
int
bpf_eq(
bpf_insn_t f1,
bpf_insn_t f2,
int bytes)
{
int count;
count = BPF_BYTES2LEN(bytes);
for (; count--; f1++, f2++) {
if (!BPF_INSN_EQ(f1, f2)) {
if ( f1->code == (BPF_MISC|BPF_KEY) &&
f2->code == (BPF_MISC|BPF_KEY) )
continue;
return FALSE;
}
};
return TRUE;
}
unsigned int
bpf_hash (int n,
const unsigned int *keys)
{
unsigned int hval = 0;
while (n--) {
hval += *keys++;
}
return (hval % NET_HASH_SIZE);
}
int
bpf_match (net_hash_header_t hash,
int n_keys,
const unsigned int *keys,
net_hash_entry_t **hash_headpp,
net_hash_entry_t *entpp)
{
net_hash_entry_t head, entp;
int i;
if (n_keys != hash->n_keys)
return FALSE;
*hash_headpp = &hash->table[bpf_hash(n_keys, keys)];
head = **hash_headpp;
if (head == 0)
return FALSE;
HASH_ITERATE (head, entp)
{
for (i = 0; i < n_keys; i++) {
if (keys[i] != entp->keys[i])
break;
}
if (i == n_keys) {
*entpp = entp;
return TRUE;
}
}
HASH_ITERATE_END (head, entp)
return FALSE;
}
int
hash_ent_remove(
struct ifnet *ifp,
net_hash_header_t hp,
int used,
net_hash_entry_t *head,
net_hash_entry_t entp,
queue_entry_t *dead_p)
{
hp->ref_count--;
if (*head == entp) {
if (queue_empty((queue_t) entp)) {
*head = 0;
ENQUEUE_DEAD(*dead_p, entp, chain);
if (hp->ref_count == 0 && !used) {
if (((net_rcv_port_t)hp)->filter[0] & NETF_IN)
queue_remove(&ifp->if_rcv_port_list,
(net_rcv_port_t)hp,
net_rcv_port_t, input);
if (((net_rcv_port_t)hp)->filter[0] & NETF_OUT)
queue_remove(&ifp->if_snd_port_list,
(net_rcv_port_t)hp,
net_rcv_port_t, output);
hp->n_keys = 0;
return TRUE;
}
return FALSE;
} else {
*head = (net_hash_entry_t)queue_next((queue_t) entp);
}
}
remqueue((queue_t)*head, (queue_entry_t)entp);
ENQUEUE_DEAD(*dead_p, entp, chain);
return FALSE;
}
int
net_add_q_info(ipc_port_t rcv_port)
{
mach_port_msgcount_t qlimit = 0;
if (IP_VALID(rcv_port)) {
ip_lock(rcv_port);
if (ip_active(rcv_port))
qlimit = rcv_port->ip_qlimit;
ip_unlock(rcv_port);
}
simple_lock(&net_kmsg_total_lock);
net_queue_free_min++;
net_kmsg_max += qlimit + 1;
simple_unlock(&net_kmsg_total_lock);
return (int)qlimit;
}
static void
net_del_q_info(int qlimit)
{
simple_lock(&net_kmsg_total_lock);
net_queue_free_min--;
net_kmsg_max -= qlimit + 1;
simple_unlock(&net_kmsg_total_lock);
}
void
net_free_dead_infp(queue_entry_t dead_infp)
{
net_rcv_port_t infp, nextfp;
for (infp = (net_rcv_port_t) dead_infp; infp != 0; infp = nextfp)
{
nextfp = (net_rcv_port_t) queue_next(&infp->input);
ipc_port_release_send(infp->rcv_port);
net_del_q_info(infp->rcv_qlimit);
kmem_cache_free(&net_rcv_cache, (vm_offset_t) infp);
}
}
void
net_free_dead_entp(queue_entry_t dead_entp)
{
net_hash_entry_t entp, nextentp;
for (entp = (net_hash_entry_t)dead_entp; entp != 0; entp = nextentp)
{
nextentp = (net_hash_entry_t) queue_next(&entp->chain);
ipc_port_release_send(entp->rcv_port);
net_del_q_info(entp->rcv_qlimit);
kmem_cache_free(&net_hash_entry_cache, (vm_offset_t) entp);
}
}