#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/poll.h>
#include <linux/init.h>
#include <net/icmp.h>
#include <net/tcp.h>
#include <asm/uaccess.h>
int sysctl_tcp_fin_timeout = TCP_FIN_TIMEOUT;
struct tcp_mib	tcp_statistics;
kmem_cache_t *tcp_openreq_cachep;
kmem_cache_t *tcp_bucket_cachep;
kmem_cache_t *tcp_timewait_cachep;
static struct open_request *tcp_find_established(struct tcp_opt *tp,
struct open_request **prevp)
{
struct open_request *req = tp->syn_wait_queue;
struct open_request *prev = (struct open_request *)&tp->syn_wait_queue;
while(req) {
if (req->sk &&
((1 << req->sk->state) &
~(TCPF_SYN_SENT|TCPF_SYN_RECV)))
break;
prev = req;
req = req->dl_next;
}
*prevp = prev;
return req;
}
static int tcp_readable(struct sock *sk)
{
unsigned long counted;
unsigned long amount;
struct sk_buff *skb;
int sum;
SOCK_DEBUG(sk, "tcp_readable: %p - ",sk);
skb = skb_peek(&sk->receive_queue);
if (skb == NULL) {
SOCK_DEBUG(sk, "empty\n");
return(0);
}
counted = sk->tp_pinfo.af_tcp.copied_seq;
amount = 0;
do {
if (before(counted, TCP_SKB_CB(skb)->seq))
break;
sum = skb->len - (counted - TCP_SKB_CB(skb)->seq);
if (sum >= 0) {
amount += sum;
counted += sum;
if (skb->h.th->syn)
counted++;
}
if (skb->h.th->urg)
amount--;
#if 0
if (amount && skb->h.th->psh) break;
#endif
skb = skb->next;
} while(skb != (struct sk_buff *)&sk->receive_queue);
SOCK_DEBUG(sk, "got %lu bytes.\n",amount);
return(amount);
}
static unsigned int tcp_listen_poll(struct sock *sk, poll_table *wait)
{
struct open_request *req, *dummy;
lock_sock(sk);
req = tcp_find_established(&sk->tp_pinfo.af_tcp, &dummy);
release_sock(sk);
if (req)
return POLLIN | POLLRDNORM;
return 0;
}
#define tcp_min_write_space(__sk) \
(atomic_read(&(__sk)->wmem_alloc) / 2)
unsigned int tcp_poll(struct file * file, struct socket *sock, poll_table *wait)
{
unsigned int mask;
struct sock *sk = sock->sk;
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
poll_wait(file, sk->sleep, wait);
if (sk->state == TCP_LISTEN)
return tcp_listen_poll(sk, wait);
mask = 0;
if (sk->err)
mask = POLLERR;
if (sk->shutdown & RCV_SHUTDOWN)
mask |= POLLHUP;
if ((1 << sk->state) & ~(TCPF_SYN_SENT|TCPF_SYN_RECV)) {
if ((tp->rcv_nxt != tp->copied_seq) &&
(tp->urg_seq != tp->copied_seq ||
tp->rcv_nxt != tp->copied_seq+1 ||
sk->urginline || !tp->urg_data))
mask |= POLLIN | POLLRDNORM;
if (!(sk->shutdown & SEND_SHUTDOWN)) {
if (sock_wspace(sk) >= tcp_min_write_space(sk)) {
mask |= POLLOUT | POLLWRNORM;
} else {
sk->socket->flags |= SO_NOSPACE;
}
} else
mask |= POLLOUT | POLLWRNORM;
if (tp->urg_data & URG_VALID)
mask |= POLLPRI;
}
return mask;
}
void tcp_write_space(struct sock *sk)
{
if (sk->dead)
return;
wake_up_interruptible(sk->sleep);
if (sock_wspace(sk) >=
tcp_min_write_space(sk))
sock_wake_async(sk->socket, 2);
}
#ifdef _HURD_
#define tcp_ioctl 0
error_t
tcp_tiocinq(struct sock *sk, mach_msg_type_number_t *amount)
{
if (sk->state == TCP_LISTEN)
return EINVAL;
lock_sock(sk);
*amount = tcp_readable(sk);
release_sock(sk);
return 0;
}
#else
int tcp_ioctl(struct sock *sk, int cmd, unsigned long arg)
{
int answ;
switch(cmd) {
case TIOCINQ:
#ifdef FIXME
case FIONREAD:
#endif
if (sk->state == TCP_LISTEN)
return(-EINVAL);
lock_sock(sk);
answ = tcp_readable(sk);
release_sock(sk);
break;
case SIOCATMARK:
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
answ = tp->urg_data && tp->urg_seq == tp->copied_seq;
break;
}
case TIOCOUTQ:
if (sk->state == TCP_LISTEN)
return(-EINVAL);
answ = sock_wspace(sk);
break;
default:
return(-ENOIOCTLCMD);
};
return put_user(answ, (int *)arg);
}
#endif
static int wait_for_tcp_connect(struct sock * sk, int flags)
{
struct task_struct *tsk = current;
struct wait_queue wait = { tsk, NULL };
while((1 << sk->state) & ~(TCPF_ESTABLISHED | TCPF_CLOSE_WAIT)) {
if(sk->err)
return sock_error(sk);
if((1 << sk->state) &
~(TCPF_SYN_SENT | TCPF_SYN_RECV)) {
if(sk->keepopen && !(flags&MSG_NOSIGNAL))
send_sig(SIGPIPE, tsk, 0);
return -EPIPE;
}
if(flags & MSG_DONTWAIT)
return -EAGAIN;
if(signal_pending(tsk))
return -ERESTARTSYS;
tsk->state = TASK_INTERRUPTIBLE;
add_wait_queue(sk->sleep, &wait);
release_sock(sk);
if (((1 << sk->state) & ~(TCPF_ESTABLISHED|TCPF_CLOSE_WAIT)) &&
sk->err == 0)
schedule();
tsk->state = TASK_RUNNING;
remove_wait_queue(sk->sleep, &wait);
lock_sock(sk);
}
return 0;
}
static inline int tcp_memory_free(struct sock *sk)
{
return atomic_read(&sk->wmem_alloc) < sk->sndbuf;
}
static void wait_for_tcp_memory(struct sock * sk)
{
release_sock(sk);
if (!tcp_memory_free(sk)) {
struct wait_queue wait = { current, NULL };
sk->socket->flags &= ~SO_NOSPACE;
add_wait_queue(sk->sleep, &wait);
for (;;) {
if (signal_pending(current))
break;
current->state = TASK_INTERRUPTIBLE;
if (tcp_memory_free(sk))
break;
if (sk->shutdown & SEND_SHUTDOWN)
break;
if (sk->err)
break;
schedule();
}
current->state = TASK_RUNNING;
remove_wait_queue(sk->sleep, &wait);
}
lock_sock(sk);
}
static int wait_for_buffer(struct sock *sk)
{
struct wait_queue wait = { current, NULL };
release_sock(sk);
add_wait_queue(sk->sleep, &wait);
current->state = TASK_INTERRUPTIBLE;
schedule();
current->state = TASK_RUNNING;
remove_wait_queue(sk->sleep, &wait);
lock_sock(sk);
return 0;
}
#define PSH_NEEDED (seglen == 0 && iovlen == 0)
int tcp_do_sendmsg(struct sock *sk, struct msghdr *msg)
{
struct iovec *iov;
struct tcp_opt *tp;
struct sk_buff *skb;
int iovlen, flags;
int mss_now;
int err, copied;
lock_sock(sk);
err = 0;
tp = &(sk->tp_pinfo.af_tcp);
flags = msg->msg_flags;
if ((1 << sk->state) & ~(TCPF_ESTABLISHED | TCPF_CLOSE_WAIT))
if((err = wait_for_tcp_connect(sk, flags)) != 0)
goto out;
sk->socket->flags &= ~SO_NOSPACE;
mss_now = tcp_current_mss(sk);
iovlen = msg->msg_iovlen;
iov = msg->msg_iov;
copied = 0;
while(--iovlen >= 0) {
int seglen=iov->iov_len;
unsigned char * from=iov->iov_base;
iov++;
while(seglen > 0) {
int copy, tmp, queue_it, psh;
if (err)
goto do_fault2;
if (sk->err)
goto do_sock_err;
if (sk->shutdown & SEND_SHUTDOWN)
goto do_shutdown;
if (tp->send_head && !(flags & MSG_OOB)) {
skb = sk->write_queue.prev;
copy = skb->len;
if (skb_tailroom(skb) > 0 &&
(mss_now - copy) > 0 &&
tp->snd_nxt < TCP_SKB_CB(skb)->end_seq) {
int last_byte_was_odd = (copy % 4);
if (tp->partial_writers++ > 0) {
wait_for_buffer(sk);
tp->partial_writers--;
continue;
}
copy = mss_now - copy;
if(copy > skb_tailroom(skb))
copy = skb_tailroom(skb);
if(copy > seglen)
copy = seglen;
if(last_byte_was_odd) {
if(copy_from_user(skb_put(skb, copy),
from, copy))
err = -EFAULT;
skb->csum = csum_partial(skb->data,
skb->len, 0);
} else {
skb->csum =
csum_and_copy_from_user(
from, skb_put(skb, copy),
copy, skb->csum, &err);
}
tp->write_seq += copy;
TCP_SKB_CB(skb)->end_seq += copy;
from += copy;
copied += copy;
seglen -= copy;
if (PSH_NEEDED)
TCP_SKB_CB(skb)->flags |= TCPCB_FLAG_PSH;
if (--tp->partial_writers > 0)
wake_up_interruptible(sk->sleep);
continue;
}
}
psh = 0;
copy = tp->snd_wnd - (tp->snd_nxt - tp->snd_una);
if(copy > (tp->max_window >> 1)) {
copy = min(copy, mss_now);
psh = 1;
} else {
copy = mss_now;
}
if(copy > seglen)
copy = seglen;
tmp = MAX_HEADER + sk->prot->max_header;
if (copy < min(mss_now, tp->max_window >> 1) &&
!(flags & MSG_OOB)) {
tmp += min(mss_now, tp->max_window);
queue_it = 1;
} else {
tmp += copy;
queue_it = 0;
}
skb = sock_wmalloc(sk, tmp, 0, GFP_KERNEL);
if (skb == NULL) {
sk->socket->flags |= SO_NOSPACE;
if (flags&MSG_DONTWAIT) {
err = -EAGAIN;
goto do_interrupted;
}
if (signal_pending(current)) {
err = -ERESTARTSYS;
goto do_interrupted;
}
tcp_push_pending_frames(sk, tp);
wait_for_tcp_memory(sk);
mss_now = tcp_current_mss(sk);
continue;
}
seglen -= copy;
TCP_SKB_CB(skb)->flags = (TCPCB_FLAG_ACK |
((PSH_NEEDED || psh) ?
TCPCB_FLAG_PSH : 0));
TCP_SKB_CB(skb)->sacked = 0;
if (flags & MSG_OOB) {
TCP_SKB_CB(skb)->flags |= TCPCB_FLAG_URG;
TCP_SKB_CB(skb)->urg_ptr = copy;
} else
TCP_SKB_CB(skb)->urg_ptr = 0;
skb_reserve(skb, MAX_HEADER + sk->prot->max_header);
skb->csum = csum_and_copy_from_user(from,
skb_put(skb, copy), copy, 0, &err);
if (err)
goto do_fault;
from += copy;
copied += copy;
TCP_SKB_CB(skb)->seq = tp->write_seq;
TCP_SKB_CB(skb)->end_seq = TCP_SKB_CB(skb)->seq + copy;
tcp_send_skb(sk, skb, queue_it);
}
}
sk->err = 0;
err = copied;
goto out;
do_sock_err:
if(copied)
err = copied;
else
err = sock_error(sk);
goto out;
do_shutdown:
if(copied)
err = copied;
else {
if (!(flags&MSG_NOSIGNAL))
send_sig(SIGPIPE, current, 0);
err = -EPIPE;
}
goto out;
do_interrupted:
if(copied)
err = copied;
goto out;
do_fault:
kfree_skb(skb);
do_fault2:
err = -EFAULT;
out:
tcp_push_pending_frames(sk, tp);
release_sock(sk);
return err;
}
#undef PSH_NEEDED
void tcp_read_wakeup(struct sock *sk)
{
if (sk->state != TCP_CLOSE)
tcp_send_ack(sk);
}
static int tcp_recv_urg(struct sock * sk, int nonblock,
struct msghdr *msg, int len, int flags,
int *addr_len)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
if (sk->urginline || !tp->urg_data || tp->urg_data == URG_READ)
return -EINVAL;
if (sk->err)
return sock_error(sk);
if (sk->state == TCP_CLOSE && !sk->done)
return -ENOTCONN;
if (sk->state == TCP_CLOSE || (sk->shutdown & RCV_SHUTDOWN))
return 0;
lock_sock(sk);
if (tp->urg_data & URG_VALID) {
int err = 0;
char c = tp->urg_data;
if (!(flags & MSG_PEEK))
tp->urg_data = URG_READ;
if(msg->msg_name)
tp->af_specific->addr2sockaddr(sk, (struct sockaddr *)
msg->msg_name);
if(addr_len)
*addr_len = tp->af_specific->sockaddr_len;
msg->msg_flags|=MSG_OOB;
release_sock(sk);
if(len>0)
{
err = memcpy_toiovec(msg->msg_iov, &c, 1);
msg->msg_flags|=MSG_OOB;
}
else
msg->msg_flags|=MSG_TRUNC;
return err ? -EFAULT : 1;
}
release_sock(sk);
return -EAGAIN;
}
static inline void tcp_eat_skb(struct sock *sk, struct sk_buff * skb)
{
__skb_unlink(skb, &sk->receive_queue);
kfree_skb(skb);
}
static void cleanup_rbuf(struct sock *sk, int copied)
{
struct sk_buff *skb;
while ((skb=skb_peek(&sk->receive_queue)) != NULL) {
if (!skb->used || atomic_read(&skb->users) > 1)
break;
tcp_eat_skb(sk, skb);
}
if(copied > 0) {
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
__u32 rcv_window_now = tcp_receive_window(tp);
__u32 new_window = __tcp_select_window(sk);
if((new_window && (new_window >= rcv_window_now * 2)) &&
((rcv_window_now + tp->mss_cache) <= tp->window_clamp))
tcp_read_wakeup(sk);
}
}
int tcp_recvmsg(struct sock *sk, struct msghdr *msg,
int len, int nonblock, int flags, int *addr_len)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct wait_queue wait = { current, NULL };
int copied = 0;
u32 peek_seq;
volatile u32 *seq;
unsigned long used;
int err = 0;
int target = 1;
if (sk->state == TCP_LISTEN)
return -ENOTCONN;
if (flags & MSG_OOB)
return tcp_recv_urg(sk, nonblock, msg, len, flags, addr_len);
peek_seq = tp->copied_seq;
seq = &tp->copied_seq;
if (flags & MSG_PEEK)
seq = &peek_seq;
if (flags & MSG_WAITALL)
target=len;
add_wait_queue(sk->sleep, &wait);
lock_sock(sk);
while (len > 0) {
struct sk_buff * skb;
u32 offset;
if (copied && tp->urg_data && tp->urg_seq == *seq)
break;
if (signal_pending(current)) {
if (copied)
break;
copied = -ERESTARTSYS;
if (nonblock)
copied = -EAGAIN;
break;
}
current->state = TASK_INTERRUPTIBLE;
skb = skb_peek(&sk->receive_queue);
do {
if (!skb)
break;
if (before(*seq, TCP_SKB_CB(skb)->seq)) {
printk(KERN_INFO "recvmsg bug: copied %X seq %X\n",
*seq, TCP_SKB_CB(skb)->seq);
break;
}
offset = *seq - TCP_SKB_CB(skb)->seq;
if (skb->h.th->syn)
offset--;
if (offset < skb->len)
goto found_ok_skb;
if (skb->h.th->fin)
goto found_fin_ok;
if (!(flags & MSG_PEEK))
skb->used = 1;
skb = skb->next;
} while (skb != (struct sk_buff *)&sk->receive_queue);
if (copied >= target)
break;
if (copied) {
if (sk->err ||
sk->state == TCP_CLOSE ||
(sk->shutdown & RCV_SHUTDOWN) ||
nonblock)
break;
} else {
if (sk->done)
break;
if (sk->err) {
copied = sock_error(sk);
break;
}
if (sk->shutdown & RCV_SHUTDOWN)
break;
if (sk->state == TCP_CLOSE) {
if (!sk->done) {
copied = -ENOTCONN;
break;
}
break;
}
if (nonblock) {
copied = -EAGAIN;
break;
}
}
cleanup_rbuf(sk, copied);
release_sock(sk);
sk->socket->flags |= SO_WAITDATA;
schedule();
sk->socket->flags &= ~SO_WAITDATA;
lock_sock(sk);
continue;
found_ok_skb:
atomic_inc(&skb->users);
used = skb->len - offset;
if (len < used)
used = len;
if (tp->urg_data) {
u32 urg_offset = tp->urg_seq - *seq;
if (urg_offset < used) {
if (!urg_offset) {
if (!sk->urginline) {
++*seq;
offset++;
used--;
}
} else
used = urg_offset;
}
}
*seq += used;
err = memcpy_toiovec(msg->msg_iov, ((unsigned char *)skb->h.th) + skb->h.th->doff*4 + offset, used);
if (err) {
atomic_dec(&skb->users);
copied = -EFAULT;
break;
}
copied += used;
len -= used;
atomic_dec(&skb->users);
if (after(tp->copied_seq,tp->urg_seq))
tp->urg_data = 0;
if (used + offset < skb->len)
continue;
if (skb->h.th->fin)
goto found_fin_ok;
if (flags & MSG_PEEK)
continue;
skb->used = 1;
if (atomic_read(&skb->users) == 1)
tcp_eat_skb(sk, skb);
continue;
found_fin_ok:
++*seq;
if (flags & MSG_PEEK)
break;
skb->used = 1;
sk->shutdown |= RCV_SHUTDOWN;
break;
}
if(copied >= 0 && msg->msg_name) {
tp->af_specific->addr2sockaddr(sk, (struct sockaddr *)
msg->msg_name);
if(addr_len)
*addr_len = tp->af_specific->sockaddr_len;
}
remove_wait_queue(sk->sleep, &wait);
current->state = TASK_RUNNING;
cleanup_rbuf(sk, copied);
release_sock(sk);
return copied;
}
static inline void tcp_check_fin_timer(struct sock *sk)
{
if (sk->state == TCP_FIN_WAIT2 && !sk->timer.prev)
tcp_reset_msl_timer(sk, TIME_CLOSE, sysctl_tcp_fin_timeout);
}
static unsigned char new_state[16] = {
TCP_CLOSE,
TCP_FIN_WAIT1 | TCP_ACTION_FIN,
TCP_CLOSE,
TCP_FIN_WAIT1 | TCP_ACTION_FIN,
TCP_FIN_WAIT1,
TCP_FIN_WAIT2,
TCP_CLOSE,
TCP_CLOSE,
TCP_LAST_ACK  | TCP_ACTION_FIN,
TCP_LAST_ACK,
TCP_CLOSE,
TCP_CLOSING,
};
static int tcp_close_state(struct sock *sk, int dead)
{
int next = (int) new_state[sk->state];
int ns = (next & TCP_STATE_MASK);
tcp_set_state(sk, ns);
if (dead)
tcp_check_fin_timer(sk);
return (next & TCP_ACTION_FIN);
}
void tcp_shutdown(struct sock *sk, int how)
{
if (!(how & SEND_SHUTDOWN))
return;
if ((1 << sk->state) &
(TCPF_ESTABLISHED|TCPF_SYN_SENT|TCPF_SYN_RECV|TCPF_CLOSE_WAIT)) {
lock_sock(sk);
if (tcp_close_state(sk,0))
tcp_send_fin(sk);
release_sock(sk);
}
}
static inline int closing(struct sock * sk)
{
return ((1 << sk->state) & (TCPF_FIN_WAIT1|TCPF_CLOSING|TCPF_LAST_ACK));
}
static void tcp_close_pending (struct sock *sk)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
struct open_request *req = tp->syn_wait_queue;
while(req) {
struct open_request *iter;
if (req->sk)
tcp_close(req->sk, 0);
iter = req;
req = req->dl_next;
(*iter->class->destructor)(iter);
tcp_dec_slow_timer(TCP_SLT_SYNACK);
sk->ack_backlog--;
tcp_openreq_free(iter);
}
tcp_synq_init(tp);
}
void tcp_close(struct sock *sk, long timeout)
{
struct sk_buff *skb;
int data_was_unread = 0;
if (atomic_read(&sk->sock_readers))
printk("tcp_close: socket already locked!\n");
lock_sock(sk);
if(sk->state == TCP_LISTEN) {
tcp_set_state(sk, TCP_CLOSE);
tcp_close_pending(sk);
release_sock(sk);
sk->dead = 1;
return;
}
sk->shutdown = SHUTDOWN_MASK;
if (!sk->dead)
sk->state_change(sk);
while((skb=__skb_dequeue(&sk->receive_queue))!=NULL) {
u32 len = TCP_SKB_CB(skb)->end_seq - TCP_SKB_CB(skb)->seq - skb->h.th->fin;
data_was_unread += len;
kfree_skb(skb);
}
if(data_was_unread != 0) {
tcp_set_state(sk, TCP_CLOSE);
tcp_send_active_reset(sk);
} else if (tcp_close_state(sk,1)) {
tcp_send_fin(sk);
}
if (timeout) {
struct task_struct *tsk = current;
struct wait_queue wait = { tsk, NULL };
add_wait_queue(sk->sleep, &wait);
release_sock(sk);
while (1) {
tsk->state = TASK_INTERRUPTIBLE;
if (!closing(sk))
break;
timeout = schedule_timeout(timeout);
if (signal_pending(tsk) || !timeout)
break;
}
tsk->state = TASK_RUNNING;
remove_wait_queue(sk->sleep, &wait);
lock_sock(sk);
}
tcp_check_fin_timer(sk);
release_sock(sk);
sk->dead = 1;
}
static struct open_request * wait_for_connect(struct sock * sk,
struct open_request **pprev)
{
struct wait_queue wait = { current, NULL };
struct open_request *req;
add_wait_queue(sk->sleep, &wait);
for (;;) {
current->state = TASK_INTERRUPTIBLE;
release_sock(sk);
schedule();
lock_sock(sk);
req = tcp_find_established(&(sk->tp_pinfo.af_tcp), pprev);
if (req)
break;
if (signal_pending(current))
break;
}
current->state = TASK_RUNNING;
remove_wait_queue(sk->sleep, &wait);
return req;
}
struct sock *tcp_accept(struct sock *sk, int flags)
{
struct tcp_opt *tp = &sk->tp_pinfo.af_tcp;
struct open_request *req, *prev;
struct sock *newsk = NULL;
int error;
lock_sock(sk);
error = EINVAL;
if (sk->state != TCP_LISTEN)
goto out;
req = tcp_find_established(tp, &prev);
if (!req) {
error = EAGAIN;
if (flags & O_NONBLOCK)
goto out;
error = ERESTARTSYS;
req = wait_for_connect(sk, &prev);
if (!req)
goto out;
}
tcp_synq_unlink(tp, req, prev);
newsk = req->sk;
req->class->destructor(req);
tcp_openreq_free(req);
sk->ack_backlog--;
if(sk->keepopen)
tcp_inc_slow_timer(TCP_SLT_KEEPALIVE);
release_sock(sk);
return newsk;
out:
sk->err = error;
release_sock(sk);
return newsk;
}
int tcp_setsockopt(struct sock *sk, int level, int optname, char *optval,
int optlen)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
int val;
if (level != SOL_TCP)
return tp->af_specific->setsockopt(sk, level, optname,
optval, optlen);
if(optlen<sizeof(int))
return -EINVAL;
if (get_user(val, (int *)optval))
return -EFAULT;
switch(optname) {
case TCP_MAXSEG:
if(val < 1 || val > MAX_WINDOW)
return -EINVAL;
tp->user_mss = val;
return 0;
case TCP_NODELAY:
if (sk->nonagle == 2)
return -EINVAL;
sk->nonagle = (val == 0) ? 0 : 1;
return 0;
case TCP_CORK:
if (sk->nonagle == 1)
return -EINVAL;
if (val != 0) {
sk->nonagle = 2;
} else {
sk->nonagle = 0;
lock_sock(sk);
tcp_push_pending_frames(sk, tp);
release_sock(sk);
}
return 0;
default:
return -ENOPROTOOPT;
};
}
int tcp_getsockopt(struct sock *sk, int level, int optname, char *optval,
int *optlen)
{
struct tcp_opt *tp = &(sk->tp_pinfo.af_tcp);
int val, len;
if(level != SOL_TCP)
return tp->af_specific->getsockopt(sk, level, optname,
optval, optlen);
if(get_user(len,optlen))
return -EFAULT;
len = min(len, sizeof(int));
switch(optname) {
case TCP_MAXSEG:
val = tp->user_mss;
break;
case TCP_NODELAY:
val = (sk->nonagle == 1);
break;
case TCP_CORK:
val = (sk->nonagle == 2);
break;
default:
return -ENOPROTOOPT;
};
if(put_user(len, optlen))
return -EFAULT;
if(copy_to_user(optval, &val,len))
return -EFAULT;
return 0;
}
void tcp_set_keepalive(struct sock *sk, int val)
{
if (!sk->keepopen && val)
tcp_inc_slow_timer(TCP_SLT_KEEPALIVE);
else if (sk->keepopen && !val)
tcp_dec_slow_timer(TCP_SLT_KEEPALIVE);
}
extern void __skb_cb_too_small_for_tcp(int, int);
void __init tcp_init(void)
{
struct sk_buff *skb = NULL;
unsigned long goal;
int order;
if(sizeof(struct tcp_skb_cb) > sizeof(skb->cb))
__skb_cb_too_small_for_tcp(sizeof(struct tcp_skb_cb),
sizeof(skb->cb));
tcp_openreq_cachep = kmem_cache_create("tcp_open_request",
sizeof(struct open_request),
0, SLAB_HWCACHE_ALIGN,
NULL, NULL);
if(!tcp_openreq_cachep)
panic("tcp_init: Cannot alloc open_request cache.");
tcp_bucket_cachep = kmem_cache_create("tcp_bind_bucket",
sizeof(struct tcp_bind_bucket),
0, SLAB_HWCACHE_ALIGN,
NULL, NULL);
if(!tcp_bucket_cachep)
panic("tcp_init: Cannot alloc tcp_bind_bucket cache.");
tcp_timewait_cachep = kmem_cache_create("tcp_tw_bucket",
sizeof(struct tcp_tw_bucket),
0, SLAB_HWCACHE_ALIGN,
NULL, NULL);
if(!tcp_timewait_cachep)
panic("tcp_init: Cannot alloc tcp_tw_bucket cache.");
goal = num_physpages >> (20 - PAGE_SHIFT);
for (order = 0; (1UL << order) < goal; order++)
;
do {
tcp_ehash_size = (1UL << order) * PAGE_SIZE /
sizeof(struct sock *);
tcp_ehash = (struct sock **)
__get_free_pages(GFP_ATOMIC, order);
} while (tcp_ehash == NULL && --order >= 0);
if (!tcp_ehash)
panic("Failed to allocate TCP established hash table\n");
memset(tcp_ehash, 0, tcp_ehash_size * sizeof(struct sock *));
goal = (((1UL << order) * PAGE_SIZE) / sizeof(struct tcp_bind_bucket *));
if (goal > (64 * 1024)) {
goal = (((64 * 1024) * sizeof(struct tcp_bind_bucket *)) / PAGE_SIZE);
for (order = 0; (1UL << order) < goal; order++)
;
}
do {
tcp_bhash_size = (1UL << order) * PAGE_SIZE /
sizeof(struct tcp_bind_bucket *);
tcp_bhash = (struct tcp_bind_bucket **)
__get_free_pages(GFP_ATOMIC, order);
} while (tcp_bhash == NULL && --order >= 0);
if (!tcp_bhash)
panic("Failed to allocate TCP bind hash table\n");
memset(tcp_bhash, 0, tcp_bhash_size * sizeof(struct tcp_bind_bucket *));
printk("TCP: Hash tables configured (ehash %d bhash %d)\n",
tcp_ehash_size, tcp_bhash_size);
}