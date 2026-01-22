#include <linux/types.h>
#include <linux/kernel.h>
#include <asm/uaccess.h>
#include <asm/system.h>
#include <linux/mm.h>
#include <linux/interrupt.h>
#include <linux/in.h>
#include <linux/errno.h>
#include <linux/sched.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/poll.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <net/route.h>
#include <net/tcp.h>
#include <net/udp.h>
#include <linux/skbuff.h>
#include <net/sock.h>
static inline void wait_for_packet(struct sock * sk)
{
struct wait_queue wait = { current, NULL };
add_wait_queue(sk->sleep, &wait);
current->state = TASK_INTERRUPTIBLE;
if (skb_peek(&sk->receive_queue) == NULL)
schedule();
current->state = TASK_RUNNING;
remove_wait_queue(sk->sleep, &wait);
}
static inline int connection_based(struct sock *sk)
{
return (sk->type==SOCK_SEQPACKET || sk->type==SOCK_STREAM);
}
struct sk_buff *skb_recv_datagram(struct sock *sk, unsigned flags, int noblock, int *err)
{
int error;
struct sk_buff *skb;
error = sock_error(sk);
if (error)
goto no_packet;
restart:
while(skb_queue_empty(&sk->receive_queue))
{
error = sock_error(sk);
if (error)
goto no_packet;
if (sk->shutdown & RCV_SHUTDOWN)
goto no_packet;
error = -ENOTCONN;
if(connection_based(sk) && sk->state!=TCP_ESTABLISHED)
goto no_packet;
error = -ERESTARTSYS;
if (signal_pending(current))
goto no_packet;
error = -EAGAIN;
if (noblock)
goto no_packet;
wait_for_packet(sk);
}
if (flags & MSG_PEEK)
{
unsigned long cpu_flags;
spin_lock_irqsave(&skb_queue_lock, cpu_flags);
skb = skb_peek(&sk->receive_queue);
if(skb!=NULL)
atomic_inc(&skb->users);
spin_unlock_irqrestore(&skb_queue_lock, cpu_flags);
} else
skb = skb_dequeue(&sk->receive_queue);
if (!skb)
goto restart;
return skb;
no_packet:
*err = error;
return NULL;
}
void skb_free_datagram(struct sock * sk, struct sk_buff *skb)
{
kfree_skb(skb);
}
int skb_copy_datagram(struct sk_buff *skb, int offset, char *to, int size)
{
int err = -EFAULT;
if (!copy_to_user(to, skb->h.raw + offset, size))
err = 0;
return err;
}
int skb_copy_datagram_iovec(struct sk_buff *skb, int offset, struct iovec *to,
int size)
{
return memcpy_toiovec(to, skb->h.raw + offset, size);
}
unsigned int datagram_poll(struct file * file, struct socket *sock, poll_table *wait)
{
struct sock *sk = sock->sk;
unsigned int mask;
poll_wait(file, sk->sleep, wait);
mask = 0;
if (sk->err || !skb_queue_empty(&sk->error_queue))
mask |= POLLERR;
if (sk->shutdown & RCV_SHUTDOWN)
mask |= POLLHUP;
if (!skb_queue_empty(&sk->receive_queue))
mask |= POLLIN | POLLRDNORM;
if (connection_based(sk)) {
if (sk->state==TCP_CLOSE)
mask |= POLLHUP;
if (sk->state == TCP_SYN_SENT)
return mask;
}
if (sock_writeable(sk))
mask |= POLLOUT | POLLWRNORM | POLLWRBAND;
else
sk->socket->flags |= SO_NOSPACE;
return mask;
}