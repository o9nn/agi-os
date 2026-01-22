#include <linux/types.h>
#include <linux/errno.h>
#include <linux/socket.h>
#include <linux/in.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/timer.h>
#include <asm/system.h>
#include <linux/interrupt.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <net/tcp.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/arp.h>
void net_delete_timer (struct sock *t)
{
if(t->timer.prev)
del_timer (&t->timer);
t->timeout = 0;
}
void net_reset_timer (struct sock *t, int timeout, unsigned long len)
{
t->timeout = timeout;
mod_timer(&t->timer, jiffies+len);
}
void net_timer (unsigned long data)
{
struct sock *sk = (struct sock*)data;
int why = sk->timeout;
if (atomic_read(&sk->sock_readers)) {
mod_timer(&sk->timer, jiffies+HZ/20);
return;
}
if (sk->tp_pinfo.af_tcp.delayed_acks && !sk->zapped) {
sk->prot->read_wakeup (sk);
if (!sk->dead)
sk->data_ready(sk,0);
}
switch (why) {
case TIME_DONE:
if (!sk->dead) {
net_reset_timer(sk, TIME_DONE, TCP_DONE_TIME);
break;
}
if (sk->state != TCP_CLOSE) {
printk (KERN_DEBUG "non CLOSE socket in time_done\n");
break;
}
destroy_sock (sk);
break;
case TIME_DESTROY:
destroy_sock(sk);
break;
case TIME_CLOSE:
tcp_set_state(sk, TCP_CLOSE);
sk->shutdown = SHUTDOWN_MASK;
if (!sk->dead)
sk->state_change(sk);
net_reset_timer (sk, TIME_DONE, TCP_DONE_TIME);
break;
default:
printk ("net_timer: timer expired - reason %d is unknown\n", why);
break;
}
}