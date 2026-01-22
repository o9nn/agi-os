#include <linux/config.h>
#include <linux/errno.h>
#include <linux/types.h>
#include <linux/socket.h>
#include <linux/in.h>
#include <linux/kernel.h>
#include <linux/major.h>
#include <linux/sched.h>
#include <linux/timer.h>
#include <linux/string.h>
#include <linux/sockios.h>
#include <linux/net.h>
#include <linux/fcntl.h>
#include <linux/mm.h>
#include <linux/interrupt.h>
#include <linux/proc_fs.h>
#include <linux/stat.h>
#include <linux/init.h>
#include <linux/poll.h>
#include <asm/uaccess.h>
#include <asm/system.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <net/arp.h>
#include <net/rarp.h>
#include <net/route.h>
#include <net/tcp.h>
#include <net/udp.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/raw.h>
#include <net/icmp.h>
#include <net/ipip.h>
#include <net/inet_common.h>
#include <linux/ip_fw.h>
#ifdef CONFIG_IP_MROUTE
#include <linux/mroute.h>
#endif
#ifdef CONFIG_IP_MASQUERADE
#include <net/ip_masq.h>
#endif
#ifdef CONFIG_BRIDGE
#include <net/br.h>
#endif
#ifdef CONFIG_KMOD
#include <linux/kmod.h>
#endif
#ifdef CONFIG_NET_RADIO
#include <linux/wireless.h>
#endif
#define min(a,b)	((a)<(b)?(a):(b))
struct linux_mib net_statistics;
extern int raw_get_info(char *, char **, off_t, int, int);
extern int snmp_get_info(char *, char **, off_t, int, int);
extern int netstat_get_info(char *, char **, off_t, int, int);
extern int afinet_get_info(char *, char **, off_t, int, int);
extern int tcp_get_info(char *, char **, off_t, int, int);
extern int udp_get_info(char *, char **, off_t, int, int);
extern void ip_mc_drop_socket(struct sock *sk);
#ifdef CONFIG_DLCI
extern int dlci_ioctl(unsigned int, void*);
#endif
#ifdef CONFIG_DLCI_MODULE
int (*dlci_ioctl_hook)(unsigned int, void *) = NULL;
#endif
int (*rarp_ioctl_hook)(unsigned int,void*) = NULL;
static __inline__ void kill_sk_queues(struct sock *sk)
{
struct sk_buff *skb;
while((skb = skb_dequeue(&sk->receive_queue)) != NULL)
kfree_skb(skb);
while((skb = skb_dequeue(&sk->error_queue)) != NULL)
kfree_skb(skb);
while((skb=skb_dequeue(&sk->back_log)) != NULL)
kfree_skb(skb);
}
static __inline__ void kill_sk_now(struct sock *sk)
{
del_from_prot_sklist(sk);
sk->prot->unhash(sk);
if(sk->opt)
kfree(sk->opt);
dst_release(sk->dst_cache);
sk_free(sk);
}
static __inline__ void kill_sk_later(struct sock *sk)
{
NETDEBUG(printk(KERN_DEBUG "Socket destroy delayed (r=%d w=%d)\n",
atomic_read(&sk->rmem_alloc),
atomic_read(&sk->wmem_alloc)));
sk->ack_backlog = 0;
release_sock(sk);
net_reset_timer(sk, TIME_DESTROY, SOCK_DESTROY_TIME);
}
void destroy_sock(struct sock *sk)
{
lock_sock(sk);
net_delete_timer(sk);
if (sk->prot->destroy && !sk->destroy)
sk->prot->destroy(sk);
sk->destroy = 1;
kill_sk_queues(sk);
if (atomic_read(&sk->rmem_alloc) == 0 && atomic_read(&sk->wmem_alloc) == 0)
kill_sk_now(sk);
else
kill_sk_later(sk);
}
int inet_setsockopt(struct socket *sock, int level, int optname,
char *optval, int optlen)
{
struct sock *sk=sock->sk;
if (sk->prot->setsockopt==NULL)
return(-EOPNOTSUPP);
return sk->prot->setsockopt(sk,level,optname,optval,optlen);
}
int inet_getsockopt(struct socket *sock, int level, int optname,
char *optval, int *optlen)
{
struct sock *sk=sock->sk;
if (sk->prot->getsockopt==NULL)
return(-EOPNOTSUPP);
return sk->prot->getsockopt(sk,level,optname,optval,optlen);
}
static int inet_autobind(struct sock *sk)
{
if (sk->num == 0) {
if (sk->prot->get_port(sk, 0) != 0)
return(-EAGAIN);
sk->sport = htons(sk->num);
sk->prot->hash(sk);
add_to_prot_sklist(sk);
}
return 0;
}
int inet_listen(struct socket *sock, int backlog)
{
struct sock *sk = sock->sk;
unsigned char old_state;
if (sock->state != SS_UNCONNECTED || sock->type != SOCK_STREAM)
return(-EINVAL);
if ((unsigned) backlog == 0)
backlog = 1;
if ((unsigned) backlog > SOMAXCONN)
backlog = SOMAXCONN;
sk->max_ack_backlog = backlog;
old_state = sk->state;
if (old_state != TCP_LISTEN) {
sk->state = TCP_LISTEN;
sk->ack_backlog = 0;
if (sk->num == 0) {
if (sk->prot->get_port(sk, 0) != 0) {
sk->state = old_state;
return -EAGAIN;
}
sk->sport = htons(sk->num);
add_to_prot_sklist(sk);
} else {
if (sk->prev)
((struct tcp_bind_bucket*)sk->prev)->fastreuse = 0;
}
dst_release(xchg(&sk->dst_cache, NULL));
sk->prot->hash(sk);
sk->socket->flags |= SO_ACCEPTCON;
}
return 0;
}
static int inet_create(struct socket *sock, int protocol)
{
struct sock *sk;
struct proto *prot;
if (sock->type == SOCK_PACKET) {
static int warned;
if (net_families[PF_PACKET]==NULL)
{
#if defined(CONFIG_KMOD) && defined(CONFIG_PACKET_MODULE)
char module_name[30];
sprintf(module_name,"net-pf-%d", PF_PACKET);
request_module(module_name);
if (net_families[PF_PACKET] == NULL)
#endif
return -ESOCKTNOSUPPORT;
}
if (!warned++)
printk(KERN_INFO "%s uses obsolete (PF_INET,SOCK_PACKET)\n", current->comm);
return net_families[PF_PACKET]->create(sock, protocol);
}
sock->state = SS_UNCONNECTED;
sk = sk_alloc(PF_INET, GFP_KERNEL, 1);
if (sk == NULL)
goto do_oom;
switch (sock->type) {
case SOCK_STREAM:
if (protocol && protocol != IPPROTO_TCP)
goto free_and_noproto;
protocol = IPPROTO_TCP;
if (ipv4_config.no_pmtu_disc)
sk->ip_pmtudisc = IP_PMTUDISC_DONT;
else
sk->ip_pmtudisc = IP_PMTUDISC_WANT;
prot = &tcp_prot;
sock->ops = &inet_stream_ops;
break;
case SOCK_SEQPACKET:
goto free_and_badtype;
case SOCK_DGRAM:
if (protocol && protocol != IPPROTO_UDP)
goto free_and_noproto;
protocol = IPPROTO_UDP;
sk->no_check = UDP_NO_CHECK;
sk->ip_pmtudisc = IP_PMTUDISC_DONT;
prot=&udp_prot;
sock->ops = &inet_dgram_ops;
break;
case SOCK_RAW:
if (!capable(CAP_NET_RAW))
goto free_and_badperm;
if (!protocol)
goto free_and_noproto;
prot = &raw_prot;
sk->reuse = 1;
sk->ip_pmtudisc = IP_PMTUDISC_DONT;
sk->num = protocol;
sock->ops = &inet_dgram_ops;
if (protocol == IPPROTO_RAW)
sk->ip_hdrincl = 1;
break;
default:
goto free_and_badtype;
}
sock_init_data(sock,sk);
sk->destruct = NULL;
sk->zapped=0;
#ifdef CONFIG_TCP_NAGLE_OFF
sk->nonagle = 1;
#endif
sk->family = PF_INET;
sk->protocol = protocol;
sk->prot = prot;
sk->backlog_rcv = prot->backlog_rcv;
sk->timer.data = (unsigned long)sk;
sk->timer.function = &net_timer;
sk->ip_ttl=ip_statistics.IpDefaultTTL;
sk->ip_mc_loop=1;
sk->ip_mc_ttl=1;
sk->ip_mc_index=0;
sk->ip_mc_list=NULL;
if (sk->num) {
sk->sport = htons(sk->num);
sk->prot->hash(sk);
add_to_prot_sklist(sk);
}
if (sk->prot->init) {
int err = sk->prot->init(sk);
if (err != 0) {
destroy_sock(sk);
return(err);
}
}
return(0);
free_and_badtype:
sk_free(sk);
return -ESOCKTNOSUPPORT;
free_and_badperm:
sk_free(sk);
return -EPERM;
free_and_noproto:
sk_free(sk);
return -EPROTONOSUPPORT;
do_oom:
return -ENOBUFS;
}
int inet_release(struct socket *sock, struct socket *peersock)
{
struct sock *sk = sock->sk;
if (sk) {
long timeout;
if (sock->state != SS_UNCONNECTED)
sock->state = SS_DISCONNECTING;
sk->state_change(sk);
ip_mc_drop_socket(sk);
timeout = 0;
if (sk->linger && !(current->flags & PF_EXITING)) {
timeout = HZ * sk->lingertime;
if (!timeout)
timeout = MAX_SCHEDULE_TIMEOUT;
}
sock->sk = NULL;
sk->socket = NULL;
sk->prot->close(sk, timeout);
}
return(0);
}
static int inet_bind(struct socket *sock, struct sockaddr *uaddr, int addr_len)
{
struct sockaddr_in *addr=(struct sockaddr_in *)uaddr;
struct sock *sk=sock->sk;
unsigned short snum;
int chk_addr_ret;
if(sk->prot->bind)
return sk->prot->bind(sk, uaddr, addr_len);
if ((sk->state != TCP_CLOSE)			||
(addr_len < sizeof(struct sockaddr_in))	||
(sk->num != 0))
return -EINVAL;
chk_addr_ret = inet_addr_type(addr->sin_addr.s_addr);
if (addr->sin_addr.s_addr != 0 && chk_addr_ret != RTN_LOCAL &&
chk_addr_ret != RTN_MULTICAST && chk_addr_ret != RTN_BROADCAST) {
#ifdef CONFIG_IP_TRANSPARENT_PROXY
if(chk_addr_ret != RTN_UNICAST || !capable(CAP_NET_ADMIN))
#endif
return -EADDRNOTAVAIL;
}
sk->rcv_saddr = sk->saddr = addr->sin_addr.s_addr;
if(chk_addr_ret == RTN_MULTICAST || chk_addr_ret == RTN_BROADCAST)
sk->saddr = 0;
snum = ntohs(addr->sin_port);
#ifdef CONFIG_IP_MASQUERADE
if((snum >= PORT_MASQ_BEGIN) && (snum <= PORT_MASQ_END))
return -EADDRINUSE;
#endif
if (snum && snum < PROT_SOCK && !capable(CAP_NET_BIND_SERVICE))
return(-EACCES);
if (sk->prot->get_port(sk, snum) != 0)
return -EADDRINUSE;
sk->sport = htons(sk->num);
sk->daddr = 0;
sk->dport = 0;
sk->prot->hash(sk);
add_to_prot_sklist(sk);
dst_release(sk->dst_cache);
sk->dst_cache=NULL;
return(0);
}
int inet_dgram_connect(struct socket *sock, struct sockaddr * uaddr,
int addr_len, int flags)
{
struct sock *sk=sock->sk;
int err;
if (inet_autobind(sk) != 0)
return(-EAGAIN);
if (sk->prot->connect == NULL)
return(-EOPNOTSUPP);
err = sk->prot->connect(sk, (struct sockaddr *)uaddr, addr_len);
if (err < 0)
return(err);
return(0);
}
static void inet_wait_for_connect(struct sock *sk)
{
struct wait_queue wait = { current, NULL };
add_wait_queue(sk->sleep, &wait);
current->state = TASK_INTERRUPTIBLE;
while (sk->state == TCP_SYN_SENT || sk->state == TCP_SYN_RECV) {
if (signal_pending(current))
break;
if (sk->err)
break;
schedule();
current->state = TASK_INTERRUPTIBLE;
}
current->state = TASK_RUNNING;
remove_wait_queue(sk->sleep, &wait);
}
int inet_stream_connect(struct socket *sock, struct sockaddr * uaddr,
int addr_len, int flags)
{
struct sock *sk=sock->sk;
int err;
if(sock->state != SS_UNCONNECTED && sock->state != SS_CONNECTING) {
if(sock->state == SS_CONNECTED)
return -EISCONN;
return -EINVAL;
}
if(sock->state == SS_CONNECTING) {
if(tcp_connected(sk->state)) {
sock->state = SS_CONNECTED;
return 0;
}
if (sk->zapped || sk->err)
goto sock_error;
if (flags & O_NONBLOCK)
return -EALREADY;
} else {
if (sk->prot->connect == NULL)
return(-EOPNOTSUPP);
if (inet_autobind(sk) != 0)
return(-EAGAIN);
err = sk->prot->connect(sk, uaddr, addr_len);
if (err < 0)
return(err);
sock->state = SS_CONNECTING;
}
if (sk->state > TCP_FIN_WAIT2 && sock->state == SS_CONNECTING)
goto sock_error;
if (sk->state != TCP_ESTABLISHED && (flags & O_NONBLOCK))
return (-EINPROGRESS);
if (sk->state == TCP_SYN_SENT || sk->state == TCP_SYN_RECV) {
inet_wait_for_connect(sk);
if (signal_pending(current))
return -ERESTARTSYS;
}
sock->state = SS_CONNECTED;
if ((sk->state != TCP_ESTABLISHED) && sk->err)
goto sock_error;
return(0);
sock_error:
if (sk->zapped && sk->state != TCP_CLOSE) {
lock_sock(sk);
tcp_set_state(sk, TCP_CLOSE);
release_sock(sk);
sk->zapped = 0;
}
sock->state = SS_UNCONNECTED;
return sock_error(sk);
}
int inet_accept(struct socket *sock, struct socket *newsock, int flags)
{
struct sock *sk1 = sock->sk, *sk2;
struct sock *newsk = newsock->sk;
int err = -EINVAL;
if (sock->state != SS_UNCONNECTED || !(sock->flags & SO_ACCEPTCON))
goto do_err;
err = -EOPNOTSUPP;
if (sk1->prot->accept == NULL)
goto do_err;
if((sk2 = sk1->prot->accept(sk1,flags)) == NULL)
goto do_sk1_err;
sk2->sleep = newsk->sleep;
newsock->sk = sk2;
sk2->socket = newsock;
newsk->socket = NULL;
if (flags & O_NONBLOCK)
goto do_half_success;
if(sk2->state == TCP_ESTABLISHED)
goto do_full_success;
if(sk2->err > 0)
goto do_connect_err;
err = -ECONNABORTED;
if (sk2->state == TCP_CLOSE)
goto do_bad_connection;
do_full_success:
destroy_sock(newsk);
newsock->state = SS_CONNECTED;
return 0;
do_half_success:
destroy_sock(newsk);
return(0);
do_connect_err:
err = sock_error(sk2);
do_bad_connection:
sk2->sleep = NULL;
sk2->socket = NULL;
destroy_sock(sk2);
newsock->sk = newsk;
newsk->socket = newsock;
return err;
do_sk1_err:
err = sock_error(sk1);
do_err:
return err;
}
static int inet_getname(struct socket *sock, struct sockaddr *uaddr,
int *uaddr_len, int peer)
{
struct sock *sk		= sock->sk;
struct sockaddr_in *sin	= (struct sockaddr_in *)uaddr;
sin->sin_family = AF_INET;
if (peer) {
if (!tcp_connected(sk->state))
return(-ENOTCONN);
sin->sin_port = sk->dport;
sin->sin_addr.s_addr = sk->daddr;
} else {
__u32 addr = sk->rcv_saddr;
if (!addr)
addr = sk->saddr;
sin->sin_port = sk->sport;
sin->sin_addr.s_addr = addr;
}
memset(sin->sin_zero, 0, sizeof(sin->sin_zero));
*uaddr_len = sizeof(*sin);
return(0);
}
int inet_recvmsg(struct socket *sock, struct msghdr *msg, int size,
int flags, struct scm_cookie *scm)
{
struct sock *sk = sock->sk;
int addr_len = 0;
int err;
if (sock->flags & SO_ACCEPTCON)
return(-EINVAL);
if (sk->prot->recvmsg == NULL)
return(-EOPNOTSUPP);
if (inet_autobind(sk) != 0)
return(-EAGAIN);
err = sk->prot->recvmsg(sk, msg, size, flags&MSG_DONTWAIT,
flags&~MSG_DONTWAIT, &addr_len);
if (err >= 0)
msg->msg_namelen = addr_len;
return err;
}
int inet_sendmsg(struct socket *sock, struct msghdr *msg, int size,
struct scm_cookie *scm)
{
struct sock *sk = sock->sk;
if (sk->shutdown & SEND_SHUTDOWN) {
if (!(msg->msg_flags&MSG_NOSIGNAL))
send_sig(SIGPIPE, current, 1);
return(-EPIPE);
}
if (sk->prot->sendmsg == NULL)
return(-EOPNOTSUPP);
if(sk->err)
return sock_error(sk);
if (inet_autobind(sk) != 0)
return -EAGAIN;
return sk->prot->sendmsg(sk, msg, size);
}
int inet_shutdown(struct socket *sock, int how)
{
struct sock *sk = sock->sk;
how++;
if ((how & ~SHUTDOWN_MASK) || how==0)
return(-EINVAL);
if (!sk)
return(-ENOTCONN);
if (sock->state == SS_CONNECTING && sk->state == TCP_ESTABLISHED)
sock->state = SS_CONNECTED;
if (!tcp_connected(sk->state))
return(-ENOTCONN);
sk->shutdown |= how;
if (sk->prot->shutdown)
sk->prot->shutdown(sk, how);
sk->state_change(sk);
return(0);
}
unsigned int inet_poll(struct file * file, struct socket *sock, poll_table *wait)
{
struct sock *sk = sock->sk;
if (sk->prot->poll == NULL)
return(0);
return sk->prot->poll(file, sock, wait);
}
#ifdef _HURD_
#define inet_ioctl 0
#else
static int inet_ioctl(struct socket *sock, unsigned int cmd, unsigned long arg)
{
struct sock *sk = sock->sk;
int err;
int pid;
switch(cmd)
{
case FIOSETOWN:
case SIOCSPGRP:
err = get_user(pid, (int *) arg);
if (err)
return err;
if (current->pid != pid && current->pgrp != -pid &&
!capable(CAP_NET_ADMIN))
return -EPERM;
sk->proc = pid;
return(0);
case FIOGETOWN:
case SIOCGPGRP:
return put_user(sk->proc, (int *)arg);
case SIOCGSTAMP:
if(sk->stamp.tv_sec==0)
return -ENOENT;
err = copy_to_user((void *)arg,&sk->stamp,sizeof(struct timeval));
if (err)
err = -EFAULT;
return err;
case SIOCADDRT:
case SIOCDELRT:
case SIOCRTMSG:
return(ip_rt_ioctl(cmd,(void *) arg));
case SIOCDARP:
case SIOCGARP:
case SIOCSARP:
return(arp_ioctl(cmd,(void *) arg));
case SIOCDRARP:
case SIOCGRARP:
case SIOCSRARP:
#ifdef CONFIG_KMOD
if (rarp_ioctl_hook == NULL)
request_module("rarp");
#endif
if (rarp_ioctl_hook != NULL)
return(rarp_ioctl_hook(cmd,(void *) arg));
case SIOCGIFADDR:
case SIOCSIFADDR:
case SIOCGIFBRDADDR:
case SIOCSIFBRDADDR:
case SIOCGIFNETMASK:
case SIOCSIFNETMASK:
case SIOCGIFDSTADDR:
case SIOCSIFDSTADDR:
case SIOCSIFPFLAGS:
case SIOCGIFPFLAGS:
case SIOCSIFFLAGS:
return(devinet_ioctl(cmd,(void *) arg));
case SIOCGIFBR:
case SIOCSIFBR:
#ifdef CONFIG_BRIDGE
return(br_ioctl(cmd,(void *) arg));
#else
return -ENOPKG;
#endif
case SIOCADDDLCI:
case SIOCDELDLCI:
#ifdef CONFIG_DLCI
return(dlci_ioctl(cmd, (void *) arg));
#endif
#ifdef CONFIG_DLCI_MODULE
#ifdef CONFIG_KMOD
if (dlci_ioctl_hook == NULL)
request_module("dlci");
#endif
if (dlci_ioctl_hook)
return((*dlci_ioctl_hook)(cmd, (void *) arg));
#endif
return -ENOPKG;
default:
if ((cmd >= SIOCDEVPRIVATE) &&
(cmd <= (SIOCDEVPRIVATE + 15)))
return(dev_ioctl(cmd,(void *) arg));
#ifdef CONFIG_NET_RADIO
if((cmd >= SIOCIWFIRST) && (cmd <= SIOCIWLAST))
return(dev_ioctl(cmd,(void *) arg));
#endif
if (sk->prot->ioctl==NULL || (err=sk->prot->ioctl(sk, cmd, arg))==-ENOIOCTLCMD)
return(dev_ioctl(cmd,(void *) arg));
return err;
}
return(0);
}
#endif
struct proto_ops inet_stream_ops = {
PF_INET,
sock_no_dup,
inet_release,
inet_bind,
inet_stream_connect,
sock_no_socketpair,
inet_accept,
inet_getname,
inet_poll,
inet_ioctl,
inet_listen,
inet_shutdown,
inet_setsockopt,
inet_getsockopt,
sock_no_fcntl,
inet_sendmsg,
inet_recvmsg
};
struct proto_ops inet_dgram_ops = {
PF_INET,
sock_no_dup,
inet_release,
inet_bind,
inet_dgram_connect,
sock_no_socketpair,
sock_no_accept,
inet_getname,
datagram_poll,
inet_ioctl,
sock_no_listen,
inet_shutdown,
inet_setsockopt,
inet_getsockopt,
sock_no_fcntl,
inet_sendmsg,
inet_recvmsg
};
struct net_proto_family inet_family_ops = {
PF_INET,
inet_create
};
#ifdef CONFIG_PROC_FS
#ifdef CONFIG_INET_RARP
static struct proc_dir_entry proc_net_rarp = {
PROC_NET_RARP, 4, "rarp",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
rarp_get_info
};
#endif
static struct proc_dir_entry proc_net_raw = {
PROC_NET_RAW, 3, "raw",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
raw_get_info
};
static struct proc_dir_entry proc_net_netstat = {
PROC_NET_NETSTAT, 7, "netstat",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
netstat_get_info
};
static struct proc_dir_entry proc_net_snmp = {
PROC_NET_SNMP, 4, "snmp",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
snmp_get_info
};
static struct proc_dir_entry proc_net_sockstat = {
PROC_NET_SOCKSTAT, 8, "sockstat",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
afinet_get_info
};
static struct proc_dir_entry proc_net_tcp = {
PROC_NET_TCP, 3, "tcp",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
tcp_get_info
};
static struct proc_dir_entry proc_net_udp = {
PROC_NET_UDP, 3, "udp",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
udp_get_info
};
#endif
extern void tcp_init(void);
extern void tcp_v4_init(struct net_proto_family *);
__initfunc(void inet_proto_init(struct net_proto *pro))
{
struct sk_buff *dummy_skb;
struct inet_protocol *p;
printk(KERN_INFO "NET4: Linux TCP/IP 1.0 for NET4.0\n");
if (sizeof(struct inet_skb_parm) > sizeof(dummy_skb->cb))
{
printk(KERN_CRIT "inet_proto_init: panic\n");
return;
}
(void) sock_register(&inet_family_ops);
printk(KERN_INFO "IP Protocols: ");
for(p = inet_protocol_base; p != NULL;)
{
struct inet_protocol *tmp = (struct inet_protocol *) p->next;
inet_add_protocol(p);
printk("%s%s",p->name,tmp?", ":"\n");
p = tmp;
}
arp_init();
ip_init();
tcp_v4_init(&inet_family_ops);
tcp_init();
icmp_init(&inet_family_ops);
#ifdef CONFIG_NET_IPIP
ipip_init();
#endif
#ifdef CONFIG_NET_IPGRE
ipgre_init();
#endif
#if defined(CONFIG_IP_FIREWALL)
ip_fw_init();
#endif
#ifdef CONFIG_IP_MASQUERADE
ip_masq_init();
#endif
#if defined(CONFIG_IP_MROUTE)
ip_mr_init();
#endif
#ifdef CONFIG_INET_RARP
rarp_ioctl_hook = rarp_ioctl;
#endif
#ifdef CONFIG_PROC_FS
#ifdef CONFIG_INET_RARP
proc_net_register(&proc_net_rarp);
#endif
proc_net_register(&proc_net_raw);
proc_net_register(&proc_net_snmp);
proc_net_register(&proc_net_netstat);
proc_net_register(&proc_net_sockstat);
proc_net_register(&proc_net_tcp);
proc_net_register(&proc_net_udp);
#endif
}