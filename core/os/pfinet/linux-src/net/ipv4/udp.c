#include <asm/system.h>
#include <asm/uaccess.h>
#include <linux/types.h>
#include <linux/fcntl.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/in.h>
#include <linux/errno.h>
#include <linux/timer.h>
#include <linux/mm.h>
#include <linux/config.h>
#include <linux/inet.h>
#include <linux/ipv6.h>
#include <linux/netdevice.h>
#include <net/snmp.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/udp.h>
#include <net/icmp.h>
#include <net/route.h>
#include <net/checksum.h>
struct udp_mib		udp_statistics;
struct sock *udp_hash[UDP_HTABLE_SIZE];
static int udp_v4_get_port(struct sock *sk, unsigned short snum)
{
SOCKHASH_LOCK();
if (snum == 0) {
int low = sysctl_local_port_range[0];
int high = sysctl_local_port_range[1];
int best_size_so_far, best, result, i;
best_size_so_far = 32767;
best = result = net_random() % (high - low) + low;
for (i = 0; i < UDP_HTABLE_SIZE; i++, result++) {
struct sock *sk;
int size;
sk = udp_hash[result & (UDP_HTABLE_SIZE - 1)];
if (!sk) {
if (result > sysctl_local_port_range[1])
result = sysctl_local_port_range[0] +
((result - sysctl_local_port_range[0]) &
(UDP_HTABLE_SIZE - 1));
goto gotit;
}
size = 0;
do {
if (++size >= best_size_so_far)
goto next;
} while ((sk = sk->next) != NULL);
best_size_so_far = size;
best = result;
next:
;
}
result = best;
for(;; result += UDP_HTABLE_SIZE) {
if (result > sysctl_local_port_range[1])
result = sysctl_local_port_range[0]
+ ((result - sysctl_local_port_range[0]) &
(UDP_HTABLE_SIZE - 1));
if (!udp_lport_inuse(result))
break;
}
gotit:
snum = result;
} else {
struct sock *sk2;
for (sk2 = udp_hash[snum & (UDP_HTABLE_SIZE - 1)];
sk2 != NULL;
sk2 = sk2->next) {
if (sk2->num == snum &&
sk2 != sk &&
!ipv6_only_sock(sk2) &&
sk2->bound_dev_if == sk->bound_dev_if &&
(!sk2->rcv_saddr ||
!sk->rcv_saddr ||
sk2->rcv_saddr == sk->rcv_saddr) &&
(!sk2->reuse || !sk->reuse))
goto fail;
}
}
sk->num = snum;
SOCKHASH_UNLOCK();
return 0;
fail:
SOCKHASH_UNLOCK();
return 1;
}
static u32 uh_cache_saddr, uh_cache_daddr;
static u16 uh_cache_dport, uh_cache_sport;
static struct sock *uh_cache_sk = NULL;
static void udp_v4_hash(struct sock *sk)
{
struct sock **skp = &udp_hash[sk->num & (UDP_HTABLE_SIZE - 1)];
SOCKHASH_LOCK();
if ((sk->next = *skp) != NULL)
(*skp)->pprev = &sk->next;
*skp = sk;
sk->pprev = skp;
SOCKHASH_UNLOCK();
}
static void udp_v4_unhash(struct sock *sk)
{
SOCKHASH_LOCK();
if (sk->pprev) {
if (sk->next)
sk->next->pprev = sk->pprev;
*sk->pprev = sk->next;
sk->pprev = NULL;
if(uh_cache_sk == sk)
uh_cache_sk = NULL;
}
SOCKHASH_UNLOCK();
}
struct sock *udp_v4_lookup_longway(u32 saddr, u16 sport, u32 daddr, u16 dport, int dif)
{
struct sock *sk, *result = NULL;
unsigned short hnum = ntohs(dport);
int badness = -1;
for(sk = udp_hash[hnum & (UDP_HTABLE_SIZE - 1)]; sk != NULL; sk = sk->next) {
if((sk->num == hnum) && !ipv6_only_sock(sk)
&& !(sk->dead && (sk->state == TCP_CLOSE))) {
int score = (sk->family == PF_INET ? 1 : 0);
if(sk->rcv_saddr) {
if(sk->rcv_saddr != daddr)
continue;
score+=2;
}
if(sk->daddr) {
if(sk->daddr != saddr)
continue;
score+=2;
}
if(sk->dport) {
if(sk->dport != sport)
continue;
score+=2;
}
if(sk->bound_dev_if) {
if(sk->bound_dev_if != dif)
continue;
score+=2;
}
if(score == 9) {
result = sk;
break;
} else if(score > badness) {
result = sk;
badness = score;
}
}
}
return result;
}
struct sock *udp_v4_lookup(u32 saddr, u16 sport, u32 daddr, u16 dport, int dif)
{
struct sock *sk;
if(!dif && uh_cache_sk		&&
uh_cache_saddr == saddr	&&
uh_cache_sport == sport	&&
uh_cache_dport == dport	&&
uh_cache_daddr == daddr)
return uh_cache_sk;
sk = udp_v4_lookup_longway(saddr, sport, daddr, dport, dif);
if(!dif) {
uh_cache_sk	= sk;
uh_cache_saddr	= saddr;
uh_cache_daddr	= daddr;
uh_cache_sport	= sport;
uh_cache_dport	= dport;
}
return sk;
}
#ifdef CONFIG_IP_TRANSPARENT_PROXY
#define secondlist(hpnum, sk, fpass) \
({ struct sock *s1; if(!(sk) && (fpass)--) \
s1 = udp_hash[(hpnum) & (UDP_HTABLE_SIZE - 1)]; \
else \
s1 = (sk); \
s1; \
})
#define udp_v4_proxy_loop_init(hnum, hpnum, sk, fpass) \
secondlist((hpnum), udp_hash[(hnum)&(UDP_HTABLE_SIZE-1)],(fpass))
#define udp_v4_proxy_loop_next(hnum, hpnum, sk, fpass) \
secondlist((hpnum),(sk)->next,(fpass))
static struct sock *udp_v4_proxy_lookup(unsigned short num, unsigned long raddr,
unsigned short rnum, unsigned long laddr,
struct device *dev, unsigned short pnum,
int dif)
{
struct sock *s, *result = NULL;
int badness = -1;
u32 paddr = 0;
unsigned short hnum = ntohs(num);
unsigned short hpnum = ntohs(pnum);
int firstpass = 1;
if(dev && dev->ip_ptr) {
struct in_device *idev = dev->ip_ptr;
if(idev->ifa_list)
paddr = idev->ifa_list->ifa_local;
}
SOCKHASH_LOCK();
for(s = udp_v4_proxy_loop_init(hnum, hpnum, s, firstpass);
s != NULL;
s = udp_v4_proxy_loop_next(hnum, hpnum, s, firstpass)) {
if(s->num == hnum || s->num == hpnum) {
int score = 0;
if(s->dead && (s->state == TCP_CLOSE))
continue;
if(s->rcv_saddr) {
if((s->num != hpnum || s->rcv_saddr != paddr) &&
(s->num != hnum || s->rcv_saddr != laddr))
continue;
score++;
}
if(s->daddr) {
if(s->daddr != raddr)
continue;
score++;
}
if(s->dport) {
if(s->dport != rnum)
continue;
score++;
}
if(s->bound_dev_if) {
if(s->bound_dev_if != dif)
continue;
score++;
}
if(score == 4 && s->num == hnum) {
result = s;
break;
} else if(score > badness && (s->num == hpnum || s->rcv_saddr)) {
result = s;
badness = score;
}
}
}
SOCKHASH_UNLOCK();
return result;
}
#undef secondlist
#undef udp_v4_proxy_loop_init
#undef udp_v4_proxy_loop_next
#endif
static inline struct sock *udp_v4_mcast_next(struct sock *sk,
unsigned short num,
unsigned long raddr,
unsigned short rnum,
unsigned long laddr,
int dif)
{
struct sock *s = sk;
unsigned short hnum = ntohs(num);
for(; s; s = s->next) {
if ((s->num != hnum)					||
(s->dead && (s->state == TCP_CLOSE))		||
(s->daddr && s->daddr!=raddr)			||
(s->dport != rnum && s->dport != 0)			||
(s->rcv_saddr  && s->rcv_saddr != laddr)		||
ipv6_only_sock(s)					||
(s->bound_dev_if && s->bound_dev_if != dif))
continue;
break;
}
return s;
}
void udp_err(struct sk_buff *skb, unsigned char *dp, int len)
{
struct iphdr *iph = (struct iphdr*)dp;
struct udphdr *uh = (struct udphdr*)(dp+(iph->ihl<<2));
int type = skb->h.icmph->type;
int code = skb->h.icmph->code;
struct sock *sk;
int harderr;
u32 info;
int err;
if (len < (iph->ihl<<2)+sizeof(struct udphdr)) {
icmp_statistics.IcmpInErrors++;
return;
}
sk = udp_v4_lookup(iph->daddr, uh->dest, iph->saddr, uh->source, skb->dev->ifindex);
if (sk == NULL) {
icmp_statistics.IcmpInErrors++;
return;
}
err = 0;
info = 0;
harderr = 0;
switch (type) {
default:
case ICMP_TIME_EXCEEDED:
err = EHOSTUNREACH;
break;
case ICMP_SOURCE_QUENCH:
return;
case ICMP_PARAMETERPROB:
err = EPROTO;
info = ntohl(skb->h.icmph->un.gateway)>>24;
harderr = 1;
break;
case ICMP_DEST_UNREACH:
if (code == ICMP_FRAG_NEEDED) {
if (sk->ip_pmtudisc != IP_PMTUDISC_DONT) {
err = EMSGSIZE;
info = ntohs(skb->h.icmph->un.frag.mtu);
harderr = 1;
break;
}
return;
}
err = EHOSTUNREACH;
if (code <= NR_ICMP_UNREACH) {
harderr = icmp_err_convert[code].fatal;
err = icmp_err_convert[code].errno;
}
break;
}
if (!sk->ip_recverr) {
if (!harderr || sk->state != TCP_ESTABLISHED)
return;
} else {
ip_icmp_error(sk, skb, err, uh->dest, info, (u8*)(uh+1));
}
sk->err = err;
sk->error_report(sk);
}
static unsigned short udp_check(struct udphdr *uh, int len, unsigned long saddr, unsigned long daddr, unsigned long base)
{
return(csum_tcpudp_magic(saddr, daddr, len, IPPROTO_UDP, base));
}
struct udpfakehdr
{
struct udphdr uh;
u32 saddr;
u32 daddr;
struct iovec *iov;
u32 wcheck;
};
static int udp_getfrag(const void *p, char * to, unsigned int offset, unsigned int fraglen)
{
struct udpfakehdr *ufh = (struct udpfakehdr *)p;
if (offset==0) {
if (csum_partial_copy_fromiovecend(to+sizeof(struct udphdr), ufh->iov, offset,
fraglen-sizeof(struct udphdr), &ufh->wcheck))
return -EFAULT;
ufh->wcheck = csum_partial((char *)ufh, sizeof(struct udphdr),
ufh->wcheck);
ufh->uh.check = csum_tcpudp_magic(ufh->saddr, ufh->daddr,
ntohs(ufh->uh.len),
IPPROTO_UDP, ufh->wcheck);
if (ufh->uh.check == 0)
ufh->uh.check = -1;
memcpy(to, ufh, sizeof(struct udphdr));
return 0;
}
if (csum_partial_copy_fromiovecend(to, ufh->iov, offset-sizeof(struct udphdr),
fraglen, &ufh->wcheck))
return -EFAULT;
return 0;
}
static int udp_getfrag_nosum(const void *p, char * to, unsigned int offset, unsigned int fraglen)
{
struct udpfakehdr *ufh = (struct udpfakehdr *)p;
if (offset==0) {
memcpy(to, ufh, sizeof(struct udphdr));
return memcpy_fromiovecend(to+sizeof(struct udphdr), ufh->iov, offset,
fraglen-sizeof(struct udphdr));
}
return memcpy_fromiovecend(to, ufh->iov, offset-sizeof(struct udphdr),
fraglen);
}
int udp_sendmsg(struct sock *sk, struct msghdr *msg, int len)
{
int ulen = len + sizeof(struct udphdr);
struct ipcm_cookie ipc;
struct udpfakehdr ufh;
struct rtable *rt = NULL;
int free = 0;
int connected = 0;
u32 daddr;
u8  tos;
int err;
if (len < 0 || len > 0xFFFF)
return -EMSGSIZE;
if (msg->msg_flags&MSG_OOB)
return -EOPNOTSUPP;
#ifdef CONFIG_IP_TRANSPARENT_PROXY
if (msg->msg_flags&~(MSG_DONTROUTE|MSG_DONTWAIT|MSG_PROXY|MSG_NOSIGNAL))
return -EINVAL;
if ((msg->msg_flags&MSG_PROXY) && !capable(CAP_NET_ADMIN))
return -EPERM;
#else
if (msg->msg_flags&~(MSG_DONTROUTE|MSG_DONTWAIT|MSG_NOSIGNAL))
return -EINVAL;
#endif
if (msg->msg_name) {
struct sockaddr_in * usin = (struct sockaddr_in*)msg->msg_name;
if (msg->msg_namelen < sizeof(*usin))
return(-EINVAL);
if (usin->sin_family != AF_INET) {
static int complained;
if (!complained++)
printk(KERN_WARNING "%s forgot to set AF_INET in udp sendmsg. Fix it!\n", current->comm);
if (usin->sin_family)
return -EINVAL;
}
ufh.daddr = usin->sin_addr.s_addr;
ufh.uh.dest = usin->sin_port;
if (ufh.uh.dest == 0)
return -EINVAL;
} else {
if (sk->state != TCP_ESTABLISHED)
return -ENOTCONN;
ufh.daddr = sk->daddr;
ufh.uh.dest = sk->dport;
connected = 1;
}
#ifdef CONFIG_IP_TRANSPARENT_PROXY
if (msg->msg_flags&MSG_PROXY) {
struct sockaddr_in *from = (struct sockaddr_in *)msg->msg_name;
from = (struct sockaddr_in *)&from->sin_zero;
if (from->sin_family != AF_INET)
return -EINVAL;
ipc.addr = from->sin_addr.s_addr;
ufh.uh.source = from->sin_port;
if (ipc.addr == 0)
ipc.addr = sk->saddr;
connected = 0;
} else
#endif
{
ipc.addr = sk->saddr;
ufh.uh.source = sk->sport;
}
ipc.opt = NULL;
ipc.oif = sk->bound_dev_if;
if (msg->msg_controllen) {
err = ip_cmsg_send(msg, &ipc);
if (err)
return err;
if (ipc.opt)
free = 1;
connected = 0;
}
if (!ipc.opt)
ipc.opt = sk->opt;
ufh.saddr = ipc.addr;
ipc.addr = daddr = ufh.daddr;
if (ipc.opt && ipc.opt->srr) {
if (!daddr)
return -EINVAL;
daddr = ipc.opt->faddr;
connected = 0;
}
tos = RT_TOS(sk->ip_tos);
if (sk->localroute || (msg->msg_flags&MSG_DONTROUTE) ||
(ipc.opt && ipc.opt->is_strictroute)) {
tos |= RTO_ONLINK;
connected = 0;
}
if (MULTICAST(daddr)) {
if (!ipc.oif)
ipc.oif = sk->ip_mc_index;
if (!ufh.saddr)
ufh.saddr = sk->ip_mc_addr;
connected = 0;
}
if (connected && sk->dst_cache) {
rt = (struct rtable*)sk->dst_cache;
if (rt->u.dst.obsolete) {
sk->dst_cache = NULL;
dst_release(&rt->u.dst);
rt = NULL;
} else
dst_clone(&rt->u.dst);
}
if (rt == NULL) {
err = ip_route_output(&rt, daddr, ufh.saddr,
#ifdef CONFIG_IP_TRANSPARENT_PROXY
(msg->msg_flags&MSG_PROXY ? RTO_TPROXY : 0) |
#endif
tos, ipc.oif);
if (err)
goto out;
err = -EACCES;
if (rt->rt_flags&RTCF_BROADCAST && !sk->broadcast)
goto out;
if (connected && sk->dst_cache == NULL)
sk->dst_cache = dst_clone(&rt->u.dst);
}
ufh.saddr = rt->rt_src;
if (!ipc.addr)
ufh.daddr = ipc.addr = rt->rt_dst;
ufh.uh.len = htons(ulen);
ufh.uh.check = 0;
ufh.iov = msg->msg_iov;
ufh.wcheck = 0;
err = ip_build_xmit(sk,sk->no_check ? udp_getfrag_nosum : udp_getfrag,
&ufh, ulen, &ipc, rt, msg->msg_flags);
out:
ip_rt_put(rt);
if (free)
kfree(ipc.opt);
if (!err) {
udp_statistics.UdpOutDatagrams++;
return len;
}
return err;
}
#ifdef _HURD_
#define udp_ioctl 0
#else
int udp_ioctl(struct sock *sk, int cmd, unsigned long arg)
{
switch(cmd)
{
case TIOCOUTQ:
{
unsigned long amount;
amount = sock_wspace(sk);
return put_user(amount, (int *)arg);
}
case TIOCINQ:
{
struct sk_buff *skb;
unsigned long amount;
amount = 0;
skb = skb_peek(&sk->receive_queue);
if (skb != NULL) {
amount = skb->len - sizeof(struct udphdr);
}
return put_user(amount, (int *)arg);
}
default:
return(-ENOIOCTLCMD);
}
return(0);
}
#endif
#ifndef HAVE_CSUM_COPY_USER
#undef CONFIG_UDP_DELAY_CSUM
#endif
int udp_recvmsg(struct sock *sk, struct msghdr *msg, int len,
int noblock, int flags, int *addr_len)
{
struct sockaddr_in *sin = (struct sockaddr_in *)msg->msg_name;
struct sk_buff *skb;
int copied, err;
if (flags & MSG_ERRQUEUE)
return ip_recv_error(sk, msg, len);
skb = skb_recv_datagram(sk, flags, noblock, &err);
if (!skb)
goto out;
copied = skb->len - sizeof(struct udphdr);
if (copied > len) {
copied = len;
msg->msg_flags |= MSG_TRUNC;
}
#ifndef CONFIG_UDP_DELAY_CSUM
err = skb_copy_datagram_iovec(skb, sizeof(struct udphdr), msg->msg_iov,
copied);
#else
if (skb->ip_summed==CHECKSUM_UNNECESSARY) {
err = skb_copy_datagram_iovec(skb, sizeof(struct udphdr), msg->msg_iov,
copied);
} else if (copied > msg->msg_iov[0].iov_len || (msg->msg_flags&MSG_TRUNC)) {
if ((unsigned short)csum_fold(csum_partial(skb->h.raw, skb->len, skb->csum)))
goto csum_copy_err;
err = skb_copy_datagram_iovec(skb, sizeof(struct udphdr), msg->msg_iov,
copied);
} else {
unsigned int csum;
err = 0;
csum = csum_partial(skb->h.raw, sizeof(struct udphdr), skb->csum);
csum = csum_and_copy_to_user((char*)&skb->h.uh[1], msg->msg_iov[0].iov_base,
copied, csum, &err);
if (err)
goto out_free;
if ((unsigned short)csum_fold(csum))
goto csum_copy_err;
}
#endif
if (err)
goto out_free;
sk->stamp=skb->stamp;
if (sin)
{
if (addr_len)
*addr_len=sizeof(*sin);
sin->sin_family = AF_INET;
sin->sin_port = skb->h.uh->source;
sin->sin_addr.s_addr = skb->nh.iph->saddr;
#ifdef CONFIG_IP_TRANSPARENT_PROXY
if (flags&MSG_PROXY)
{
struct sockaddr_in *sinto =
(struct sockaddr_in *) sin->sin_zero;
sinto->sin_family = AF_INET;
sinto->sin_port = skb->h.uh->dest;
sinto->sin_addr.s_addr = skb->nh.iph->daddr;
}
#endif
}
if (sk->ip_cmsg_flags)
ip_cmsg_recv(msg, skb);
err = copied;
out_free:
skb_free_datagram(sk, skb);
out:
return err;
#ifdef CONFIG_UDP_DELAY_CSUM
csum_copy_err:
udp_statistics.UdpInErrors++;
skb_free_datagram(sk, skb);
return (flags&MSG_DONTWAIT) ? -EAGAIN : -EHOSTUNREACH;
#endif
}
int udp_connect(struct sock *sk, struct sockaddr *uaddr, int addr_len)
{
struct sockaddr_in *usin = (struct sockaddr_in *) uaddr;
struct rtable *rt;
int err;
if (addr_len < sizeof(*usin))
return(-EINVAL);
if (usin->sin_family==AF_UNSPEC)
{
sk->saddr=INADDR_ANY;
sk->rcv_saddr=INADDR_ANY;
sk->daddr=INADDR_ANY;
sk->state = TCP_CLOSE;
if(uh_cache_sk == sk)
uh_cache_sk = NULL;
return 0;
}
if (usin->sin_family && usin->sin_family != AF_INET)
return(-EAFNOSUPPORT);
dst_release(xchg(&sk->dst_cache, NULL));
err = ip_route_connect(&rt, usin->sin_addr.s_addr, sk->saddr,
sk->ip_tos|sk->localroute, sk->bound_dev_if);
if (err)
return err;
if ((rt->rt_flags&RTCF_BROADCAST) && !sk->broadcast) {
ip_rt_put(rt);
return -EACCES;
}
if(!sk->saddr)
sk->saddr = rt->rt_src;
if(!sk->rcv_saddr)
sk->rcv_saddr = rt->rt_src;
sk->daddr = rt->rt_dst;
sk->dport = usin->sin_port;
sk->state = TCP_ESTABLISHED;
if(uh_cache_sk == sk)
uh_cache_sk = NULL;
sk->dst_cache = &rt->u.dst;
return(0);
}
static void udp_close(struct sock *sk, long timeout)
{
sk->state = TCP_CLOSE;
udp_v4_unhash(sk);
sk->dead = 1;
destroy_sock(sk);
}
static int udp_queue_rcv_skb(struct sock * sk, struct sk_buff *skb)
{
#if defined(CONFIG_FILTER) && defined(CONFIG_UDP_DELAY_CSUM)
if (sk->filter && skb->ip_summed != CHECKSUM_UNNECESSARY) {
if ((unsigned short)csum_fold(csum_partial(skb->h.raw, skb->len, skb->csum))) {
udp_statistics.UdpInErrors++;
ip_statistics.IpInDiscards++;
ip_statistics.IpInDelivers--;
kfree_skb(skb);
return -1;
}
skb->ip_summed = CHECKSUM_UNNECESSARY;
}
#endif
if (sock_queue_rcv_skb(sk,skb)<0) {
udp_statistics.UdpInErrors++;
ip_statistics.IpInDiscards++;
ip_statistics.IpInDelivers--;
kfree_skb(skb);
return -1;
}
udp_statistics.UdpInDatagrams++;
return 0;
}
static inline void udp_deliver(struct sock *sk, struct sk_buff *skb)
{
udp_queue_rcv_skb(sk, skb);
}
static int udp_v4_mcast_deliver(struct sk_buff *skb, struct udphdr *uh,
u32 saddr, u32 daddr)
{
struct sock *sk;
int dif;
sk = udp_hash[ntohs(uh->dest) & (UDP_HTABLE_SIZE - 1)];
dif = skb->dev->ifindex;
sk = udp_v4_mcast_next(sk, uh->dest, saddr, uh->source, daddr, dif);
if (sk) {
struct sock *sknext = NULL;
do {
struct sk_buff *skb1 = skb;
sknext = udp_v4_mcast_next(sk->next, uh->dest, saddr,
uh->source, daddr, dif);
if(sknext)
skb1 = skb_clone(skb, GFP_ATOMIC);
if(skb1)
udp_deliver(sk, skb1);
sk = sknext;
} while(sknext);
} else
kfree_skb(skb);
return 0;
}
#ifdef CONFIG_IP_TRANSPARENT_PROXY
int udp_chkaddr(struct sk_buff *skb)
{
struct iphdr *iph = skb->nh.iph;
struct udphdr *uh = (struct udphdr *)(skb->nh.raw + iph->ihl*4);
struct sock *sk;
sk = udp_v4_lookup(iph->saddr, uh->source, iph->daddr, uh->dest, skb->dev->ifindex);
if (!sk)
return 0;
if (sk->rcv_saddr == 0)
return 0;
return 1;
}
#endif
int udp_rcv(struct sk_buff *skb, unsigned short len)
{
struct sock *sk;
struct udphdr *uh;
unsigned short ulen;
struct rtable *rt = (struct rtable*)skb->dst;
u32 saddr = skb->nh.iph->saddr;
u32 daddr = skb->nh.iph->daddr;
uh = skb->h.uh;
__skb_pull(skb, skb->h.raw - skb->data);
ip_statistics.IpInDelivers++;
ulen = ntohs(uh->len);
if (ulen > len || ulen < sizeof(*uh)) {
NETDEBUG(printk(KERN_DEBUG "UDP: short packet: %d/%d\n", ulen, len));
udp_statistics.UdpInErrors++;
kfree_skb(skb);
return(0);
}
skb_trim(skb, ulen);
#ifndef CONFIG_UDP_DELAY_CSUM
if (uh->check &&
(((skb->ip_summed==CHECKSUM_HW)&&udp_check(uh,ulen,saddr,daddr,skb->csum)) ||
((skb->ip_summed==CHECKSUM_NONE) &&
(udp_check(uh,ulen,saddr,daddr, csum_partial((char*)uh, ulen, 0))))))
goto csum_error;
#else
if (uh->check==0)
skb->ip_summed = CHECKSUM_UNNECESSARY;
else if (skb->ip_summed==CHECKSUM_HW) {
if (udp_check(uh,ulen,saddr,daddr,skb->csum))
goto csum_error;
skb->ip_summed = CHECKSUM_UNNECESSARY;
} else if (skb->ip_summed != CHECKSUM_UNNECESSARY)
skb->csum = csum_tcpudp_nofold(saddr, daddr, ulen, IPPROTO_UDP, 0);
#endif
if(rt->rt_flags & (RTCF_BROADCAST|RTCF_MULTICAST))
return udp_v4_mcast_deliver(skb, uh, saddr, daddr);
#ifdef CONFIG_IP_TRANSPARENT_PROXY
if (IPCB(skb)->redirport)
sk = udp_v4_proxy_lookup(uh->dest, saddr, uh->source,
daddr, skb->dev, IPCB(skb)->redirport,
skb->dev->ifindex);
else
#endif
sk = udp_v4_lookup(saddr, uh->source, daddr, uh->dest, skb->dev->ifindex);
if (sk == NULL) {
#ifdef CONFIG_UDP_DELAY_CSUM
if (skb->ip_summed != CHECKSUM_UNNECESSARY &&
(unsigned short)csum_fold(csum_partial((char*)uh, ulen, skb->csum)))
goto csum_error;
#endif
udp_statistics.UdpNoPorts++;
icmp_send(skb, ICMP_DEST_UNREACH, ICMP_PORT_UNREACH, 0);
kfree_skb(skb);
return(0);
}
udp_deliver(sk, skb);
return 0;
csum_error:
NETDEBUG(printk(KERN_DEBUG "UDP: bad checksum. From %d.%d.%d.%d:%d to %d.%d.%d.%d:%d ulen %d\n",
NIPQUAD(saddr),
ntohs(uh->source),
NIPQUAD(daddr),
ntohs(uh->dest),
ulen));
udp_statistics.UdpInErrors++;
kfree_skb(skb);
return(0);
}
struct proto udp_prot = {
(struct sock *)&udp_prot,
(struct sock *)&udp_prot,
udp_close,
udp_connect,
NULL,
NULL,
NULL,
NULL,
datagram_poll,
udp_ioctl,
NULL,
NULL,
NULL,
ip_setsockopt,
ip_getsockopt,
udp_sendmsg,
udp_recvmsg,
NULL,
udp_queue_rcv_skb,
udp_v4_hash,
udp_v4_unhash,
udp_v4_get_port,
128,
0,
"UDP",
0,
0
};