#include <linux/config.h>
#include <asm/system.h>
#include <asm/uaccess.h>
#include <linux/types.h>
#include <linux/sched.h>
#include <linux/errno.h>
#include <linux/timer.h>
#include <linux/mm.h>
#include <linux/kernel.h>
#include <linux/fcntl.h>
#include <linux/socket.h>
#include <linux/in.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/mroute.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/icmp.h>
#include <net/udp.h>
#include <net/raw.h>
#include <net/checksum.h>
#ifdef CONFIG_IP_MROUTE
struct sock *mroute_socket=NULL;
#endif
struct sock *raw_v4_htable[RAWV4_HTABLE_SIZE];
static void raw_v4_hash(struct sock *sk)
{
struct sock **skp = &raw_v4_htable[sk->num & (RAWV4_HTABLE_SIZE - 1)];
SOCKHASH_LOCK();
if ((sk->next = *skp) != NULL)
(*skp)->pprev = &sk->next;
*skp = sk;
sk->pprev = skp;
SOCKHASH_UNLOCK();
}
static void raw_v4_unhash(struct sock *sk)
{
SOCKHASH_LOCK();
if (sk->pprev) {
if (sk->next)
sk->next->pprev = sk->pprev;
*sk->pprev = sk->next;
sk->pprev = NULL;
}
SOCKHASH_UNLOCK();
}
struct sock *raw_v4_lookup(struct sock *sk, unsigned short num,
unsigned long raddr, unsigned long laddr, int dif)
{
struct sock *s = sk;
SOCKHASH_LOCK();
for(s = sk; s; s = s->next) {
if((s->num == num) &&
!(s->dead && (s->state == TCP_CLOSE)) &&
!(s->daddr && s->daddr != raddr) &&
!(s->rcv_saddr && s->rcv_saddr != laddr) &&
!(s->bound_dev_if && s->bound_dev_if != dif))
break;
}
SOCKHASH_UNLOCK();
return s;
}
void raw_err (struct sock *sk, struct sk_buff *skb)
{
int type = skb->h.icmph->type;
int code = skb->h.icmph->code;
u32 info = 0;
int err = 0;
int harderr = 0;
if (!sk->ip_recverr && sk->state != TCP_ESTABLISHED)
return;
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
err = EHOSTUNREACH;
if (code > NR_ICMP_UNREACH)
break;
err = icmp_err_convert[code].errno;
harderr = icmp_err_convert[code].fatal;
if (code == ICMP_FRAG_NEEDED) {
harderr = (sk->ip_pmtudisc != IP_PMTUDISC_DONT);
err = EMSGSIZE;
info = ntohs(skb->h.icmph->un.frag.mtu);
}
}
if (sk->ip_recverr)
ip_icmp_error(sk, skb, err, 0, info, (u8 *)(skb->h.icmph + 1));
if (sk->ip_recverr || harderr) {
sk->err = err;
sk->error_report(sk);
}
}
static int raw_rcv_skb(struct sock * sk, struct sk_buff * skb)
{
if (sock_queue_rcv_skb(sk,skb)<0)
{
ip_statistics.IpInDiscards++;
kfree_skb(skb);
return -1;
}
ip_statistics.IpInDelivers++;
return 0;
}
int raw_rcv(struct sock *sk, struct sk_buff *skb)
{
skb_trim(skb, ntohs(skb->nh.iph->tot_len));
skb->h.raw = skb->nh.raw;
raw_rcv_skb(sk, skb);
return 0;
}
struct rawfakehdr
{
struct iovec *iov;
u32 saddr;
};
static int raw_getfrag(const void *p, char *to, unsigned int offset, unsigned int fraglen)
{
struct rawfakehdr *rfh = (struct rawfakehdr *) p;
return memcpy_fromiovecend(to, rfh->iov, offset, fraglen);
}
static int raw_getrawfrag(const void *p, char *to, unsigned int offset, unsigned int fraglen)
{
struct rawfakehdr *rfh = (struct rawfakehdr *) p;
if (memcpy_fromiovecend(to, rfh->iov, offset, fraglen))
return -EFAULT;
if (offset==0) {
struct iphdr *iph = (struct iphdr *)to;
if (!iph->saddr)
iph->saddr = rfh->saddr;
iph->check=0;
iph->tot_len=htons(fraglen);
if (!iph->id)
iph->id = htons(ip_id_count++);
iph->check=ip_fast_csum((unsigned char *)iph, iph->ihl);
}
return 0;
}
static int raw_sendmsg(struct sock *sk, struct msghdr *msg, int len)
{
struct ipcm_cookie ipc;
struct rawfakehdr rfh;
struct rtable *rt = NULL;
int free = 0;
u32 daddr;
u8 tos;
int err;
if (len < 0 || len > 0xFFFF)
return -EMSGSIZE;
if (msg->msg_flags & MSG_OOB)
return -EOPNOTSUPP;
if (msg->msg_flags & ~(MSG_DONTROUTE|MSG_DONTWAIT))
return(-EINVAL);
if (msg->msg_namelen) {
struct sockaddr_in *usin = (struct sockaddr_in*)msg->msg_name;
if (msg->msg_namelen < sizeof(*usin))
return(-EINVAL);
if (usin->sin_family != AF_INET) {
static int complained;
if (!complained++)
printk(KERN_INFO "%s forgot to set AF_INET in raw sendmsg. Fix it!\n", current->comm);
if (usin->sin_family)
return -EINVAL;
}
daddr = usin->sin_addr.s_addr;
} else {
if (sk->state != TCP_ESTABLISHED)
return(-EINVAL);
daddr = sk->daddr;
}
ipc.addr = sk->saddr;
ipc.opt = NULL;
ipc.oif = sk->bound_dev_if;
if (msg->msg_controllen) {
int tmp = ip_cmsg_send(msg, &ipc);
if (tmp)
return tmp;
if (ipc.opt)
free=1;
}
rfh.saddr = ipc.addr;
ipc.addr = daddr;
if (!ipc.opt)
ipc.opt = sk->opt;
if (ipc.opt) {
err = -EINVAL;
if (sk->ip_hdrincl)
goto done;
if (ipc.opt->srr) {
if (!daddr)
goto done;
daddr = ipc.opt->faddr;
}
}
tos = RT_TOS(sk->ip_tos) | sk->localroute;
if (msg->msg_flags&MSG_DONTROUTE)
tos |= RTO_ONLINK;
if (MULTICAST(daddr)) {
if (!ipc.oif)
ipc.oif = sk->ip_mc_index;
if (!rfh.saddr)
rfh.saddr = sk->ip_mc_addr;
}
err = ip_route_output(&rt, daddr, rfh.saddr, tos, ipc.oif);
if (err)
goto done;
err = -EACCES;
if (rt->rt_flags&RTCF_BROADCAST && !sk->broadcast)
goto done;
rfh.iov = msg->msg_iov;
rfh.saddr = rt->rt_src;
if (!ipc.addr)
ipc.addr = rt->rt_dst;
err=ip_build_xmit(sk, sk->ip_hdrincl ? raw_getrawfrag : raw_getfrag,
&rfh, len, &ipc, rt, msg->msg_flags);
done:
if (free)
kfree(ipc.opt);
ip_rt_put(rt);
return err<0 ? err : len;
}
static void raw_close(struct sock *sk, long timeout)
{
sk->state = TCP_CLOSE;
raw_v4_unhash(sk);
ip_ra_control(sk, 0, NULL);
sk->dead=1;
destroy_sock(sk);
}
static int raw_bind(struct sock *sk, struct sockaddr *uaddr, int addr_len)
{
struct sockaddr_in *addr = (struct sockaddr_in *) uaddr;
int chk_addr_ret;
if((sk->state != TCP_CLOSE) || (addr_len < sizeof(struct sockaddr_in)))
return -EINVAL;
chk_addr_ret = inet_addr_type(addr->sin_addr.s_addr);
if(addr->sin_addr.s_addr != 0 && chk_addr_ret != RTN_LOCAL &&
chk_addr_ret != RTN_MULTICAST && chk_addr_ret != RTN_BROADCAST) {
#ifdef CONFIG_IP_TRANSPARENT_PROXY
if(chk_addr_ret != RTN_UNICAST || !capable(CAP_NET_ADMIN))
#endif
return -EADDRNOTAVAIL;
}
sk->rcv_saddr = sk->saddr = addr->sin_addr.s_addr;
if(chk_addr_ret == RTN_MULTICAST || chk_addr_ret == RTN_BROADCAST)
sk->saddr = 0;
dst_release(xchg(&sk->dst_cache, NULL));
return 0;
}
int raw_recvmsg(struct sock *sk, struct msghdr *msg, int len,
int noblock, int flags,int *addr_len)
{
int copied=0;
struct sk_buff *skb;
int err;
struct sockaddr_in *sin=(struct sockaddr_in *)msg->msg_name;
if (flags & MSG_OOB)
return -EOPNOTSUPP;
if (addr_len)
*addr_len=sizeof(*sin);
if (flags & MSG_ERRQUEUE)
return ip_recv_error(sk, msg, len);
skb=skb_recv_datagram(sk,flags,noblock,&err);
if(skb==NULL)
return err;
copied = skb->len;
if (len < copied)
{
msg->msg_flags |= MSG_TRUNC;
copied = len;
}
err = skb_copy_datagram_iovec(skb, 0, msg->msg_iov, copied);
if (err)
goto done;
sk->stamp=skb->stamp;
if (sin) {
sin->sin_family = AF_INET;
sin->sin_addr.s_addr = skb->nh.iph->saddr;
}
if (sk->ip_cmsg_flags)
ip_cmsg_recv(msg, skb);
done:
skb_free_datagram(sk, skb);
return (err ? : copied);
}
static int raw_init(struct sock *sk)
{
struct raw_opt *tp = &(sk->tp_pinfo.tp_raw4);
if (sk->num == IPPROTO_ICMP)
memset(&tp->filter, 0, sizeof(tp->filter));
return 0;
}
static int raw_seticmpfilter(struct sock *sk, char *optval, int optlen)
{
if (optlen > sizeof(struct icmp_filter))
optlen = sizeof(struct icmp_filter);
if (copy_from_user(&sk->tp_pinfo.tp_raw4.filter, optval, optlen))
return -EFAULT;
return 0;
}
static int raw_geticmpfilter(struct sock *sk, char *optval, int *optlen)
{
int len;
if (get_user(len,optlen))
return -EFAULT;
if (len > sizeof(struct icmp_filter))
len = sizeof(struct icmp_filter);
if (put_user(len, optlen))
return -EFAULT;
if (copy_to_user(optval, &sk->tp_pinfo.tp_raw4.filter, len))
return -EFAULT;
return 0;
}
static int raw_setsockopt(struct sock *sk, int level, int optname,
char *optval, int optlen)
{
if (level != SOL_RAW)
return ip_setsockopt(sk, level, optname, optval, optlen);
switch (optname) {
case ICMP_FILTER:
if (sk->num != IPPROTO_ICMP)
return -EOPNOTSUPP;
return raw_seticmpfilter(sk, optval, optlen);
};
return -ENOPROTOOPT;
}
static int raw_getsockopt(struct sock *sk, int level, int optname,
char *optval, int *optlen)
{
if (level != SOL_RAW)
return ip_getsockopt(sk, level, optname, optval, optlen);
switch (optname) {
case ICMP_FILTER:
if (sk->num != IPPROTO_ICMP)
return -EOPNOTSUPP;
return raw_geticmpfilter(sk, optval, optlen);
};
return -ENOPROTOOPT;
}
struct proto raw_prot = {
(struct sock *)&raw_prot,
(struct sock *)&raw_prot,
raw_close,
udp_connect,
NULL,
NULL,
NULL,
NULL,
datagram_poll,
#ifdef CONFIG_IP_MROUTE
ipmr_ioctl,
#else
NULL,
#endif
raw_init,
NULL,
NULL,
raw_setsockopt,
raw_getsockopt,
raw_sendmsg,
raw_recvmsg,
raw_bind,
raw_rcv_skb,
raw_v4_hash,
raw_v4_unhash,
NULL,
128,
0,
"RAW",
0,
0
};