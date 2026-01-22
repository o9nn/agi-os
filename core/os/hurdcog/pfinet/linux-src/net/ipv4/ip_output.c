#include <asm/uaccess.h>
#include <asm/system.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/mm.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/config.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/in.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <linux/proc_fs.h>
#include <linux/stat.h>
#include <linux/init.h>
#include <net/snmp.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <net/route.h>
#include <net/tcp.h>
#include <net/udp.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/arp.h>
#include <net/icmp.h>
#include <net/raw.h>
#include <net/checksum.h>
#include <linux/igmp.h>
#include <linux/ip_fw.h>
#include <linux/firewall.h>
#include <linux/mroute.h>
#include <linux/netlink.h>
int sysctl_ip_dynaddr = 0;
int ip_id_count = 0;
__inline__ void ip_send_check(struct iphdr *iph)
{
iph->check = 0;
iph->check = ip_fast_csum((unsigned char *)iph, iph->ihl);
}
void ip_build_and_send_pkt(struct sk_buff *skb, struct sock *sk,
u32 saddr, u32 daddr, struct ip_options *opt)
{
struct rtable *rt = (struct rtable *)skb->dst;
struct iphdr *iph;
struct device *dev;
if (opt)
iph=(struct iphdr *)skb_push(skb,sizeof(struct iphdr) + opt->optlen);
else
iph=(struct iphdr *)skb_push(skb,sizeof(struct iphdr));
iph->version = 4;
iph->ihl = 5;
iph->tos = sk->ip_tos;
iph->frag_off = 0;
if (ip_dont_fragment(sk, &rt->u.dst))
iph->frag_off |= htons(IP_DF);
iph->ttl = sk->ip_ttl;
iph->daddr = rt->rt_dst;
iph->saddr = rt->rt_src;
iph->protocol = sk->protocol;
iph->tot_len = htons(skb->len);
iph->id = htons(ip_id_count++);
skb->nh.iph = iph;
if (opt && opt->optlen) {
iph->ihl += opt->optlen>>2;
ip_options_build(skb, opt, daddr, rt, 0);
}
dev = rt->u.dst.dev;
#ifdef CONFIG_FIREWALL
switch (call_out_firewall(PF_INET, dev, iph, NULL, &skb)) {
case FW_REJECT:
icmp_send(skb, ICMP_DEST_UNREACH, ICMP_PORT_UNREACH, 0);
case FW_BLOCK:
case FW_QUEUE:
kfree_skb(skb);
return;
}
#endif
ip_send_check(iph);
skb->dst->output(skb);
return;
}
int __ip_finish_output(struct sk_buff *skb)
{
return ip_finish_output(skb);
}
int ip_mc_output(struct sk_buff *skb)
{
struct sock *sk = skb->sk;
struct rtable *rt = (struct rtable*)skb->dst;
struct device *dev = rt->u.dst.dev;
ip_statistics.IpOutRequests++;
#ifdef CONFIG_IP_ROUTE_NAT
if (rt->rt_flags & RTCF_NAT)
ip_do_nat(skb);
#endif
skb->dev = dev;
skb->protocol = __constant_htons(ETH_P_IP);
if (rt->rt_flags&RTCF_MULTICAST && (!sk || sk->ip_mc_loop)) {
#ifdef CONFIG_IP_MROUTE
if ((rt->rt_flags&RTCF_LOCAL) || !(IPCB(skb)->flags&IPSKB_FORWARDED))
#endif
dev_loopback_xmit(skb);
if (skb->nh.iph->ttl == 0) {
kfree_skb(skb);
return 0;
}
}
if (rt->rt_flags&RTCF_BROADCAST)
dev_loopback_xmit(skb);
return ip_finish_output(skb);
}
int ip_output(struct sk_buff *skb)
{
#ifdef CONFIG_IP_ROUTE_NAT
struct rtable *rt = (struct rtable*)skb->dst;
#endif
ip_statistics.IpOutRequests++;
#ifdef CONFIG_IP_ROUTE_NAT
if (rt->rt_flags&RTCF_NAT)
ip_do_nat(skb);
#endif
return ip_finish_output(skb);
}
void ip_queue_xmit(struct sk_buff *skb)
{
struct sock *sk = skb->sk;
struct ip_options *opt = sk->opt;
struct rtable *rt;
struct device *dev;
struct iphdr *iph;
unsigned int tot_len;
rt = (struct rtable *) sk->dst_cache;
if(rt == NULL || rt->u.dst.obsolete) {
u32 daddr;
sk->dst_cache = NULL;
ip_rt_put(rt);
daddr = sk->daddr;
if(opt && opt->srr)
daddr = opt->faddr;
if(ip_route_output(&rt, daddr, sk->saddr,
RT_TOS(sk->ip_tos) | RTO_CONN | sk->localroute,
sk->bound_dev_if))
goto drop;
sk->dst_cache = &rt->u.dst;
}
if(opt && opt->is_strictroute && rt->rt_dst != rt->rt_gateway)
goto no_route;
skb->dst = dst_clone(sk->dst_cache);
iph = (struct iphdr *) skb_push(skb, sizeof(struct iphdr) + (opt ? opt->optlen : 0));
iph->version = 4;
iph->ihl = 5;
iph->tos = sk->ip_tos;
iph->frag_off = 0;
iph->ttl = sk->ip_ttl;
iph->daddr = rt->rt_dst;
iph->saddr = rt->rt_src;
iph->protocol = sk->protocol;
skb->nh.iph = iph;
if(opt && opt->optlen) {
iph->ihl += opt->optlen >> 2;
ip_options_build(skb, opt, sk->daddr, rt, 0);
}
tot_len = skb->len;
iph->tot_len = htons(tot_len);
iph->id = htons(ip_id_count++);
dev = rt->u.dst.dev;
#ifdef CONFIG_FIREWALL
switch (call_out_firewall(PF_INET, dev, iph, NULL, &skb)) {
case FW_REJECT:
start_bh_atomic();
icmp_send(skb, ICMP_DEST_UNREACH, ICMP_PORT_UNREACH, 0);
end_bh_atomic();
case FW_BLOCK:
case FW_QUEUE:
goto drop;
}
#endif
if (skb_headroom(skb) < dev->hard_header_len && dev->hard_header) {
struct sk_buff *skb2;
skb2 = skb_realloc_headroom(skb, (dev->hard_header_len + 15) & ~15);
kfree_skb(skb);
if (skb2 == NULL)
return;
if (sk)
skb_set_owner_w(skb2, sk);
skb = skb2;
iph = skb->nh.iph;
}
if (tot_len > rt->u.dst.pmtu)
goto fragment;
if (ip_dont_fragment(sk, &rt->u.dst))
iph->frag_off |= __constant_htons(IP_DF);
ip_send_check(iph);
skb->priority = sk->priority;
skb->dst->output(skb);
return;
fragment:
if (ip_dont_fragment(sk, &rt->u.dst) &&
tot_len > (iph->ihl<<2) + sizeof(struct tcphdr)+16) {
iph->frag_off |= __constant_htons(IP_DF);
NETDEBUG(printk(KERN_DEBUG "sending pkt_too_big to self\n"));
start_bh_atomic();
icmp_send(skb, ICMP_DEST_UNREACH, ICMP_FRAG_NEEDED,
htonl(rt->u.dst.pmtu));
end_bh_atomic();
goto drop;
}
ip_fragment(skb, skb->dst->output);
return;
no_route:
sk->dst_cache = NULL;
ip_rt_put(rt);
ip_statistics.IpOutNoRoutes++;
drop:
kfree_skb(skb);
}
int ip_build_xmit_slow(struct sock *sk,
int getfrag (const void *,
char *,
unsigned int,
unsigned int),
const void *frag,
unsigned length,
struct ipcm_cookie *ipc,
struct rtable *rt,
int flags)
{
unsigned int fraglen, maxfraglen, fragheaderlen;
int err;
int offset, mf;
int mtu;
unsigned short id;
int hh_len = (rt->u.dst.dev->hard_header_len + 15)&~15;
int nfrags=0;
struct ip_options *opt = ipc->opt;
int df = 0;
mtu = rt->u.dst.pmtu;
if (ip_dont_fragment(sk, &rt->u.dst))
df = htons(IP_DF);
length -= sizeof(struct iphdr);
if (opt) {
fragheaderlen = sizeof(struct iphdr) + opt->optlen;
maxfraglen = ((mtu-sizeof(struct iphdr)-opt->optlen) & ~7) + fragheaderlen;
} else {
fragheaderlen = sizeof(struct iphdr);
maxfraglen = ((mtu-sizeof(struct iphdr)) & ~7) + fragheaderlen;
}
if (length + fragheaderlen > 0xFFFF) {
ip_local_error(sk, EMSGSIZE, rt->rt_dst, sk->dport, mtu);
return -EMSGSIZE;
}
offset = length - (length % (maxfraglen - fragheaderlen));
fraglen = length - offset + fragheaderlen;
if (length-offset==0) {
fraglen = maxfraglen;
offset -= maxfraglen-fragheaderlen;
}
mf = 0;
if (offset > 0 && df) {
ip_local_error(sk, EMSGSIZE, rt->rt_dst, sk->dport, mtu);
return(-EMSGSIZE);
}
dev_lock_list();
id = htons(ip_id_count++);
do {
char *data;
struct sk_buff * skb;
skb = sock_alloc_send_skb(sk, fraglen+hh_len+15, 0, flags&MSG_DONTWAIT, &err);
if (skb == NULL)
goto error;
skb->priority = sk->priority;
skb->dst = dst_clone(&rt->u.dst);
skb_reserve(skb, hh_len);
data = skb_put(skb, fraglen);
skb->nh.iph = (struct iphdr *)data;
{
struct iphdr *iph = (struct iphdr *)data;
iph->version = 4;
iph->ihl = 5;
if (opt) {
iph->ihl += opt->optlen>>2;
ip_options_build(skb, opt,
ipc->addr, rt, offset);
}
iph->tos = sk->ip_tos;
iph->tot_len = htons(fraglen - fragheaderlen + iph->ihl*4);
iph->id = id;
iph->frag_off = htons(offset>>3);
iph->frag_off |= mf|df;
if (rt->rt_type == RTN_MULTICAST)
iph->ttl = sk->ip_mc_ttl;
else
iph->ttl = sk->ip_ttl;
iph->protocol = sk->protocol;
iph->check = 0;
iph->saddr = rt->rt_src;
iph->daddr = rt->rt_dst;
iph->check = ip_fast_csum((unsigned char *)iph, iph->ihl);
data += iph->ihl*4;
mf = htons(IP_MF);
}
if (getfrag(frag, data, offset, fraglen-fragheaderlen)) {
err = -EFAULT;
kfree_skb(skb);
goto error;
}
offset -= (maxfraglen-fragheaderlen);
fraglen = maxfraglen;
nfrags++;
#ifdef CONFIG_FIREWALL
switch (call_out_firewall(PF_INET, rt->u.dst.dev, skb->nh.iph, NULL, &skb)) {
case FW_QUEUE:
kfree_skb(skb);
continue;
case FW_BLOCK:
case FW_REJECT:
kfree_skb(skb);
err = -EPERM;
goto error;
}
#endif
err = -ENETDOWN;
if (rt->u.dst.output(skb))
goto error;
} while (offset >= 0);
if (nfrags>1)
ip_statistics.IpFragCreates += nfrags;
dev_unlock_list();
return 0;
error:
ip_statistics.IpOutDiscards++;
if (nfrags>1)
ip_statistics.IpFragCreates += nfrags;
dev_unlock_list();
return err;
}
int ip_build_xmit(struct sock *sk,
int getfrag (const void *,
char *,
unsigned int,
unsigned int),
const void *frag,
unsigned length,
struct ipcm_cookie *ipc,
struct rtable *rt,
int flags)
{
int err;
struct sk_buff *skb;
int df;
struct iphdr *iph;
if (!sk->ip_hdrincl) {
length += sizeof(struct iphdr);
if (length > rt->u.dst.pmtu || ipc->opt != NULL)
return ip_build_xmit_slow(sk,getfrag,frag,length,ipc,rt,flags);
} else {
if (length > rt->u.dst.dev->mtu) {
ip_local_error(sk, EMSGSIZE, rt->rt_dst, sk->dport, rt->u.dst.dev->mtu);
return -EMSGSIZE;
}
}
df = 0;
if (ip_dont_fragment(sk, &rt->u.dst))
df = htons(IP_DF);
{
int hh_len = (rt->u.dst.dev->hard_header_len + 15)&~15;
skb = sock_alloc_send_skb(sk, length+hh_len+15,
0, flags&MSG_DONTWAIT, &err);
if(skb==NULL)
goto error;
skb_reserve(skb, hh_len);
}
skb->priority = sk->priority;
skb->dst = dst_clone(&rt->u.dst);
skb->nh.iph = iph = (struct iphdr *)skb_put(skb, length);
dev_lock_list();
if(!sk->ip_hdrincl) {
iph->version=4;
iph->ihl=5;
iph->tos=sk->ip_tos;
iph->tot_len = htons(length);
iph->id=htons(ip_id_count++);
iph->frag_off = df;
iph->ttl=sk->ip_mc_ttl;
if (rt->rt_type != RTN_MULTICAST)
iph->ttl=sk->ip_ttl;
iph->protocol=sk->protocol;
iph->saddr=rt->rt_src;
iph->daddr=rt->rt_dst;
iph->check=0;
iph->check = ip_fast_csum((unsigned char *)iph, iph->ihl);
err = getfrag(frag, ((char *)iph)+iph->ihl*4,0, length-iph->ihl*4);
}
else
err = getfrag(frag, (void *)iph, 0, length);
dev_unlock_list();
if (err)
goto error_fault;
#ifdef CONFIG_FIREWALL
switch (call_out_firewall(PF_INET, rt->u.dst.dev, iph, NULL, &skb)) {
case FW_QUEUE:
kfree_skb(skb);
return 0;
case FW_BLOCK:
case FW_REJECT:
kfree_skb(skb);
err = -EPERM;
goto error;
}
#endif
return rt->u.dst.output(skb);
error_fault:
err = -EFAULT;
kfree_skb(skb);
error:
ip_statistics.IpOutDiscards++;
return err;
}
void ip_fragment(struct sk_buff *skb, int (*output)(struct sk_buff*))
{
struct iphdr *iph;
unsigned char *raw;
unsigned char *ptr;
struct device *dev;
struct sk_buff *skb2;
unsigned int mtu, hlen, left, len;
int offset;
int not_last_frag;
struct rtable *rt = (struct rtable*)skb->dst;
dev = rt->u.dst.dev;
raw = skb->nh.raw;
iph = (struct iphdr*)raw;
hlen = iph->ihl * 4;
left = ntohs(iph->tot_len) - hlen;
mtu = rt->u.dst.pmtu - hlen;
ptr = raw + hlen;
#ifdef CONFIG_NET_PARANOIA
if (mtu<8)
goto fail;
#endif
offset = (ntohs(iph->frag_off) & IP_OFFSET) << 3;
not_last_frag = iph->frag_off & htons(IP_MF);
while(left > 0) {
len = left;
if (len > mtu)
len = mtu;
if (len < left) {
len &= ~7;
}
if ((skb2 = alloc_skb(len+hlen+dev->hard_header_len+15,GFP_ATOMIC)) == NULL) {
NETDEBUG(printk(KERN_INFO "IP: frag: no memory for new fragment!\n"));
goto fail;
}
skb2->pkt_type = skb->pkt_type;
skb2->priority = skb->priority;
skb_reserve(skb2, (dev->hard_header_len+15)&~15);
skb_put(skb2, len + hlen);
skb2->nh.raw = skb2->data;
skb2->h.raw = skb2->data + hlen;
if (skb->sk)
skb_set_owner_w(skb2, skb->sk);
skb2->dst = dst_clone(skb->dst);
memcpy(skb2->nh.raw, raw, hlen);
memcpy(skb2->h.raw, ptr, len);
left -= len;
iph = skb2->nh.iph;
iph->frag_off = htons((offset >> 3));
if (offset == 0)
ip_options_fragment(skb);
if (left > 0 || not_last_frag)
iph->frag_off |= htons(IP_MF);
ptr += len;
offset += len;
ip_statistics.IpFragCreates++;
iph->tot_len = htons(len + hlen);
ip_send_check(iph);
output(skb2);
}
kfree_skb(skb);
ip_statistics.IpFragOKs++;
return;
fail:
kfree_skb(skb);
ip_statistics.IpFragFails++;
}
static int ip_reply_glue_bits(const void *dptr, char *to, unsigned int offset,
unsigned int fraglen)
{
struct ip_reply_arg *dp = (struct ip_reply_arg*)dptr;
u16 *pktp = (u16 *)to;
struct iovec *iov;
int len;
int hdrflag = 1;
iov = &dp->iov[0];
if (offset >= iov->iov_len) {
offset -= iov->iov_len;
iov++;
hdrflag = 0;
}
len = iov->iov_len - offset;
if (fraglen > len) {
dp->csum = csum_partial_copy_nocheck(iov->iov_base+offset, to, len,
dp->csum);
offset = 0;
fraglen -= len;
to += len;
iov++;
}
dp->csum = csum_partial_copy_nocheck(iov->iov_base+offset, to, fraglen,
dp->csum);
if (hdrflag && dp->csumoffset)
*(pktp + dp->csumoffset) = csum_fold(dp->csum);
return 0;
}
void ip_send_reply(struct sock *sk, struct sk_buff *skb, struct ip_reply_arg *arg,
unsigned int len)
{
struct {
struct ip_options opt;
char data[40];
} replyopts;
struct ipcm_cookie ipc;
u32 daddr;
struct rtable *rt = (struct rtable*)skb->dst;
if (ip_options_echo(&replyopts.opt, skb))
return;
sk->ip_tos = skb->nh.iph->tos;
sk->priority = skb->priority;
sk->protocol = skb->nh.iph->protocol;
daddr = ipc.addr = rt->rt_src;
ipc.opt = &replyopts.opt;
if (ipc.opt->srr)
daddr = replyopts.opt.faddr;
if (ip_route_output(&rt, daddr, rt->rt_spec_dst, RT_TOS(skb->nh.iph->tos), 0))
return;
ip_build_xmit(sk, ip_reply_glue_bits, arg, len, &ipc, rt, MSG_DONTWAIT);
ip_rt_put(rt);
}
static struct packet_type ip_packet_type =
{
__constant_htons(ETH_P_IP),
NULL,
ip_rcv,
NULL,
NULL,
};
#ifdef CONFIG_PROC_FS
#ifdef CONFIG_IP_MULTICAST
static struct proc_dir_entry proc_net_igmp = {
PROC_NET_IGMP, 4, "igmp",
S_IFREG | S_IRUGO, 1, 0, 0,
0, &proc_net_inode_operations,
ip_mc_procinfo
};
#endif
#endif
__initfunc(void ip_init(void))
{
dev_add_pack(&ip_packet_type);
ip_rt_init();
#ifdef CONFIG_PROC_FS
#ifdef CONFIG_IP_MULTICAST
proc_net_register(&proc_net_igmp);
#endif
#endif
}