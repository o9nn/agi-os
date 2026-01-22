#include <linux/errno.h>
#include <linux/types.h>
#include <linux/socket.h>
#include <linux/net.h>
#include <linux/netdevice.h>
#include <linux/if_arp.h>
#include <linux/in6.h>
#include <linux/route.h>
#include <net/sock.h>
#include <net/snmp.h>
#include <net/ipv6.h>
#include <net/ndisc.h>
#include <net/protocol.h>
#include <net/ip6_route.h>
#include <net/addrconf.h>
#include <net/rawv6.h>
#include <net/icmp.h>
static u32	ipv6_fragmentation_id = 1;
int ip6_output(struct sk_buff *skb)
{
struct dst_entry *dst = skb->dst;
struct device *dev = dst->dev;
struct hh_cache *hh = dst->hh;
skb->protocol = __constant_htons(ETH_P_IPV6);
skb->dev = dev;
if (ipv6_addr_is_multicast(&skb->nh.ipv6h->daddr)) {
if (!(dev->flags&IFF_LOOPBACK) &&
(skb->sk == NULL || skb->sk->net_pinfo.af_inet6.mc_loop) &&
ipv6_chk_mcast_addr(dev, &skb->nh.ipv6h->daddr)) {
dev_loopback_xmit(skb);
if (skb->nh.ipv6h->hop_limit == 0) {
kfree_skb(skb);
return 0;
}
}
ipv6_statistics.Ip6OutMcastPkts++;
}
if (hh) {
#ifdef __alpha__
u64 *aligned_hdr = (u64*)(skb->data - 16);
u64 *aligned_hdr0 = hh->hh_data;
read_lock_irq(&hh->hh_lock);
aligned_hdr[0] = aligned_hdr0[0];
aligned_hdr[1] = aligned_hdr0[1];
#else
read_lock_irq(&hh->hh_lock);
memcpy(skb->data - 16, hh->hh_data, 16);
#endif
read_unlock_irq(&hh->hh_lock);
skb_push(skb, dev->hard_header_len);
return hh->hh_output(skb);
} else if (dst->neighbour)
return dst->neighbour->output(skb);
kfree_skb(skb);
return -EINVAL;
}
int ip6_xmit(struct sock *sk, struct sk_buff *skb, struct flowi *fl,
struct ipv6_txoptions *opt)
{
struct ipv6_pinfo * np = sk ? &sk->net_pinfo.af_inet6 : NULL;
struct in6_addr *first_hop = fl->nl_u.ip6_u.daddr;
struct dst_entry *dst = skb->dst;
struct ipv6hdr *hdr;
u8  proto = fl->proto;
int seg_len = skb->len;
int hlimit;
if (opt) {
int head_room;
head_room = opt->opt_nflen + opt->opt_flen;
seg_len += head_room;
head_room += sizeof(struct ipv6hdr) + ((dst->dev->hard_header_len + 15)&~15);
if (skb_headroom(skb) < head_room) {
struct sk_buff *skb2 = skb_realloc_headroom(skb, head_room);
kfree(skb);
skb = skb2;
if (skb == NULL)
return -ENOBUFS;
if (sk)
skb_set_owner_w(skb, sk);
}
if (opt->opt_flen)
ipv6_push_frag_opts(skb, opt, &proto);
if (opt->opt_nflen)
ipv6_push_nfrag_opts(skb, opt, &proto, &first_hop);
}
hdr = skb->nh.ipv6h = (struct ipv6hdr*)skb_push(skb, sizeof(struct ipv6hdr));
*(u32*)hdr = __constant_htonl(0x60000000) | fl->fl6_flowlabel;
hlimit = -1;
if (np)
hlimit = np->hop_limit;
if (hlimit < 0)
hlimit = ((struct rt6_info*)dst)->rt6i_hoplimit;
hdr->payload_len = htons(seg_len);
hdr->nexthdr = proto;
hdr->hop_limit = hlimit;
ipv6_addr_copy(&hdr->saddr, fl->nl_u.ip6_u.saddr);
ipv6_addr_copy(&hdr->daddr, first_hop);
if (skb->len <= dst->pmtu) {
ipv6_statistics.Ip6OutRequests++;
dst->output(skb);
return 0;
}
printk(KERN_DEBUG "IPv6: sending pkt_too_big to self\n");
start_bh_atomic();
icmpv6_send(skb, ICMPV6_PKT_TOOBIG, 0, dst->pmtu, skb->dev);
end_bh_atomic();
kfree_skb(skb);
return -EMSGSIZE;
}
int ip6_nd_hdr(struct sock *sk, struct sk_buff *skb, struct device *dev,
struct in6_addr *saddr, struct in6_addr *daddr,
int proto, int len)
{
struct ipv6_pinfo *np = &sk->net_pinfo.af_inet6;
struct ipv6hdr *hdr;
int totlen;
skb->protocol = __constant_htons(ETH_P_IPV6);
skb->dev = dev;
totlen = len + sizeof(struct ipv6hdr);
hdr = (struct ipv6hdr *) skb_put(skb, sizeof(struct ipv6hdr));
skb->nh.ipv6h = hdr;
*(u32*)hdr = htonl(0x60000000);
hdr->payload_len = htons(len);
hdr->nexthdr = proto;
hdr->hop_limit = np->hop_limit;
ipv6_addr_copy(&hdr->saddr, saddr);
ipv6_addr_copy(&hdr->daddr, daddr);
return 0;
}
static struct ipv6hdr * ip6_bld_1(struct sock *sk, struct sk_buff *skb, struct flowi *fl,
int hlimit, unsigned pktlength)
{
struct ipv6hdr *hdr;
skb->nh.raw = skb_put(skb, sizeof(struct ipv6hdr));
hdr = skb->nh.ipv6h;
*(u32*)hdr = fl->fl6_flowlabel | htonl(0x60000000);
hdr->payload_len = htons(pktlength - sizeof(struct ipv6hdr));
hdr->hop_limit = hlimit;
hdr->nexthdr = fl->proto;
ipv6_addr_copy(&hdr->saddr, fl->nl_u.ip6_u.saddr);
ipv6_addr_copy(&hdr->daddr, fl->nl_u.ip6_u.daddr);
return hdr;
}
static __inline__ u8 * ipv6_build_fraghdr(struct sk_buff *skb, u8* prev_hdr, unsigned offset)
{
struct frag_hdr *fhdr;
fhdr = (struct frag_hdr *) skb_put(skb, sizeof(struct frag_hdr));
fhdr->nexthdr  = *prev_hdr;
*prev_hdr = NEXTHDR_FRAGMENT;
prev_hdr = &fhdr->nexthdr;
fhdr->reserved = 0;
fhdr->frag_off = htons(offset);
fhdr->identification = ipv6_fragmentation_id++;
return &fhdr->nexthdr;
}
static int ip6_frag_xmit(struct sock *sk, inet_getfrag_t getfrag,
const void *data, struct dst_entry *dst,
struct flowi *fl, struct ipv6_txoptions *opt,
struct in6_addr *final_dst,
int hlimit, int flags, unsigned length, int mtu)
{
struct ipv6hdr *hdr;
struct sk_buff *last_skb;
u8 *prev_hdr;
int unfrag_len;
int frag_len;
int last_len;
int nfrags;
int fhdr_dist;
int frag_off;
int data_off;
int err;
unfrag_len = sizeof(struct ipv6hdr) + sizeof(struct frag_hdr);
last_len = length;
if (opt) {
unfrag_len += opt->opt_nflen;
last_len += opt->opt_flen;
}
frag_len = (mtu - unfrag_len) & ~0x7;
if (frag_len <= 0) {
ipv6_local_error(sk, EMSGSIZE, fl, mtu);
return -EMSGSIZE;
}
nfrags = last_len / frag_len;
frag_off = nfrags * frag_len;
last_len -= frag_off;
if (last_len == 0) {
last_len = frag_len;
frag_off -= frag_len;
nfrags--;
}
data_off = frag_off;
if (opt) {
if (frag_len < opt->opt_flen) {
ipv6_local_error(sk, EMSGSIZE, fl, mtu);
return -EMSGSIZE;
}
data_off = frag_off - opt->opt_flen;
}
last_skb = sock_alloc_send_skb(sk, unfrag_len + frag_len +
dst->dev->hard_header_len + 15,
0, flags & MSG_DONTWAIT, &err);
if (last_skb == NULL)
return err;
last_skb->dst = dst_clone(dst);
skb_reserve(last_skb, (dst->dev->hard_header_len + 15) & ~15);
hdr = ip6_bld_1(sk, last_skb, fl, hlimit, frag_len+unfrag_len);
prev_hdr = &hdr->nexthdr;
if (opt && opt->opt_nflen)
prev_hdr = ipv6_build_nfrag_opts(last_skb, prev_hdr, opt, final_dst, 0);
prev_hdr = ipv6_build_fraghdr(last_skb, prev_hdr, frag_off);
fhdr_dist = prev_hdr - last_skb->data;
err = getfrag(data, &hdr->saddr, last_skb->tail, data_off, last_len);
if (!err) {
while (nfrags--) {
struct sk_buff *skb;
struct frag_hdr *fhdr2;
skb = skb_copy(last_skb, sk->allocation);
if (skb == NULL) {
ipv6_statistics.Ip6FragFails++;
kfree_skb(last_skb);
return -ENOMEM;
}
frag_off -= frag_len;
data_off -= frag_len;
fhdr2 = (struct frag_hdr *) (skb->data + fhdr_dist);
fhdr2->frag_off = htons(frag_off | 1);
if (nfrags == 0 && opt && opt->opt_flen) {
ipv6_build_frag_opts(skb, &fhdr2->nexthdr, opt);
frag_len -= opt->opt_flen;
data_off = 0;
}
err = getfrag(data, &hdr->saddr,skb_put(skb, frag_len),
data_off, frag_len);
if (err) {
kfree_skb(skb);
break;
}
ipv6_statistics.Ip6FragCreates++;
ipv6_statistics.Ip6OutRequests++;
dst->output(skb);
}
}
if (err) {
ipv6_statistics.Ip6FragFails++;
kfree_skb(last_skb);
return -EFAULT;
}
hdr->payload_len = htons(unfrag_len + last_len - sizeof(struct ipv6hdr));
skb_put(last_skb, last_len);
ipv6_statistics.Ip6FragCreates++;
ipv6_statistics.Ip6FragOKs++;
ipv6_statistics.Ip6OutRequests++;
dst->output(last_skb);
return 0;
}
int ip6_build_xmit(struct sock *sk, inet_getfrag_t getfrag, const void *data,
struct flowi *fl, unsigned length,
struct ipv6_txoptions *opt, int hlimit, int flags)
{
struct ipv6_pinfo *np = &sk->net_pinfo.af_inet6;
struct in6_addr *final_dst = NULL;
struct dst_entry *dst;
int err = 0;
unsigned int pktlength, jumbolen, mtu;
struct in6_addr saddr;
if (opt && opt->srcrt) {
struct rt0_hdr *rt0 = (struct rt0_hdr *) opt->srcrt;
final_dst = fl->fl6_dst;
fl->fl6_dst = rt0->addr;
}
if (!fl->oif && ipv6_addr_is_multicast(fl->nl_u.ip6_u.daddr))
fl->oif = np->mcast_oif;
dst = NULL;
if (sk->dst_cache) {
dst = dst_check(&sk->dst_cache, np->dst_cookie);
if (dst) {
struct rt6_info *rt = (struct rt6_info*)dst_clone(dst);
if (((rt->rt6i_dst.plen != 128 ||
ipv6_addr_cmp(fl->fl6_dst, &rt->rt6i_dst.addr))
&& (np->daddr_cache == NULL ||
ipv6_addr_cmp(fl->fl6_dst, np->daddr_cache)))
|| (fl->oif && fl->oif != dst->dev->ifindex)) {
dst_release(dst);
dst = NULL;
}
}
}
if (dst == NULL)
dst = ip6_route_output(sk, fl);
if (dst->error) {
ipv6_statistics.Ip6OutNoRoutes++;
dst_release(dst);
return -ENETUNREACH;
}
if (fl->fl6_src == NULL) {
err = ipv6_get_saddr(dst, fl->fl6_dst, &saddr);
if (err) {
#if IP6_DEBUG >= 2
printk(KERN_DEBUG "ip6_build_xmit: "
"no available source address\n");
#endif
goto out;
}
fl->fl6_src = &saddr;
}
pktlength = length;
if (hlimit < 0) {
if (ipv6_addr_is_multicast(fl->fl6_dst))
hlimit = np->mcast_hops;
else
hlimit = np->hop_limit;
if (hlimit < 0)
hlimit = ((struct rt6_info*)dst)->rt6i_hoplimit;
}
jumbolen = 0;
if (!sk->ip_hdrincl) {
pktlength += sizeof(struct ipv6hdr);
if (opt)
pktlength += opt->opt_flen + opt->opt_nflen;
if (pktlength > 0xFFFF + sizeof(struct ipv6hdr)) {
pktlength += 8;
jumbolen = pktlength - sizeof(struct ipv6hdr);
}
}
mtu = dst->pmtu;
if (np->frag_size < mtu) {
if (np->frag_size)
mtu = np->frag_size;
else if (np->pmtudisc == IPV6_PMTUDISC_DONT)
mtu = IPV6_MIN_MTU;
}
if (pktlength < length) {
ipv6_local_error(sk, EMSGSIZE, fl, mtu);
err = -EMSGSIZE;
goto out;
}
if (pktlength <= mtu) {
struct sk_buff *skb;
struct ipv6hdr *hdr;
struct device *dev = dst->dev;
skb = sock_alloc_send_skb(sk, pktlength + 15 +
dev->hard_header_len, 0,
flags & MSG_DONTWAIT, &err);
if (skb == NULL) {
ipv6_statistics.Ip6OutDiscards++;
goto out;
}
skb->dst = dst_clone(dst);
skb_reserve(skb, (dev->hard_header_len + 15) & ~15);
hdr = (struct ipv6hdr *) skb->tail;
skb->nh.ipv6h = hdr;
if (!sk->ip_hdrincl) {
ip6_bld_1(sk, skb, fl, hlimit,
jumbolen ? sizeof(struct ipv6hdr) : pktlength);
if (opt || jumbolen) {
u8 *prev_hdr = &hdr->nexthdr;
prev_hdr = ipv6_build_nfrag_opts(skb, prev_hdr, opt, final_dst, jumbolen);
if (opt && opt->opt_flen)
ipv6_build_frag_opts(skb, prev_hdr, opt);
}
}
skb_put(skb, length);
err = getfrag(data, &hdr->saddr,
((char *) hdr) + (pktlength - length),
0, length);
if (!err) {
ipv6_statistics.Ip6OutRequests++;
dst->output(skb);
} else {
err = -EFAULT;
kfree_skb(skb);
}
} else {
if (sk->ip_hdrincl || jumbolen ||
np->pmtudisc == IPV6_PMTUDISC_DO) {
ipv6_local_error(sk, EMSGSIZE, fl, mtu);
err = -EMSGSIZE;
goto out;
}
err = ip6_frag_xmit(sk, getfrag, data, dst, fl, opt, final_dst, hlimit,
flags, length, mtu);
}
out:
ip6_dst_store(sk, dst, fl->nl_u.ip6_u.daddr == &np->daddr ? &np->daddr : NULL);
return err;
}
int ip6_call_ra_chain(struct sk_buff *skb, int sel)
{
struct ip6_ra_chain *ra;
struct sock *last = NULL;
for (ra = ip6_ra_chain; ra; ra = ra->next) {
struct sock *sk = ra->sk;
if (sk && ra->sel == sel) {
if (last) {
struct sk_buff *skb2 = skb_clone(skb, GFP_ATOMIC);
if (skb2)
rawv6_rcv(last, skb2, skb2->len);
}
last = sk;
}
}
if (last) {
rawv6_rcv(last, skb, skb->len);
return 1;
}
return 0;
}
int ip6_forward(struct sk_buff *skb)
{
struct dst_entry *dst = skb->dst;
struct ipv6hdr *hdr = skb->nh.ipv6h;
struct inet6_skb_parm *opt =(struct inet6_skb_parm*)skb->cb;
if (ipv6_devconf.forwarding == 0 && opt->srcrt == 0)
goto drop;
if (opt->ra) {
u8 *ptr = skb->nh.raw + opt->ra;
if (ip6_call_ra_chain(skb, (ptr[2]<<8) + ptr[3]))
return 0;
}
if (hdr->hop_limit <= 1) {
skb->dev = dst->dev;
icmpv6_send(skb, ICMPV6_TIME_EXCEED, ICMPV6_EXC_HOPLIMIT,
0, skb->dev);
kfree_skb(skb);
return -ETIMEDOUT;
}
if (skb->dev == dst->dev && dst->neighbour && opt->srcrt == 0) {
struct in6_addr *target = NULL;
struct rt6_info *rt;
struct neighbour *n = dst->neighbour;
rt = (struct rt6_info *) dst;
if ((rt->rt6i_flags & RTF_GATEWAY))
target = (struct in6_addr*)&n->primary_key;
else
target = &hdr->daddr;
if (xrlim_allow(dst, 1*HZ))
ndisc_send_redirect(skb, n, target);
} else if (ipv6_addr_type(&hdr->saddr)&(IPV6_ADDR_MULTICAST|IPV6_ADDR_LOOPBACK
|IPV6_ADDR_LINKLOCAL)) {
goto drop;
}
if (skb->len > dst->pmtu) {
skb->dev = dst->dev;
icmpv6_send(skb, ICMPV6_PKT_TOOBIG, 0, dst->pmtu, skb->dev);
ipv6_statistics.Ip6InTooBigErrors++;
kfree_skb(skb);
return -EMSGSIZE;
}
if ((skb = skb_cow(skb, dst->dev->hard_header_len)) == NULL)
return 0;
hdr = skb->nh.ipv6h;
hdr->hop_limit--;
ipv6_statistics.Ip6OutForwDatagrams++;
return dst->output(skb);
drop:
ipv6_statistics.Ip6InAddrErrors++;
kfree_skb(skb);
return -EINVAL;
}