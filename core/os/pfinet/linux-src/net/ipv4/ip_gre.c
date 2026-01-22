#include <linux/config.h>
#include <linux/module.h>
#include <linux/types.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <asm/uaccess.h>
#include <linux/skbuff.h>
#include <linux/netdevice.h>
#include <linux/in.h>
#include <linux/tcp.h>
#include <linux/udp.h>
#include <linux/if_arp.h>
#include <linux/mroute.h>
#include <linux/init.h>
#include <linux/in6.h>
#include <linux/inetdevice.h>
#include <linux/igmp.h>
#include <net/sock.h>
#include <net/ip.h>
#include <net/icmp.h>
#include <net/protocol.h>
#include <net/ipip.h>
#include <net/arp.h>
#include <net/checksum.h>
#ifdef CONFIG_IPV6
#include <net/ipv6.h>
#include <net/ip6_fib.h>
#include <net/ip6_route.h>
#endif
static int ipgre_tunnel_init(struct device *dev);
static int ipgre_fb_tunnel_init(struct device *dev);
static struct device ipgre_fb_tunnel_dev = {
NULL, 0x0, 0x0, 0x0, 0x0, 0, 0, 0, 0, 0, NULL, ipgre_fb_tunnel_init,
};
static struct ip_tunnel ipgre_fb_tunnel = {
NULL, &ipgre_fb_tunnel_dev, {0, }, 0, 0, 0, 0, 0, 0, 0, {"gre0", }
};
#define HASH_SIZE  16
#define HASH(addr) ((addr^(addr>>4))&0xF)
static struct ip_tunnel *tunnels[4][HASH_SIZE];
#define tunnels_r_l	(tunnels[3])
#define tunnels_r	(tunnels[2])
#define tunnels_l	(tunnels[1])
#define tunnels_wc	(tunnels[0])
static struct ip_tunnel * ipgre_tunnel_lookup(u32 remote, u32 local, u32 key)
{
unsigned h0 = HASH(remote);
unsigned h1 = HASH(key);
struct ip_tunnel *t;
for (t = tunnels_r_l[h0^h1]; t; t = t->next) {
if (local == t->parms.iph.saddr && remote == t->parms.iph.daddr) {
if (t->parms.i_key == key && (t->dev->flags&IFF_UP))
return t;
}
}
for (t = tunnels_r[h0^h1]; t; t = t->next) {
if (remote == t->parms.iph.daddr) {
if (t->parms.i_key == key && (t->dev->flags&IFF_UP))
return t;
}
}
for (t = tunnels_l[h1]; t; t = t->next) {
if (local == t->parms.iph.saddr ||
(local == t->parms.iph.daddr && MULTICAST(local))) {
if (t->parms.i_key == key && (t->dev->flags&IFF_UP))
return t;
}
}
for (t = tunnels_wc[h1]; t; t = t->next) {
if (t->parms.i_key == key && (t->dev->flags&IFF_UP))
return t;
}
if (ipgre_fb_tunnel_dev.flags&IFF_UP)
return &ipgre_fb_tunnel;
return NULL;
}
static struct ip_tunnel **ipgre_bucket(struct ip_tunnel *t)
{
u32 remote = t->parms.iph.daddr;
u32 local = t->parms.iph.saddr;
u32 key = t->parms.i_key;
unsigned h = HASH(key);
int prio = 0;
if (local)
prio |= 1;
if (remote && !MULTICAST(remote)) {
prio |= 2;
h ^= HASH(remote);
}
return &tunnels[prio][h];
}
static void ipgre_tunnel_link(struct ip_tunnel *t)
{
struct ip_tunnel **tp = ipgre_bucket(t);
t->next = *tp;
wmb();
*tp = t;
}
static void ipgre_tunnel_unlink(struct ip_tunnel *t)
{
struct ip_tunnel **tp;
for (tp = ipgre_bucket(t); *tp; tp = &(*tp)->next) {
if (t == *tp) {
*tp = t->next;
synchronize_bh();
break;
}
}
}
static struct ip_tunnel * ipgre_tunnel_locate(struct ip_tunnel_parm *parms, int create)
{
u32 remote = parms->iph.daddr;
u32 local = parms->iph.saddr;
u32 key = parms->i_key;
struct ip_tunnel *t, **tp, *nt;
struct device *dev;
unsigned h = HASH(key);
int prio = 0;
if (local)
prio |= 1;
if (remote && !MULTICAST(remote)) {
prio |= 2;
h ^= HASH(remote);
}
for (tp = &tunnels[prio][h]; (t = *tp) != NULL; tp = &t->next) {
if (local == t->parms.iph.saddr && remote == t->parms.iph.daddr) {
if (key == t->parms.i_key)
return t;
}
}
if (!create)
return NULL;
MOD_INC_USE_COUNT;
dev = kmalloc(sizeof(*dev) + sizeof(*t), GFP_KERNEL);
if (dev == NULL) {
MOD_DEC_USE_COUNT;
return NULL;
}
memset(dev, 0, sizeof(*dev) + sizeof(*t));
dev->priv = (void*)(dev+1);
nt = (struct ip_tunnel*)dev->priv;
nt->dev = dev;
dev->name = nt->parms.name;
dev->init = ipgre_tunnel_init;
memcpy(&nt->parms, parms, sizeof(*parms));
if (dev->name[0] == 0) {
int i;
for (i=1; i<100; i++) {
sprintf(dev->name, "gre%d", i);
if (dev_get(dev->name) == NULL)
break;
}
if (i==100)
goto failed;
memcpy(parms->name, dev->name, IFNAMSIZ);
}
if (register_netdevice(dev) < 0)
goto failed;
ipgre_tunnel_link(nt);
return nt;
failed:
kfree(dev);
MOD_DEC_USE_COUNT;
return NULL;
}
static void ipgre_tunnel_destroy(struct device *dev)
{
ipgre_tunnel_unlink((struct ip_tunnel*)dev->priv);
if (dev != &ipgre_fb_tunnel_dev) {
kfree(dev);
MOD_DEC_USE_COUNT;
}
}
void ipgre_err(struct sk_buff *skb, unsigned char *dp, int len)
{
#ifndef I_WISH_WORLD_WERE_PERFECT
struct iphdr *iph = (struct iphdr*)dp;
u16	     *p = (u16*)(dp+(iph->ihl<<2));
int grehlen = (iph->ihl<<2) + 4;
int type = skb->h.icmph->type;
int code = skb->h.icmph->code;
struct ip_tunnel *t;
u16 flags;
flags = p[0];
if (flags&(GRE_CSUM|GRE_KEY|GRE_SEQ|GRE_ROUTING|GRE_VERSION)) {
if (flags&(GRE_VERSION|GRE_ROUTING))
return;
if (flags&GRE_KEY) {
grehlen += 4;
if (flags&GRE_CSUM)
grehlen += 4;
}
}
if (len < grehlen)
return;
switch (type) {
default:
case ICMP_PARAMETERPROB:
return;
case ICMP_DEST_UNREACH:
switch (code) {
case ICMP_SR_FAILED:
case ICMP_PORT_UNREACH:
return;
case ICMP_FRAG_NEEDED:
return;
default:
break;
}
break;
case ICMP_TIME_EXCEEDED:
if (code != ICMP_EXC_TTL)
return;
break;
}
t = ipgre_tunnel_lookup(iph->daddr, iph->saddr, (flags&GRE_KEY) ? *(((u32*)p) + (grehlen>>2) - 1) : 0);
if (t == NULL || t->parms.iph.daddr == 0 || MULTICAST(t->parms.iph.daddr))
return;
if (t->parms.iph.ttl == 0 && type == ICMP_TIME_EXCEEDED)
return;
if (jiffies - t->err_time < IPTUNNEL_ERR_TIMEO)
t->err_count++;
else
t->err_count = 1;
t->err_time = jiffies;
return;
#else
struct iphdr *iph = (struct iphdr*)dp;
struct iphdr *eiph;
u16	     *p = (u16*)(dp+(iph->ihl<<2));
int type = skb->h.icmph->type;
int code = skb->h.icmph->code;
int rel_type = 0;
int rel_code = 0;
int rel_info = 0;
u16 flags;
int grehlen = (iph->ihl<<2) + 4;
struct sk_buff *skb2;
struct rtable *rt;
if (p[1] != __constant_htons(ETH_P_IP))
return;
flags = p[0];
if (flags&(GRE_CSUM|GRE_KEY|GRE_SEQ|GRE_ROUTING|GRE_VERSION)) {
if (flags&(GRE_VERSION|GRE_ROUTING))
return;
if (flags&GRE_CSUM)
grehlen += 4;
if (flags&GRE_KEY)
grehlen += 4;
if (flags&GRE_SEQ)
grehlen += 4;
}
if (len < grehlen + sizeof(struct iphdr))
return;
eiph = (struct iphdr*)(dp + grehlen);
switch (type) {
default:
return;
case ICMP_PARAMETERPROB:
if (skb->h.icmph->un.gateway < (iph->ihl<<2))
return;
rel_type = ICMP_PARAMETERPROB;
rel_info = skb->h.icmph->un.gateway - grehlen;
break;
case ICMP_DEST_UNREACH:
switch (code) {
case ICMP_SR_FAILED:
case ICMP_PORT_UNREACH:
return;
case ICMP_FRAG_NEEDED:
rel_info = ntohs(skb->h.icmph->un.frag.mtu);
if (rel_info < grehlen+68)
return;
rel_info -= grehlen;
if (rel_info > ntohs(eiph->tot_len))
return;
break;
default:
rel_type = ICMP_DEST_UNREACH;
rel_code = ICMP_HOST_UNREACH;
break;
}
break;
case ICMP_TIME_EXCEEDED:
if (code != ICMP_EXC_TTL)
return;
break;
}
skb2 = skb_clone(skb, GFP_ATOMIC);
if (skb2 == NULL)
return;
dst_release(skb2->dst);
skb2->dst = NULL;
skb_pull(skb2, skb->data - (u8*)eiph);
skb2->nh.raw = skb2->data;
if (ip_route_output(&rt, eiph->saddr, 0, RT_TOS(eiph->tos), 0)) {
kfree_skb(skb2);
return;
}
skb2->dev = rt->u.dst.dev;
if (rt->rt_flags&RTCF_LOCAL) {
ip_rt_put(rt);
rt = NULL;
if (ip_route_output(&rt, eiph->daddr, eiph->saddr, eiph->tos, 0) ||
rt->u.dst.dev->type != ARPHRD_IPGRE) {
ip_rt_put(rt);
kfree_skb(skb2);
return;
}
} else {
ip_rt_put(rt);
if (ip_route_input(skb2, eiph->daddr, eiph->saddr, eiph->tos, skb2->dev) ||
skb2->dst->dev->type != ARPHRD_IPGRE) {
kfree_skb(skb2);
return;
}
}
if (type == ICMP_DEST_UNREACH && code == ICMP_FRAG_NEEDED) {
if (rel_info > skb2->dst->pmtu) {
kfree_skb(skb2);
return;
}
skb2->dst->pmtu = rel_info;
rel_info = htonl(rel_info);
} else if (type == ICMP_TIME_EXCEEDED) {
struct ip_tunnel *t = (struct ip_tunnel*)skb2->dev->priv;
if (t->parms.iph.ttl) {
rel_type = ICMP_DEST_UNREACH;
rel_code = ICMP_HOST_UNREACH;
}
}
icmp_send(skb2, rel_type, rel_code, rel_info);
kfree_skb(skb2);
#endif
}
int ipgre_rcv(struct sk_buff *skb, unsigned short len)
{
struct iphdr *iph = skb->nh.iph;
u8     *h = skb->h.raw;
u16    flags = *(u16*)h;
u16    csum = 0;
u32    key = 0;
u32    seqno = 0;
struct ip_tunnel *tunnel;
int    offset = 4;
if (flags&(GRE_CSUM|GRE_KEY|GRE_ROUTING|GRE_SEQ|GRE_VERSION)) {
if (flags&(GRE_VERSION|GRE_ROUTING))
goto drop;
if (flags&GRE_CSUM) {
csum = ip_compute_csum(h, len);
offset += 4;
}
if (flags&GRE_KEY) {
key = *(u32*)(h + offset);
offset += 4;
}
if (flags&GRE_SEQ) {
seqno = ntohl(*(u32*)(h + offset));
offset += 4;
}
}
if ((tunnel = ipgre_tunnel_lookup(iph->saddr, iph->daddr, key)) != NULL) {
skb->mac.raw = skb->nh.raw;
skb->nh.raw = skb_pull(skb, h + offset - skb->data);
memset(&(IPCB(skb)->opt), 0, sizeof(struct ip_options));
skb->ip_summed = 0;
skb->protocol = *(u16*)(h + 2);
skb->pkt_type = PACKET_HOST;
#ifdef CONFIG_NET_IPGRE_BROADCAST
if (MULTICAST(iph->daddr)) {
if (((struct rtable*)skb->dst)->key.iif == 0)
goto drop;
tunnel->stat.multicast++;
skb->pkt_type = PACKET_BROADCAST;
}
#endif
if (((flags&GRE_CSUM) && csum) ||
(!(flags&GRE_CSUM) && tunnel->parms.i_flags&GRE_CSUM)) {
tunnel->stat.rx_crc_errors++;
tunnel->stat.rx_errors++;
goto drop;
}
if (tunnel->parms.i_flags&GRE_SEQ) {
if (!(flags&GRE_SEQ) ||
(tunnel->i_seqno && (s32)(seqno - tunnel->i_seqno) < 0)) {
tunnel->stat.rx_fifo_errors++;
tunnel->stat.rx_errors++;
goto drop;
}
tunnel->i_seqno = seqno + 1;
}
tunnel->stat.rx_packets++;
tunnel->stat.rx_bytes += skb->len;
skb->dev = tunnel->dev;
dst_release(skb->dst);
skb->dst = NULL;
netif_rx(skb);
return(0);
}
icmp_send(skb, ICMP_DEST_UNREACH, ICMP_PROT_UNREACH, 0);
drop:
kfree_skb(skb);
return(0);
}
static int ipgre_tunnel_xmit(struct sk_buff *skb, struct device *dev)
{
struct ip_tunnel *tunnel = (struct ip_tunnel*)dev->priv;
struct net_device_stats *stats = &tunnel->stat;
struct iphdr  *old_iph = skb->nh.iph;
struct iphdr  *tiph;
u8     tos;
u16    df;
struct rtable *rt;
struct device *tdev;
struct iphdr  *iph;
int    max_headroom;
int    gre_hlen;
u32    dst;
int    mtu;
if (tunnel->recursion++) {
tunnel->stat.collisions++;
goto tx_error;
}
if (dev->hard_header) {
gre_hlen = 0;
tiph = (struct iphdr*)skb->data;
} else {
gre_hlen = tunnel->hlen;
tiph = &tunnel->parms.iph;
}
if ((dst = tiph->daddr) == 0) {
if (skb->dst == NULL) {
tunnel->stat.tx_fifo_errors++;
goto tx_error;
}
if (skb->protocol == __constant_htons(ETH_P_IP)) {
rt = (struct rtable*)skb->dst;
if ((dst = rt->rt_gateway) == 0)
goto tx_error_icmp;
}
#ifdef CONFIG_IPV6
else if (skb->protocol == __constant_htons(ETH_P_IPV6)) {
struct in6_addr *addr6;
int addr_type;
struct neighbour *neigh = skb->dst->neighbour;
if (neigh == NULL)
goto tx_error;
addr6 = (struct in6_addr*)&neigh->primary_key;
addr_type = ipv6_addr_type(addr6);
if (addr_type == IPV6_ADDR_ANY) {
addr6 = &skb->nh.ipv6h->daddr;
addr_type = ipv6_addr_type(addr6);
}
if ((addr_type & IPV6_ADDR_COMPATv4) == 0)
goto tx_error_icmp;
dst = addr6->s6_addr32[3];
}
#endif
else
goto tx_error;
}
tos = tiph->tos;
if (tos&1) {
if (skb->protocol == __constant_htons(ETH_P_IP))
tos = old_iph->tos;
tos &= ~1;
}
if (ip_route_output(&rt, dst, tiph->saddr, RT_TOS(tos), tunnel->parms.link)) {
tunnel->stat.tx_carrier_errors++;
goto tx_error;
}
tdev = rt->u.dst.dev;
if (tdev == dev) {
ip_rt_put(rt);
tunnel->stat.collisions++;
goto tx_error;
}
df = tiph->frag_off;
mtu = rt->u.dst.pmtu - tunnel->hlen;
if (skb->protocol == __constant_htons(ETH_P_IP)) {
if (skb->dst && mtu < skb->dst->pmtu && mtu >= 68)
skb->dst->pmtu = mtu;
df |= (old_iph->frag_off&__constant_htons(IP_DF));
if ((old_iph->frag_off&__constant_htons(IP_DF)) &&
mtu < ntohs(old_iph->tot_len)) {
icmp_send(skb, ICMP_DEST_UNREACH, ICMP_FRAG_NEEDED, htonl(mtu));
ip_rt_put(rt);
goto tx_error;
}
}
#ifdef CONFIG_IPV6
else if (skb->protocol == __constant_htons(ETH_P_IPV6)) {
struct rt6_info *rt6 = (struct rt6_info*)skb->dst;
if (rt6 && mtu < rt6->u.dst.pmtu && mtu >= IPV6_MIN_MTU) {
if ((tunnel->parms.iph.daddr && !MULTICAST(tunnel->parms.iph.daddr)) ||
rt6->rt6i_dst.plen == 128) {
rt6->rt6i_flags |= RTF_MODIFIED;
skb->dst->pmtu = mtu;
}
}
if (mtu >= IPV6_MIN_MTU && mtu < skb->len - tunnel->hlen + gre_hlen) {
icmpv6_send(skb, ICMPV6_PKT_TOOBIG, 0, mtu, dev);
ip_rt_put(rt);
goto tx_error;
}
}
#endif
if (tunnel->err_count > 0) {
if (jiffies - tunnel->err_time < IPTUNNEL_ERR_TIMEO) {
tunnel->err_count--;
dst_link_failure(skb);
} else
tunnel->err_count = 0;
}
skb->h.raw = skb->nh.raw;
max_headroom = ((tdev->hard_header_len+15)&~15)+ gre_hlen;
if (skb_headroom(skb) < max_headroom || skb_cloned(skb) || skb_shared(skb)) {
struct sk_buff *new_skb = skb_realloc_headroom(skb, max_headroom);
if (!new_skb) {
ip_rt_put(rt);
stats->tx_dropped++;
dev_kfree_skb(skb);
tunnel->recursion--;
return 0;
}
if (skb->sk)
skb_set_owner_w(new_skb, skb->sk);
dev_kfree_skb(skb);
skb = new_skb;
}
skb->nh.raw = skb_push(skb, gre_hlen);
memset(&(IPCB(skb)->opt), 0, sizeof(IPCB(skb)->opt));
dst_release(skb->dst);
skb->dst = &rt->u.dst;
iph 			=	skb->nh.iph;
iph->version		=	4;
iph->ihl		=	sizeof(struct iphdr) >> 2;
iph->frag_off		=	df;
iph->protocol		=	IPPROTO_GRE;
iph->tos		=	tos;
iph->daddr		=	rt->rt_dst;
iph->saddr		=	rt->rt_src;
if ((iph->ttl = tiph->ttl) == 0) {
if (skb->protocol == __constant_htons(ETH_P_IP))
iph->ttl = old_iph->ttl;
#ifdef CONFIG_IPV6
else if (skb->protocol == __constant_htons(ETH_P_IPV6))
iph->ttl = ((struct ipv6hdr*)old_iph)->hop_limit;
#endif
else
iph->ttl = ip_statistics.IpDefaultTTL;
}
((u16*)(iph+1))[0] = tunnel->parms.o_flags;
((u16*)(iph+1))[1] = skb->protocol;
if (tunnel->parms.o_flags&(GRE_KEY|GRE_CSUM|GRE_SEQ)) {
u32 *ptr = (u32*)(((u8*)iph) + tunnel->hlen - 4);
if (tunnel->parms.o_flags&GRE_SEQ) {
++tunnel->o_seqno;
*ptr = htonl(tunnel->o_seqno);
ptr--;
}
if (tunnel->parms.o_flags&GRE_KEY) {
*ptr = tunnel->parms.o_key;
ptr--;
}
if (tunnel->parms.o_flags&GRE_CSUM) {
*ptr = 0;
*(__u16*)ptr = ip_compute_csum((void*)(iph+1), skb->len - sizeof(struct iphdr));
}
}
iph->tot_len		=	htons(skb->len);
iph->id			=	htons(ip_id_count++);
ip_send_check(iph);
stats->tx_bytes += skb->len;
stats->tx_packets++;
ip_send(skb);
tunnel->recursion--;
return 0;
tx_error_icmp:
dst_link_failure(skb);
tx_error:
stats->tx_errors++;
dev_kfree_skb(skb);
tunnel->recursion--;
return 0;
}
static int
ipgre_tunnel_ioctl (struct device *dev, struct ifreq *ifr, int cmd)
{
int err = 0;
struct ip_tunnel_parm p;
struct ip_tunnel *t;
MOD_INC_USE_COUNT;
switch (cmd) {
case SIOCGETTUNNEL:
t = NULL;
if (dev == &ipgre_fb_tunnel_dev) {
if (copy_from_user(&p, ifr->ifr_ifru.ifru_data, sizeof(p))) {
err = -EFAULT;
break;
}
t = ipgre_tunnel_locate(&p, 0);
}
if (t == NULL)
t = (struct ip_tunnel*)dev->priv;
memcpy(&p, &t->parms, sizeof(p));
if (copy_to_user(ifr->ifr_ifru.ifru_data, &p, sizeof(p)))
err = -EFAULT;
break;
case SIOCADDTUNNEL:
case SIOCCHGTUNNEL:
err = -EPERM;
if (!capable(CAP_NET_ADMIN))
goto done;
err = -EFAULT;
if (copy_from_user(&p, ifr->ifr_ifru.ifru_data, sizeof(p)))
goto done;
err = -EINVAL;
if (p.iph.version != 4 || p.iph.protocol != IPPROTO_GRE ||
p.iph.ihl != 5 || (p.iph.frag_off&__constant_htons(~IP_DF)) ||
((p.i_flags|p.o_flags)&(GRE_VERSION|GRE_ROUTING)))
goto done;
if (p.iph.ttl)
p.iph.frag_off |= __constant_htons(IP_DF);
if (!(p.i_flags&GRE_KEY))
p.i_key = 0;
if (!(p.o_flags&GRE_KEY))
p.o_key = 0;
t = ipgre_tunnel_locate(&p, cmd == SIOCADDTUNNEL);
if (dev != &ipgre_fb_tunnel_dev && cmd == SIOCCHGTUNNEL &&
t != &ipgre_fb_tunnel) {
if (t != NULL) {
if (t->dev != dev) {
err = -EEXIST;
break;
}
} else {
unsigned nflags=0;
t = (struct ip_tunnel*)dev->priv;
if (MULTICAST(p.iph.daddr))
nflags = IFF_BROADCAST;
else if (p.iph.daddr)
nflags = IFF_POINTOPOINT;
if ((dev->flags^nflags)&(IFF_POINTOPOINT|IFF_BROADCAST)) {
err = -EINVAL;
break;
}
start_bh_atomic();
ipgre_tunnel_unlink(t);
t->parms.iph.saddr = p.iph.saddr;
t->parms.iph.daddr = p.iph.daddr;
t->parms.i_key = p.i_key;
t->parms.o_key = p.o_key;
memcpy(dev->dev_addr, &p.iph.saddr, 4);
memcpy(dev->broadcast, &p.iph.daddr, 4);
ipgre_tunnel_link(t);
end_bh_atomic();
netdev_state_change(dev);
}
}
if (t) {
err = 0;
if (cmd == SIOCCHGTUNNEL) {
t->parms.iph.ttl = p.iph.ttl;
t->parms.iph.tos = p.iph.tos;
t->parms.iph.frag_off = p.iph.frag_off;
}
if (copy_to_user(ifr->ifr_ifru.ifru_data, &t->parms, sizeof(p)))
err = -EFAULT;
} else
err = (cmd == SIOCADDTUNNEL ? -ENOBUFS : -ENOENT);
break;
case SIOCDELTUNNEL:
err = -EPERM;
if (!capable(CAP_NET_ADMIN))
goto done;
if (dev == &ipgre_fb_tunnel_dev) {
err = -EFAULT;
if (copy_from_user(&p, ifr->ifr_ifru.ifru_data, sizeof(p)))
goto done;
err = -ENOENT;
if ((t = ipgre_tunnel_locate(&p, 0)) == NULL)
goto done;
err = -EPERM;
if (t == &ipgre_fb_tunnel)
goto done;
}
err = unregister_netdevice(dev);
break;
default:
err = -EINVAL;
}
done:
MOD_DEC_USE_COUNT;
return err;
}
static struct net_device_stats *ipgre_tunnel_get_stats(struct device *dev)
{
return &(((struct ip_tunnel*)dev->priv)->stat);
}
static int ipgre_tunnel_change_mtu(struct device *dev, int new_mtu)
{
struct ip_tunnel *tunnel = (struct ip_tunnel*)dev->priv;
if (new_mtu < 68 || new_mtu > 0xFFF8 - tunnel->hlen)
return -EINVAL;
dev->mtu = new_mtu;
return 0;
}
#ifdef CONFIG_NET_IPGRE_BROADCAST
static int ipgre_header(struct sk_buff *skb, struct device *dev, unsigned short type,
void *daddr, void *saddr, unsigned len)
{
struct ip_tunnel *t = (struct ip_tunnel*)dev->priv;
struct iphdr *iph = (struct iphdr *)skb_push(skb, t->hlen);
u16 *p = (u16*)(iph+1);
memcpy(iph, &t->parms.iph, sizeof(struct iphdr));
p[0]		= t->parms.o_flags;
p[1]		= htons(type);
if (saddr)
memcpy(&iph->saddr, saddr, 4);
if (daddr) {
memcpy(&iph->daddr, daddr, 4);
return t->hlen;
}
if (iph->daddr && !MULTICAST(iph->daddr))
return t->hlen;
return -t->hlen;
}
static int ipgre_open(struct device *dev)
{
struct ip_tunnel *t = (struct ip_tunnel*)dev->priv;
MOD_INC_USE_COUNT;
if (MULTICAST(t->parms.iph.daddr)) {
struct rtable *rt;
if (ip_route_output(&rt, t->parms.iph.daddr,
t->parms.iph.saddr, RT_TOS(t->parms.iph.tos),
t->parms.link)) {
MOD_DEC_USE_COUNT;
return -EADDRNOTAVAIL;
}
dev = rt->u.dst.dev;
ip_rt_put(rt);
if (dev->ip_ptr == NULL) {
MOD_DEC_USE_COUNT;
return -EADDRNOTAVAIL;
}
t->mlink = dev->ifindex;
ip_mc_inc_group(dev->ip_ptr, t->parms.iph.daddr);
}
return 0;
}
static int ipgre_close(struct device *dev)
{
struct ip_tunnel *t = (struct ip_tunnel*)dev->priv;
if (MULTICAST(t->parms.iph.daddr) && t->mlink) {
dev = dev_get_by_index(t->mlink);
if (dev && dev->ip_ptr)
ip_mc_dec_group(dev->ip_ptr, t->parms.iph.daddr);
}
MOD_DEC_USE_COUNT;
return 0;
}
#endif
static void ipgre_tunnel_init_gen(struct device *dev)
{
struct ip_tunnel *t = (struct ip_tunnel*)dev->priv;
dev->destructor		= ipgre_tunnel_destroy;
dev->hard_start_xmit	= ipgre_tunnel_xmit;
dev->get_stats		= ipgre_tunnel_get_stats;
dev->do_ioctl		= ipgre_tunnel_ioctl;
dev->change_mtu		= ipgre_tunnel_change_mtu;
dev_init_buffers(dev);
dev->type		= ARPHRD_IPGRE;
dev->hard_header_len 	= LL_MAX_HEADER + sizeof(struct iphdr) + 4;
dev->mtu		= 1500 - sizeof(struct iphdr) - 4;
dev->flags		= IFF_NOARP;
dev->iflink		= 0;
dev->addr_len		= 4;
memcpy(dev->dev_addr, &t->parms.iph.saddr, 4);
memcpy(dev->broadcast, &t->parms.iph.daddr, 4);
}
static int ipgre_tunnel_init(struct device *dev)
{
struct device *tdev = NULL;
struct ip_tunnel *tunnel;
struct iphdr *iph;
int hlen = LL_MAX_HEADER;
int mtu = 1500;
int addend = sizeof(struct iphdr) + 4;
tunnel = (struct ip_tunnel*)dev->priv;
iph = &tunnel->parms.iph;
ipgre_tunnel_init_gen(dev);
if (iph->daddr) {
struct rtable *rt;
if (!ip_route_output(&rt, iph->daddr, iph->saddr, RT_TOS(iph->tos), tunnel->parms.link)) {
tdev = rt->u.dst.dev;
ip_rt_put(rt);
}
dev->flags |= IFF_POINTOPOINT;
#ifdef CONFIG_NET_IPGRE_BROADCAST
if (MULTICAST(iph->daddr)) {
if (!iph->saddr)
return -EINVAL;
dev->flags = IFF_BROADCAST;
dev->hard_header = ipgre_header;
dev->open = ipgre_open;
dev->stop = ipgre_close;
}
#endif
}
if (!tdev && tunnel->parms.link)
tdev = dev_get_by_index(tunnel->parms.link);
if (tdev) {
hlen = tdev->hard_header_len;
mtu = tdev->mtu;
}
dev->iflink = tunnel->parms.link;
if (tunnel->parms.o_flags&(GRE_CSUM|GRE_KEY|GRE_SEQ)) {
if (tunnel->parms.o_flags&GRE_CSUM)
addend += 4;
if (tunnel->parms.o_flags&GRE_KEY)
addend += 4;
if (tunnel->parms.o_flags&GRE_SEQ)
addend += 4;
}
dev->hard_header_len = hlen + addend;
dev->mtu = mtu - addend;
tunnel->hlen = addend;
return 0;
}
#ifdef MODULE
static int ipgre_fb_tunnel_open(struct device *dev)
{
MOD_INC_USE_COUNT;
return 0;
}
static int ipgre_fb_tunnel_close(struct device *dev)
{
MOD_DEC_USE_COUNT;
return 0;
}
#endif
__initfunc(int ipgre_fb_tunnel_init(struct device *dev))
{
struct ip_tunnel *tunnel = (struct ip_tunnel*)dev->priv;
struct iphdr *iph;
ipgre_tunnel_init_gen(dev);
#ifdef MODULE
dev->open		= ipgre_fb_tunnel_open;
dev->stop		= ipgre_fb_tunnel_close;
#endif
iph = &ipgre_fb_tunnel.parms.iph;
iph->version		= 4;
iph->protocol		= IPPROTO_GRE;
iph->ihl		= 5;
tunnel->hlen		= sizeof(struct iphdr) + 4;
tunnels_wc[0]		= &ipgre_fb_tunnel;
return 0;
}
static struct inet_protocol ipgre_protocol = {
ipgre_rcv,
ipgre_err,
0,
IPPROTO_GRE,
0,
NULL,
"GRE"
};
#ifdef MODULE
int init_module(void)
#else
__initfunc(int ipgre_init(void))
#endif
{
printk(KERN_INFO "GRE over IPv4 tunneling driver\n");
ipgre_fb_tunnel_dev.priv = (void*)&ipgre_fb_tunnel;
ipgre_fb_tunnel_dev.name = ipgre_fb_tunnel.parms.name;
#ifdef MODULE
register_netdev(&ipgre_fb_tunnel_dev);
#else
register_netdevice(&ipgre_fb_tunnel_dev);
#endif
inet_add_protocol(&ipgre_protocol);
return 0;
}
#ifdef MODULE
void cleanup_module(void)
{
if ( inet_del_protocol(&ipgre_protocol) < 0 )
printk(KERN_INFO "ipgre close: can't remove protocol\n");
unregister_netdev(&ipgre_fb_tunnel_dev);
}
#endif