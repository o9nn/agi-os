#define __NO_VERSION__
#include <linux/module.h>
#include <linux/errno.h>
#include <linux/types.h>
#include <linux/socket.h>
#include <linux/in.h>
#include <linux/kernel.h>
#include <linux/sched.h>
#include <linux/sockios.h>
#include <linux/net.h>
#include <linux/skbuff.h>
#include <linux/init.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/icmpv6.h>
#include <net/ip.h>
#include <net/sock.h>
#include <net/ipv6.h>
#include <net/checksum.h>
#include <net/protocol.h>
#include <net/raw.h>
#include <net/rawv6.h>
#include <net/transp_v6.h>
#include <net/ip6_route.h>
#include <net/addrconf.h>
#include <net/icmp.h>
#include <asm/uaccess.h>
#include <asm/system.h>
struct icmpv6_mib icmpv6_statistics;
struct socket *icmpv6_socket;
int icmpv6_rcv(struct sk_buff *skb, unsigned long len);
static struct inet6_protocol icmpv6_protocol =
{
icmpv6_rcv,
NULL,
NULL,
IPPROTO_ICMPV6,
0,
NULL,
"ICMPv6"
};
struct icmpv6_msg {
struct icmp6hdr		icmph;
__u8 			*data;
struct in6_addr		*daddr;
int			len;
__u32			csum;
};
static int icmpv6_getfrag(const void *data, struct in6_addr *saddr,
char *buff, unsigned int offset, unsigned int len)
{
struct icmpv6_msg *msg = (struct icmpv6_msg *) data;
struct icmp6hdr *icmph;
__u32 csum;
if (offset) {
csum = csum_partial_copy((void *) msg->data +
offset - sizeof(struct icmp6hdr),
buff, len, msg->csum);
msg->csum = csum;
return 0;
}
csum = csum_partial_copy((void *) &msg->icmph, buff,
sizeof(struct icmp6hdr), msg->csum);
csum = csum_partial_copy((void *) msg->data,
buff + sizeof(struct icmp6hdr),
len - sizeof(struct icmp6hdr), csum);
icmph = (struct icmp6hdr *) buff;
icmph->icmp6_cksum = csum_ipv6_magic(saddr, msg->daddr, msg->len,
IPPROTO_ICMPV6, csum);
return 0;
}
void icmpv6_param_prob(struct sk_buff *skb, int code, void *pos)
{
int offset = (u8*)pos - (u8*)skb->nh.ipv6h;
icmpv6_send(skb, ICMPV6_PARAMPROB, code, offset, skb->dev);
kfree_skb(skb);
}
static int is_ineligible(struct ipv6hdr *hdr, int len)
{
u8 *ptr;
__u8 nexthdr = hdr->nexthdr;
if (len < (int)sizeof(*hdr))
return 1;
ptr = ipv6_skip_exthdr((struct ipv6_opt_hdr *)(hdr+1), &nexthdr, len - sizeof(*hdr));
if (!ptr)
return 0;
if (nexthdr == IPPROTO_ICMPV6) {
struct icmp6hdr *ihdr =	(struct icmp6hdr *)ptr;
return (ptr - (u8*)hdr) > len || !(ihdr->icmp6_type & 0x80);
}
return nexthdr == NEXTHDR_FRAGMENT;
}
int sysctl_icmpv6_time = 1*HZ;
static inline int icmpv6_xrlim_allow(struct sock *sk, int type,
struct flowi *fl)
{
struct dst_entry *dst;
int res = 0;
if (type & 0x80)
return 1;
if (type == ICMPV6_PKT_TOOBIG)
return 1;
dst = ip6_route_output(sk, fl);
if (dst->error) {
ipv6_statistics.Ip6OutNoRoutes++;
} else if (dst->dev && (dst->dev->flags&IFF_LOOPBACK)) {
res = 1;
} else {
struct rt6_info *rt = (struct rt6_info *)dst;
int tmo = sysctl_icmpv6_time;
if (rt->rt6i_dst.plen < 128)
tmo >>= ((128 - rt->rt6i_dst.plen)>>5);
res = xrlim_allow(dst, tmo);
}
dst_release(dst);
return res;
}
static __inline__ int opt_unrec(struct sk_buff *skb, __u32 offset)
{
u8 *buff = skb->nh.raw;
return ( ( *(buff + offset) & 0xC0 ) == 0x80 );
}
void icmpv6_send(struct sk_buff *skb, int type, int code, __u32 info,
struct device *dev)
{
struct ipv6hdr *hdr = skb->nh.ipv6h;
struct sock *sk = icmpv6_socket->sk;
struct in6_addr *saddr = NULL;
int iif = 0;
struct icmpv6_msg msg;
struct flowi fl;
int addr_type = 0;
int len;
if (type == ICMPV6_PARAMPROB &&
(info > (skb->tail - ((unsigned char *) hdr)))) {
printk(KERN_DEBUG "icmpv6_send: bug! pointer > skb\n");
return;
}
addr_type = ipv6_addr_type(&hdr->daddr);
if (ipv6_chk_addr(&hdr->daddr, skb->dev, 0))
saddr = &hdr->daddr;
if ((addr_type & IPV6_ADDR_MULTICAST || skb->pkt_type != PACKET_HOST)) {
if (type != ICMPV6_PKT_TOOBIG &&
!(type == ICMPV6_PARAMPROB &&
code == ICMPV6_UNK_OPTION &&
(opt_unrec(skb, info))))
return;
saddr = NULL;
}
addr_type = ipv6_addr_type(&hdr->saddr);
if (addr_type & IPV6_ADDR_LINKLOCAL)
iif = skb->dev->ifindex;
if ((addr_type == IPV6_ADDR_ANY) || (addr_type & IPV6_ADDR_MULTICAST)) {
printk(KERN_DEBUG "icmpv6_send: addr_any/mcast source\n");
return;
}
if (is_ineligible(hdr, (u8*)skb->tail - (u8*)hdr)) {
if (net_ratelimit())
printk(KERN_DEBUG "icmpv6_send: no reply to icmp error/fragment\n");
return;
}
fl.proto = IPPROTO_ICMPV6;
fl.nl_u.ip6_u.daddr = &hdr->saddr;
fl.nl_u.ip6_u.saddr = saddr;
fl.oif = iif;
fl.fl6_flowlabel = 0;
fl.uli_u.icmpt.type = type;
fl.uli_u.icmpt.code = code;
if (!icmpv6_xrlim_allow(sk, type, &fl))
return;
msg.icmph.icmp6_type = type;
msg.icmph.icmp6_code = code;
msg.icmph.icmp6_cksum = 0;
msg.icmph.icmp6_pointer = htonl(info);
msg.data = skb->nh.raw;
msg.csum = 0;
msg.daddr = &hdr->saddr;
len = min((skb->tail - ((unsigned char *) hdr)) + sizeof(struct icmp6hdr),
IPV6_MIN_MTU - sizeof(struct ipv6hdr));
if (len < 0) {
printk(KERN_DEBUG "icmp: len problem\n");
return;
}
msg.len = len;
ip6_build_xmit(sk, icmpv6_getfrag, &msg, &fl, len, NULL, -1,
MSG_DONTWAIT);
if (type >= ICMPV6_DEST_UNREACH && type <= ICMPV6_PARAMPROB)
(&icmpv6_statistics.Icmp6OutDestUnreachs)[type-1]++;
icmpv6_statistics.Icmp6OutMsgs++;
}
static void icmpv6_echo_reply(struct sk_buff *skb)
{
struct sock *sk = icmpv6_socket->sk;
struct ipv6hdr *hdr = skb->nh.ipv6h;
struct icmp6hdr *icmph = (struct icmp6hdr *) skb->h.raw;
struct in6_addr *saddr;
struct icmpv6_msg msg;
struct flowi fl;
unsigned char *data;
int len;
data = (char *) (icmph + 1);
saddr = &hdr->daddr;
if (ipv6_addr_type(saddr) & IPV6_ADDR_MULTICAST)
saddr = NULL;
len = skb->tail - data;
len += sizeof(struct icmp6hdr);
msg.icmph.icmp6_type = ICMPV6_ECHO_REPLY;
msg.icmph.icmp6_code = 0;
msg.icmph.icmp6_cksum = 0;
msg.icmph.icmp6_identifier = icmph->icmp6_identifier;
msg.icmph.icmp6_sequence = icmph->icmp6_sequence;
msg.data = data;
msg.csum = 0;
msg.len = len;
msg.daddr = &hdr->saddr;
fl.proto = IPPROTO_ICMPV6;
fl.nl_u.ip6_u.daddr = &hdr->saddr;
fl.nl_u.ip6_u.saddr = saddr;
fl.oif = skb->dev->ifindex;
fl.fl6_flowlabel = 0;
fl.uli_u.icmpt.type = ICMPV6_ECHO_REPLY;
fl.uli_u.icmpt.code = 0;
ip6_build_xmit(sk, icmpv6_getfrag, &msg, &fl, len, NULL, -1,
MSG_DONTWAIT);
icmpv6_statistics.Icmp6OutEchoReplies++;
icmpv6_statistics.Icmp6OutMsgs++;
}
static void icmpv6_notify(struct sk_buff *skb,
int type, int code, u32 info, unsigned char *buff, int len)
{
struct in6_addr *saddr = &skb->nh.ipv6h->saddr;
struct in6_addr *daddr = &skb->nh.ipv6h->daddr;
struct ipv6hdr *hdr = (struct ipv6hdr *) buff;
struct inet6_protocol *ipprot;
struct sock *sk;
u8 *pb;
int hash;
u8 nexthdr;
nexthdr = hdr->nexthdr;
len -= sizeof(struct ipv6hdr);
if (len < 0)
return;
pb = ipv6_skip_exthdr((struct ipv6_opt_hdr *) (hdr + 1), &nexthdr, len);
if (!pb)
return;
hash = nexthdr & (MAX_INET_PROTOS - 1);
for (ipprot = (struct inet6_protocol *) inet6_protos[hash];
ipprot != NULL;
ipprot=(struct inet6_protocol *)ipprot->next) {
if (ipprot->protocol != nexthdr)
continue;
if (ipprot->err_handler)
ipprot->err_handler(skb, hdr, NULL, type, code, pb, info);
}
sk = raw_v6_htable[hash];
if (sk == NULL)
return;
while((sk = raw_v6_lookup(sk, nexthdr, daddr, saddr))) {
rawv6_err(sk, skb, hdr, NULL, type, code, pb, info);
sk = sk->next;
}
}
int icmpv6_rcv(struct sk_buff *skb, unsigned long len)
{
struct device *dev = skb->dev;
struct in6_addr *saddr = &skb->nh.ipv6h->saddr;
struct in6_addr *daddr = &skb->nh.ipv6h->daddr;
struct ipv6hdr *orig_hdr;
struct icmp6hdr *hdr = (struct icmp6hdr *) skb->h.raw;
int ulen;
int type;
icmpv6_statistics.Icmp6InMsgs++;
if (len < sizeof(struct icmp6hdr))
goto discard_it;
switch (skb->ip_summed) {
case CHECKSUM_NONE:
skb->csum = csum_partial((char *)hdr, len, 0);
case CHECKSUM_HW:
if (csum_ipv6_magic(saddr, daddr, len, IPPROTO_ICMPV6,
skb->csum)) {
printk(KERN_DEBUG "ICMPv6 checksum failed [%04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x > %04x:%04x:%04x:%04x:%04x:%04x:%04x:%04x]\n",
ntohs(saddr->__in6_u.__u6_addr16[0]),
ntohs(saddr->__in6_u.__u6_addr16[1]),
ntohs(saddr->__in6_u.__u6_addr16[2]),
ntohs(saddr->__in6_u.__u6_addr16[3]),
ntohs(saddr->__in6_u.__u6_addr16[4]),
ntohs(saddr->__in6_u.__u6_addr16[5]),
ntohs(saddr->__in6_u.__u6_addr16[6]),
ntohs(saddr->__in6_u.__u6_addr16[7]),
ntohs(daddr->__in6_u.__u6_addr16[0]),
ntohs(daddr->__in6_u.__u6_addr16[1]),
ntohs(daddr->__in6_u.__u6_addr16[2]),
ntohs(daddr->__in6_u.__u6_addr16[3]),
ntohs(daddr->__in6_u.__u6_addr16[4]),
ntohs(daddr->__in6_u.__u6_addr16[5]),
ntohs(daddr->__in6_u.__u6_addr16[6]),
ntohs(daddr->__in6_u.__u6_addr16[7]));
goto discard_it;
}
default:
break;
};
ulen = skb->tail - (unsigned char *) (hdr + 1);
type = hdr->icmp6_type;
if (type >= ICMPV6_DEST_UNREACH && type <= ICMPV6_PARAMPROB)
(&icmpv6_statistics.Icmp6InDestUnreachs)[type-ICMPV6_DEST_UNREACH]++;
else if (type >= ICMPV6_ECHO_REQUEST && type <= NDISC_REDIRECT)
(&icmpv6_statistics.Icmp6InEchos)[type-ICMPV6_ECHO_REQUEST]++;
switch (type) {
case ICMPV6_ECHO_REQUEST:
icmpv6_echo_reply(skb);
break;
case ICMPV6_ECHO_REPLY:
break;
case ICMPV6_PKT_TOOBIG:
orig_hdr = (struct ipv6hdr *) (hdr + 1);
if (ulen >= sizeof(struct ipv6hdr))
rt6_pmtu_discovery(&orig_hdr->daddr, &orig_hdr->saddr, dev,
ntohl(hdr->icmp6_mtu));
case ICMPV6_DEST_UNREACH:
case ICMPV6_TIME_EXCEED:
case ICMPV6_PARAMPROB:
icmpv6_notify(skb, type, hdr->icmp6_code, hdr->icmp6_mtu,
(char *) (hdr + 1), ulen);
break;
case NDISC_ROUTER_SOLICITATION:
case NDISC_ROUTER_ADVERTISEMENT:
case NDISC_NEIGHBOUR_SOLICITATION:
case NDISC_NEIGHBOUR_ADVERTISEMENT:
case NDISC_REDIRECT:
ndisc_rcv(skb, len);
break;
case ICMPV6_MGM_QUERY:
igmp6_event_query(skb, hdr, len);
break;
case ICMPV6_MGM_REPORT:
igmp6_event_report(skb, hdr, len);
break;
case ICMPV6_MGM_REDUCTION:
break;
default:
if (net_ratelimit())
printk(KERN_DEBUG "icmpv6: msg of unknown type\n");
if (type & 0x80)
break;
icmpv6_notify(skb, type, hdr->icmp6_code, hdr->icmp6_mtu,
(char *) (hdr + 1), ulen);
};
kfree_skb(skb);
return 0;
discard_it:
icmpv6_statistics.Icmp6InErrors++;
kfree_skb(skb);
return 0;
}
int __init icmpv6_init(struct net_proto_family *ops)
{
struct sock *sk;
int err;
icmpv6_socket = sock_alloc();
if (icmpv6_socket == NULL) {
printk(KERN_ERR
"Failed to create the ICMP6 control socket.\n");
return -1;
}
#ifndef _HURD_
icmpv6_socket->inode->i_uid = 0;
icmpv6_socket->inode->i_gid = 0;
#endif
icmpv6_socket->type = SOCK_RAW;
if ((err = ops->create(icmpv6_socket, IPPROTO_ICMPV6)) < 0) {
printk(KERN_ERR
"Failed to initialize the ICMP6 control socket (err %d).\n",
err);
sock_release(icmpv6_socket);
icmpv6_socket = NULL;
return err;
}
sk = icmpv6_socket->sk;
sk->allocation = GFP_ATOMIC;
sk->num = 256;
inet6_add_protocol(&icmpv6_protocol);
return 0;
}
void icmpv6_cleanup(void)
{
sock_release(icmpv6_socket);
icmpv6_socket = NULL;
inet6_del_protocol(&icmpv6_protocol);
}
static struct icmp6_err {
int err;
int fatal;
} tab_unreach[] = {
{ ENETUNREACH,	0},
{ EACCES,	1},
{ EHOSTUNREACH,	0},
{ EHOSTUNREACH,	0},
{ ECONNREFUSED,	1},
};
int icmpv6_err_convert(int type, int code, int *err)
{
int fatal = 0;
*err = EPROTO;
switch (type) {
case ICMPV6_DEST_UNREACH:
fatal = 1;
if (code <= ICMPV6_PORT_UNREACH) {
*err  = tab_unreach[code].err;
fatal = tab_unreach[code].fatal;
}
break;
case ICMPV6_PKT_TOOBIG:
*err = EMSGSIZE;
break;
case ICMPV6_PARAMPROB:
*err = EPROTO;
fatal = 1;
break;
case ICMPV6_TIME_EXCEED:
*err = EHOSTUNREACH;
break;
};
return fatal;
}