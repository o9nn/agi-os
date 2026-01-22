#include <linux/config.h>
#include <linux/types.h>
#include <linux/mm.h>
#include <linux/sched.h>
#include <linux/skbuff.h>
#include <linux/ip.h>
#include <linux/icmp.h>
#include <linux/netdevice.h>
#include <net/sock.h>
#include <net/ip.h>
#include <net/icmp.h>
#include <linux/tcp.h>
#include <linux/udp.h>
#include <linux/firewall.h>
#include <linux/ip_fw.h>
#include <net/checksum.h>
#include <linux/route.h>
#include <net/route.h>
#include <net/ip_fib.h>
int
ip_do_nat(struct sk_buff *skb)
{
struct rtable *rt = (struct rtable*)skb->dst;
struct iphdr *iph = skb->nh.iph;
u32 odaddr = iph->daddr;
u32 osaddr = iph->saddr;
u16	check;
IPCB(skb)->flags |= IPSKB_TRANSLATED;
iph->daddr = rt->rt_dst_map;
iph->saddr = rt->rt_src_map;
iph->check = 0;
iph->check = ip_fast_csum((unsigned char *)iph, iph->ihl);
if (!(iph->frag_off & htons(IP_OFFSET))) {
u16	*cksum;
switch(iph->protocol) {
case IPPROTO_TCP:
cksum  = (u16*)&((struct tcphdr*)(((char*)iph) + (iph->ihl<<2)))->check;
if ((u8*)(cksum+1) > skb->tail)
goto truncated;
check  = csum_tcpudp_magic(iph->saddr, iph->daddr, 0, 0, ~(*cksum));
*cksum = csum_tcpudp_magic(~osaddr, ~odaddr, 0, 0, ~check);
break;
case IPPROTO_UDP:
cksum  = (u16*)&((struct udphdr*)(((char*)iph) + (iph->ihl<<2)))->check;
if ((u8*)(cksum+1) > skb->tail)
goto truncated;
if ((check = *cksum) != 0) {
check = csum_tcpudp_magic(iph->saddr, iph->daddr, 0, 0, ~check);
check = csum_tcpudp_magic(~osaddr, ~odaddr, 0, 0, ~check);
*cksum = check ? : 0xFFFF;
}
break;
case IPPROTO_ICMP:
{
struct icmphdr *icmph = (struct icmphdr*)((char*)iph + (iph->ihl<<2));
struct   iphdr *ciph;
u32 idaddr, isaddr;
int updated;
if ((icmph->type != ICMP_DEST_UNREACH) &&
(icmph->type != ICMP_TIME_EXCEEDED) &&
(icmph->type != ICMP_PARAMETERPROB))
break;
ciph = (struct iphdr *) (icmph + 1);
if ((u8*)(ciph+1) > skb->tail)
goto truncated;
isaddr = ciph->saddr;
idaddr = ciph->daddr;
updated = 0;
if (rt->rt_flags&RTCF_DNAT && ciph->saddr == odaddr) {
ciph->saddr = iph->daddr;
updated = 1;
}
if (rt->rt_flags&RTCF_SNAT) {
if (ciph->daddr != osaddr) {
struct   fib_result res;
struct   rt_key key;
unsigned flags = 0;
key.src = ciph->daddr;
key.dst = ciph->saddr;
key.iif = skb->dev->ifindex;
key.oif = 0;
#ifdef CONFIG_IP_ROUTE_TOS
key.tos = RT_TOS(ciph->tos);
#endif
#ifdef CONFIG_IP_ROUTE_FWMARK
key.fwmark = 0;
#endif
if (fib_lookup(&key, &res) == 0 && res.r) {
ciph->daddr = fib_rules_policy(ciph->daddr, &res, &flags);
if (ciph->daddr != idaddr)
updated = 1;
}
} else {
ciph->daddr = iph->saddr;
updated = 1;
}
}
if (updated) {
cksum  = &icmph->checksum;
check  = csum_tcpudp_magic(ciph->saddr, ciph->daddr, 0, 0, ~(*cksum));
*cksum = csum_tcpudp_magic(~isaddr, ~idaddr, 0, 0, ~check);
}
break;
}
default:
break;
}
}
return 0;
truncated:
return -EINVAL;
}