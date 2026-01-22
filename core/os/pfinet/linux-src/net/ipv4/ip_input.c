#include <asm/system.h>
#include <linux/types.h>
#include <linux/kernel.h>
#include <linux/string.h>
#include <linux/errno.h>
#include <linux/config.h>
#include <linux/net.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/in.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/etherdevice.h>
#include <net/snmp.h>
#include <net/ip.h>
#include <net/protocol.h>
#include <net/route.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <net/arp.h>
#include <net/icmp.h>
#include <net/raw.h>
#include <net/checksum.h>
#include <linux/ip_fw.h>
#ifdef CONFIG_IP_MASQUERADE
#include <net/ip_masq.h>
#endif
#include <linux/firewall.h>
#include <linux/mroute.h>
#include <linux/netlink.h>
struct ip_mib ip_statistics={2,IPDEFTTL,};
int sysctl_ip_always_defrag = 0;
int ip_ioctl(struct sock *sk, int cmd, unsigned long arg)
{
switch(cmd)
{
default:
return(-EINVAL);
}
}
static __inline__ int icmp_filter(struct sock *sk, struct sk_buff *skb)
{
int type;
type = skb->h.icmph->type;
if (type < 32)
return test_bit(type, &sk->tp_pinfo.tp_raw4.filter);
return 0;
}
int ip_call_ra_chain(struct sk_buff *skb)
{
struct ip_ra_chain *ra;
u8 protocol = skb->nh.iph->protocol;
struct sock *last = NULL;
for (ra = ip_ra_chain; ra; ra = ra->next) {
struct sock *sk = ra->sk;
if (sk && sk->num == protocol) {
if (skb->nh.iph->frag_off & htons(IP_MF|IP_OFFSET)) {
skb = ip_defrag(skb);
if (skb == NULL)
return 1;
}
if (last) {
struct sk_buff *skb2 = skb_clone(skb, GFP_ATOMIC);
if (skb2)
raw_rcv(last, skb2);
}
last = sk;
}
}
if (last) {
raw_rcv(last, skb);
return 1;
}
return 0;
}
int ip_local_deliver(struct sk_buff *skb)
{
struct iphdr *iph = skb->nh.iph;
struct inet_protocol *ipprot;
struct sock *raw_sk=NULL;
unsigned char hash;
int flag = 0;
if (sysctl_ip_always_defrag == 0 &&
(iph->frag_off & htons(IP_MF|IP_OFFSET))) {
skb = ip_defrag(skb);
if (!skb)
return 0;
iph = skb->nh.iph;
}
#ifdef CONFIG_IP_MASQUERADE
{
int ret;
if((IPCB(skb)->flags&IPSKB_MASQUERADED)) {
printk(KERN_DEBUG "ip_input(): demasq recursion detected. Check masq modules configuration\n");
kfree_skb(skb);
return 0;
}
ret = ip_fw_demasquerade(&skb);
if (ret < 0) {
kfree_skb(skb);
return 0;
}
if (ret) {
iph=skb->nh.iph;
IPCB(skb)->flags |= IPSKB_MASQUERADED;
dst_release(skb->dst);
skb->dst = NULL;
if (ip_route_input(skb, iph->daddr, iph->saddr, iph->tos, skb->dev)) {
kfree_skb(skb);
return 0;
}
return skb->dst->input(skb);
}
}
#endif
skb->h.raw = skb->nh.raw + iph->ihl*4;
hash = iph->protocol & (MAX_INET_PROTOS - 1);
if((raw_sk = raw_v4_htable[hash]) != NULL) {
struct sock *sknext = NULL;
struct sk_buff *skb1;
raw_sk = raw_v4_lookup(raw_sk, iph->protocol, iph->saddr, iph->daddr, skb->dev->ifindex);
if(raw_sk) {
do {
sknext = raw_v4_lookup(raw_sk->next, iph->protocol,
iph->saddr, iph->daddr, skb->dev->ifindex);
if (iph->protocol != IPPROTO_ICMP || !icmp_filter(raw_sk, skb)) {
if (sknext == NULL)
break;
skb1 = skb_clone(skb, GFP_ATOMIC);
if(skb1)
{
raw_rcv(raw_sk, skb1);
}
}
raw_sk = sknext;
} while(raw_sk!=NULL);
}
}
for (ipprot = (struct inet_protocol *)inet_protos[hash];ipprot != NULL;ipprot=(struct inet_protocol *)ipprot->next)
{
struct sk_buff *skb2;
if (ipprot->protocol != iph->protocol)
continue;
if (ipprot->copy || raw_sk)
{
skb2 = skb_clone(skb, GFP_ATOMIC);
if(skb2==NULL)
continue;
}
else
{
skb2 = skb;
}
flag = 1;
ipprot->handler(skb2, ntohs(iph->tot_len) - (iph->ihl * 4));
}
if(raw_sk!=NULL)
{
raw_rcv(raw_sk, skb);
}
else if (!flag)
{
icmp_send(skb, ICMP_DEST_UNREACH, ICMP_PROT_UNREACH, 0);
kfree_skb(skb);
}
return(0);
}
int ip_rcv(struct sk_buff *skb, struct device *dev, struct packet_type *pt)
{
struct iphdr *iph = skb->nh.iph;
#ifdef CONFIG_FIREWALL
int fwres;
u16 rport;
#endif
if (skb->pkt_type == PACKET_OTHERHOST)
goto drop;
ip_statistics.IpInReceives++;
if (skb->len < sizeof(struct iphdr))
goto inhdr_error;
if (skb->len < (iph->ihl << 2))
goto inhdr_error;
if (iph->ihl < 5 || iph->version != 4 || ip_fast_csum((u8 *)iph, iph->ihl) != 0)
goto inhdr_error;
{
__u32 len = ntohs(iph->tot_len);
if (skb->len < len)
goto inhdr_error;
if (len < (iph->ihl << 2))
goto inhdr_error;
__skb_trim(skb, len);
}
if (sysctl_ip_always_defrag != 0 &&
iph->frag_off & htons(IP_MF|IP_OFFSET)) {
skb = ip_defrag(skb);
if (!skb)
return 0;
iph = skb->nh.iph;
ip_send_check(iph);
}
#ifdef CONFIG_FIREWALL
fwres = call_in_firewall(PF_INET, dev, iph, &rport, &skb);
if (fwres < FW_ACCEPT && fwres != FW_REJECT)
goto drop;
iph = skb->nh.iph;
#endif
if (skb->dst == NULL) {
if (ip_route_input(skb, iph->daddr, iph->saddr, iph->tos, dev))
goto drop;
#ifdef CONFIG_CPU_IS_SLOW
if (net_cpu_congestion > 10 && !(iph->tos&IPTOS_RELIABILITY) &&
IPTOS_PREC(iph->tos) < IPTOS_PREC_INTERNETCONTROL) {
goto drop;
}
#endif
}
#ifdef CONFIG_NET_CLS_ROUTE
if (skb->dst->tclassid) {
u32 idx = skb->dst->tclassid;
ip_rt_acct[idx&0xFF].o_packets++;
ip_rt_acct[idx&0xFF].o_bytes+=skb->len;
ip_rt_acct[(idx>>16)&0xFF].i_packets++;
ip_rt_acct[(idx>>16)&0xFF].i_bytes+=skb->len;
}
#endif
if (iph->ihl > 5) {
struct ip_options *opt;
skb = skb_cow(skb, skb_headroom(skb));
if (skb == NULL)
return 0;
iph = skb->nh.iph;
skb->ip_summed = 0;
if (ip_options_compile(NULL, skb))
goto inhdr_error;
opt = &(IPCB(skb)->opt);
if (opt->srr) {
struct in_device *in_dev = dev->ip_ptr;
if (in_dev && !IN_DEV_SOURCE_ROUTE(in_dev)) {
if (IN_DEV_LOG_MARTIANS(in_dev) && net_ratelimit())
printk(KERN_INFO "source route option %d.%d.%d.%d -> %d.%d.%d.%d\n",
NIPQUAD(iph->saddr), NIPQUAD(iph->daddr));
goto drop;
}
if (ip_options_rcv_srr(skb))
goto drop;
}
}
#ifdef CONFIG_FIREWALL
#ifdef CONFIG_IP_TRANSPARENT_PROXY
if (fwres == FW_REDIRECT && (IPCB(skb)->redirport = rport) != 0)
return ip_local_deliver(skb);
#endif
if (fwres == FW_REJECT) {
icmp_send(skb, ICMP_DEST_UNREACH, ICMP_PORT_UNREACH, 0);
goto drop;
}
#endif
return skb->dst->input(skb);
inhdr_error:
ip_statistics.IpInHdrErrors++;
drop:
kfree_skb(skb);
return(0);
}