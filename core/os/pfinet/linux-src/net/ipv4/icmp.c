#include <linux/config.h>
#include <linux/types.h>
#include <linux/sched.h>
#include <linux/kernel.h>
#include <linux/fcntl.h>
#include <linux/socket.h>
#include <linux/in.h>
#include <linux/inet.h>
#include <linux/netdevice.h>
#include <linux/string.h>
#include <net/snmp.h>
#include <net/ip.h>
#include <net/route.h>
#include <net/protocol.h>
#include <net/icmp.h>
#include <net/tcp.h>
#include <net/udp.h>
#include <net/raw.h>
#include <net/snmp.h>
#include <linux/skbuff.h>
#include <net/sock.h>
#include <linux/errno.h>
#include <linux/timer.h>
#include <linux/init.h>
#include <asm/system.h>
#include <asm/uaccess.h>
#include <net/checksum.h>
#ifdef CONFIG_IP_MASQUERADE
#include <net/ip_masq.h>
#endif
#define min(a,b) ((a)<(b)?(a):(b))
struct icmp_mib icmp_statistics;
struct icmp_err icmp_err_convert[] = {
{ ENETUNREACH, 0 },
{ EHOSTUNREACH, 0 },
{ ENOPROTOOPT, 1 },
{ ECONNREFUSED, 1 },
{ EMSGSIZE, 0 },
{ EOPNOTSUPP, 0 },
{ ENETUNREACH, 1 },
{ EHOSTDOWN, 1 },
{ ENONET, 1 },
{ ENETUNREACH, 1 },
{ EHOSTUNREACH, 1 },
{ ENETUNREACH, 0 },
{ EHOSTUNREACH, 0 },
{ EHOSTUNREACH, 1 },
{ EHOSTUNREACH, 1 },
{ EHOSTUNREACH, 1 }
};
int sysctl_icmp_echo_ignore_all = 0;
int sysctl_icmp_echo_ignore_broadcasts = 0;
int sysctl_icmp_ignore_bogus_error_responses =0;
extern int sysctl_ip_always_defrag;
struct icmp_control
{
unsigned long *output;
unsigned long *input;
void (*handler)(struct icmphdr *icmph, struct sk_buff *skb, int len);
short error;
int *timeout;
};
static struct icmp_control icmp_pointers[NR_ICMP_TYPES+1];
struct icmp_bxm
{
void *data_ptr;
int data_len;
struct icmphdr icmph;
unsigned long csum;
struct ip_options replyopts;
unsigned char optbuf[40];
};
struct inode icmp_inode;
struct socket *icmp_socket=&icmp_inode.u.socket_i;
#define XRLIM_BURST_FACTOR 6
int xrlim_allow(struct dst_entry *dst, int timeout)
{
unsigned long now;
now = jiffies;
dst->rate_tokens += now - dst->rate_last;
dst->rate_last = now;
if (dst->rate_tokens > XRLIM_BURST_FACTOR*timeout)
dst->rate_tokens = XRLIM_BURST_FACTOR*timeout;
if (dst->rate_tokens >= timeout) {
dst->rate_tokens -= timeout;
return 1;
}
return 0;
}
static inline int icmpv4_xrlim_allow(struct rtable *rt, int type, int code)
{
struct dst_entry *dst = &rt->u.dst;
if (type > NR_ICMP_TYPES || !icmp_pointers[type].timeout)
return 1;
if (type == ICMP_DEST_UNREACH && code == ICMP_FRAG_NEEDED)
return 1;
if (type == ICMP_REDIRECT)
return 1;
if (dst->dev && (dst->dev->flags&IFF_LOOPBACK))
return 1;
return xrlim_allow(dst, *(icmp_pointers[type].timeout));
}
static void icmp_out_count(int type)
{
if (type>NR_ICMP_TYPES)
return;
(*icmp_pointers[type].output)++;
icmp_statistics.IcmpOutMsgs++;
}
static int icmp_glue_bits(const void *p, char *to, unsigned int offset, unsigned int fraglen)
{
struct icmp_bxm *icmp_param = (struct icmp_bxm *)p;
struct icmphdr *icmph;
unsigned long csum;
if (offset) {
icmp_param->csum=csum_partial_copy(icmp_param->data_ptr+offset-sizeof(struct icmphdr),
to, fraglen,icmp_param->csum);
return 0;
}
csum = csum_partial_copy((void *)&icmp_param->icmph,
to, sizeof(struct icmphdr),
icmp_param->csum);
csum = csum_partial_copy(icmp_param->data_ptr,
to+sizeof(struct icmphdr),
fraglen-sizeof(struct icmphdr), csum);
icmph=(struct icmphdr *)to;
icmph->checksum = csum_fold(csum);
return 0;
}
static void icmp_reply(struct icmp_bxm *icmp_param, struct sk_buff *skb)
{
struct sock *sk=icmp_socket->sk;
struct ipcm_cookie ipc;
struct rtable *rt = (struct rtable*)skb->dst;
u32 daddr;
if (ip_options_echo(&icmp_param->replyopts, skb))
return;
icmp_param->icmph.checksum=0;
icmp_param->csum=0;
icmp_out_count(icmp_param->icmph.type);
sk->ip_tos = skb->nh.iph->tos;
daddr = ipc.addr = rt->rt_src;
ipc.opt = &icmp_param->replyopts;
if (ipc.opt->srr)
daddr = icmp_param->replyopts.faddr;
if (ip_route_output(&rt, daddr, rt->rt_spec_dst, RT_TOS(skb->nh.iph->tos), 0))
return;
ip_build_xmit(sk, icmp_glue_bits, icmp_param,
icmp_param->data_len+sizeof(struct icmphdr),
&ipc, rt, MSG_DONTWAIT);
ip_rt_put(rt);
}
void icmp_send(struct sk_buff *skb_in, int type, int code, unsigned long info)
{
struct iphdr *iph;
struct icmphdr *icmph;
int room;
struct icmp_bxm icmp_param;
struct rtable *rt = (struct rtable*)skb_in->dst;
struct ipcm_cookie ipc;
u32 saddr;
u8 tos;
iph = skb_in->nh.iph;
if (skb_in->pkt_type!=PACKET_HOST)
return;
if (!rt) {
if (sysctl_ip_always_defrag == 0 &&
net_ratelimit())
printk(KERN_DEBUG "icmp_send: destinationless packet\n");
return;
}
if (rt->rt_flags&(RTCF_BROADCAST|RTCF_MULTICAST))
return;
if (iph->frag_off&htons(IP_OFFSET))
return;
if (icmp_pointers[type].error) {
if (iph->protocol==IPPROTO_ICMP) {
icmph = (struct icmphdr *)((char *)iph + (iph->ihl<<2));
if (icmph->type>NR_ICMP_TYPES || icmp_pointers[icmph->type].error)
return;
}
}
#ifdef CONFIG_IP_ROUTE_NAT
if (rt->rt_flags&RTCF_NAT && IPCB(skb_in)->flags&IPSKB_TRANSLATED) {
iph->daddr = rt->key.dst;
iph->saddr = rt->key.src;
}
#endif
#ifdef CONFIG_IP_MASQUERADE
if (type==ICMP_DEST_UNREACH && IPCB(skb_in)->flags&IPSKB_MASQUERADED) {
ip_fw_unmasq_icmp(skb_in);
}
#endif
saddr = iph->daddr;
if (!(rt->rt_flags & RTCF_LOCAL))
saddr = 0;
tos = icmp_pointers[type].error ?
((iph->tos & IPTOS_TOS_MASK) | IPTOS_PREC_INTERNETCONTROL) :
iph->tos;
if (ip_route_output(&rt, iph->saddr, saddr, RT_TOS(tos), 0))
return;
if (ip_options_echo(&icmp_param.replyopts, skb_in))
goto ende;
icmp_param.icmph.type=type;
icmp_param.icmph.code=code;
icmp_param.icmph.un.gateway = info;
icmp_param.icmph.checksum=0;
icmp_param.csum=0;
icmp_param.data_ptr=iph;
icmp_out_count(icmp_param.icmph.type);
icmp_socket->sk->ip_tos = tos;
ipc.addr = iph->saddr;
ipc.opt = &icmp_param.replyopts;
if (icmp_param.replyopts.srr) {
ip_rt_put(rt);
if (ip_route_output(&rt, icmp_param.replyopts.faddr, saddr, RT_TOS(tos), 0))
return;
}
if (!icmpv4_xrlim_allow(rt, type, code))
goto ende;
room = rt->u.dst.pmtu;
if (room > 576)
room = 576;
room -= sizeof(struct iphdr) + icmp_param.replyopts.optlen;
room -= sizeof(struct icmphdr);
icmp_param.data_len=(iph->ihl<<2)+skb_in->len;
if (icmp_param.data_len > room)
icmp_param.data_len = room;
ip_build_xmit(icmp_socket->sk, icmp_glue_bits, &icmp_param,
icmp_param.data_len+sizeof(struct icmphdr),
&ipc, rt, MSG_DONTWAIT);
ende:
ip_rt_put(rt);
}
static void icmp_unreach(struct icmphdr *icmph, struct sk_buff *skb, int len)
{
struct iphdr *iph;
int hash;
struct inet_protocol *ipprot;
unsigned char *dp;
struct sock *raw_sk;
if(len<sizeof(struct iphdr)) {
icmp_statistics.IcmpInErrors++;
return;
}
iph = (struct iphdr *) (icmph + 1);
dp = (unsigned char*)iph;
if(icmph->type==ICMP_DEST_UNREACH) {
switch(icmph->code & 15) {
case ICMP_NET_UNREACH:
break;
case ICMP_HOST_UNREACH:
break;
case ICMP_PROT_UNREACH:
break;
case ICMP_PORT_UNREACH:
break;
case ICMP_FRAG_NEEDED:
if (ipv4_config.no_pmtu_disc) {
if (sysctl_ip_always_defrag == 0 && net_ratelimit())
printk(KERN_INFO "ICMP: %d.%d.%d.%d: fragmentation needed and DF set.\n",
NIPQUAD(iph->daddr));
} else {
unsigned short new_mtu;
new_mtu = ip_rt_frag_needed(iph, ntohs(icmph->un.frag.mtu));
if (!new_mtu)
return;
icmph->un.frag.mtu = htons(new_mtu);
}
break;
case ICMP_SR_FAILED:
if (sysctl_ip_always_defrag == 0 && net_ratelimit())
printk(KERN_INFO "ICMP: %d.%d.%d.%d: Source Route Failed.\n", NIPQUAD(iph->daddr));
break;
default:
break;
}
if (icmph->code>NR_ICMP_UNREACH)
return;
}
if (!sysctl_icmp_ignore_bogus_error_responses)
{
if (inet_addr_type(iph->daddr) == RTN_BROADCAST)
{
if (net_ratelimit())
printk(KERN_WARNING "%d.%d.%d.%d sent an invalid ICMP error to a broadcast.\n",
NIPQUAD(skb->nh.iph->saddr));
return;
}
}
hash = iph->protocol & (MAX_INET_PROTOS - 1);
if ((raw_sk = raw_v4_htable[hash]) != NULL)
{
while ((raw_sk = raw_v4_lookup(raw_sk, iph->protocol, iph->saddr,
iph->daddr, skb->dev->ifindex)) != NULL) {
raw_err(raw_sk, skb);
raw_sk = raw_sk->next;
}
}
ipprot = (struct inet_protocol *) inet_protos[hash];
while(ipprot != NULL) {
struct inet_protocol *nextip;
nextip = (struct inet_protocol *) ipprot->next;
if (iph->protocol == ipprot->protocol && ipprot->err_handler)
ipprot->err_handler(skb, dp, len);
ipprot = nextip;
}
}
static void icmp_redirect(struct icmphdr *icmph, struct sk_buff *skb, int len)
{
struct iphdr *iph;
unsigned long ip;
if (len < sizeof(struct iphdr)) {
icmp_statistics.IcmpInErrors++;
return;
}
iph = (struct iphdr *) (icmph + 1);
ip = iph->daddr;
switch(icmph->code & 7) {
case ICMP_REDIR_NET:
case ICMP_REDIR_NETTOS:
case ICMP_REDIR_HOST:
case ICMP_REDIR_HOSTTOS:
ip_rt_redirect(skb->nh.iph->saddr, ip, icmph->un.gateway, iph->saddr, iph->tos, skb->dev);
break;
default:
break;
}
}
static void icmp_echo(struct icmphdr *icmph, struct sk_buff *skb, int len)
{
if (!sysctl_icmp_echo_ignore_all) {
struct icmp_bxm icmp_param;
icmp_param.icmph=*icmph;
icmp_param.icmph.type=ICMP_ECHOREPLY;
icmp_param.data_ptr=(icmph+1);
icmp_param.data_len=len;
icmp_reply(&icmp_param, skb);
}
}
static void icmp_timestamp(struct icmphdr *icmph, struct sk_buff *skb, int len)
{
struct timeval tv;
__u32 times[3];
struct icmp_bxm icmp_param;
if(len<12) {
icmp_statistics.IcmpInErrors++;
return;
}
do_gettimeofday(&tv);
times[1] = htonl((tv.tv_sec % 86400) * 1000 + tv.tv_usec / 1000);
times[2] = times[1];
memcpy((void *)&times[0], icmph+1, 4);
icmp_param.icmph=*icmph;
icmp_param.icmph.type=ICMP_TIMESTAMPREPLY;
icmp_param.icmph.code=0;
icmp_param.data_ptr=&times;
icmp_param.data_len=12;
icmp_reply(&icmp_param, skb);
}
static void icmp_address(struct icmphdr *icmph, struct sk_buff *skb, int len)
{
#if 0
if (sysctl_ip_always_defrag == 0 && net_ratelimit())
printk(KERN_DEBUG "a guy asks for address mask. Who is it?\n");
#endif
}
static void icmp_address_reply(struct icmphdr *icmph, struct sk_buff *skb, int len)
{
struct rtable *rt = (struct rtable*)skb->dst;
struct device *dev = skb->dev;
struct in_device *in_dev = dev->ip_ptr;
struct in_ifaddr *ifa;
u32 mask;
if (!in_dev || !in_dev->ifa_list ||
!IN_DEV_LOG_MARTIANS(in_dev) ||
!IN_DEV_FORWARD(in_dev) ||
len < 4 ||
!(rt->rt_flags&RTCF_DIRECTSRC))
return;
mask = *(u32*)&icmph[1];
for (ifa=in_dev->ifa_list; ifa; ifa = ifa->ifa_next) {
if (mask == ifa->ifa_mask && inet_ifa_match(rt->rt_src, ifa))
return;
}
if (sysctl_ip_always_defrag == 0 && net_ratelimit())
printk(KERN_INFO "Wrong address mask %08X from %08X/%s\n",
ntohl(mask), ntohl(rt->rt_src), dev->name);
}
static void icmp_discard(struct icmphdr *icmph, struct sk_buff *skb, int len)
{
}
#ifdef CONFIG_IP_TRANSPARENT_PROXY
extern struct sock *tcp_v4_lookup(u32 saddr, u16 sport, u32 daddr, u16 dport, int dif);
extern struct sock *udp_v4_lookup(u32 saddr, u16 sport, u32 daddr, u16 dport, int dif);
int icmp_chkaddr(struct sk_buff *skb)
{
struct icmphdr *icmph=(struct icmphdr *)(skb->nh.raw + skb->nh.iph->ihl*4);
struct iphdr *iph = (struct iphdr *) (icmph + 1);
void (*handler)(struct icmphdr *icmph, struct sk_buff *skb, int len) = icmp_pointers[icmph->type].handler;
if (handler == icmp_unreach || handler == icmp_redirect) {
struct sock *sk;
switch (iph->protocol) {
case IPPROTO_TCP:
{
struct tcphdr *th = (struct tcphdr *)(((unsigned char *)iph)+(iph->ihl<<2));
sk = tcp_v4_lookup(iph->daddr, th->dest, iph->saddr, th->source, skb->dev->ifindex);
if (!sk || (sk->state == TCP_LISTEN))
return 0;
return 1;
}
case IPPROTO_UDP:
{
struct udphdr *uh = (struct udphdr *)(((unsigned char *)iph)+(iph->ihl<<2));
sk = udp_v4_lookup(iph->daddr, uh->dest, iph->saddr, uh->source, skb->dev->ifindex);
if (!sk) return 0;
if (sk->saddr != iph->saddr && inet_addr_type(iph->saddr) != RTN_LOCAL)
return 0;
return 1;
}
}
}
return 0;
}
#endif
int icmp_rcv(struct sk_buff *skb, unsigned short len)
{
struct icmphdr *icmph = skb->h.icmph;
struct rtable *rt = (struct rtable*)skb->dst;
icmp_statistics.IcmpInMsgs++;
if(len < sizeof(struct icmphdr) ||
ip_compute_csum((unsigned char *) icmph, len) ||
icmph->type > NR_ICMP_TYPES)
goto error;
if (rt->rt_flags&(RTCF_BROADCAST|RTCF_MULTICAST)) {
if (icmph->type == ICMP_ECHO &&
sysctl_icmp_echo_ignore_broadcasts) {
goto error;
}
if (icmph->type != ICMP_ECHO &&
icmph->type != ICMP_TIMESTAMP &&
icmph->type != ICMP_ADDRESS &&
icmph->type != ICMP_ADDRESSREPLY) {
goto error;
}
}
len -= sizeof(struct icmphdr);
(*icmp_pointers[icmph->type].input)++;
(icmp_pointers[icmph->type].handler)(icmph, skb, len);
drop:
kfree_skb(skb);
return 0;
error:
icmp_statistics.IcmpInErrors++;
goto drop;
}
static unsigned long dummy;
int sysctl_icmp_destunreach_time = 1*HZ;
int sysctl_icmp_timeexceed_time = 1*HZ;
int sysctl_icmp_paramprob_time = 1*HZ;
int sysctl_icmp_echoreply_time = 0;
static struct icmp_control icmp_pointers[NR_ICMP_TYPES+1] = {
{ &icmp_statistics.IcmpOutEchoReps, &icmp_statistics.IcmpInEchoReps, icmp_discard, 0, &sysctl_icmp_echoreply_time},
{ &dummy, &icmp_statistics.IcmpInErrors, icmp_discard, 1, },
{ &dummy, &icmp_statistics.IcmpInErrors, icmp_discard, 1, },
{ &icmp_statistics.IcmpOutDestUnreachs, &icmp_statistics.IcmpInDestUnreachs, icmp_unreach, 1, &sysctl_icmp_destunreach_time },
{ &icmp_statistics.IcmpOutSrcQuenchs, &icmp_statistics.IcmpInSrcQuenchs, icmp_unreach, 1, },
{ &icmp_statistics.IcmpOutRedirects, &icmp_statistics.IcmpInRedirects, icmp_redirect, 1, },
{ &dummy, &icmp_statistics.IcmpInErrors, icmp_discard, 1, },
{ &dummy, &icmp_statistics.IcmpInErrors, icmp_discard, 1, },
{ &icmp_statistics.IcmpOutEchos, &icmp_statistics.IcmpInEchos, icmp_echo, 0, },
{ &dummy, &icmp_statistics.IcmpInErrors, icmp_discard, 1, },
{ &dummy, &icmp_statistics.IcmpInErrors, icmp_discard, 1, },
{ &icmp_statistics.IcmpOutTimeExcds, &icmp_statistics.IcmpInTimeExcds, icmp_unreach, 1, &sysctl_icmp_timeexceed_time },
{ &icmp_statistics.IcmpOutParmProbs, &icmp_statistics.IcmpInParmProbs, icmp_unreach, 1, &sysctl_icmp_paramprob_time },
{ &icmp_statistics.IcmpOutTimestamps, &icmp_statistics.IcmpInTimestamps, icmp_timestamp, 0, },
{ &icmp_statistics.IcmpOutTimestampReps, &icmp_statistics.IcmpInTimestampReps, icmp_discard, 0, },
{ &dummy, &dummy, icmp_discard, 0, },
{ &dummy, &dummy, icmp_discard, 0, },
{ &icmp_statistics.IcmpOutAddrMasks, &icmp_statistics.IcmpInAddrMasks, icmp_address, 0, },
{ &icmp_statistics.IcmpOutAddrMaskReps, &icmp_statistics.IcmpInAddrMaskReps, icmp_address_reply, 0, }
};
__initfunc(void icmp_init(struct net_proto_family *ops))
{
int err;
icmp_inode.i_mode = S_IFSOCK;
icmp_inode.i_sock = 1;
icmp_inode.i_uid = 0;
icmp_inode.i_gid = 0;
icmp_socket->inode = &icmp_inode;
icmp_socket->state = SS_UNCONNECTED;
icmp_socket->type=SOCK_RAW;
if ((err=ops->create(icmp_socket, IPPROTO_ICMP))<0)
panic("Failed to create the ICMP control socket.\n");
icmp_socket->sk->allocation=GFP_ATOMIC;
icmp_socket->sk->num = 256;
icmp_socket->sk->ip_ttl = MAXTTL;
}