#include <linux/errno.h>
#include <linux/types.h>
#include <linux/socket.h>
#include <linux/sockios.h>
#include <linux/sched.h>
#include <linux/net.h>
#include <linux/netdevice.h>
#include <linux/in6.h>
#include <linux/icmpv6.h>
#include <net/sock.h>
#include <net/snmp.h>
#include <net/ipv6.h>
#include <net/protocol.h>
#include <net/transp_v6.h>
#include <net/rawv6.h>
#include <net/ndisc.h>
#include <net/ip6_route.h>
#include <net/addrconf.h>
#include <asm/uaccess.h>
struct hdrtype_proc
{
int type;
u8* (*func) (struct sk_buff **, u8 *ptr);
};
struct tlvtype_proc
{
int type;
int (*func) (struct sk_buff *, __u8 *ptr);
};
int ip6_tlvopt_unknown(struct sk_buff *skb, u8 *opt)
{
switch ((opt[0] & 0xC0) >> 6) {
case 0:
return 1;
case 1:
break;
case 3:
if (ipv6_addr_is_multicast(&skb->nh.ipv6h->daddr))
break;
case 2:
icmpv6_param_prob(skb, ICMPV6_UNK_OPTION, opt);
return 0;
};
kfree_skb(skb);
return 0;
}
static int ip6_parse_tlv(struct tlvtype_proc *procs, struct sk_buff *skb,
__u8 *nhptr)
{
struct tlvtype_proc *curr;
u8 *ptr = skb->h.raw;
int len = ((ptr[1]+1)<<3) - 2;
ptr += 2;
if (skb->tail - (ptr + len) < 0) {
kfree_skb(skb);
return 0;
}
while (len > 0) {
int optlen = ptr[1]+2;
switch (ptr[0]) {
case IPV6_TLV_PAD0:
optlen = 1;
break;
case IPV6_TLV_PADN:
break;
default:
for (curr=procs; curr->type >= 0; curr++) {
if (curr->type == ptr[0]) {
if (curr->func(skb, ptr) == 0)
return 0;
break;
}
}
if (curr->type < 0) {
if (ip6_tlvopt_unknown(skb, ptr) == 0)
return 0;
}
break;
}
ptr += optlen;
len -= optlen;
}
if (len == 0)
return 1;
kfree_skb(skb);
return 0;
}
struct tlvtype_proc tlvprocdestopt_lst[] = {
{-1, NULL}
};
static u8 *ipv6_dest_opt(struct sk_buff **skb_ptr, u8 *nhptr)
{
struct sk_buff *skb=*skb_ptr;
struct inet6_skb_parm *opt = (struct inet6_skb_parm *)skb->cb;
struct ipv6_destopt_hdr *hdr = (struct ipv6_destopt_hdr *) skb->h.raw;
opt->dst1 = (u8*)hdr - skb->nh.raw;
if (ip6_parse_tlv(tlvprocdestopt_lst, skb, nhptr)) {
skb->h.raw += ((hdr->hdrlen+1)<<3);
return &hdr->nexthdr;
}
return NULL;
}
static u8 *ipv6_nodata(struct sk_buff **skb_ptr, u8 *nhptr)
{
kfree_skb(*skb_ptr);
return NULL;
}
static u8* ipv6_routing_header(struct sk_buff **skb_ptr, u8 *nhptr)
{
struct sk_buff *skb = *skb_ptr;
struct inet6_skb_parm *opt = (struct inet6_skb_parm *)skb->cb;
struct in6_addr *addr;
struct in6_addr daddr;
int addr_type;
int n, i;
struct ipv6_rt_hdr *hdr = (struct ipv6_rt_hdr *) skb->h.raw;
struct rt0_hdr *rthdr;
if (((hdr->hdrlen+1)<<3) > skb->tail - skb->h.raw) {
ipv6_statistics.Ip6InHdrErrors++;
kfree_skb(skb);
return NULL;
}
looped_back:
if (hdr->segments_left == 0) {
opt->srcrt = (u8*)hdr - skb->nh.raw;
skb->h.raw += (hdr->hdrlen + 1) << 3;
opt->dst0 = opt->dst1;
opt->dst1 = 0;
return &hdr->nexthdr;
}
if (hdr->type != IPV6_SRCRT_TYPE_0 || hdr->hdrlen & 0x01) {
u8 *pos = (u8*) hdr;
if (hdr->type != IPV6_SRCRT_TYPE_0)
pos += 2;
else
pos += 1;
icmpv6_param_prob(skb, ICMPV6_HDR_FIELD, pos);
return NULL;
}
n = hdr->hdrlen >> 1;
if (hdr->segments_left > n) {
icmpv6_param_prob(skb, ICMPV6_HDR_FIELD, &hdr->segments_left);
return NULL;
}
if (skb_cloned(skb)) {
struct sk_buff *skb2 = skb_copy(skb, GFP_ATOMIC);
kfree_skb(skb);
if (skb2 == NULL)
return NULL;
*skb_ptr = skb = skb2;
opt = (struct inet6_skb_parm *)skb2->cb;
hdr = (struct ipv6_rt_hdr *) skb2->h.raw;
}
i = n - --hdr->segments_left;
rthdr = (struct rt0_hdr *) hdr;
addr = rthdr->addr;
addr += i - 1;
addr_type = ipv6_addr_type(addr);
if (addr_type == IPV6_ADDR_MULTICAST) {
kfree_skb(skb);
return NULL;
}
ipv6_addr_copy(&daddr, addr);
ipv6_addr_copy(addr, &skb->nh.ipv6h->daddr);
ipv6_addr_copy(&skb->nh.ipv6h->daddr, &daddr);
dst_release(xchg(&skb->dst, NULL));
ip6_route_input(skb);
if (skb->dst->error) {
skb->dst->input(skb);
return NULL;
}
if (skb->dst->dev->flags&IFF_LOOPBACK) {
if (skb->nh.ipv6h->hop_limit <= 1) {
icmpv6_send(skb, ICMPV6_TIME_EXCEED, ICMPV6_EXC_HOPLIMIT,
0, skb->dev);
kfree_skb(skb);
return NULL;
}
skb->nh.ipv6h->hop_limit--;
goto looped_back;
}
skb->dst->input(skb);
return NULL;
}
struct ipv6_txoptions *
ipv6_invert_rthdr(struct sock *sk, struct ipv6_rt_hdr *hdr)
{
int n, i;
struct rt0_hdr *rthdr = (struct rt0_hdr*)hdr;
struct rt0_hdr *irthdr;
struct ipv6_txoptions *opt;
int hdrlen = ipv6_optlen(hdr);
if (hdr->segments_left ||
hdr->type != IPV6_SRCRT_TYPE_0 ||
hdr->hdrlen & 0x01)
return NULL;
n = hdr->hdrlen >> 1;
opt = sock_kmalloc(sk, sizeof(*opt) + hdrlen, GFP_ATOMIC);
if (opt == NULL)
return NULL;
memset(opt, 0, sizeof(*opt));
opt->tot_len = sizeof(*opt) + hdrlen;
opt->srcrt = (void*)(opt+1);
opt->opt_nflen = hdrlen;
memcpy(opt->srcrt, hdr, sizeof(*hdr));
irthdr = (struct rt0_hdr*)opt->srcrt;
irthdr->bitmap = 0;
opt->srcrt->segments_left = n;
for (i=0; i<n; i++)
memcpy(irthdr->addr+i, rthdr->addr+(n-1-i), 16);
return opt;
}
static u8 *ipv6_auth_hdr(struct sk_buff **skb_ptr, u8 *nhptr)
{
struct sk_buff *skb=*skb_ptr;
struct inet6_skb_parm *opt = (struct inet6_skb_parm *)skb->cb;
struct ipv6_opt_hdr *hdr = (struct ipv6_opt_hdr *)skb->h.raw;
int len = (hdr->hdrlen+2)<<2;
if (len&7)
return NULL;
opt->auth = (u8*)hdr - skb->nh.raw;
if (skb->h.raw + len > skb->tail)
return NULL;
skb->h.raw += len;
return &hdr->nexthdr;
}
struct hdrtype_proc hdrproc_lst[] = {
{NEXTHDR_FRAGMENT, ipv6_reassembly},
{NEXTHDR_ROUTING, ipv6_routing_header},
{NEXTHDR_DEST, ipv6_dest_opt},
{NEXTHDR_NONE, ipv6_nodata},
{NEXTHDR_AUTH, ipv6_auth_hdr},
{-1, NULL}
};
u8 *ipv6_parse_exthdrs(struct sk_buff **skb_in, u8 *nhptr)
{
struct hdrtype_proc *hdrt;
u8 nexthdr = *nhptr;
restart:
for (hdrt=hdrproc_lst; hdrt->type >= 0; hdrt++) {
if (hdrt->type == nexthdr) {
if ((nhptr = hdrt->func(skb_in, nhptr)) != NULL) {
nexthdr = *nhptr;
goto restart;
}
return NULL;
}
}
return nhptr;
}
static int ipv6_hop_ra(struct sk_buff *skb, u8 *ptr)
{
if (ptr[1] == 2) {
((struct inet6_skb_parm*)skb->cb)->ra = ptr - skb->nh.raw;
return 1;
}
if (net_ratelimit())
printk(KERN_DEBUG "ipv6_hop_ra: wrong RA length %d\n", ptr[1]);
kfree_skb(skb);
return 0;
}
static int ipv6_hop_jumbo(struct sk_buff *skb, u8 *ptr)
{
u32 pkt_len;
if (ptr[1] != 4 || ((ptr-skb->nh.raw)&3) != 2) {
if (net_ratelimit())
printk(KERN_DEBUG "ipv6_hop_jumbo: wrong jumbo opt length/alignment %d\n", ptr[1]);
goto drop;
}
pkt_len = ntohl(*(u32*)(ptr+2));
if (pkt_len < 0x10000) {
icmpv6_param_prob(skb, ICMPV6_HDR_FIELD, ptr+2);
return 0;
}
if (skb->nh.ipv6h->payload_len) {
icmpv6_param_prob(skb, ICMPV6_HDR_FIELD, ptr);
return 0;
}
if (pkt_len > skb->len - sizeof(struct ipv6hdr)) {
ipv6_statistics.Ip6InTruncatedPkts++;
goto drop;
}
skb_trim(skb, pkt_len + sizeof(struct ipv6hdr));
return 1;
drop:
kfree_skb(skb);
return 0;
}
struct tlvtype_proc tlvprochopopt_lst[] = {
{IPV6_TLV_ROUTERALERT, ipv6_hop_ra},
{IPV6_TLV_JUMBO, ipv6_hop_jumbo},
{-1, NULL}
};
u8 * ipv6_parse_hopopts(struct sk_buff *skb, u8 *nhptr)
{
((struct inet6_skb_parm*)skb->cb)->hop = sizeof(struct ipv6hdr);
if (ip6_parse_tlv(tlvprochopopt_lst, skb, nhptr))
return nhptr+((nhptr[1]+1)<<3);
return NULL;
}
u8 *ipv6_build_rthdr(struct sk_buff *skb, u8 *prev_hdr,
struct ipv6_rt_hdr *opt, struct in6_addr *addr)
{
struct rt0_hdr *phdr, *ihdr;
int hops;
ihdr = (struct rt0_hdr *) opt;
phdr = (struct rt0_hdr *) skb_put(skb, (ihdr->rt_hdr.hdrlen + 1) << 3);
memcpy(phdr, ihdr, sizeof(struct rt0_hdr));
hops = ihdr->rt_hdr.hdrlen >> 1;
if (hops > 1)
memcpy(phdr->addr, ihdr->addr + 1,
(hops - 1) * sizeof(struct in6_addr));
ipv6_addr_copy(phdr->addr + (hops - 1), addr);
phdr->rt_hdr.nexthdr = *prev_hdr;
*prev_hdr = NEXTHDR_ROUTING;
return &phdr->rt_hdr.nexthdr;
}
static u8 *ipv6_build_exthdr(struct sk_buff *skb, u8 *prev_hdr, u8 type, struct ipv6_opt_hdr *opt)
{
struct ipv6_opt_hdr *h = (struct ipv6_opt_hdr *)skb_put(skb, ipv6_optlen(opt));
memcpy(h, opt, ipv6_optlen(opt));
h->nexthdr = *prev_hdr;
*prev_hdr = type;
return &h->nexthdr;
}
static u8 *ipv6_build_authhdr(struct sk_buff *skb, u8 *prev_hdr, struct ipv6_opt_hdr *opt)
{
struct ipv6_opt_hdr *h = (struct ipv6_opt_hdr *)skb_put(skb, (opt->hdrlen+2)<<2);
memcpy(h, opt, (opt->hdrlen+2)<<2);
h->nexthdr = *prev_hdr;
*prev_hdr = NEXTHDR_AUTH;
return &h->nexthdr;
}
u8 *ipv6_build_nfrag_opts(struct sk_buff *skb, u8 *prev_hdr, struct ipv6_txoptions *opt,
struct in6_addr *daddr, u32 jumbolen)
{
struct ipv6_opt_hdr *h = (struct ipv6_opt_hdr *)skb->data;
if (opt && opt->hopopt)
prev_hdr = ipv6_build_exthdr(skb, prev_hdr, NEXTHDR_HOP, opt->hopopt);
if (jumbolen) {
u8 *jumboopt = (u8 *)skb_put(skb, 8);
if (opt && opt->hopopt) {
*jumboopt++ = IPV6_TLV_PADN;
*jumboopt++ = 0;
h->hdrlen++;
} else {
h = (struct ipv6_opt_hdr *)jumboopt;
h->nexthdr = *prev_hdr;
h->hdrlen = 0;
jumboopt += 2;
*prev_hdr = NEXTHDR_HOP;
prev_hdr = &h->nexthdr;
}
jumboopt[0] = IPV6_TLV_JUMBO;
jumboopt[1] = 4;
*(u32*)(jumboopt+2) = htonl(jumbolen);
}
if (opt) {
if (opt->dst0opt)
prev_hdr = ipv6_build_exthdr(skb, prev_hdr, NEXTHDR_DEST, opt->dst0opt);
if (opt->srcrt)
prev_hdr = ipv6_build_rthdr(skb, prev_hdr, opt->srcrt, daddr);
}
return prev_hdr;
}
u8 *ipv6_build_frag_opts(struct sk_buff *skb, u8 *prev_hdr, struct ipv6_txoptions *opt)
{
if (opt->auth)
prev_hdr = ipv6_build_authhdr(skb, prev_hdr, opt->auth);
if (opt->dst1opt)
prev_hdr = ipv6_build_exthdr(skb, prev_hdr, NEXTHDR_DEST, opt->dst1opt);
return prev_hdr;
}
static void ipv6_push_rthdr(struct sk_buff *skb, u8 *proto,
struct ipv6_rt_hdr *opt,
struct in6_addr **addr_p)
{
struct rt0_hdr *phdr, *ihdr;
int hops;
ihdr = (struct rt0_hdr *) opt;
phdr = (struct rt0_hdr *) skb_push(skb, (ihdr->rt_hdr.hdrlen + 1) << 3);
memcpy(phdr, ihdr, sizeof(struct rt0_hdr));
hops = ihdr->rt_hdr.hdrlen >> 1;
if (hops > 1)
memcpy(phdr->addr, ihdr->addr + 1,
(hops - 1) * sizeof(struct in6_addr));
ipv6_addr_copy(phdr->addr + (hops - 1), *addr_p);
*addr_p = ihdr->addr;
phdr->rt_hdr.nexthdr = *proto;
*proto = NEXTHDR_ROUTING;
}
static void ipv6_push_exthdr(struct sk_buff *skb, u8 *proto, u8 type, struct ipv6_opt_hdr *opt)
{
struct ipv6_opt_hdr *h = (struct ipv6_opt_hdr *)skb_push(skb, ipv6_optlen(opt));
memcpy(h, opt, ipv6_optlen(opt));
h->nexthdr = *proto;
*proto = type;
}
static void ipv6_push_authhdr(struct sk_buff *skb, u8 *proto, struct ipv6_opt_hdr *opt)
{
struct ipv6_opt_hdr *h = (struct ipv6_opt_hdr *)skb_push(skb, (opt->hdrlen+2)<<2);
memcpy(h, opt, (opt->hdrlen+2)<<2);
h->nexthdr = *proto;
*proto = NEXTHDR_AUTH;
}
void ipv6_push_nfrag_opts(struct sk_buff *skb, struct ipv6_txoptions *opt,
u8 *proto,
struct in6_addr **daddr)
{
if (opt->srcrt)
ipv6_push_rthdr(skb, proto, opt->srcrt, daddr);
if (opt->dst0opt)
ipv6_push_exthdr(skb, proto, NEXTHDR_DEST, opt->dst0opt);
if (opt->hopopt)
ipv6_push_exthdr(skb, proto, NEXTHDR_HOP, opt->hopopt);
}
void ipv6_push_frag_opts(struct sk_buff *skb, struct ipv6_txoptions *opt, u8 *proto)
{
if (opt->dst1opt)
ipv6_push_exthdr(skb, proto, NEXTHDR_DEST, opt->dst1opt);
if (opt->auth)
ipv6_push_authhdr(skb, proto, opt->auth);
}
struct ipv6_txoptions *
ipv6_dup_options(struct sock *sk, struct ipv6_txoptions *opt)
{
struct ipv6_txoptions *opt2;
opt2 = sock_kmalloc(sk, opt->tot_len, GFP_ATOMIC);
if (opt2) {
long dif = (char*)opt2 - (char*)opt;
memcpy(opt2, opt, opt->tot_len);
if (opt2->hopopt)
*((char**)&opt2->hopopt) += dif;
if (opt2->dst0opt)
*((char**)&opt2->dst0opt) += dif;
if (opt2->dst1opt)
*((char**)&opt2->dst1opt) += dif;
if (opt2->auth)
*((char**)&opt2->auth) += dif;
if (opt2->srcrt)
*((char**)&opt2->srcrt) += dif;
}
return opt2;
}
static __inline__ int ipv6_ext_hdr(u8 nexthdr)
{
return ( (nexthdr == NEXTHDR_HOP) ||
(nexthdr == NEXTHDR_ROUTING) ||
(nexthdr == NEXTHDR_FRAGMENT) ||
(nexthdr == NEXTHDR_AUTH) ||
(nexthdr == NEXTHDR_NONE) ||
(nexthdr == NEXTHDR_DEST) );
}
u8 *ipv6_skip_exthdr(struct ipv6_opt_hdr *hdr, u8 *nexthdrp, int len)
{
u8 nexthdr = *nexthdrp;
while (ipv6_ext_hdr(nexthdr)) {
int hdrlen;
if (len < sizeof(struct ipv6_opt_hdr))
return NULL;
if (nexthdr == NEXTHDR_NONE)
return NULL;
if (nexthdr == NEXTHDR_FRAGMENT) {
struct frag_hdr *fhdr = (struct frag_hdr *) hdr;
if (ntohs(fhdr->frag_off) & ~0x7)
break;
hdrlen = 8;
} else if (nexthdr == NEXTHDR_AUTH)
hdrlen = (hdr->hdrlen+2)<<2;
else
hdrlen = ipv6_optlen(hdr);
nexthdr = hdr->nexthdr;
hdr = (struct ipv6_opt_hdr *) ((u8*)hdr + hdrlen);
len -= hdrlen;
}
*nexthdrp = nexthdr;
return (u8*)hdr;
}