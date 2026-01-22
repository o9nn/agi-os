#ifndef _IPV6_H
#define _IPV6_H
#include <linux/config.h>
#include <linux/in6.h>
#include <asm/byteorder.h>
#define IPV6_MIN_MTU	1280
struct in6_ifreq {
struct in6_addr	ifr6_addr;
__u32		ifr6_prefixlen;
int		ifr6_ifindex;
};
#define IPV6_SRCRT_STRICT	0x01
#define IPV6_SRCRT_TYPE_0	0
struct ipv6_rt_hdr {
__u8		nexthdr;
__u8		hdrlen;
__u8		type;
__u8		segments_left;
};
struct ipv6_opt_hdr {
__u8 		nexthdr;
__u8 		hdrlen;
};
#define ipv6_destopt_hdr ipv6_opt_hdr
#define ipv6_hopopt_hdr  ipv6_opt_hdr
#ifdef __KERNEL__
#define ipv6_optlen(p)  (((p)->hdrlen+1) << 3)
#endif
struct rt0_hdr {
struct ipv6_rt_hdr	rt_hdr;
__u32			bitmap;
struct in6_addr		addr[0];
#define rt0_type		rt_hdr.type;
};
struct ipv6hdr {
#if defined(__LITTLE_ENDIAN_BITFIELD)
__u8			priority:4,
version:4;
#elif defined(__BIG_ENDIAN_BITFIELD)
__u8			version:4,
priority:4;
#else
#error	"Please fix <asm/byteorder.h>"
#endif
__u8			flow_lbl[3];
__u16			payload_len;
__u8			nexthdr;
__u8			hop_limit;
struct	in6_addr	saddr;
struct	in6_addr	daddr;
};
#ifdef __KERNEL__
struct inet6_skb_parm
{
int			iif;
__u16			ra;
__u16			hop;
__u16			auth;
__u16			dst0;
__u16			srcrt;
__u16			dst1;
};
#if defined(CONFIG_IPV6) || defined(CONFIG_IPV6_MODULE)
#define __ipv6_only_sock(sk)   (sk->net_pinfo.af_inet6.ipv6only)
#define ipv6_only_sock(sk)     ((sk)->family == PF_INET6 && __ipv6_only_sock(sk))
#else
#define __ipv6_only_sock(sk)   0
#define ipv6_only_sock(sk)     0
#endif
#endif
#endif