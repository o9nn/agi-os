#ifndef	_NETINET_IN_H
#define	_NETINET_IN_H	1
#include <features.h>
#include <bits/stdint-uintn.h>
#include <sys/socket.h>
#include <bits/types.h>
__BEGIN_DECLS
typedef uint32_t in_addr_t;
struct in_addr
{
in_addr_t s_addr;
};
#include <bits/in.h>
enum
{
IPPROTO_IP = 0,
#define IPPROTO_IP		IPPROTO_IP
IPPROTO_ICMP = 1,
#define IPPROTO_ICMP		IPPROTO_ICMP
IPPROTO_IGMP = 2,
#define IPPROTO_IGMP		IPPROTO_IGMP
IPPROTO_IPIP = 4,
#define IPPROTO_IPIP		IPPROTO_IPIP
IPPROTO_TCP = 6,
#define IPPROTO_TCP		IPPROTO_TCP
IPPROTO_EGP = 8,
#define IPPROTO_EGP		IPPROTO_EGP
IPPROTO_PUP = 12,
#define IPPROTO_PUP		IPPROTO_PUP
IPPROTO_UDP = 17,
#define IPPROTO_UDP		IPPROTO_UDP
IPPROTO_IDP = 22,
#define IPPROTO_IDP		IPPROTO_IDP
IPPROTO_TP = 29,
#define IPPROTO_TP		IPPROTO_TP
IPPROTO_DCCP = 33,
#define IPPROTO_DCCP		IPPROTO_DCCP
IPPROTO_IPV6 = 41,
#define IPPROTO_IPV6		IPPROTO_IPV6
IPPROTO_RSVP = 46,
#define IPPROTO_RSVP		IPPROTO_RSVP
IPPROTO_GRE = 47,
#define IPPROTO_GRE		IPPROTO_GRE
IPPROTO_ESP = 50,
#define IPPROTO_ESP		IPPROTO_ESP
IPPROTO_AH = 51,
#define IPPROTO_AH		IPPROTO_AH
IPPROTO_MTP = 92,
#define IPPROTO_MTP		IPPROTO_MTP
IPPROTO_BEETPH = 94,
#define IPPROTO_BEETPH		IPPROTO_BEETPH
IPPROTO_ENCAP = 98,
#define IPPROTO_ENCAP		IPPROTO_ENCAP
IPPROTO_PIM = 103,
#define IPPROTO_PIM		IPPROTO_PIM
IPPROTO_COMP = 108,
#define IPPROTO_COMP		IPPROTO_COMP
IPPROTO_L2TP = 115,
#define IPPROTO_L2TP		IPPROTO_L2TP
IPPROTO_SCTP = 132,
#define IPPROTO_SCTP		IPPROTO_SCTP
IPPROTO_UDPLITE = 136,
#define IPPROTO_UDPLITE		IPPROTO_UDPLITE
IPPROTO_MPLS = 137,
#define IPPROTO_MPLS		IPPROTO_MPLS
IPPROTO_ETHERNET = 143,
#define IPPROTO_ETHERNET	IPPROTO_ETHERNET
IPPROTO_RAW = 255,
#define IPPROTO_RAW		IPPROTO_RAW
IPPROTO_MPTCP = 262,
#define IPPROTO_MPTCP		IPPROTO_MPTCP
IPPROTO_MAX
};
#if !__USE_KERNEL_IPV6_DEFS
enum
{
IPPROTO_HOPOPTS = 0,
#define IPPROTO_HOPOPTS		IPPROTO_HOPOPTS
IPPROTO_ROUTING = 43,
#define IPPROTO_ROUTING		IPPROTO_ROUTING
IPPROTO_FRAGMENT = 44,
#define IPPROTO_FRAGMENT	IPPROTO_FRAGMENT
IPPROTO_ICMPV6 = 58,
#define IPPROTO_ICMPV6		IPPROTO_ICMPV6
IPPROTO_NONE = 59,
#define IPPROTO_NONE		IPPROTO_NONE
IPPROTO_DSTOPTS = 60,
#define IPPROTO_DSTOPTS		IPPROTO_DSTOPTS
IPPROTO_MH = 135
#define IPPROTO_MH		IPPROTO_MH
};
#endif
typedef uint16_t in_port_t;
enum
{
IPPORT_ECHO = 7,
IPPORT_DISCARD = 9,
IPPORT_SYSTAT = 11,
IPPORT_DAYTIME = 13,
IPPORT_NETSTAT = 15,
IPPORT_FTP = 21,
IPPORT_TELNET = 23,
IPPORT_SMTP = 25,
IPPORT_TIMESERVER = 37,
IPPORT_NAMESERVER = 42,
IPPORT_WHOIS = 43,
IPPORT_MTP = 57,
IPPORT_TFTP = 69,
IPPORT_RJE = 77,
IPPORT_FINGER = 79,
IPPORT_TTYLINK = 87,
IPPORT_SUPDUP = 95,
IPPORT_EXECSERVER = 512,
IPPORT_LOGINSERVER = 513,
IPPORT_CMDSERVER = 514,
IPPORT_EFSSERVER = 520,
IPPORT_BIFFUDP = 512,
IPPORT_WHOSERVER = 513,
IPPORT_ROUTESERVER = 520,
IPPORT_RESERVED = 1024,
IPPORT_USERRESERVED = 5000
};
#define	IN_CLASSA(a)		((((in_addr_t)(a)) & 0x80000000) == 0)
#define	IN_CLASSA_NET		0xff000000
#define	IN_CLASSA_NSHIFT	24
#define	IN_CLASSA_HOST		(0xffffffff & ~IN_CLASSA_NET)
#define	IN_CLASSA_MAX		128
#define	IN_CLASSB(a)		((((in_addr_t)(a)) & 0xc0000000) == 0x80000000)
#define	IN_CLASSB_NET		0xffff0000
#define	IN_CLASSB_NSHIFT	16
#define	IN_CLASSB_HOST		(0xffffffff & ~IN_CLASSB_NET)
#define	IN_CLASSB_MAX		65536
#define	IN_CLASSC(a)		((((in_addr_t)(a)) & 0xe0000000) == 0xc0000000)
#define	IN_CLASSC_NET		0xffffff00
#define	IN_CLASSC_NSHIFT	8
#define	IN_CLASSC_HOST		(0xffffffff & ~IN_CLASSC_NET)
#define	IN_CLASSD(a)		((((in_addr_t)(a)) & 0xf0000000) == 0xe0000000)
#define	IN_MULTICAST(a)		IN_CLASSD(a)
#define	IN_EXPERIMENTAL(a)	((((in_addr_t)(a)) & 0xe0000000) == 0xe0000000)
#define	IN_BADCLASS(a)		((((in_addr_t)(a)) & 0xf0000000) == 0xf0000000)
#define	INADDR_ANY		((in_addr_t) 0x00000000)
#define	INADDR_BROADCAST	((in_addr_t) 0xffffffff)
#define	INADDR_NONE		((in_addr_t) 0xffffffff)
#define	INADDR_DUMMY		((in_addr_t) 0xc0000008)
#define	IN_LOOPBACKNET		127
#ifndef INADDR_LOOPBACK
# define INADDR_LOOPBACK	((in_addr_t) 0x7f000001)
#endif
#define INADDR_UNSPEC_GROUP	((in_addr_t) 0xe0000000)
#define INADDR_ALLHOSTS_GROUP	((in_addr_t) 0xe0000001)
#define INADDR_ALLRTRS_GROUP    ((in_addr_t) 0xe0000002)
#define INADDR_ALLSNOOPERS_GROUP ((in_addr_t) 0xe000006a)
#define INADDR_MAX_LOCAL_GROUP  ((in_addr_t) 0xe00000ff)
#if !__USE_KERNEL_IPV6_DEFS
struct in6_addr
{
union
{
uint8_t	__u6_addr8[16];
uint16_t __u6_addr16[8];
uint32_t __u6_addr32[4];
} __in6_u;
#define s6_addr			__in6_u.__u6_addr8
#ifdef __USE_MISC
# define s6_addr16		__in6_u.__u6_addr16
# define s6_addr32		__in6_u.__u6_addr32
#endif
};
#endif
extern const struct in6_addr in6addr_any;
extern const struct in6_addr in6addr_loopback;
#define IN6ADDR_ANY_INIT { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 } } }
#define IN6ADDR_LOOPBACK_INIT { { { 0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1 } } }
#define INET_ADDRSTRLEN 16
#define INET6_ADDRSTRLEN 46
struct sockaddr_in
{
__SOCKADDR_COMMON (sin_);
in_port_t sin_port;
struct in_addr sin_addr;
unsigned char sin_zero[sizeof (struct sockaddr)
- __SOCKADDR_COMMON_SIZE
- sizeof (in_port_t)
- sizeof (struct in_addr)];
};
#if !__USE_KERNEL_IPV6_DEFS
struct sockaddr_in6
{
__SOCKADDR_COMMON (sin6_);
in_port_t sin6_port;
uint32_t sin6_flowinfo;
struct in6_addr sin6_addr;
uint32_t sin6_scope_id;
};
#endif
#ifdef __USE_MISC
struct ip_mreq
{
struct in_addr imr_multiaddr;
struct in_addr imr_interface;
};
struct ip_mreqn
{
struct in_addr imr_multiaddr;
struct in_addr imr_address;
int imr_ifindex;
};
struct ip_mreq_source
{
struct in_addr imr_multiaddr;
struct in_addr imr_interface;
struct in_addr imr_sourceaddr;
};
#endif
#if !__USE_KERNEL_IPV6_DEFS
struct ipv6_mreq
{
struct in6_addr ipv6mr_multiaddr;
unsigned int ipv6mr_interface;
};
#endif
#ifdef __USE_MISC
struct group_req
{
uint32_t gr_interface;
struct sockaddr_storage gr_group;
};
struct group_source_req
{
uint32_t gsr_interface;
struct sockaddr_storage gsr_group;
struct sockaddr_storage gsr_source;
};
struct ip_msfilter
{
struct in_addr imsf_multiaddr;
struct in_addr imsf_interface;
uint32_t imsf_fmode;
uint32_t imsf_numsrc;
struct in_addr imsf_slist[1];
};
#define IP_MSFILTER_SIZE(numsrc) (sizeof (struct ip_msfilter) \
- sizeof (struct in_addr)		      \
+ (numsrc) * sizeof (struct in_addr))
struct group_filter
{
uint32_t gf_interface;
struct sockaddr_storage gf_group;
uint32_t gf_fmode;
uint32_t gf_numsrc;
struct sockaddr_storage gf_slist[1];
};
#define GROUP_FILTER_SIZE(numsrc) (sizeof (struct group_filter) \
- sizeof (struct sockaddr_storage)	      \
+ ((numsrc)				      \
* sizeof (struct sockaddr_storage)))
#endif
extern uint32_t ntohl (uint32_t __netlong) __THROW __attribute__ ((__const__));
extern uint16_t ntohs (uint16_t __netshort)
__THROW __attribute__ ((__const__));
extern uint32_t htonl (uint32_t __hostlong)
__THROW __attribute__ ((__const__));
extern uint16_t htons (uint16_t __hostshort)
__THROW __attribute__ ((__const__));
#include <endian.h>
#include <bits/byteswap.h>
#include <bits/uintn-identity.h>
#ifdef __OPTIMIZE__
# if __BYTE_ORDER == __BIG_ENDIAN
# define ntohl(x)	__uint32_identity (x)
# define ntohs(x)	__uint16_identity (x)
# define htonl(x)	__uint32_identity (x)
# define htons(x)	__uint16_identity (x)
# else
#  if __BYTE_ORDER == __LITTLE_ENDIAN
#   define ntohl(x)	__bswap_32 (x)
#   define ntohs(x)	__bswap_16 (x)
#   define htonl(x)	__bswap_32 (x)
#   define htons(x)	__bswap_16 (x)
#  endif
# endif
#endif
#ifdef __GNUC__
# define IN6_IS_ADDR_UNSPECIFIED(a) \
(__extension__							      \
({ const struct in6_addr *__a = (const struct in6_addr *) (a);	      \
__a->__in6_u.__u6_addr32[0] == 0					      \
&& __a->__in6_u.__u6_addr32[1] == 0				      \
&& __a->__in6_u.__u6_addr32[2] == 0				      \
&& __a->__in6_u.__u6_addr32[3] == 0; }))
# define IN6_IS_ADDR_LOOPBACK(a) \
(__extension__							      \
({ const struct in6_addr *__a = (const struct in6_addr *) (a);	      \
__a->__in6_u.__u6_addr32[0] == 0					      \
&& __a->__in6_u.__u6_addr32[1] == 0				      \
&& __a->__in6_u.__u6_addr32[2] == 0				      \
&& __a->__in6_u.__u6_addr32[3] == htonl (1); }))
# define IN6_IS_ADDR_LINKLOCAL(a) \
(__extension__							      \
({ const struct in6_addr *__a = (const struct in6_addr *) (a);	      \
(__a->__in6_u.__u6_addr32[0] & htonl (0xffc00000)) == htonl (0xfe800000); }))
# define IN6_IS_ADDR_SITELOCAL(a) \
(__extension__							      \
({ const struct in6_addr *__a = (const struct in6_addr *) (a);	      \
(__a->__in6_u.__u6_addr32[0] & htonl (0xffc00000)) == htonl (0xfec00000); }))
# define IN6_IS_ADDR_V4MAPPED(a) \
(__extension__							      \
({ const struct in6_addr *__a = (const struct in6_addr *) (a);	      \
__a->__in6_u.__u6_addr32[0] == 0					      \
&& __a->__in6_u.__u6_addr32[1] == 0				      \
&& __a->__in6_u.__u6_addr32[2] == htonl (0xffff); }))
# define IN6_IS_ADDR_V4COMPAT(a) \
(__extension__							      \
({ const struct in6_addr *__a = (const struct in6_addr *) (a);	      \
__a->__in6_u.__u6_addr32[0] == 0					      \
&& __a->__in6_u.__u6_addr32[1] == 0				      \
&& __a->__in6_u.__u6_addr32[2] == 0				      \
&& ntohl (__a->__in6_u.__u6_addr32[3]) > 1; }))
# define IN6_ARE_ADDR_EQUAL(a,b) \
(__extension__							      \
({ const struct in6_addr *__a = (const struct in6_addr *) (a);	      \
const struct in6_addr *__b = (const struct in6_addr *) (b);	      \
__a->__in6_u.__u6_addr32[0] == __b->__in6_u.__u6_addr32[0]	      \
&& __a->__in6_u.__u6_addr32[1] == __b->__in6_u.__u6_addr32[1]	      \
&& __a->__in6_u.__u6_addr32[2] == __b->__in6_u.__u6_addr32[2]	      \
&& __a->__in6_u.__u6_addr32[3] == __b->__in6_u.__u6_addr32[3]; }))
#else
# define IN6_IS_ADDR_UNSPECIFIED(a) \
(((const uint32_t *) (a))[0] == 0				      \
&& ((const uint32_t *) (a))[1] == 0				      \
&& ((const uint32_t *) (a))[2] == 0				      \
&& ((const uint32_t *) (a))[3] == 0)
# define IN6_IS_ADDR_LOOPBACK(a) \
(((const uint32_t *) (a))[0] == 0				      \
&& ((const uint32_t *) (a))[1] == 0				      \
&& ((const uint32_t *) (a))[2] == 0				      \
&& ((const uint32_t *) (a))[3] == htonl (1))
# define IN6_IS_ADDR_LINKLOCAL(a) \
((((const uint32_t *) (a))[0] & htonl (0xffc00000))		      \
== htonl (0xfe800000))
# define IN6_IS_ADDR_SITELOCAL(a) \
((((const uint32_t *) (a))[0] & htonl (0xffc00000))		      \
== htonl (0xfec00000))
# define IN6_IS_ADDR_V4MAPPED(a) \
((((const uint32_t *) (a))[0] == 0)				      \
&& (((const uint32_t *) (a))[1] == 0)				      \
&& (((const uint32_t *) (a))[2] == htonl (0xffff)))
# define IN6_IS_ADDR_V4COMPAT(a) \
((((const uint32_t *) (a))[0] == 0)				      \
&& (((const uint32_t *) (a))[1] == 0)				      \
&& (((const uint32_t *) (a))[2] == 0)				      \
&& (ntohl (((const uint32_t *) (a))[3]) > 1))
# define IN6_ARE_ADDR_EQUAL(a,b) \
((((const uint32_t *) (a))[0] == ((const uint32_t *) (b))[0])	      \
&& (((const uint32_t *) (a))[1] == ((const uint32_t *) (b))[1])      \
&& (((const uint32_t *) (a))[2] == ((const uint32_t *) (b))[2])      \
&& (((const uint32_t *) (a))[3] == ((const uint32_t *) (b))[3]))
#endif
#define IN6_IS_ADDR_MULTICAST(a) (((const uint8_t *) (a))[0] == 0xff)
#ifdef __USE_MISC
extern int bindresvport (int __sockfd, struct sockaddr_in *__sock_in) __THROW;
extern int bindresvport6 (int __sockfd, struct sockaddr_in6 *__sock_in)
__THROW;
#endif
#define IN6_IS_ADDR_MC_NODELOCAL(a) \
(IN6_IS_ADDR_MULTICAST(a)					      \
&& ((((const uint8_t *) (a))[1] & 0xf) == 0x1))
#define IN6_IS_ADDR_MC_LINKLOCAL(a) \
(IN6_IS_ADDR_MULTICAST(a)					      \
&& ((((const uint8_t *) (a))[1] & 0xf) == 0x2))
#define IN6_IS_ADDR_MC_SITELOCAL(a) \
(IN6_IS_ADDR_MULTICAST(a)					      \
&& ((((const uint8_t *) (a))[1] & 0xf) == 0x5))
#define IN6_IS_ADDR_MC_ORGLOCAL(a) \
(IN6_IS_ADDR_MULTICAST(a)					      \
&& ((((const uint8_t *) (a))[1] & 0xf) == 0x8))
#define IN6_IS_ADDR_MC_GLOBAL(a) \
(IN6_IS_ADDR_MULTICAST(a)					      \
&& ((((const uint8_t *) (a))[1] & 0xf) == 0xe))
#ifdef __USE_GNU
struct cmsghdr;
#if !__USE_KERNEL_IPV6_DEFS
struct in6_pktinfo
{
struct in6_addr ipi6_addr;
unsigned int ipi6_ifindex;
};
struct ip6_mtuinfo
{
struct sockaddr_in6 ip6m_addr;
uint32_t ip6m_mtu;
};
#endif
extern int inet6_option_space (int __nbytes)
__THROW __attribute_deprecated__;
extern int inet6_option_init (void *__bp, struct cmsghdr **__cmsgp,
int __type) __THROW __attribute_deprecated__;
extern int inet6_option_append (struct cmsghdr *__cmsg,
const uint8_t *__typep, int __multx,
int __plusy) __THROW __attribute_deprecated__;
extern uint8_t *inet6_option_alloc (struct cmsghdr *__cmsg, int __datalen,
int __multx, int __plusy)
__THROW __attribute_deprecated__;
extern int inet6_option_next (const struct cmsghdr *__cmsg,
uint8_t **__tptrp)
__THROW __attribute_deprecated__;
extern int inet6_option_find (const struct cmsghdr *__cmsg,
uint8_t **__tptrp, int __type)
__THROW __attribute_deprecated__;
extern int inet6_opt_init (void *__extbuf, socklen_t __extlen) __THROW;
extern int inet6_opt_append (void *__extbuf, socklen_t __extlen, int __offset,
uint8_t __type, socklen_t __len, uint8_t __align,
void **__databufp) __THROW;
extern int inet6_opt_finish (void *__extbuf, socklen_t __extlen, int __offset)
__THROW;
extern int inet6_opt_set_val (void *__databuf, int __offset, void *__val,
socklen_t __vallen) __THROW;
extern int inet6_opt_next (void *__extbuf, socklen_t __extlen, int __offset,
uint8_t *__typep, socklen_t *__lenp,
void **__databufp) __THROW;
extern int inet6_opt_find (void *__extbuf, socklen_t __extlen, int __offset,
uint8_t __type, socklen_t *__lenp,
void **__databufp) __THROW;
extern int inet6_opt_get_val (void *__databuf, int __offset, void *__val,
socklen_t __vallen) __THROW;
extern socklen_t inet6_rth_space (int __type, int __segments) __THROW;
extern void *inet6_rth_init (void *__bp, socklen_t __bp_len, int __type,
int __segments) __THROW;
extern int inet6_rth_add (void *__bp, const struct in6_addr *__addr) __THROW;
extern int inet6_rth_reverse (const void *__in, void *__out) __THROW;
extern int inet6_rth_segments (const void *__bp) __THROW;
extern struct in6_addr *inet6_rth_getaddr (const void *__bp, int __index)
__THROW;
extern int getipv4sourcefilter (int __s, struct in_addr __interface_addr,
struct in_addr __group, uint32_t *__fmode,
uint32_t *__numsrc, struct in_addr *__slist)
__THROW;
extern int setipv4sourcefilter (int __s, struct in_addr __interface_addr,
struct in_addr __group, uint32_t __fmode,
uint32_t __numsrc,
const struct in_addr *__slist)
__THROW;
extern int getsourcefilter (int __s, uint32_t __interface_addr,
const struct sockaddr *__group,
socklen_t __grouplen, uint32_t *__fmode,
uint32_t *__numsrc,
struct sockaddr_storage *__slist) __THROW;
extern int setsourcefilter (int __s, uint32_t __interface_addr,
const struct sockaddr *__group,
socklen_t __grouplen, uint32_t __fmode,
uint32_t __numsrc,
const struct sockaddr_storage *__slist) __THROW;
#endif
__END_DECLS
#endif