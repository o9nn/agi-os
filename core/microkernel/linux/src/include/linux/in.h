#ifndef _LINUX_IN_H
#define _LINUX_IN_H
#include <linux/types.h>
enum {
IPPROTO_IP = 0,
IPPROTO_ICMP = 1,
IPPROTO_IGMP = 2,
IPPROTO_IPIP = 4,
IPPROTO_TCP = 6,
IPPROTO_EGP = 8,
IPPROTO_PUP = 12,
IPPROTO_UDP = 17,
IPPROTO_IDP = 22,
IPPROTO_RAW = 255,
IPPROTO_MAX
};
struct in_addr {
__u32	s_addr;
};
struct ip_mreq
{
struct in_addr imr_multiaddr;
struct in_addr imr_interface;
};
#define __SOCK_SIZE__	16
struct sockaddr_in {
short int		sin_family;
unsigned short int	sin_port;
struct in_addr	sin_addr;
unsigned char		__pad[__SOCK_SIZE__ - sizeof(short int) -
sizeof(unsigned short int) - sizeof(struct in_addr)];
};
#define sin_zero	__pad
#define	IN_CLASSA(a)		((((long int) (a)) & 0x80000000) == 0)
#define	IN_CLASSA_NET		0xff000000
#define	IN_CLASSA_NSHIFT	24
#define	IN_CLASSA_HOST		(0xffffffff & ~IN_CLASSA_NET)
#define	IN_CLASSA_MAX		128
#define	IN_CLASSB(a)		((((long int) (a)) & 0xc0000000) == 0x80000000)
#define	IN_CLASSB_NET		0xffff0000
#define	IN_CLASSB_NSHIFT	16
#define	IN_CLASSB_HOST		(0xffffffff & ~IN_CLASSB_NET)
#define	IN_CLASSB_MAX		65536
#define	IN_CLASSC(a)		((((long int) (a)) & 0xe0000000) == 0xc0000000)
#define	IN_CLASSC_NET		0xffffff00
#define	IN_CLASSC_NSHIFT	8
#define	IN_CLASSC_HOST		(0xffffffff & ~IN_CLASSC_NET)
#define	IN_CLASSD(a)		((((long int) (a)) & 0xf0000000) == 0xe0000000)
#define	IN_MULTICAST(a)		IN_CLASSD(a)
#define IN_MULTICAST_NET	0xF0000000
#define	IN_EXPERIMENTAL(a)	((((long int) (a)) & 0xe0000000) == 0xe0000000)
#define	IN_BADCLASS(a)		((((long int) (a)) & 0xf0000000) == 0xf0000000)
#define	INADDR_ANY		((unsigned long int) 0x00000000)
#define	INADDR_BROADCAST	((unsigned long int) 0xffffffff)
#define	INADDR_NONE		((unsigned long int) 0xffffffff)
#define	IN_LOOPBACKNET		127
#define	INADDR_LOOPBACK		0x7f000001
#define	IN_LOOPBACK(a)		((((long int) (a)) & 0xff000000) == 0x7f000000)
#define INADDR_UNSPEC_GROUP   	0xe0000000
#define INADDR_ALLHOSTS_GROUP 	0xe0000001
#define INADDR_MAX_LOCAL_GROUP  0xe00000ff
#include <asm/byteorder.h>
#ifdef __KERNEL__
#define LOOPBACK(x)	(((x) & htonl(0xff000000)) == htonl(0x7f000000))
#define MULTICAST(x)	(((x) & htonl(0xf0000000)) == htonl(0xe0000000))
#endif
struct in_addr6
{
unsigned char s6_addr[16];
};
struct sockaddr_in6
{
unsigned short sin6_family;
unsigned short sin6_port;
unsigned long sin6_flowinfo;
struct in_addr6 sin6_addr;
};
#endif