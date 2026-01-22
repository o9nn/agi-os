#ifndef __BITS_SOCKET_H
#define __BITS_SOCKET_H
#ifndef _SYS_SOCKET_H
# error "Never include <bits/socket.h> directly; use <sys/socket.h> instead."
#endif
#define __need_size_t
#include <stddef.h>
#include <sys/types.h>
#ifndef __socklen_t_defined
typedef __socklen_t socklen_t;
# define __socklen_t_defined
#endif
#include <bits/socket_type.h>
#define PF_UNSPEC	0
#define PF_LOCAL	1
#define PF_UNIX		PF_LOCAL
#define PF_FILE		PF_LOCAL
#define PF_INET		2
#define PF_AX25		3
#define PF_IPX		4
#define PF_APPLETALK	5
#define PF_NETROM	6
#define PF_BRIDGE	7
#define PF_ATMPVC	8
#define PF_X25		9
#define PF_INET6	10
#define PF_ROSE		11
#define PF_DECnet	12
#define PF_NETBEUI	13
#define PF_SECURITY	14
#define PF_KEY		15
#define PF_NETLINK	16
#define PF_ROUTE	PF_NETLINK
#define PF_PACKET	17
#define PF_ASH		18
#define PF_ECONET	19
#define PF_ATMSVC	20
#define PF_RDS		21
#define PF_SNA		22
#define PF_IRDA		23
#define PF_PPPOX	24
#define PF_WANPIPE	25
#define PF_LLC		26
#define PF_IB		27
#define PF_MPLS		28
#define PF_CAN		29
#define PF_TIPC		30
#define PF_BLUETOOTH	31
#define PF_IUCV		32
#define PF_RXRPC	33
#define PF_ISDN		34
#define PF_PHONET	35
#define PF_IEEE802154	36
#define PF_CAIF		37
#define PF_ALG		38
#define PF_NFC		39
#define PF_VSOCK	40
#define PF_KCM		41
#define PF_QIPCRTR	42
#define PF_SMC		43
#define PF_XDP		44
#define PF_MCTP		45
#define PF_MAX		46
#define AF_UNSPEC	PF_UNSPEC
#define AF_LOCAL	PF_LOCAL
#define AF_UNIX		PF_UNIX
#define AF_FILE		PF_FILE
#define AF_INET		PF_INET
#define AF_AX25		PF_AX25
#define AF_IPX		PF_IPX
#define AF_APPLETALK	PF_APPLETALK
#define AF_NETROM	PF_NETROM
#define AF_BRIDGE	PF_BRIDGE
#define AF_ATMPVC	PF_ATMPVC
#define AF_X25		PF_X25
#define AF_INET6	PF_INET6
#define AF_ROSE		PF_ROSE
#define AF_DECnet	PF_DECnet
#define AF_NETBEUI	PF_NETBEUI
#define AF_SECURITY	PF_SECURITY
#define AF_KEY		PF_KEY
#define AF_NETLINK	PF_NETLINK
#define AF_ROUTE	PF_ROUTE
#define AF_PACKET	PF_PACKET
#define AF_ASH		PF_ASH
#define AF_ECONET	PF_ECONET
#define AF_ATMSVC	PF_ATMSVC
#define AF_RDS		PF_RDS
#define AF_SNA		PF_SNA
#define AF_IRDA		PF_IRDA
#define AF_PPPOX	PF_PPPOX
#define AF_WANPIPE	PF_WANPIPE
#define AF_LLC		PF_LLC
#define AF_IB		PF_IB
#define AF_MPLS		PF_MPLS
#define AF_CAN		PF_CAN
#define AF_TIPC		PF_TIPC
#define AF_BLUETOOTH	PF_BLUETOOTH
#define AF_IUCV		PF_IUCV
#define AF_RXRPC	PF_RXRPC
#define AF_ISDN		PF_ISDN
#define AF_PHONET	PF_PHONET
#define AF_IEEE802154	PF_IEEE802154
#define AF_CAIF		PF_CAIF
#define AF_ALG		PF_ALG
#define AF_NFC		PF_NFC
#define AF_VSOCK	PF_VSOCK
#define AF_KCM		PF_KCM
#define AF_QIPCRTR	PF_QIPCRTR
#define AF_SMC		PF_SMC
#define AF_XDP		PF_XDP
#define AF_MCTP		PF_MCTP
#define AF_MAX		PF_MAX
#define SOL_RAW		255
#define SOL_DECNET      261
#define SOL_X25         262
#define SOL_PACKET	263
#define SOL_ATM		264
#define SOL_AAL		265
#define SOL_IRDA	266
#define SOL_NETBEUI	267
#define SOL_LLC		268
#define SOL_DCCP	269
#define SOL_NETLINK	270
#define SOL_TIPC	271
#define SOL_RXRPC	272
#define SOL_PPPOL2TP	273
#define SOL_BLUETOOTH	274
#define SOL_PNPIPE	275
#define SOL_RDS		276
#define SOL_IUCV	277
#define SOL_CAIF	278
#define SOL_ALG		279
#define SOL_NFC		280
#define SOL_KCM		281
#define SOL_TLS		282
#define SOL_XDP		283
#define SOL_MPTCP	284
#define SOL_MCTP	285
#define SOL_SMC		286
#define SOMAXCONN	4096
#include <bits/sockaddr.h>
struct sockaddr
{
__SOCKADDR_COMMON (sa_);
char sa_data[14];
};
#define __ss_aligntype	unsigned long int
#define _SS_PADSIZE \
(_SS_SIZE - __SOCKADDR_COMMON_SIZE - sizeof (__ss_aligntype))
struct sockaddr_storage
{
__SOCKADDR_COMMON (ss_);
char __ss_padding[_SS_PADSIZE];
__ss_aligntype __ss_align;
};
enum
{
MSG_OOB		= 0x01,
#define MSG_OOB		MSG_OOB
MSG_PEEK		= 0x02,
#define MSG_PEEK	MSG_PEEK
MSG_DONTROUTE	= 0x04,
#define MSG_DONTROUTE	MSG_DONTROUTE
#ifdef __USE_GNU
MSG_TRYHARD		= MSG_DONTROUTE,
# define MSG_TRYHARD	MSG_DONTROUTE
#endif
MSG_CTRUNC		= 0x08,
#define MSG_CTRUNC	MSG_CTRUNC
MSG_PROXY		= 0x10,
#define MSG_PROXY	MSG_PROXY
MSG_TRUNC		= 0x20,
#define MSG_TRUNC	MSG_TRUNC
MSG_DONTWAIT	= 0x40,
#define MSG_DONTWAIT	MSG_DONTWAIT
MSG_EOR		= 0x80,
#define MSG_EOR		MSG_EOR
MSG_WAITALL		= 0x100,
#define MSG_WAITALL	MSG_WAITALL
MSG_FIN		= 0x200,
#define MSG_FIN		MSG_FIN
MSG_SYN		= 0x400,
#define MSG_SYN		MSG_SYN
MSG_CONFIRM		= 0x800,
#define MSG_CONFIRM	MSG_CONFIRM
MSG_RST		= 0x1000,
#define MSG_RST		MSG_RST
MSG_ERRQUEUE	= 0x2000,
#define MSG_ERRQUEUE	MSG_ERRQUEUE
MSG_NOSIGNAL	= 0x4000,
#define MSG_NOSIGNAL	MSG_NOSIGNAL
MSG_MORE		= 0x8000,
#define MSG_MORE	MSG_MORE
MSG_WAITFORONE	= 0x10000,
#define MSG_WAITFORONE	MSG_WAITFORONE
MSG_BATCH		= 0x40000,
#define MSG_BATCH	MSG_BATCH
MSG_ZEROCOPY	= 0x4000000,
#define MSG_ZEROCOPY	MSG_ZEROCOPY
MSG_FASTOPEN	= 0x20000000,
#define MSG_FASTOPEN	MSG_FASTOPEN
MSG_CMSG_CLOEXEC	= 0x40000000
#define MSG_CMSG_CLOEXEC MSG_CMSG_CLOEXEC
};
struct msghdr
{
void *msg_name;
socklen_t msg_namelen;
struct iovec *msg_iov;
size_t msg_iovlen;
void *msg_control;
size_t msg_controllen;
int msg_flags;
};
struct cmsghdr
{
size_t cmsg_len;
int cmsg_level;
int cmsg_type;
#if __glibc_c99_flexarr_available
__extension__ unsigned char __cmsg_data __flexarr;
#endif
};
#if __glibc_c99_flexarr_available
# define CMSG_DATA(cmsg) ((cmsg)->__cmsg_data)
#else
# define CMSG_DATA(cmsg) ((unsigned char *) ((struct cmsghdr *) (cmsg) + 1))
#endif
#define CMSG_NXTHDR(mhdr, cmsg) __cmsg_nxthdr (mhdr, cmsg)
#define CMSG_FIRSTHDR(mhdr) \
((size_t) (mhdr)->msg_controllen >= sizeof (struct cmsghdr)		      \
? (struct cmsghdr *) (mhdr)->msg_control : (struct cmsghdr *) 0)
#define CMSG_ALIGN(len) (((len) + sizeof (size_t) - 1) \
& (size_t) ~(sizeof (size_t) - 1))
#define CMSG_SPACE(len) (CMSG_ALIGN (len) \
+ CMSG_ALIGN (sizeof (struct cmsghdr)))
#define CMSG_LEN(len)   (CMSG_ALIGN (sizeof (struct cmsghdr)) + (len))
#define __CMSG_PADDING(len) ((sizeof (size_t) \
- ((len) & (sizeof (size_t) - 1))) \
& (sizeof (size_t) - 1))
extern struct cmsghdr *__cmsg_nxthdr (struct msghdr *__mhdr,
struct cmsghdr *__cmsg) __THROW;
#ifdef __USE_EXTERN_INLINES
# ifndef _EXTERN_INLINE
#  define _EXTERN_INLINE __extern_inline
# endif
_EXTERN_INLINE struct cmsghdr *
__NTH (__cmsg_nxthdr (struct msghdr *__mhdr, struct cmsghdr *__cmsg))
{
unsigned char * __msg_control_ptr = (unsigned char *) __mhdr->msg_control;
unsigned char * __cmsg_ptr = (unsigned char *) __cmsg;
size_t __size_needed = sizeof (struct cmsghdr)
+ __CMSG_PADDING (__cmsg->cmsg_len);
if ((size_t) __cmsg->cmsg_len < sizeof (struct cmsghdr))
return (struct cmsghdr *) 0;
if (((size_t)
(__msg_control_ptr + __mhdr->msg_controllen - __cmsg_ptr)
< __size_needed)
|| ((size_t)
(__msg_control_ptr + __mhdr->msg_controllen - __cmsg_ptr
- __size_needed)
< __cmsg->cmsg_len))
return (struct cmsghdr *) 0;
__cmsg = (struct cmsghdr *) ((unsigned char *) __cmsg
+ CMSG_ALIGN (__cmsg->cmsg_len));
return __cmsg;
}
#endif
enum
{
SCM_RIGHTS = 0x01
#define SCM_RIGHTS SCM_RIGHTS
#ifdef __USE_GNU
, SCM_CREDENTIALS = 0x02
# define SCM_CREDENTIALS SCM_CREDENTIALS
, SCM_SECURITY = 0x03
# define SCM_SECURITY SCM_SECURITY
, SCM_PIDFD = 0x04
# define SCM_PIDFD SCM_PIDFD
#endif
};
#ifdef __USE_GNU
struct ucred
{
pid_t pid;
uid_t uid;
gid_t gid;
};
#endif
#ifdef __USE_MISC
# include <bits/types/time_t.h>
# include <asm/socket.h>
#else
# define SO_DEBUG 1
# include <bits/socket-constants.h>
#endif
struct linger
{
int l_onoff;
int l_linger;
};
#endif