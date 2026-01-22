#ifndef _LINUX_SOCKET_H
#define _LINUX_SOCKET_H
#include <asm/socket.h>
#include <linux/sockios.h>
#include <linux/uio.h>
struct sockaddr
{
unsigned short sa_family;
char sa_data[14];
};
struct linger {
int l_onoff;
int l_linger;
};
struct msghdr
{
void * msg_name;
int msg_namelen;
struct iovec * msg_iov;
int msg_iovlen;
void * msg_control;
int msg_controllen;
int msg_flags;
};
#define SCM_RIGHTS 1
#define SOCK_STREAM 1
#define SOCK_DGRAM 2
#define SOCK_RAW 3
#define SOCK_RDM 4
#define SOCK_SEQPACKET 5
#define SOCK_PACKET 10
#define AF_UNSPEC 0
#define AF_UNIX 1
#define AF_INET 2
#define AF_AX25 3
#define AF_IPX 4
#define AF_APPLETALK 5
#define AF_NETROM 6
#define AF_BRIDGE 7
#define AF_AAL5 8
#define AF_X25 9
#ifdef LINUX_2_1_X
#define AF_INET6 10
#endif
#define AF_ROSE 11
#define AF_MAX 13
#define AF_PACKET 17
#define PF_UNSPEC AF_UNSPEC
#define PF_UNIX AF_UNIX
#define PF_INET AF_INET
#define PF_AX25 AF_AX25
#define PF_IPX AF_IPX
#define PF_APPLETALK AF_APPLETALK
#define PF_NETROM AF_NETROM
#define PF_BRIDGE AF_BRIDGE
#define PF_AAL5 AF_AAL5
#define PF_X25 AF_X25
#ifdef LINUX_2_1_X
#define PF_INET6 AF_INET6
#endif
#define PF_ROSE AF_ROSE
#define PF_MAX AF_MAX
#define PF_PACKET AF_PACKET
#define SOMAXCONN 128
#define MSG_OOB 1
#define MSG_PEEK 2
#define MSG_DONTROUTE 4
#define MSG_PROXY 16
#define SOL_IP 0
#define SOL_IPX 256
#define SOL_AX25 257
#define SOL_ATALK 258
#define SOL_NETROM 259
#define SOL_ROSE 260
#define SOL_TCP 6
#define SOL_UDP 17
#define IP_TOS 1
#define IPTOS_LOWDELAY 0x10
#define IPTOS_THROUGHPUT 0x08
#define IPTOS_RELIABILITY 0x04
#define IPTOS_MINCOST 0x02
#define IP_TTL 2
#define IP_HDRINCL 3
#define IP_OPTIONS 4
#define IP_MULTICAST_IF 32
#define IP_MULTICAST_TTL 33
#define IP_MULTICAST_LOOP 34
#define IP_ADD_MEMBERSHIP 35
#define IP_DROP_MEMBERSHIP 36
#define IP_DEFAULT_MULTICAST_TTL 1
#define IP_DEFAULT_MULTICAST_LOOP 1
#define IP_MAX_MEMBERSHIPS 20
#define IPX_TYPE 1
#define TCP_NODELAY 1
#define TCP_MAXSEG 2
#define SOPRI_INTERACTIVE 0
#define SOPRI_NORMAL 1
#define SOPRI_BACKGROUND 2
#ifdef __KERNEL__
extern void memcpy_fromiovec(unsigned char *kdata, struct iovec *iov, int len);
extern int verify_iovec(struct msghdr *m, struct iovec *iov, char *address, int mode);
extern void memcpy_toiovec(struct iovec *v, unsigned char *kdata, int len);
extern int move_addr_to_user(void *kaddr, int klen, void *uaddr, int *ulen);
extern int move_addr_to_kernel(void *uaddr, int ulen, void *kaddr);
#endif
#endif