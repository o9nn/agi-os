#ifndef __SYS_SOCKET_H__
#define __SYS_SOCKET_H__
#ifndef _BSD_EXTENSION
This header file is an extension to ANSI/POSIX
#endif
#pragma lib "/$M/lib/ape/libbsd.a"
#ifdef __cplusplus
extern "C" {
#endif
#define SOCK_STREAM 1
#define SOCK_DGRAM 2
#define SOCK_RAW 3
#define SOCK_RDM 4
#define SOCK_SEQPACKET 5
#ifdef HAVE_SOCK_OPTS
#define SO_DEBUG 0x0001
#define SO_ACCEPTCONN 0x0002
#define SO_REUSEADDR 0x0004
#define SO_KEEPALIVE 0x0008
#define SO_DONTROUTE 0x0010
#define SO_BROADCAST 0x0020
#define SO_USELOOPBACK 0x0040
#define SO_LINGER 0x0080
#define SO_OOBINLINE 0x0100
#endif
#define SO_SNDBUF 0x1001
#define SO_RCVBUF 0x1002
#define SO_SNDLOWAT 0x1003
#define SO_RCVLOWAT 0x1004
#define SO_SNDTIMEO 0x1005
#define SO_RCVTIMEO 0x1006
#define SO_ERROR 0x1007
#define SO_TYPE 0x1008
struct linger {
int l_onoff;
int l_linger;
};
#define SOL_SOCKET 0xffff
#define AF_UNSPEC 0
#define AF_UNIX 1
#define AF_INET 2
#define AF_IMPLINK 3
#define AF_PUP 4
#define AF_CHAOS 5
#define AF_NS 6
#define AF_ISO 7
#define AF_OSI AF_ISO
#define AF_ECMA 8
#define AF_DATAKIT 9
#define AF_CCITT 10
#define AF_SNA 11
#define AF_DECnet 12
#define AF_DLI 13
#define AF_LAT 14
#define AF_HYLINK 15
#define AF_APPLETALK 16
#define AF_ROUTE 17
#define AF_LINK 18
#define pseudo_AF_XTP 19
#define AF_INET6 24
#define AF_MAX 30
struct sockaddr {
unsigned short sa_family;
char sa_data[108];
};
struct sockproto {
unsigned short sp_family;
unsigned short sp_protocol;
};
#define PF_UNSPEC AF_UNSPEC
#define PF_UNIX AF_UNIX
#define PF_INET AF_INET
#define PF_IMPLINK AF_IMPLINK
#define PF_PUP AF_PUP
#define PF_CHAOS AF_CHAOS
#define PF_NS AF_NS
#define PF_ISO AF_ISO
#define PF_OSI AF_ISO
#define PF_ECMA AF_ECMA
#define PF_DATAKIT AF_DATAKIT
#define PF_CCITT AF_CCITT
#define PF_SNA AF_SNA
#define PF_DECnet AF_DECnet
#define PF_DLI AF_DLI
#define PF_LAT AF_LAT
#define PF_HYLINK AF_HYLINK
#define PF_APPLETALK AF_APPLETALK
#define PF_ROUTE AF_ROUTE
#define PF_LINK AF_LINK
#define PF_XTP pseudo_AF_XTP
#define PF_INET6 AF_INET6
#define PF_MAX AF_MAX
#define SOMAXCONN 5
struct msghdr {
char *msg_name;
int msg_namelen;
struct iovec *msg_iov;
int msg_iovlen;
char *msg_accrights;
int msg_accrightslen;
};
#define MSG_OOB 0x1
#define MSG_PEEK 0x2
#define MSG_DONTROUTE 0x4
#define MSG_MAXIOVLEN 16
extern int accept(int, void *, int *);
extern int bind(int, void *, int);
extern int connect(int, void *, int);
extern int getpeername(int, void *, int *);
extern int getsockname(int, void *, int *);
extern int getsockopt(int, int, int, void *, int *);
extern int setsockopt(int, int, int, void *, int);
extern int listen(int, int);
extern int recv(int, void *, int, int);
extern int recvfrom(int, void *, int, int, void *, int *);
extern int recvmsg(int, struct msghdr *, int);
extern int send(int, void *, int, int);
extern int sendto(int, void *, int, int, void *, int);
extern int sendmsg(int, struct msghdr *, int);
extern int shutdown(int, int);
extern int socket(int, int, int);
extern int socketpair(int, int, int, int *);
#ifdef __cplusplus
}
#endif
#endif