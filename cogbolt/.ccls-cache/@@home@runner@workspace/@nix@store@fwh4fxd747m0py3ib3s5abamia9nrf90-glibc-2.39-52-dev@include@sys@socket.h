#ifndef _SYS_SOCKET_H
#define _SYS_SOCKET_H 1
#include <features.h>
__BEGIN_DECLS
#include <bits/types/struct_iovec.h>
#define __need_size_t
#include <stddef.h>
#include <bits/socket.h>
#ifdef __USE_MISC
# include <bits/types/struct_osockaddr.h>
#endif
enum
{
SHUT_RD = 0,
#define SHUT_RD SHUT_RD
SHUT_WR,
#define SHUT_WR SHUT_WR
SHUT_RDWR
#define SHUT_RDWR SHUT_RDWR
};
#if defined __cplusplus || !__GNUC_PREREQ (2, 7) || !defined __USE_GNU
# define __SOCKADDR_ARG struct sockaddr *__restrict
# define __CONST_SOCKADDR_ARG const struct sockaddr *
#else
# define __SOCKADDR_ALLTYPES \
__SOCKADDR_ONETYPE (sockaddr) \
__SOCKADDR_ONETYPE (sockaddr_at) \
__SOCKADDR_ONETYPE (sockaddr_ax25) \
__SOCKADDR_ONETYPE (sockaddr_dl) \
__SOCKADDR_ONETYPE (sockaddr_eon) \
__SOCKADDR_ONETYPE (sockaddr_in) \
__SOCKADDR_ONETYPE (sockaddr_in6) \
__SOCKADDR_ONETYPE (sockaddr_inarp) \
__SOCKADDR_ONETYPE (sockaddr_ipx) \
__SOCKADDR_ONETYPE (sockaddr_iso) \
__SOCKADDR_ONETYPE (sockaddr_ns) \
__SOCKADDR_ONETYPE (sockaddr_un) \
__SOCKADDR_ONETYPE (sockaddr_x25)
# define __SOCKADDR_ONETYPE(type) struct type *__restrict __##type##__;
typedef union { __SOCKADDR_ALLTYPES
} __SOCKADDR_ARG __attribute__ ((__transparent_union__));
# undef __SOCKADDR_ONETYPE
# define __SOCKADDR_ONETYPE(type) const struct type *__restrict __##type##__;
typedef union { __SOCKADDR_ALLTYPES
} __CONST_SOCKADDR_ARG __attribute__ ((__transparent_union__));
# undef __SOCKADDR_ONETYPE
#endif
#ifdef __USE_GNU
struct mmsghdr
{
struct msghdr msg_hdr;
unsigned int msg_len;
};
#endif
extern int socket (int __domain, int __type, int __protocol) __THROW;
extern int socketpair (int __domain, int __type, int __protocol,
int __fds[2]) __THROW;
extern int bind (int __fd, __CONST_SOCKADDR_ARG __addr, socklen_t __len)
__THROW;
extern int getsockname (int __fd, __SOCKADDR_ARG __addr,
socklen_t *__restrict __len) __THROW;
extern int connect (int __fd, __CONST_SOCKADDR_ARG __addr, socklen_t __len);
extern int getpeername (int __fd, __SOCKADDR_ARG __addr,
socklen_t *__restrict __len) __THROW;
extern ssize_t send (int __fd, const void *__buf, size_t __n, int __flags);
extern ssize_t recv (int __fd, void *__buf, size_t __n, int __flags);
extern ssize_t sendto (int __fd, const void *__buf, size_t __n,
int __flags, __CONST_SOCKADDR_ARG __addr,
socklen_t __addr_len);
extern ssize_t recvfrom (int __fd, void *__restrict __buf, size_t __n,
int __flags, __SOCKADDR_ARG __addr,
socklen_t *__restrict __addr_len);
#ifndef __USE_TIME_BITS64
extern ssize_t sendmsg (int __fd, const struct msghdr *__message,
int __flags);
#else
# ifdef __REDIRECT
extern ssize_t __REDIRECT (sendmsg, (int __fd, const struct msghdr *__message,
int __flags),
__sendmsg64);
# else
extern ssize_t __sendmsg64 (int __fd, const struct msghdr *__message,
int __flags);
# define sendmsg __sendmsg64
# endif
#endif
#ifdef __USE_GNU
# ifndef __USE_TIME_BITS64
extern int sendmmsg (int __fd, struct mmsghdr *__vmessages,
unsigned int __vlen, int __flags);
# else
# ifdef __REDIRECT
extern int __REDIRECT (sendmmsg, (int __fd, struct mmsghdr *__vmessages,
unsigned int __vlen, int __flags),
__sendmmsg64);
# else
extern int __sendmmsg64 (int __fd, struct mmsghdr *__vmessages,
unsigned int __vlen, int __flags);
# define sendmmsg __sendmmsg64
# endif
# endif
#endif
#ifndef __USE_TIME_BITS64
extern ssize_t recvmsg (int __fd, struct msghdr *__message, int __flags);
#else
# ifdef __REDIRECT
extern ssize_t __REDIRECT (recvmsg,
(int __fd, struct msghdr *__message, int __flags),
__recvmsg64);
# else
extern ssize_t __recvmsg64 (int __fd, struct msghdr *__message, int __flags);
# define recvmsg __recvmsg64
# endif
#endif
#ifdef __USE_GNU
# ifndef __USE_TIME_BITS64
extern int recvmmsg (int __fd, struct mmsghdr *__vmessages,
unsigned int __vlen, int __flags,
struct timespec *__tmo);
# else
# ifdef __REDIRECT
extern int __REDIRECT (recvmmsg, (int __fd, struct mmsghdr *__vmessages,
unsigned int __vlen, int __flags,
struct timespec *__tmo),
__recvmmsg64);
# else
# define recvmmsg __recvmmsg64
# endif
# endif
#endif
#ifndef __USE_TIME_BITS64
extern int getsockopt (int __fd, int __level, int __optname,
void *__restrict __optval,
socklen_t *__restrict __optlen) __THROW;
#else
# ifdef __REDIRECT
extern int __REDIRECT_NTH (getsockopt,
(int __fd, int __level, int __optname,
void *__restrict __optval,
socklen_t *__restrict __optlen),
__getsockopt64);
# else
extern int __getsockopt64 (int __fd, int __level, int __optname,
void *__restrict __optval,
socklen_t *__restrict __optlen) __THROW;
# define getsockopt __getsockopt64
# endif
#endif
#ifndef __USE_TIME_BITS64
extern int setsockopt (int __fd, int __level, int __optname,
const void *__optval, socklen_t __optlen) __THROW;
#else
# ifdef __REDIRECT
extern int __REDIRECT_NTH (setsockopt,
(int __fd, int __level, int __optname,
const void *__optval, socklen_t __optlen),
__setsockopt64);
# else
extern int __setsockopt64 (int __fd, int __level, int __optname,
const void *__optval, socklen_t __optlen) __THROW;
# define setsockopt __setsockopt64
# endif
#endif
extern int listen (int __fd, int __n) __THROW;
extern int accept (int __fd, __SOCKADDR_ARG __addr,
socklen_t *__restrict __addr_len);
#ifdef __USE_GNU
extern int accept4 (int __fd, __SOCKADDR_ARG __addr,
socklen_t *__restrict __addr_len, int __flags);
#endif
extern int shutdown (int __fd, int __how) __THROW;
#ifdef __USE_XOPEN2K
extern int sockatmark (int __fd) __THROW;
#endif
#ifdef __USE_MISC
extern int isfdtype (int __fd, int __fdtype) __THROW;
#endif
#if __USE_FORTIFY_LEVEL > 0 && defined __fortify_function
# include <bits/socket2.h>
#endif
__END_DECLS
#endif