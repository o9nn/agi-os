#ifndef _SYS_SELECT_H
#define _SYS_SELECT_H 1
#include <features.h>
#include <bits/types.h>
#include <bits/select.h>
#include <bits/types/sigset_t.h>
#include <bits/types/time_t.h>
#include <bits/types/struct_timeval.h>
#ifdef __USE_XOPEN2K
# include <bits/types/struct_timespec.h>
#endif
#ifndef __suseconds_t_defined
typedef __suseconds_t suseconds_t;
# define __suseconds_t_defined
#endif
typedef long int __fd_mask;
#undef __NFDBITS
#define __NFDBITS (8 * (int) sizeof (__fd_mask))
#define __FD_ELT(d) ((d) / __NFDBITS)
#define __FD_MASK(d) ((__fd_mask) (1UL << ((d) % __NFDBITS)))
typedef struct
{
#ifdef __USE_XOPEN
__fd_mask fds_bits[__FD_SETSIZE / __NFDBITS];
# define __FDS_BITS(set) ((set)->fds_bits)
#else
__fd_mask __fds_bits[__FD_SETSIZE / __NFDBITS];
# define __FDS_BITS(set) ((set)->__fds_bits)
#endif
} fd_set;
#define FD_SETSIZE __FD_SETSIZE
#ifdef __USE_MISC
typedef __fd_mask fd_mask;
# define NFDBITS __NFDBITS
#endif
#define FD_SET(fd, fdsetp) __FD_SET (fd, fdsetp)
#define FD_CLR(fd, fdsetp) __FD_CLR (fd, fdsetp)
#define FD_ISSET(fd, fdsetp) __FD_ISSET (fd, fdsetp)
#define FD_ZERO(fdsetp) __FD_ZERO (fdsetp)
__BEGIN_DECLS
#ifndef __USE_TIME_BITS64
extern int select (int __nfds, fd_set *__restrict __readfds,
fd_set *__restrict __writefds,
fd_set *__restrict __exceptfds,
struct timeval *__restrict __timeout);
#else
# ifdef __REDIRECT
extern int __REDIRECT (select,
(int __nfds, fd_set *__restrict __readfds,
fd_set *__restrict __writefds,
fd_set *__restrict __exceptfds,
struct timeval *__restrict __timeout),
__select64);
# else
# define select __select64
# endif
#endif
#ifdef __USE_XOPEN2K
# ifndef __USE_TIME_BITS64
extern int pselect (int __nfds, fd_set *__restrict __readfds,
fd_set *__restrict __writefds,
fd_set *__restrict __exceptfds,
const struct timespec *__restrict __timeout,
const __sigset_t *__restrict __sigmask);
# else
# ifdef __REDIRECT
extern int __REDIRECT (pselect,
(int __nfds, fd_set *__restrict __readfds,
fd_set *__restrict __writefds,
fd_set *__restrict __exceptfds,
const struct timespec *__restrict __timeout,
const __sigset_t *__restrict __sigmask),
__pselect64);
# else
# define pselect __pselect64
# endif
# endif
#endif
#if __USE_FORTIFY_LEVEL > 0 && defined __GNUC__
# include <bits/select2.h>
#endif
__END_DECLS
#endif