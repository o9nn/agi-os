#ifndef _LINUX_POSIX_TYPES_H
#define _LINUX_POSIX_TYPES_H
#ifndef NULL
# define NULL ((void *) 0)
#endif
#undef __NFDBITS
#define __NFDBITS (8 * sizeof(unsigned long))
#undef __FD_SETSIZE
#define __FD_SETSIZE 1024
#undef __FDSET_LONGS
#define __FDSET_LONGS (__FD_SETSIZE/__NFDBITS)
#undef __FDELT
#define __FDELT(d) ((d) / __NFDBITS)
#undef __FDMASK
#define __FDMASK(d) (1UL << ((d) % __NFDBITS))
typedef struct {
unsigned long fds_bits [__FDSET_LONGS];
} __kernel_fd_set;
#include <asm/posix_types.h>
#endif