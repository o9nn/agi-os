#ifndef _LINUX_ERRNO_H
#define _LINUX_ERRNO_H
#include <asm/errno.h>
#ifdef __KERNEL__
#define ERESTARTSYS	512
#define ERESTARTNOINTR	513
#define ERESTARTNOHAND	514
#define ENOIOCTLCMD	515
#endif
#endif