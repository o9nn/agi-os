#ifndef __LINUX_UIO_H
#define __LINUX_UIO_H
#include <linux/types.h>
struct iovec
{
void *iov_base;
__kernel_size_t iov_len;
};
#define UIO_FASTIOV	8
#define UIO_MAXIOV	1024
#if 0
#define UIO_MAXIOV	16
#endif
#endif