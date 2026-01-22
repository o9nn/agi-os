#ifndef __LINUX_UIO_H
#define __LINUX_UIO_H
struct iovec
{
void *iov_base;
int iov_len;
};
#define UIO_MAXIOV	16
#endif