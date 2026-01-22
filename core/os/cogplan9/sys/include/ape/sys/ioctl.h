#ifndef __IOCTL_H__
#define __IOCTL_H__
#ifndef _BSD_EXTENSION
This header file is an extension to ANSI/POSIX
#endif
#ifdef __cplusplus
extern "C" {
#endif
#define FIONREAD 1
int ioctl(int, unsigned long, void*);
#ifdef __cplusplus
}
#endif
#endif