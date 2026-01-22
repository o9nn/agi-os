#ifndef __FCNTL_H
#define __FCNTL_H
#ifndef _POSIX_SOURCE
This header file is not defined in pure ANSI
#endif
#pragma lib "/$M/lib/ape/libap.a"
#include	<sys/types.h>
#define	O_RDONLY 0
#define	O_WRONLY 1
#define	O_RDWR	 2
#define O_ACCMODE	0x003
#define O_NONBLOCK	0x004
#define O_APPEND	0x008
#define O_CREAT		0x100
#define O_TRUNC		0x200
#define O_EXCL		0x400
#define O_NOCTTY	0x800
#define O_DSYNC		0x1000
#define	O_RSYNC		0x2000
#define O_SYNC		0x4000
#define	F_DUPFD		0
#define	F_GETFD		1
#define	F_SETFD		2
#define	F_GETFL		3
#define	F_SETFL		4
#define	F_GETLK		5
#define	F_SETLK		6
#define	F_SETLKW	7
#define FD_CLOEXEC	1
struct flock {
short	l_type;
short	l_whence;
off_t	l_start;
off_t	l_len;
pid_t	l_pid;
};
#define F_RDLCK		1
#define F_UNLCK		2
#define F_WRLCK		3
#ifdef __cplusplus
extern "C" {
#endif
extern int fcntl(int, int, ...);
extern int open(const char *, int, ...);
extern int creat(const char *, mode_t);
#ifdef __cplusplus
}
#endif
#endif