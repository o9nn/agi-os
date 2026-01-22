#ifndef _LINUX_TIME_H
#define _LINUX_TIME_H
#ifndef _STRUCT_TIMESPEC
#define _STRUCT_TIMESPEC
struct timespec {
long	tv_sec;
long	tv_nsec;
};
#endif
struct timeval {
int	tv_sec;
int	tv_usec;
};
struct timezone {
int	tz_minuteswest;
int	tz_dsttime;
};
#define NFDBITS			__NFDBITS
#ifdef __KERNEL__
void do_gettimeofday(struct timeval *tv);
void do_settimeofday(struct timeval *tv);
#endif
#define FD_SETSIZE		__FD_SETSIZE
#define FD_SET(fd,fdsetp)	__FD_SET(fd,fdsetp)
#define FD_CLR(fd,fdsetp)	__FD_CLR(fd,fdsetp)
#define FD_ISSET(fd,fdsetp)	__FD_ISSET(fd,fdsetp)
#define FD_ZERO(fdsetp)		__FD_ZERO(fdsetp)
#define	ITIMER_REAL	0
#define	ITIMER_VIRTUAL	1
#define	ITIMER_PROF	2
struct  itimerspec {
struct  timespec it_interval;
struct  timespec it_value;
};
struct	itimerval {
struct	timeval it_interval;
struct	timeval it_value;
};
#endif