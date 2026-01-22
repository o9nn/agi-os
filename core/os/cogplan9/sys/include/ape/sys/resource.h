#ifndef __RESOURCE_H__
#define __RESOURCE_H__
#ifndef _BSD_EXTENSION
This header file is an extension to ANSI/POSIX
#endif
struct rusage {
struct timeval ru_utime;
struct timeval ru_stime;
long	ru_maxrss;
#define	ru_first	ru_ixrss
long	ru_ixrss;
long	ru_idrss;
long	ru_isrss;
long	ru_minflt;
long	ru_majflt;
long	ru_nswap;
long	ru_inblock;
long	ru_oublock;
long	ru_msgsnd;
long	ru_msgrcv;
long	ru_nsignals;
long	ru_nvcsw;
long	ru_nivcsw;
#define	ru_last		ru_nivcsw
};
#endif