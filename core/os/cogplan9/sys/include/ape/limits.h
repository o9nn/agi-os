#ifndef __LIMITS
#define __LIMITS
#define CHAR_BIT	8
#define MB_LEN_MAX	4
#define UCHAR_MAX	0xff
#define USHRT_MAX	0xffff
#define UINT_MAX	0xffffffffU
#define ULONG_MAX	0xffffffffUL
#define CHAR_MAX	SCHAR_MAX
#define SCHAR_MAX	0x7f
#define SHRT_MAX	0x7fff
#define INT_MAX		0x7fffffff
#define LONG_MAX	0x7fffffffL
#define CHAR_MIN	SCHAR_MIN
#define SCHAR_MIN	(-SCHAR_MAX-1)
#define SHRT_MIN	(-SHRT_MAX-1)
#define INT_MIN		(-INT_MAX-1)
#define LONG_MIN	(-LONG_MAX-1)
#ifdef _POSIX_SOURCE
#define _POSIX_AIO_LISTIO_MAX	2
#define _POSIX_AIO_MAX			1
#define _POSIX_ARG_MAX			4096
#define _POSIX_CHILD_MAX		6
#define	_POSIX_CLOCKRES_MIN		20000000
#define	_POSIX_DELAYTIMER_MAX	32
#define _POSIX_LINK_MAX			8
#define _POSIX_MAX_CANON		255
#define _POSIX_MAX_INPUT		255
#define _POSIX_MQ_OPEN_MAX		8
#define	_POSIX_MQ_PRIO_MAX		32
#define _POSIX_NAME_MAX			14
#define _POSIX_NGROUPS_MAX		0
#define _POSIX_OPEN_MAX			16
#define _POSIX_PATH_MAX			255
#define _POSIX_PIPE_BUF			512
#define	_POSIX_RTSIG_MAX		8
#define	_POSIX_SEM_NSEMS_MAX	256
#define	_POSIX_SEM_VALUE_MAX	32767
#define	_POSIX_SIGQUEUE_MAX		32
#define _POSIX_SSIZE_MAX		32767
#define _POSIX_STREAM_MAX		8
#define	_POSIX_TIMER_MAX		32
#define _POSIX_TZNAME_MAX		3
#define NGROUPS_MAX 10
#define PASS_MAX	64
#define SSIZE_MAX LONG_MAX
#define TZNAME_MAX _POSIX_TZNAME_MAX
#ifdef _LIMITS_EXTENSION
#include <sys/limits.h>
#endif
#endif
#endif