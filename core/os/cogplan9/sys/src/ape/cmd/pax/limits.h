#ifndef _PAX_LIMITS_H
#define _PAX_LIMITS_H
#if defined(__STDC__) || defined(_POSIX_SOURCE)
# include <limits.h>
#endif
#ifndef _POSIX_SOURCE
#define MAX_INPUT 256
#define NGROUPS_MAX 1
#define PASS_MAX 8
#define PID_MAX 30000
#define UID_MAX 32000
#define ARG_MAX 4096
#define CHILD_MAX 6
#define MAX_CANON 256
#define OPEN_MAX 16
#define NAME_MAX 14
#define PATH_MAX 255
#define LINK_MAX 8
#define PIPE_BUF 512
#endif
#endif