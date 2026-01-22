#ifndef CONFIG_H
#define CONFIG_H
#define PLAN9
#ifndef _ALL_SOURCE
#endif
#define HAVE_SYS_WAIT_H 1
#define HAVE_UNISTD_H
#define _POSIX_1_SOURCE 2
#undef _POSIX_SOURCE
#define _POSIX_SOURCE 1
#define RETSIGTYPE void
#define TIME_WITH_SYS_TIME
#define DUP2_BROKEN 1
#define RETSIGVAL
#define NO_PGRP 1
#undef void
#define volatile
#define HAVE_PROTOTYPES
#define clock_t long
#define rlim_t long
#define TIME_DECLARED
#define _BSD_EXTENSION
#define HAVE_SYS_ERRLIST
#define SYS_ERRLIST_DECLARED
#define HAVE_TERMIOS_H
#define HAVE_MEMSET
#define HAVE_MEMMOVE
#define HAVE_LSTAT
#define POSIX_SYS_WAIT
#define DEFAULT_PATH "/bin:."
#define KSH 1
#define POSIXLY_CORRECT
#define SIZEOF_INT sizeof(int)
#define SIZEOF_LONG sizeof(long)
#define HAVE_DUP2
#define HAVE_GETCWD
#define HAVE_GETGROUPS
#define HAVE_SIGSETJMP
#define HAVE_STRERROR
#define HAVE_STRSTR
#define HAVE_TCSETPGRP
#define HAVE_WAITPID 1
#define HAVE_DIRENT_H 1
#define HAVE_FCNTL_H 1
#define HAVE_LIMITS_H 1
#define HAVE_STDDEF_H 1
#define HAVE_STDLIB_H 1
#define HAVE_STRING_H 1
#define HAVE_SYS_PARAM_H 1
#define HAVE_SYS_RESOURCE_H 1
#define HAVE_SYS_TIME_H 1
#define HAVE_SYS_WAIT_H 1
#include "conf-end.h"
#endif