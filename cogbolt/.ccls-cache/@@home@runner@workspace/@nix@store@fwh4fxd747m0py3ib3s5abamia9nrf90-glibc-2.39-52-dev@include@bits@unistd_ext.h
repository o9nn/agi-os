#ifndef _UNISTD_H
# error "Never include <bits/unistd_ext.h> directly; use <unistd.h> instead."
#endif
#ifdef __USE_GNU
extern __pid_t gettid (void) __THROW;
#ifdef __has_include
# if __has_include ("linux/close_range.h")
# include "linux/close_range.h"
# endif
#endif
#ifndef CLOSE_RANGE_UNSHARE
# define CLOSE_RANGE_UNSHARE (1U << 1)
#endif
#ifndef CLOSE_RANGE_CLOEXEC
# define CLOSE_RANGE_CLOEXEC (1U << 2)
#endif
#endif