#ifndef _GLIBCXX_OS_DEFINES
#define _GLIBCXX_OS_DEFINES 1
#define __NO_CTYPE 1
#include <features.h>
#if __GLIBC_PREREQ(2,15) && defined(_GNU_SOURCE)
# undef _GLIBCXX_HAVE_GETS
#endif
#define _GLIBCXX_NO_OBSOLETE_ISINF_ISNAN_DYNAMIC __GLIBC_PREREQ(2,23)
#if __GLIBC_PREREQ(2, 27)
# define _GLIBCXX_NATIVE_THREAD_ID pthread_self()
#else
# define _GLIBCXX_NATIVE_THREAD_ID \
(__gthread_active_p() ? __gthread_self() : (__gthread_t)1)
#endif
#if __GLIBC_PREREQ(2, 34)
# define _GLIBCXX_GTHREAD_USE_WEAK 0
#endif
#endif