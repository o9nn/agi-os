#ifndef memory__INCLUDED
#  define memory__INCLUDED
#include "std.h"
#ifdef __TURBOC__
#  ifdef __WIN32__
#    define memcmp_inline(b1,b2,len) memcmp(b1,b2,len)
#  else
#    define memcmp_inline(b1,b2,len) __memcmp__(b1,b2,len)
#  endif
#  include <mem.h>
#else
#  define memcmp_inline(b1,b2,len) memcmp(b1,b2,len)
#  if defined(VMS) || defined(_POSIX_SOURCE) || (defined(__STDC__) && (!defined(sun) || defined(__svr4__))) || defined(_HPUX_SOURCE) || defined(__WATCOMC__) || defined(THINK_C) || defined(bsdi) || defined(__FreeBSD) || (defined(_MSC_VER) && _MSC_VER >= 1000)
#    include <string.h>
#  else
#    if defined(BSD4_2) || defined(UTEK)
extern bcopy(), bcmp(), bzero();
#	 define memcpy(dest,src,len) bcopy(src,dest,len)
#	 define memcmp(b1,b2,len) bcmp(b1,b2,len)
#	 define MEMORY__NEED_MEMMOVE
#        include <sys/types.h>
#	 define MEMORY__NEED_MEMSET
#	 if defined(UTEK)
#          define MEMORY__NEED_MEMCHR
#        endif
#    else
#      include <memory.h>
#      if defined(__SVR3) || defined(sun)
#	 define MEMORY__NEED_MEMMOVE
#        include <sys/types.h>
#      endif
#    endif
#  endif
#endif
#ifdef PROFILE
#  define MEMORY__NEED_MEMCPY
#  define MEMORY__NEED_MEMMOVE
#  define MEMORY__NEED_MEMSET
#endif
#ifdef MEMORY__NEED_MEMMOVE
#  undef memmove
#  define memmove(dest,src,len) gs_memmove(dest,src,len)
void *gs_memmove(void *, const void *, size_t);
#endif
#ifdef MEMORY__NEED_MEMCPY
#  undef memcpy
#  define memcpy(dest,src,len) gs_memcpy(dest,src,len)
void *gs_memcpy(void *, const void *, size_t);
#endif
#ifdef MEMORY__NEED_MEMSET
#  undef memset
#  define memset(dest,ch,len) gs_memset(dest,ch,len)
void *gs_memset(void *, int, size_t);
#endif
#ifdef MEMORY__NEED_MEMCHR
#  undef memchr
#  define memchr(ptr,ch,len) gs_memchr(ptr,ch,len)
void *gs_memchr(const void *, int, size_t);
#endif
#endif