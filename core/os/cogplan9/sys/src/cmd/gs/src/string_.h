#ifndef string__INCLUDED
#  define string__INCLUDED
#include "std.h"
#ifdef BSD4_2
#  include <strings.h>
#  define strchr index
#else
#  ifdef MEMORY__NEED_MEMMOVE
#    undef memmove
#  endif
#  include <string.h>
#  if defined(THINK_C)
#    define strlen (uint)strlen
#  endif
#  ifdef MEMORY__NEED_MEMMOVE
#    define memmove(dest,src,len) gs_memmove(dest,src,len)
#  endif
#endif
#endif