#ifndef malloc__INCLUDED
#  define malloc__INCLUDED
#include "std.h"
#ifdef __TURBOC__
#  include <alloc.h>
#else
#  if defined(BSD4_2) || defined(apollo) || defined(vax) || defined(sequent) || defined(UTEK)
#    if defined(_POSIX_SOURCE) || (defined(__STDC__) && (!defined(sun) || defined(__svr4__)))
#      include <stdlib.h>
#    else
extern char *malloc();
extern void free();
#    endif
#  else
#    if defined(_HPUX_SOURCE) || defined(__CONVEX__) || defined(__convex__) || defined(__OSF__) || defined(__386BSD__) || defined(_POSIX_SOURCE) || defined(__STDC__) || defined(VMS)
#      include <stdlib.h>
#    else
#      include <malloc.h>
#    endif
#  endif
#endif
#ifdef linux
#  define malloc__need_realloc
void *gs_realloc(void *, size_t, size_t);
#else
#  define gs_realloc(ptr, old_size, new_size) realloc(ptr, new_size)
#endif
#endif