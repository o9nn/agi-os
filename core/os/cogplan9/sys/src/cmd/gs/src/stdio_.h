#ifndef stdio__INCLUDED
#  define stdio__INCLUDED
#include "std.h"
#include <stdio.h>
#ifdef VMS
#  ifdef __DECC
#    include <unixio.h>
#  endif
#  if ( __VMS_VER < 70000000 )
#    define unlink(fname) delete(fname)
#  endif
#else
#if !defined(const)
int unlink(const char *);
#endif
#endif
#ifdef Plan9
#  undef sclose
#  define sclose(s) Sclose(s)
#endif
#ifndef SEEK_SET
#  define SEEK_SET 0
#endif
#ifndef SEEK_CUR
#  define SEEK_CUR 1
#endif
#ifndef SEEK_END
#  define SEEK_END 2
#endif
#if defined(_MSC_VER)
#  define fdopen(handle,mode) _fdopen(handle,mode)
#  define fileno(file) _fileno(file)
#endif
#endif