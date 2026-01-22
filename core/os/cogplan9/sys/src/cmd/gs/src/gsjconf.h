#ifndef gsjconf_INCLUDED
#  define gsjconf_INCLUDED
#include "arch.h"
#ifdef __PROTOTYPES__
#  define HAVE_PROTOTYPES
#endif
#define HAVE_UNSIGNED_CHAR
#define HAVE_UNSIGNED_SHORT
#undef CHAR_IS_UNSIGNED
#ifdef __STDC__
#  define HAVE_STDDEF_H
#  define HAVE_STDLIB_H
#endif
#undef NEED_BSD_STRINGS
#undef NEED_SYS_TYPES_H
#undef NEED_FAR_POINTERS
#undef NEED_SHORT_EXTERNAL_NAMES
#undef INCOMPLETE_TYPES_BROKEN
#if ARCH_SIZEOF_INT <= 2
#  undef MAX_ALLOC_CHUNK
#  define MAX_ALLOC_CHUNK 0xfff0
#endif
#ifdef JPEG_INTERNALS
#if ARCH_ARITH_RSHIFT == 0
#  define RIGHT_SHIFT_IS_UNSIGNED
#else
#  undef RIGHT_SHIFT_IS_UNSIGNED
#endif
#endif
#endif