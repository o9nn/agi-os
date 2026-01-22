#ifndef stdint__INCLUDED
# define stdint__INCLUDED
#include "std.h"
#ifndef HAVE_STDINT_H
# ifdef __MACOS__
# define HAVE_STDINT_H
# endif
#endif
#if defined(HAVE_STDINT_H)
# include <stdint.h>
# define STDINT_TYPES_DEFINED
#elif defined(SYS_TYPES_HAS_STDINT_TYPES)
# define STDINT_TYPES_DEFINED
#endif
#ifndef STDINT_TYPES_DEFINED
# if defined(__WIN32__)
typedef signed char int8_t;
typedef short int int16_t;
typedef int int32_t;
typedef __int64 int64_t;
typedef unsigned char uint8_t;
typedef unsigned short int uint16_t;
typedef unsigned int uint32_t;
typedef unsigned __int64 uint64_t;
# define STDINT_TYPES_DEFINED
# elif defined(__VMS)
# include <inttypes.h>
# define STDINT_TYPES_DEFINED
# elif defined(__CYGWIN__)
typedef u_int8_t uint8_t;
typedef u_int16_t uint16_t;
typedef u_int32_t uint32_t;
typedef u_int64_t uint64_t;
# define STDINT_TYPES_DEFINED
# endif
#endif
#ifndef STDINT_TYPES_DEFINED
# if ARCH_SIZEOF_CHAR == 1
typedef signed char int8_t;
typedef unsigned char uint8_t;
# endif
# if ARCH_SIZEOF_SHORT == 2
typedef signed short int16_t;
typedef unsigned short uint16_t;
# else
# if ARCH_SIZEOF_INT == 2
typedef signed int int16_t;
typedef unsigned int uint16_t;
# endif
# endif
# if ARCH_SIZEOF_INT == 4
typedef signed int int32_t;
typedef unsigned int uint32_t;
# else
# if ARCH_SIZEOF_LONG == 4
typedef signed long int32_t;
typedef unsigned long uint32_t;
# else
# if ARCH_SIZEOF_SHORT == 4
typedef signed short int32_t;
typedef unsigned short uint32_t;
# endif
# endif
# endif
# if ARCH_SIZEOF_INT == 8
typedef signed int int64_t;
typedef unsigned int uint64_t;
# else
# if ARCH_SIZEOF_LONG == 8
typedef signed long int64_t;
typedef unsigned long uint64_t;
# else
# ifdef ARCH_SIZEOF_LONG_LONG
# if ARCH_SIZEOF_LONG_LONG == 8
typedef signed long long int64_t;
typedef unsigned long long uint64_t;
# endif
# endif
# endif
# endif
# define STDINT_TYPES_DEFINED
#endif
#endif