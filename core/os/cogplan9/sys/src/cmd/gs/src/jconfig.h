#ifndef stdpn_INCLUDED
#  define stdpn_INCLUDED
#define P0() void
#define P1(t1) t1
#define P2(t1,t2) t1,t2
#define P3(t1,t2,t3) t1,t2,t3
#define P4(t1,t2,t3,t4) t1,t2,t3,t4
#define P5(t1,t2,t3,t4,t5) t1,t2,t3,t4,t5
#define P6(t1,t2,t3,t4,t5,t6) t1,t2,t3,t4,t5,t6
#define P7(t1,t2,t3,t4,t5,t6,t7) t1,t2,t3,t4,t5,t6,t7
#define P8(t1,t2,t3,t4,t5,t6,t7,t8) t1,t2,t3,t4,t5,t6,t7,t8
#define P9(t1,t2,t3,t4,t5,t6,t7,t8,t9) t1,t2,t3,t4,t5,t6,t7,t8,t9
#define P10(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10) t1,t2,t3,t4,t5,t6,t7,t8,t9,t10
#define P11(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11) t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11
#define P12(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12) t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12
#define P13(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13) t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13
#define P14(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14) t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14
#define P15(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15) t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15
#define P16(t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16) t1,t2,t3,t4,t5,t6,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16
#endif
#ifndef stdpre_INCLUDED
#  define stdpre_INCLUDED
#if (defined(MSDOS) || defined(_MSDOS)) && !defined(__MSDOS__)
#  define __MSDOS__
#endif
#if defined(__osf__) && !defined(__OSF__)
#  define __OSF__
#endif
#if defined(M_SYSV) && !defined(SYSV)
#  define SYSV
#endif
#if defined(M_SYS3) && !defined(__SVR3)
#  define __SVR3
#endif
#if defined(__STDC__) || defined(__MSDOS__) || defined(__convex__) || defined(VMS) || defined(__OSF__) || defined(__WIN32__) || defined(__IBMC__) || defined(M_UNIX) || defined(__GNUC__) || defined(__BORLANDC__)
# if !(defined(M_XENIX) && !defined(__GNUC__))
#  define __PROTOTYPES__
# endif
#endif
#ifndef __FILE__
#  define __FILE__ NULL
#endif
#ifndef __LINE__
#  define __LINE__ 0
#endif
#ifndef __PROTOTYPES__
#  undef const
#  define const
#  undef volatile
#  define volatile
#endif
#ifdef __DECC
#  undef inline
#  define inline __inline
#else
#  ifdef __GNUC__
#    undef inline
#    define inline __inline__
#  else
#    if !(defined(__MWERKS__) || defined(inline))
#      define inline
#    endif
#  endif
#endif
#ifdef __GNUC__
#  define extern_inline extern inline
#endif
#ifdef extern_inline
#  define HAVE_EXTERN_INLINE 1
#else
#  define extern_inline
#  define HAVE_EXTERN_INLINE 0
#endif
#define DISCARD(expr) ((void)(expr))
#define discard(expr) DISCARD(expr)
#ifdef __WATCOMC__
#  pragma disable_message(124);
#endif
#ifdef __GNUC__
# if __GNUC__ == 2 & (7 < __GNUC_MINOR__ <= 95)
#  define ALIGNMENT_ALIASING_BUG
# endif
#endif
#define size_of(x) ((int)(sizeof(x)))
#undef far_data
#define far_data
#define countof(a) (sizeof(a) / sizeof((a)[0]))
#define count_of(a) (size_of(a) / size_of((a)[0]))
#ifdef __MWERKS__
#define offset_of(type, memb)\
((int) &((type *) 0)->memb)
#else
#define offset_of(type, memb)\
((int) ( (char *)&((type *)0)->memb - (char *)((type *)0) ))
#endif
#define ALIGNMENT_MOD(ptr, modu)\
((uint)( ((const char *)(ptr) - (const char *)0) & ((modu) - 1) ))
typedef unsigned char byte;
typedef unsigned char uchar;
typedef unsigned short ushort;
typedef unsigned int uint;
typedef unsigned long ulong;
#define bool bool_
#define uchar uchar_
#define uint uint_
#define ushort ushort_
#define ulong ulong_
#include <sys/types.h>
#undef bool
#undef uchar
#undef uint
#undef ushort
#undef ulong
#ifndef __cplusplus
#ifdef __BEOS__
typedef unsigned char bool;
#else
typedef int bool;
#endif
#endif
#ifndef __MACOS__
#undef false
#define false ((bool)0)
#undef true
#define true ((bool)1)
#endif
#if defined(__TURBOC__) || defined(_MSC_VER)
typedef unsigned long ptr_ord_t;
#else
typedef const char *ptr_ord_t;
#endif
#define _PTR_CMP(p1, rel, p2)  ((ptr_ord_t)(p1) rel (ptr_ord_t)(p2))
#define PTR_LE(p1, p2) _PTR_CMP(p1, <=, p2)
#define PTR_LT(p1, p2) _PTR_CMP(p1, <, p2)
#define PTR_GE(p1, p2) _PTR_CMP(p1, >=, p2)
#define PTR_GT(p1, p2) _PTR_CMP(p1, >, p2)
#define PTR_BETWEEN(ptr, lo, hi)\
(PTR_GE(ptr, lo) && PTR_LT(ptr, hi))
#ifndef min
#  define min(a, b) (((a) < (b)) ? (a) : (b))
#endif
#ifndef max
#  define max(a, b) (((a) > (b)) ? (a) : (b))
#endif
#define ROUND_DOWN(value, modulus)\
( (modulus) & ((modulus) - 1) ?	\
(value) - (value) % (modulus) :\
(value) & -(modulus) )
#define ROUND_UP(value, modulus)\
( (modulus) & ((modulus) - 1) ?	\
((value) + ((modulus) - 1)) / (modulus) * (modulus) :\
((value) + ((modulus) - 1)) & -(modulus) )
#define round_up(v, m) ROUND_UP(v, m)
#define round_down(v, m) ROUND_DOWN(v, m)
typedef double floatp;
#define BEGIN	do {
#define END	} while (0)
#ifndef DO_NOTHING
#  define DO_NOTHING BEGIN END
#endif
typedef const char *client_name_t;
#define client_name_string(cname) (cname)
#define public
#ifdef NOPRIVATE
# define private_
#else
# define private_ static
#endif
#define private private_
#ifndef stdpn_INCLUDED
#  define stdpn_INCLUDED
#include "stdpn.h"
#endif
#if defined(VMS)
#  define exit_FAILED 18
#  if (defined(OLD_VMS_C) || !defined(__DECC))
#    define exit_OK 1
#  else
#    define exit_OK 0
#  endif
#else
#  define exit_OK 0
#  define exit_FAILED 1
#endif
#endif
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