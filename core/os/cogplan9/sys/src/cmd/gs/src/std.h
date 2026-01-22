#ifndef std_INCLUDED
#  define std_INCLUDED
#include "stdpre.h"
#include "arch.h"
#define arch_align_short_mod ARCH_ALIGN_SHORT_MOD
#define arch_align_int_mod ARCH_ALIGN_INT_MOD
#define arch_align_long_mod ARCH_ALIGN_LONG_MOD
#define arch_align_ptr_mod ARCH_ALIGN_PTR_MOD
#define arch_align_float_mod ARCH_ALIGN_FLOAT_MOD
#define arch_align_double_mod ARCH_ALIGN_DOUBLE_MOD
#define arch_align_struct_mod ARCH_ALIGN_STRUCT_MOD
#define arch_log2_sizeof_short ARCH_LOG2_SIZEOF_SHORT
#define arch_log2_sizeof_int ARCH_LOG2_SIZEOF_INT
#define arch_log2_sizeof_long ARCH_LOG2_SIZEOF_LONG
#define arch_sizeof_ptr ARCH_SIZEOF_PTR
#define arch_sizeof_float ARCH_SIZEOF_FLOAT
#define arch_sizeof_double ARCH_SIZEOF_DOUBLE
#define arch_float_mantissa_bits ARCH_FLOAT_MANTISSA_BITS
#define arch_double_mantissa_bits ARCH_DOUBLE_MANTISSA_BITS
#define arch_max_uchar ARCH_MAX_UCHAR
#define arch_max_ushort ARCH_MAX_USHORT
#define arch_max_uint ARCH_MAX_UINT
#define arch_max_ulong ARCH_MAX_ULONG
#define arch_cache1_size ARCH_CACHE1_SIZE
#define arch_cache2_size ARCH_CACHE2_SIZE
#define arch_is_big_endian ARCH_IS_BIG_ENDIAN
#define arch_ptrs_are_signed ARCH_PTRS_ARE_SIGNED
#define arch_floats_are_IEEE ARCH_FLOATS_ARE_IEEE
#define arch_arith_rshift ARCH_ARITH_RSHIFT
#define arch_can_shift_full_long ARCH_CAN_SHIFT_FULL_LONG
#define ARCH_ALIGN_MEMORY_MOD\
(((ARCH_ALIGN_LONG_MOD - 1) | (ARCH_ALIGN_PTR_MOD - 1) |\
(ARCH_ALIGN_DOUBLE_MOD - 1) | (ARCH_ALIGN_STRUCT_MOD - 1)) + 1)
#define arch_align_memory_mod ARCH_ALIGN_MEMORY_MOD
#define ARCH_SIZEOF_CHAR (1 << ARCH_LOG2_SIZEOF_CHAR)
#define ARCH_SIZEOF_SHORT (1 << ARCH_LOG2_SIZEOF_SHORT)
#define ARCH_SIZEOF_INT (1 << ARCH_LOG2_SIZEOF_INT)
#define ARCH_SIZEOF_LONG (1 << ARCH_LOG2_SIZEOF_LONG)
#define ARCH_SIZEOF_LONG_LONG (1 << ARCH_LOG2_SIZEOF_LONG_LONG)
#define ARCH_INTS_ARE_SHORT (ARCH_SIZEOF_INT == ARCH_SIZEOF_SHORT)
#define arch_sizeof_short ARCH_SIZEOF_SHORT
#define arch_sizeof_int ARCH_SIZEOF_INT
#define arch_sizeof_long ARCH_SIZEOF_LONG
#define arch_ints_are_short ARCH_INTS_ARE_SHORT
#define ARCH_SMALL_MEMORY (ARCH_SIZEOF_INT <= 2)
#define arch_small_memory ARCH_SMALL_MEMORY
#if arch_sizeof_short == 2
typedef ushort bits16;
#endif
#if arch_sizeof_int == 4
typedef uint bits32;
#else
# if arch_sizeof_long == 4
typedef ulong bits32;
# endif
#endif
#define min_short (-1 << (arch_sizeof_short * 8 - 1))
#define max_short (~min_short)
#define min_int (-1 << (arch_sizeof_int * 8 - 1))
#define max_int (~min_int)
#define min_long (-1L << (arch_sizeof_long * 8 - 1))
#define max_long (~min_long)
#define max_uchar arch_max_uchar
#define max_ushort arch_max_ushort
#define max_uint arch_max_uint
#define max_ulong arch_max_ulong
#if arch_ptrs_are_signed
#  define min_ptr min_long
#  define max_ptr max_long
#else
#  define min_ptr ((ulong)0)
#  define max_ptr max_ulong
#endif
#define arith_rshift_slow(x,n) ((x) < 0 ? ~(~(x) >> (n)) : (x) >> (n))
#if arch_arith_rshift == 2
#  define arith_rshift(x,n) ((x) >> (n))
#  define arith_rshift_1(x) ((x) >> 1)
#else
#if arch_arith_rshift == 1
#  define arith_rshift(x,n) ((x) >> (n))
#  define arith_rshift_1(x) arith_rshift_slow(x,1)
#else
#  define arith_rshift(x,n) arith_rshift_slow(x,n)
#  define arith_rshift_1(x) arith_rshift_slow(x,1)
#endif
#endif
#include <stdio.h>
#ifndef gs_memory_DEFINED
#  define gs_memory_DEFINED
typedef struct gs_memory_s gs_memory_t;
#endif
#define init_proc(proc)\
int proc(gs_memory_t *)
#define dpf errprintf
#define epf errprintf
int outwrite(const gs_memory_t *mem, const char *str, int len);
int errwrite(const char *str, int len);
void outflush(const gs_memory_t *mem);
void errflush(void);
#ifdef __PROTOTYPES__
int outprintf(const gs_memory_t *mem, const char *fmt, ...);
int errprintf(const char *fmt, ...);
#else
int outprintf();
int errprintf();
#endif
#if __LINE__
void dprintf_file_and_line(const char *, int);
#  define _dpl dprintf_file_and_line(__FILE__, __LINE__),
#else
void dprintf_file_only(const char *);
#  define _dpl dprintf_file_only(__FILE__),
#endif
void dflush(void);
#define dputc(chr) dprintf1("%c", chr)
#define dlputc(chr) dlprintf1("%c", chr)
#define dputs(str) dprintf1("%s", str)
#define dlputs(str) dlprintf1("%s", str)
#define dprintf(str)\
dpf(str)
#define dlprintf(str)\
(_dpl dpf(str))
#define dprintf1(str,arg1)\
dpf(str, arg1)
#define dlprintf1(str,arg1)\
(_dpl dprintf1(str, arg1))
#define dprintf2(str,arg1,arg2)\
dpf(str, arg1, arg2)
#define dlprintf2(str,arg1,arg2)\
(_dpl dprintf2(str, arg1, arg2))
#define dprintf3(str,arg1,arg2,arg3)\
dpf(str, arg1, arg2, arg3)
#define dlprintf3(str,arg1,arg2,arg3)\
(_dpl dprintf3(str, arg1, arg2, arg3))
#define dprintf4(str,arg1,arg2,arg3,arg4)\
dpf(str, arg1, arg2, arg3, arg4)
#define dlprintf4(str,arg1,arg2,arg3,arg4)\
(_dpl dprintf4(str, arg1, arg2, arg3, arg4))
#define dprintf5(str,arg1,arg2,arg3,arg4,arg5)\
dpf(str, arg1, arg2, arg3, arg4, arg5)
#define dlprintf5(str,arg1,arg2,arg3,arg4,arg5)\
(_dpl dprintf5(str, arg1, arg2, arg3, arg4, arg5))
#define dprintf6(str,arg1,arg2,arg3,arg4,arg5,arg6)\
dpf(str, arg1, arg2, arg3, arg4, arg5, arg6)
#define dlprintf6(str,arg1,arg2,arg3,arg4,arg5,arg6)\
(_dpl dprintf6(str, arg1, arg2, arg3, arg4, arg5, arg6))
#define dprintf7(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7)\
dpf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7)
#define dlprintf7(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7)\
(_dpl dprintf7(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7))
#define dprintf8(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)\
dpf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
#define dlprintf8(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)\
(_dpl dprintf8(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
#define dprintf9(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)\
dpf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9)
#define dlprintf9(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)\
(_dpl dprintf9(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9))
#define dprintf10(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)\
dpf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10)
#define dlprintf10(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)\
(_dpl dprintf10(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10))
#define dprintf11(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)\
dpf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
#define dlprintf11(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11)\
(_dpl dprintf11(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11))
#define dprintf12(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)\
dpf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
#define dlprintf12(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10,arg11,arg12)\
(_dpl dprintf12(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12))
void printf_program_ident(const gs_memory_t *mem, const char *program_name, long revision_number);
void eprintf_program_ident(const char *program_name, long revision_number);
const char *gs_program_name(void);
long gs_revision_number(void);
#define _epi eprintf_program_ident(gs_program_name(), gs_revision_number()),
#define eprintf(str)\
(_epi epf(str))
#define eprintf1(str,arg1)\
(_epi epf(str, arg1))
#define eprintf2(str,arg1,arg2)\
(_epi epf(str, arg1, arg2))
#define eprintf3(str,arg1,arg2,arg3)\
(_epi epf(str, arg1, arg2, arg3))
#define eprintf4(str,arg1,arg2,arg3,arg4)\
(_epi epf(str, arg1, arg2, arg3, arg4))
#define eprintf5(str,arg1,arg2,arg3,arg4,arg5)\
(_epi epf(str, arg1, arg2, arg3, arg4, arg5))
#define eprintf6(str,arg1,arg2,arg3,arg4,arg5,arg6)\
(_epi epf(str, arg1, arg2, arg3, arg4, arg5, arg6))
#define eprintf7(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7)\
(_epi epf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7))
#define eprintf8(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)\
(_epi epf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
#define eprintf9(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)\
(_epi epf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9))
#define eprintf10(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)\
(_epi epf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10))
#if __LINE__
void lprintf_file_and_line(const char *, int);
#  define _epl _epi lprintf_file_and_line(__FILE__, __LINE__),
#else
void lprintf_file_only(const char *);
#  define _epl _epi lprintf_file_only(__FILE__)
#endif
#define lprintf(str)\
(_epl epf(str))
#define lprintf1(str,arg1)\
(_epl epf(str, arg1))
#define lprintf2(str,arg1,arg2)\
(_epl epf(str, arg1, arg2))
#define lprintf3(str,arg1,arg2,arg3)\
(_epl epf(str, arg1, arg2, arg3))
#define lprintf4(str,arg1,arg2,arg3,arg4)\
(_epl epf(str, arg1, arg2, arg3, arg4))
#define lprintf5(str,arg1,arg2,arg3,arg4,arg5)\
(_epl epf(str, arg1, arg2, arg3, arg4, arg5))
#define lprintf6(str,arg1,arg2,arg3,arg4,arg5,arg6)\
(_epl epf(str, arg1, arg2, arg3, arg4, arg5, arg6))
#define lprintf7(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7)\
(_epl epf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7))
#define lprintf8(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8)\
(_epl epf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8))
#define lprintf9(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9)\
(_epl epf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9))
#define lprintf10(str,arg1,arg2,arg3,arg4,arg5,arg6,arg7,arg8,arg9,arg10)\
(_epl epf(str, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10))
#ifndef gs_memory_DEFINED
#  define gs_memory_DEFINED
typedef struct gs_memory_s gs_memory_t;
#endif
#define init_proc(proc)\
int proc(gs_memory_t *)
#endif