#ifndef gxfarith_INCLUDED
#  define gxfarith_INCLUDED
#include "gconfigv.h"
#include "gxarith.h"
# if USE_FPU <= 0 && arch_floats_are_IEEE && (arch_sizeof_float == arch_sizeof_int || arch_sizeof_float == arch_sizeof_long)
#  if arch_sizeof_float == arch_sizeof_int
typedef int _f_int_t;
typedef uint _f_uint_t;
#  else
typedef long _f_int_t;
typedef ulong _f_uint_t;
#  endif
#  define _f_as_int(f) *(const _f_int_t *)(&(f))
#  define _f_as_uint(f) *(const _f_uint_t *)(&(f))
#  if arch_sizeof_double == arch_sizeof_int
#    define _d_int_t int
#  else
#   if arch_sizeof_double == arch_sizeof_long
#    define _d_int_t long
#   endif
#  endif
#  define _d_uint_t unsigned _d_int_t
#  define _d_as_int(f) *(const _d_int_t *)(&(f))
#  define _d_as_uint(f) *(const _d_uint_t *)(&(f))
#  define _ftest(v,f,n)\
(sizeof(v)==sizeof(float)?(f):(n))
#  ifdef _d_int_t
#    define _fdtest(v,f,d,n)\
(sizeof(v)==sizeof(float)?(f):sizeof(v)==sizeof(double)?(d):(n))
#  else
#    define _fdtest(v,f,d,n)\
_ftest(v,f,n)
#  endif
#  undef is_fzero
#  define is_fzero(f)	\
_fdtest(f, (_f_as_int(f) << 1) == 0, (_d_as_int(f) << 1) == 0,\
(f) == 0.0)
#  undef is_fzero2
#  define is_fzero2(f1,f2)\
(sizeof(f1) == sizeof(float) && sizeof(f2) == sizeof(float) ?\
((_f_as_int(f1) | _f_as_int(f2)) << 1) == 0 :\
(f1) == 0.0 && (f2) == 0.0)
#  undef is_fneg
#  if arch_is_big_endian
#    define _is_fnegb(f) (*(const byte *)&(f) >= 0x80)
#  else
#    define _is_fnegb(f) (((const byte *)&(f))[sizeof(f) - 1] >= 0x80)
#  endif
#  if arch_sizeof_float == arch_sizeof_int
#    define is_fneg(f)\
(sizeof(f) == sizeof(float) ? _f_as_int(f) < 0 :\
_is_fnegb(f))
#  else
#    define is_fneg(f) _is_fnegb(f)
#  endif
#  define IEEE_expt 0x7f800000
#  define IEEE_f1 0x3f800000
#  undef is_fge1
#  if arch_sizeof_float == arch_sizeof_int
#    define is_fge1(f)\
(sizeof(f) == sizeof(float) ?\
(_f_as_int(f)) >= IEEE_f1 :\
(f) >= 1.0)
#  else
#    define is_fge1(f)\
(sizeof(f) == sizeof(float) ?\
(int)(_f_as_int(f) >> 16) >= (IEEE_f1 >> 16) :\
(f) >= 1.0)
#  endif
#  undef f_fits_in_ubits
#  undef f_fits_in_bits
#  define _f_bits(n) (4.0 * (1L << ((n) - 2)))
#  define f_fits_in_ubits(f, n)\
_ftest(f, _f_as_uint(f) < (_f_uint_t)IEEE_f1 + ((_f_uint_t)(n) << 23),\
(f) >= 0 && (f) < _f_bits(n))
#  define f_fits_in_bits(f, n)\
_ftest(f, (_f_as_uint(f) & IEEE_expt) < IEEE_f1 + ((_f_uint_t)((n)-1) << 23),\
(f) >= -_f_bits((n)-1) && (f) < _f_bits((n)-1))
# endif
double gs_sin_degrees(double angle);
double gs_cos_degrees(double angle);
typedef struct gs_sincos_s {
double sin, cos;
bool orthogonal;
} gs_sincos_t;
void gs_sincos_degrees(double angle, gs_sincos_t * psincos);
int gs_atan2_degrees(double y, double x, double *pangle);
#endif