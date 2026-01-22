#ifndef gxfixed_INCLUDED
# define gxfixed_INCLUDED
typedef long fixed;
typedef ulong ufixed;
#define ARCH_SIZEOF_FIXED ARCH_SIZEOF_LONG
#define max_fixed max_long
#define min_fixed min_long
#define fixed_0 0L
#define fixed_epsilon 1L
#define _fixed_shift 8
#define fixed_fraction_bits _fixed_shift
#define fixed_int_bits (sizeof(fixed) * 8 - _fixed_shift)
#define fixed_scale (1<<_fixed_shift)
#define _fixed_rshift(x) arith_rshift(x,_fixed_shift)
#define _fixed_round_v (fixed_scale>>1)
#define _fixed_fraction_v (fixed_scale-1)
#define _fixed_pixround_v (_fixed_round_v - fixed_epsilon)
#define int2fixed(i) ((fixed)(i)<<_fixed_shift)
#define fixed_1 (fixed_epsilon << _fixed_shift)
#define fixed_half (fixed_1 >> 1)
#define fixed2int(x) ((int)_fixed_rshift(x))
#define fixed2int_rounded(x) ((int)_fixed_rshift((x)+_fixed_round_v))
#define fixed2int_ceiling(x) ((int)_fixed_rshift((x)+_fixed_fraction_v))
#define fixed_pre_pixround(x) ((x)+_fixed_pixround_v)
#define fixed2int_pixround(x) fixed2int(fixed_pre_pixround(x))
#define fixed_is_int(x) !((x)&_fixed_fraction_v)
#if arch_ints_are_short & !arch_is_big_endian
# define _fixed_hi(x) *((const uint *)&(x)+1)
# define _fixed_lo(x) *((const uint *)&(x))
# define fixed2int_var(x)\
((int)((_fixed_hi(x) << (16-_fixed_shift)) +\
(_fixed_lo(x) >> _fixed_shift)))
# define fixed2int_var_rounded(x)\
((int)((_fixed_hi(x) << (16-_fixed_shift)) +\
(((_fixed_lo(x) >> (_fixed_shift-1))+1)>>1)))
# define fixed2int_var_ceiling(x)\
(fixed2int_var(x) -\
arith_rshift((int)-(_fixed_lo(x) & _fixed_fraction_v), _fixed_shift))
#else
# define fixed2int_var(x) fixed2int(x)
# define fixed2int_var_rounded(x) fixed2int_rounded(x)
# define fixed2int_var_ceiling(x) fixed2int_ceiling(x)
#endif
#define fixed2int_var_pixround(x) fixed2int_pixround(x)
#define fixed2long(x) ((long)_fixed_rshift(x))
#define fixed2long_rounded(x) ((long)_fixed_rshift((x)+_fixed_round_v))
#define fixed2long_ceiling(x) ((long)_fixed_rshift((x)+_fixed_fraction_v))
#define fixed2long_pixround(x) ((long)_fixed_rshift((x)+_fixed_pixround_v))
#define float2fixed(f) ((fixed)((f)*(float)fixed_scale))
#define float2fixed_rounded(f) ((fixed)floor((f)*(float)fixed_scale + 0.5))
#define fixed2float(x) ((x)*(1.0/fixed_scale))
#define fixed_floor(x) ((x)&(-1L<<_fixed_shift))
#define fixed_rounded(x) (((x)+_fixed_round_v)&(-1L<<_fixed_shift))
#define fixed_ceiling(x) (((x)+_fixed_fraction_v)&(-1L<<_fixed_shift))
#define fixed_pixround(x) (((x)+_fixed_pixround_v)&(-1L<<_fixed_shift))
#define fixed_fraction(x) ((int)(x)&_fixed_fraction_v)
#define fixed_truncated(x) ((x) < 0 ? fixed_ceiling(x) : fixed_floor(x))
#if arch_sizeof_int == arch_sizeof_long
# define max_int_in_fixed fixed2int(max_fixed)
# define min_int_in_fixed fixed2int(min_fixed)
#else
# define max_int_in_fixed max_int
# define min_int_in_fixed min_int
#endif
#ifdef USE_FPU
# define USE_FPU_FIXED (USE_FPU < 0 && arch_floats_are_IEEE && arch_sizeof_long == 4)
#else
# define USE_FPU_FIXED 0
#endif
#define CHECK_SET_FIXED_SUM(r, a, b) \
((((a) ^ (b)) >= 0) && ((((a)+(b)) ^ (a)) < 0) ? \
(((r)=(((a)<0) ? min_fixed : max_fixed)), gs_error_limitcheck) : \
(((r) = ((a)+(b))), 0))
fixed fixed_mult_quo(fixed A, fixed B, fixed C);
#if USE_FPU_FIXED && arch_sizeof_short == 2
#define NEED_SET_FMUL2FIXED
int set_fmul2fixed_(fixed *, long, long);
#define CHECK_FMUL2FIXED_VARS(vr, vfa, vfb, dtemp)\
set_fmul2fixed_(&vr, *(const long *)&vfa, *(const long *)&vfb)
#define FINISH_FMUL2FIXED_VARS(vr, dtemp)\
DO_NOTHING
int set_dfmul2fixed_(fixed *, ulong, long, long);
# if arch_is_big_endian
# define CHECK_DFMUL2FIXED_VARS(vr, vda, vfb, dtemp)\
set_dfmul2fixed_(&vr, ((const ulong *)&vda)[1], *(const long *)&vfb, *(const long *)&vda)
# else
# define CHECK_DFMUL2FIXED_VARS(vr, vda, vfb, dtemp)\
set_dfmul2fixed_(&vr, *(const ulong *)&vda, *(const long *)&vfb, ((const long *)&vda)[1])
# endif
#define FINISH_DFMUL2FIXED_VARS(vr, dtemp)\
DO_NOTHING
#else
#undef NEED_SET_FMUL2FIXED
#define CHECK_FMUL2FIXED_VARS(vr, vfa, vfb, dtemp)\
(dtemp = (vfa) * (vfb),\
(f_fits_in_bits(dtemp, fixed_int_bits) ? 0 :\
gs_note_error(gs_error_limitcheck)))
#define FINISH_FMUL2FIXED_VARS(vr, dtemp)\
vr = float2fixed(dtemp)
#define CHECK_DFMUL2FIXED_VARS(vr, vda, vfb, dtemp)\
CHECK_FMUL2FIXED_VARS(vr, vda, vfb, dtemp)
#define FINISH_DFMUL2FIXED_VARS(vr, dtemp)\
FINISH_FMUL2FIXED_VARS(vr, dtemp)
#endif
#if USE_FPU_FIXED
int set_float2fixed_(fixed *, long, int);
int set_double2fixed_(fixed *, ulong, long, int);
# define set_float2fixed_vars(vr,vf)\
(sizeof(vf) == sizeof(float) ?\
set_float2fixed_(&vr, *(const long *)&vf, fixed_fraction_bits) :\
set_double2fixed_(&vr, ((const ulong *)&vf)[arch_is_big_endian],\
((const long *)&vf)[1 - arch_is_big_endian],\
fixed_fraction_bits))
long fixed2float_(fixed, int);
void set_fixed2double_(double *, fixed, int);
# define set_fixed2float_var(vf,x)\
(sizeof(vf) == sizeof(float) ?\
(*(long *)&vf = fixed2float_(x, fixed_fraction_bits), 0) :\
(set_fixed2double_((double *)&vf, x, fixed_fraction_bits), 0))
#define set_ldexp_fixed2double(vd, x, exp)\
set_fixed2double_(&vd, x, -(exp))
#else
# define set_float2fixed_vars(vr,vf)\
(f_fits_in_bits(vf, fixed_int_bits) ? (vr = float2fixed(vf), 0) :\
gs_note_error(gs_error_limitcheck))
# define set_fixed2float_var(vf,x)\
(vf = fixed2float(x), 0)
# define set_ldexp_fixed2double(vd, x, exp)\
discard(vd = ldexp((double)(x), exp))
#endif
typedef struct gs_fixed_point_s {
fixed x, y;
} gs_fixed_point;
typedef struct gs_fixed_rect_s {
gs_fixed_point p, q;
} gs_fixed_rect;
#endif