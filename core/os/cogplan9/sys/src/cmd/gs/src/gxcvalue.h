#ifndef gxcvalue_INCLUDED
#  define gxcvalue_INCLUDED
typedef unsigned short gx_color_value;
#define arch_sizeof_gx_color_value arch_sizeof_short
#define gx_color_value_bits (sizeof(gx_color_value) * 8)
#define gx_max_color_value ((gx_color_value)((1L << gx_color_value_bits) - 1))
#define gx_color_value_to_byte(cv)\
((cv) >> (gx_color_value_bits - 8))
#define gx_color_value_from_byte(cb)\
(((cb) << (gx_color_value_bits - 8)) + ((cb) >> (16 - gx_color_value_bits)))
#define frac2cv(fr) frac2ushort(fr)
#define cv2frac(cv) ushort2frac(cv)
#endif