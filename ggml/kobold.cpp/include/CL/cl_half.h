#ifndef OPENCL_CL_HALF_H
#define OPENCL_CL_HALF_H
#include <CL/cl_platform.h>
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
typedef enum
{
CL_HALF_RTE,
CL_HALF_RTZ,
CL_HALF_RTP,
CL_HALF_RTN,
} cl_half_rounding_mode;
#define CL_HALF_EXP_MASK 0x7C00
#define CL_HALF_MAX_FINITE_MAG 0x7BFF
static inline cl_half cl_half_handle_overflow(cl_half_rounding_mode rounding_mode,
uint16_t sign)
{
if (rounding_mode == CL_HALF_RTZ)
{
return (sign << 15) | CL_HALF_MAX_FINITE_MAG;
}
else if (rounding_mode == CL_HALF_RTP && sign)
{
return (1 << 15) | CL_HALF_MAX_FINITE_MAG;
}
else if (rounding_mode == CL_HALF_RTN && !sign)
{
return CL_HALF_MAX_FINITE_MAG;
}
return (sign << 15) | CL_HALF_EXP_MASK;
}
static inline cl_half cl_half_handle_underflow(cl_half_rounding_mode rounding_mode,
uint16_t sign)
{
if (rounding_mode == CL_HALF_RTP && !sign)
{
return (sign << 15) | 1;
}
else if (rounding_mode == CL_HALF_RTN && sign)
{
return (sign << 15) | 1;
}
return (sign << 15);
}
static inline cl_half cl_half_from_float(cl_float f, cl_half_rounding_mode rounding_mode)
{
union
{
cl_float f;
uint32_t i;
} f32;
f32.f = f;
uint16_t sign = f32.i >> 31;
uint32_t f_exp = (f32.i >> (CL_FLT_MANT_DIG - 1)) & 0xFF;
uint32_t f_mant = f32.i & ((1 << (CL_FLT_MANT_DIG - 1)) - 1);
int32_t exp = f_exp - CL_FLT_MAX_EXP + 1;
uint16_t h_exp = (uint16_t)(exp + CL_HALF_MAX_EXP - 1);
uint32_t lsb_pos = CL_FLT_MANT_DIG - CL_HALF_MANT_DIG;
if (f_exp == 0xFF)
{
if (f_mant)
{
uint16_t h_mant = (uint16_t)(f_mant >> lsb_pos);
h_mant |= 0x200;
return (sign << 15) | CL_HALF_EXP_MASK | h_mant;
}
else
{
return (sign << 15) | CL_HALF_EXP_MASK;
}
}
if (!f_exp && !f_mant)
{
return (sign << 15);
}
if (exp >= CL_HALF_MAX_EXP)
{
return cl_half_handle_overflow(rounding_mode, sign);
}
if (exp < (CL_HALF_MIN_EXP - CL_HALF_MANT_DIG - 1))
{
return cl_half_handle_underflow(rounding_mode, sign);
}
if (exp < -14)
{
h_exp = 0;
f_mant |= 1 << (CL_FLT_MANT_DIG - 1);
lsb_pos = -exp + (CL_FLT_MANT_DIG - 25);
}
uint16_t h_mant = (uint16_t)(f_mant >> lsb_pos);
uint32_t halfway = 1 << (lsb_pos - 1);
uint32_t mask = (halfway << 1) - 1;
switch (rounding_mode)
{
case CL_HALF_RTE:
if ((f_mant & mask) > halfway)
{
h_mant += 1;
}
else if ((f_mant & mask) == halfway)
{
if (h_mant & 0x1)
h_mant += 1;
}
break;
case CL_HALF_RTZ:
break;
case CL_HALF_RTP:
if ((f_mant & mask) && !sign)
{
h_mant += 1;
}
break;
case CL_HALF_RTN:
if ((f_mant & mask) && sign)
{
h_mant += 1;
}
break;
}
if (h_mant & 0x400)
{
h_exp += 1;
h_mant = 0;
}
return (sign << 15) | (h_exp << 10) | h_mant;
}
static inline cl_half cl_half_from_double(cl_double d, cl_half_rounding_mode rounding_mode)
{
union
{
cl_double d;
uint64_t i;
} f64;
f64.d = d;
uint16_t sign = f64.i >> 63;
uint64_t d_exp = (f64.i >> (CL_DBL_MANT_DIG - 1)) & 0x7FF;
uint64_t d_mant = f64.i & (((uint64_t)1 << (CL_DBL_MANT_DIG - 1)) - 1);
int64_t exp = d_exp - CL_DBL_MAX_EXP + 1;
uint16_t h_exp = (uint16_t)(exp + CL_HALF_MAX_EXP - 1);
uint32_t lsb_pos = CL_DBL_MANT_DIG - CL_HALF_MANT_DIG;
if (d_exp == 0x7FF)
{
if (d_mant)
{
uint16_t h_mant = (uint16_t)(d_mant >> lsb_pos);
h_mant |= 0x200;
return (sign << 15) | CL_HALF_EXP_MASK | h_mant;
}
else
{
return (sign << 15) | CL_HALF_EXP_MASK;
}
}
if (!d_exp && !d_mant)
{
return (sign << 15);
}
if (exp >= CL_HALF_MAX_EXP)
{
return cl_half_handle_overflow(rounding_mode, sign);
}
if (exp < (CL_HALF_MIN_EXP - CL_HALF_MANT_DIG - 1))
{
return cl_half_handle_underflow(rounding_mode, sign);
}
if (exp < -14)
{
h_exp = 0;
d_mant |= (uint64_t)1 << (CL_DBL_MANT_DIG - 1);
lsb_pos = (uint32_t)(-exp + (CL_DBL_MANT_DIG - 25));
}
uint16_t h_mant = (uint16_t)(d_mant >> lsb_pos);
uint64_t halfway = (uint64_t)1 << (lsb_pos - 1);
uint64_t mask = (halfway << 1) - 1;
switch (rounding_mode)
{
case CL_HALF_RTE:
if ((d_mant & mask) > halfway)
{
h_mant += 1;
}
else if ((d_mant & mask) == halfway)
{
if (h_mant & 0x1)
h_mant += 1;
}
break;
case CL_HALF_RTZ:
break;
case CL_HALF_RTP:
if ((d_mant & mask) && !sign)
{
h_mant += 1;
}
break;
case CL_HALF_RTN:
if ((d_mant & mask) && sign)
{
h_mant += 1;
}
break;
}
if (h_mant & 0x400)
{
h_exp += 1;
h_mant = 0;
}
return (sign << 15) | (h_exp << 10) | h_mant;
}
static inline cl_float cl_half_to_float(cl_half h)
{
union
{
cl_float f;
uint32_t i;
} f32;
uint16_t sign = h >> 15;
uint16_t h_exp = (h >> (CL_HALF_MANT_DIG - 1)) & 0x1F;
uint16_t h_mant = h & 0x3FF;
int32_t exp = h_exp - CL_HALF_MAX_EXP + 1;
uint32_t f_exp = exp + CL_FLT_MAX_EXP - 1;
if (h_exp == 0x1F)
{
if (h_mant)
{
uint32_t f_mant = h_mant << (CL_FLT_MANT_DIG - CL_HALF_MANT_DIG);
f_mant |= 0x400000;
f32.i = (sign << 31) | 0x7F800000 | f_mant;
return f32.f;
}
else
{
f32.i = (sign << 31) | 0x7F800000;
return f32.f;
}
}
if (h_exp == 0)
{
if (h_mant == 0)
{
f_exp = 0;
}
else
{
uint32_t shift = 0;
while ((h_mant & 0x400) == 0)
{
h_mant <<= 1;
shift++;
}
h_mant &= 0x3FF;
f_exp -= shift - 1;
}
}
f32.i = (sign << 31) | (f_exp << 23) | (h_mant << 13);
return f32.f;
}
#undef CL_HALF_EXP_MASK
#undef CL_HALF_MAX_FINITE_MAG
#ifdef __cplusplus
}
#endif
#endif