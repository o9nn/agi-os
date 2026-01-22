#ifndef HEADER_CONSTANT_TIME_LOCL_H
# define HEADER_CONSTANT_TIME_LOCL_H
# include "e_os.h"
#ifdef __cplusplus
extern "C" {
#endif
static inline unsigned int constant_time_msb(unsigned int a);
static inline unsigned int constant_time_lt(unsigned int a, unsigned int b);
static inline unsigned char constant_time_lt_8(unsigned int a,
unsigned int b);
static inline unsigned int constant_time_ge(unsigned int a, unsigned int b);
static inline unsigned char constant_time_ge_8(unsigned int a,
unsigned int b);
static inline unsigned int constant_time_is_zero(unsigned int a);
static inline unsigned char constant_time_is_zero_8(unsigned int a);
static inline unsigned int constant_time_eq(unsigned int a, unsigned int b);
static inline unsigned char constant_time_eq_8(unsigned int a,
unsigned int b);
static inline unsigned int constant_time_eq_int(int a, int b);
static inline unsigned char constant_time_eq_int_8(int a, int b);
static inline unsigned int constant_time_select(unsigned int mask,
unsigned int a,
unsigned int b);
static inline unsigned char constant_time_select_8(unsigned char mask,
unsigned char a,
unsigned char b);
static inline int constant_time_select_int(unsigned int mask, int a, int b);
static inline unsigned int constant_time_msb(unsigned int a)
{
return 0 - (a >> (sizeof(a) * 8 - 1));
}
static inline unsigned int constant_time_lt(unsigned int a, unsigned int b)
{
return constant_time_msb(a ^ ((a ^ b) | ((a - b) ^ b)));
}
static inline unsigned char constant_time_lt_8(unsigned int a, unsigned int b)
{
return (unsigned char)(constant_time_lt(a, b));
}
static inline unsigned int constant_time_ge(unsigned int a, unsigned int b)
{
return ~constant_time_lt(a, b);
}
static inline unsigned char constant_time_ge_8(unsigned int a, unsigned int b)
{
return (unsigned char)(constant_time_ge(a, b));
}
static inline unsigned int constant_time_is_zero(unsigned int a)
{
return constant_time_msb(~a & (a - 1));
}
static inline unsigned char constant_time_is_zero_8(unsigned int a)
{
return (unsigned char)(constant_time_is_zero(a));
}
static inline unsigned int constant_time_eq(unsigned int a, unsigned int b)
{
return constant_time_is_zero(a ^ b);
}
static inline unsigned char constant_time_eq_8(unsigned int a, unsigned int b)
{
return (unsigned char)(constant_time_eq(a, b));
}
static inline unsigned int constant_time_eq_int(int a, int b)
{
return constant_time_eq((unsigned)(a), (unsigned)(b));
}
static inline unsigned char constant_time_eq_int_8(int a, int b)
{
return constant_time_eq_8((unsigned)(a), (unsigned)(b));
}
static inline unsigned int constant_time_select(unsigned int mask,
unsigned int a,
unsigned int b)
{
return (mask & a) | (~mask & b);
}
static inline unsigned char constant_time_select_8(unsigned char mask,
unsigned char a,
unsigned char b)
{
return (unsigned char)(constant_time_select(mask, a, b));
}
static inline int constant_time_select_int(unsigned int mask, int a, int b)
{
return (int)(constant_time_select(mask, (unsigned)(a), (unsigned)(b)));
}
#ifdef __cplusplus
}
#endif
#endif