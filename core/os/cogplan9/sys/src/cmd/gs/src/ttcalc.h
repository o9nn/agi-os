#ifndef TTCALC_H
#define TTCALC_H
#include "ttcommon.h"
#include "tttypes.h"
typedef signed short Int16;
typedef unsigned short Word16;
#if SIZEOF_INT == 4
typedef signed int Int32;
typedef unsigned int Word32;
#elif SIZEOF_LONG == 4
typedef signed long Int32;
typedef unsigned long Word32;
#else
#error "no 32bit type found"
#endif
#if SIZEOF_LONG == 8
#define LONG64
#define INT64 long
#else
#ifdef _GNUC_LONG64_
#define LONG64
#define INT64 long long
#endif
#ifdef _MSC_VER
#if _MSC_VER >= 1100
#define LONG64
#define INT64 __int64
#endif
#endif
#endif
#ifdef __cplusplus
extern "C" {
#endif
#ifdef LONG64
typedef INT64 Int64;
#define FMulDiv( x, y, z ) ( (Int64)(x) * (y) / (z) )
#define FMulDiv_Round( x, y, z ) ( ( (Int64)(x) * (y) + (z)/2 ) / (z) )
#define ADD_64( x, y, z ) ( (z) = (x) + (y) )
#define SUB_64( x, y, z ) ( (z) = (x) - (y) )
#define MUL_64( x, y, z ) ( (z) = (Int64)(x) * (y) )
#define DIV_64( x, y ) ( (x) / (y) )
#define SQRT_64( x ) Sqrt64( x )
#define SQRT_32( x ) Sqrt32( x )
Int32 MulDiv( Int32 a, Int32 b, Int32 c );
Int32 MulDiv_Round( Int32 a, Int32 b, Int32 c );
Int32 Sqrt32( Int32 l );
Int32 Sqrt64( Int64 l );
#else
struct _Int64
{
Word32 lo;
Word32 hi;
};
typedef struct _Int64 Int64;
#define FMulDiv( x, y, z ) MulDiv( x, y, z )
#define FMulDiv_Round( x, y, z ) MulDiv_Round( x, y, z )
#define ADD_64( x, y, z ) Add64( &x, &y, &z )
#define SUB_64( x, y, z ) Sub64( &x, &y, &z )
#define MUL_64( x, y, z ) MulTo64( x, y, &z )
#define DIV_64( x, y ) Div64by32( &x, y )
#define SQRT_64( x ) Sqrt64( &x )
#define SQRT_32( x ) Sqrt32( x )
Int32 MulDiv( Int32 a, Int32 b, Int32 c );
Int32 MulDiv_Round( Int32 a, Int32 b, Int32 c );
void Add64( Int64* x, Int64* y, Int64* z );
void Sub64( Int64* x, Int64* y, Int64* z );
void MulTo64( Int32 x, Int32 y, Int64* z );
Int32 Div64by32( Int64* x, Int32 y );
Int Order64( Int64* z );
Int Order32( Int32 z );
Int32 Sqrt32( Int32 l );
Int32 Sqrt64( Int64* l );
#endif
#define MUL_FIXED( a, b ) MulDiv_Round( (a), (b), 1 << 16 )
#define INT_TO_F26DOT6( x ) ( (Long)(x) << 6 )
#define INT_TO_F2DOT14( x ) ( (Long)(x) << 14 )
#define INT_TO_FIXED( x ) ( (Long)(x) << 16 )
#define F2DOT14_TO_FIXED( x ) ( (Long)(x) << 2 )
#define FLOAT_TO_FIXED( x ) ( (Long)((x) * 65536.0) )
#define ROUND_F26DOT6( x ) ( (x) >= 0 ? ( ((x) + 32) & -64) \
: ( -((32 - (x)) & -64) ) )
#ifdef __cplusplus
}
#endif
#endif