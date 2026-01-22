#ifndef math__INCLUDED
# define math__INCLUDED
#include "std.h"
#if defined(VMS) && defined(__GNUC__)
# include "vmsmath.h"
#else
# include <math.h>
#endif
#ifndef M_PI
# ifdef PI
# define M_PI PI
# else
# define M_PI 3.14159265358979324
# endif
#endif
#define degrees_to_radians (M_PI / 180.0)
#define radians_to_degrees (180.0 / M_PI)
#undef MAX_FLOAT
#if defined(vax) || defined(VAX) || defined(__vax) || defined(__VAX)
# define MAX_FLOAT\
((0x800000 - 1.0) * 0x1000000 * 0x1000000 * 0x10000000 * 0x10000000)
#else
# define MAX_FLOAT\
((0x1000000 - 1.0) * 0x1000000 * 0x1000000 * 0x10000000 * 0x10000000)
#endif
#if defined(_IBMR2)
extern double hypot(double, double);
#elif defined(_MSC_VER)
# define hypot(x,y) _hypot(x,y)
#elif !defined(__TURBOC__) && !defined(BSD4_2) && !defined(VMS) && !defined(__MWERKS__) && !defined(HAVE_HYPOT)
# define hypot(x,y) sqrt((double)(x)*(x)+(double)(y)*(y))
#endif
#ifdef OSK
extern double atan2(), ldexp();
#endif
extern double gs_sqrt(double, const char *, int);
#ifdef DEBUG
#undef sqrt
#define sqrt(x) gs_sqrt(x, __FILE__, __LINE__)
#endif
#endif