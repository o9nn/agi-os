# ifdef HAVE_CONFIG_H
#  include "config.h"
# endif
# include "global.h"
# include "fixed.h"
mad_fixed_t mad_f_abs(mad_fixed_t x)
{
return x < 0 ? -x : x;
}
mad_fixed_t mad_f_div(mad_fixed_t x, mad_fixed_t y)
{
mad_fixed_t q, r;
unsigned int bits;
q = mad_f_abs(x / y);
if (x < 0) {
x = -x;
y = -y;
}
r = x % y;
if (y < 0) {
x = -x;
y = -y;
}
if (q > mad_f_intpart(MAD_F_MAX) &&
!(q == -mad_f_intpart(MAD_F_MIN) && r == 0 && (x < 0) != (y < 0)))
return 0;
for (bits = MAD_F_FRACBITS; bits && r; --bits) {
q <<= 1, r <<= 1;
if (r >= y)
r -= y, ++q;
}
if (2 * r >= y)
++q;
if ((x < 0) != (y < 0))
q = -q;
return q << bits;
}