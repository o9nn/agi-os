#include <u.h>
void
mul64fract(uvlong *r, uvlong a, uvlong b)
{
uvlong bh, bl;
uvlong ah, al;
uvlong res;
bl = b & 0xffffffffULL;
bh = b >> 32;
al = a & 0xffffffffULL;
ah = a >> 32;
res = (al*bl)>>32;
res += (al*bh);
res += (ah*bl);
res += (ah*bh)<<32;
*r = res;
}