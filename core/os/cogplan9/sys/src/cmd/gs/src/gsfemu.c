#include "std.h"
#include <signal.h>
#define fx(ia) (((ia) >> 23) & 0xff)
#define fx_bias 127
#define dx(ld) ((ld[msw] >> 20) & 0x7ff)
#define dx_bias 1023
#if arch_is_big_endian
#  define msw 0
#  define lsw 1
#else
#  define msw 1
#  define lsw 0
#endif
#define la ((const long *)&a)
#define ula ((const ulong *)&a)
#define lb ((const long *)&b)
#define ulb ((const ulong *)&b)
#define dc (*(const double *)lc)
#define ia (*(const long *)&a)
#define ua (*(const ulong *)&a)
#define ib (*(const long *)&b)
#define ub (*(const ulong *)&b)
#define fc (*(const float *)&lc)
#define roundr1(ms, uls)\
if ( uls == 0xffffffff ) ms++, uls = 0;\
else uls++;\
uls = (uls >> 1) + (ms << 31);\
ms >>= 1
#define extend(lc, ia)\
((lc)[msw] = ((ia) & 0x80000000) + (((ia) & 0x7fffffff) >> 3) + 0x38000000,\
(lc)[lsw] = (ia) << 29)
double
__negdf2(double a)
{
long lc[2];
lc[msw] = la[msw] ^ 0x80000000;
lc[lsw] = la[lsw];
return dc;
}
float
__negsf2(float a)
{
long lc = ia ^ 0x80000000;
return fc;
}
double
__adddf3(double a, double b)
{
long lc[2];
int expt = dx(la);
int shift = expt - dx(lb);
long sign;
ulong msa, lsa;
ulong msb, lsb;
if (shift < 0) {
double temp = a;
a = b, b = temp;
expt += (shift = -shift);
}
if (shift >= 54)
return a;
if (!(lb[msw] & 0x7fffffff))
return a;
sign = la[msw] & 0x80000000;
msa = (la[msw] & 0xfffff) + 0x100000, lsa = la[lsw];
msb = (lb[msw] & 0xfffff) + 0x100000, lsb = lb[lsw];
if ((la[msw] ^ lb[msw]) >= 0) {
if (shift >= 32)
lsb = msb, msb = 0, shift -= 32;
if (shift) {
--shift;
lsb = (lsb >> shift) + (msb << (32 - shift));
msb >>= shift;
roundr1(msb, lsb);
}
if (lsb > (ulong) 0xffffffff - lsa)
msa++;
lsa += lsb;
msa += msb;
if (msa > 0x1fffff) {
roundr1(msa, lsa);
++expt;
}
} else {
if (shift > 53)
return a;
if (shift == 0 && (msb > msa || (msb == msa && lsb >= lsa))) {
sign ^= 0x80000000;
msa = msb - msa;
if (lsb < lsa)
msa--;
lsa = lsb - lsa;
} else {
if (shift >= 33) {
lsb = ((msb >> (shift - 32)) + 1) >> 1;
msb = 0;
} else if (shift) {
lsb = (lsb >> (shift - 1)) + (msb << (33 - shift));
msb >>= shift - 1;
roundr1(msb, lsb);
}
msa -= msb;
if (lsb > lsa)
msa--;
lsa -= lsb;
}
if (!(msa | lsa))
return 0;
while (msa < 0x100000) {
msa = (msa << 1) + (lsa >> 31);
lsa <<= 1;
expt -= 1;
}
if (expt <= 0) {
lc[msw] = sign;
lc[lsw] = 0;
return dc;
}
}
lc[msw] = sign + ((ulong) expt << 20) + (msa & 0xfffff);
lc[lsw] = lsa;
return dc;
}
double
__subdf3(double a, double b)
{
long nb[2];
nb[msw] = lb[msw] ^ 0x80000000;
nb[lsw] = lb[lsw];
return a + *(const double *)nb;
}
float
__addsf3(float a, float b)
{
long lc;
int expt = fx(ia);
int shift = expt - fx(ib);
long sign;
ulong ma, mb;
if (shift < 0) {
long temp = ia;
*(long *)&a = ib;
*(long *)&b = temp;
expt += (shift = -shift);
}
if (shift >= 25)
return a;
if (!(ib & 0x7fffffff))
return a;
sign = ia & 0x80000000;
ma = (ia & 0x7fffff) + 0x800000;
mb = (ib & 0x7fffff) + 0x800000;
if ((ia ^ ib) >= 0) {
if (shift) {
--shift;
mb = ((mb >> shift) + 1) >> 1;
}
ma += mb;
if (ma > 0xffffff) {
ma = (ma + 1) >> 1;
++expt;
}
} else {
if (shift > 24)
return a;
if (shift == 0 && mb >= ma) {
sign ^= 0x80000000;
ma = mb - ma;
} else {
if (shift) {
--shift;
mb = ((mb >> shift) + 1) >> 1;
}
ma -= mb;
}
if (!ma)
return 0;
while (ma < 0x800000) {
ma <<= 1;
expt -= 1;
}
if (expt <= 0) {
lc = sign;
return fc;
}
}
lc = sign + ((ulong)expt << 23) + (ma & 0x7fffff);
return fc;
}
float
__subsf3(float a, float b)
{
long lc = ib ^ 0x80000000;
return a + fc;
}
double
__muldf3(double a, double b)
{
long lc[2];
ulong sign;
uint H, I, h, i;
ulong p0, p1, p2;
ulong expt;
if (!(la[msw] & 0x7fffffff) || !(lb[msw] & 0x7fffffff))
return 0;
#define chop_ms(ulx, h, i)\
h = ((ulx[msw] >> 7) & 0x1fff) | 0x2000,\
i = ((ulx[msw] & 0x7f) << 7) | (ulx[lsw] >> 25)
#define chop_ls(ulx, j, k)\
j = (ulx[lsw] >> 11) & 0x3fff,\
k = (ulx[lsw] & 0x7ff) << 3
chop_ms(ula, H, I);
chop_ms(ulb, h, i);
#undef chop
#define prod(m, n) ((ulong)(m) * (n))
p0 = prod(H, h);
p1 = prod(H, i) + prod(I, h);
if ((ula[lsw] | ulb[lsw]) & 0x1ffffff) {
uint J, K, j, k;
chop_ls(ula, J, K);
chop_ls(ulb, j, k);
{
ulong p6 = prod(K, k);
ulong p5 = prod(J, k) + prod(K, j) + (p6 >> 14);
ulong p4 = prod(I, k) + prod(J, j) + prod(K, i) + (p5 >> 14);
ulong p3 = prod(H, k) + prod(I, j) + prod(J, i) + prod(K, h) +
(p4 >> 14);
p2 = prod(H, j) + prod(I, i) + prod(J, h) + (p3 >> 14);
}
} else {
p2 = prod(I, i);
}
expt = (la[msw] & 0x7ff00000) + (lb[msw] & 0x7ff00000) -
(dx_bias << 20);
p1 += p2 >> 14;
p0 += p1 >> 14;
if (p0 < 0x8000000) {
p0 = (p0 << 1) + ((p1 >> 13) & 1);
p1 = (p1 << 1) + ((p2 >> 13) & 1);
p2 <<= 1;
} else
expt += 0x100000;
if (!((p2 += 4) & 0x3ffc) && !(++p1 & 0x3fff) && ++p0 >= 0x10000000) {
p0 >>= 1, p1 = 0x2000;
if ((ulong) expt < 0xc0000000)
expt += 0x100000;
}
sign = (la[msw] ^ lb[msw]) & 0x80000000;
if (expt == 0) {
lc[msw] = sign;
lc[lsw] = 0;
} else if ((ulong) expt >= 0x7ff00000) {
if ((ulong) expt <= 0xc0000000) {
raise(SIGFPE);
lc[msw] = sign + 0x7ff00000;
lc[lsw] = 0;
} else {
lc[msw] = sign;
lc[lsw] = 0;
}
} else {
lc[msw] = sign + expt + ((p0 >> 7) & 0xfffff);
lc[lsw] = (p0 << 25) | ((p1 & 0x3fff) << 11) | ((p2 >> 3) & 0x7ff);
}
return dc;
#undef prod
}
float
__mulsf3(float a, float b)
{
uint au, al, bu, bl, cu, cl, sign;
long lc;
uint expt;
if (!(ia & 0x7fffffff) || !(ib & 0x7fffffff))
return 0;
au = ((ia >> 8) & 0x7fff) | 0x8000;
bu = ((ib >> 8) & 0x7fff) | 0x8000;
cu = au * bu;
if ((al = ia & 0xff) != 0) {
cl = bu * al;
} else
cl = 0;
if ((bl = ib & 0xff) != 0) {
cl += au * bl;
if (al)
cl += (al * bl) >> 8;
}
cu += cl >> 8;
sign = (ia ^ ib) & 0x80000000;
expt = (ia & 0x7f800000) + (ib & 0x7f800000) - (fx_bias << 23);
if (cu <= 0x7fffffff)
cu <<= 1;
else
expt += 1 << 23;
cu = ((cu >> 7) + 1) >> 1;
if (expt < 1 << 23)
lc = sign;
else if (expt > (uint)(254 << 23)) {
if (expt <= 0xc0000000) {
raise(SIGFPE);
lc = sign + 0x7f800000;
} else {
lc = sign;
}
} else
lc = sign + expt + cu - 0x800000;
return fc;
}
double
__divdf3(double a, double b)
{
long lc[2];
ulong sign = (la[msw] ^ lb[msw]) & 0x80000000;
ulong msa = (la[msw] & 0xfffff) | 0x100000, lsa = la[lsw];
ulong msb = (lb[msw] & 0xfffff) | 0x100000, lsb = lb[lsw];
uint qn[5];
int i;
ulong msq, lsq;
int expt = dx(la) - dx(lb) + dx_bias;
if (!(lb[msw] & 0x7fffffff)) {
raise(SIGFPE);
lc[lsw] = 0;
lc[msw] =
(la[msw] & 0x7fffffff ?
sign + 0x7ff00000  :
0x7ff80000  );
return dc;
}
if (!(la[msw] & 0x7fffffff))
return 0;
for (i = 0; i < 5; ++i) {
uint q;
ulong msp, lsp;
msa = (msa << 11) + (lsa >> 21);
lsa <<= 11;
q = msa / msb;
msp = q * msb;
lsp = q * (lsb & 0x1fffff);
{
ulong midp = q * (lsb >> 21);
msp += (midp + (lsp >> 21)) >> 11;
lsp += midp << 21;
}
if (msp > msa || (lsp > lsa && msp == msa)) {
--q;
if (lsb > lsp)
msp--;
lsp -= lsb;
msp -= msb;
}
if (lsp > lsa)
msp--;
lsa -= lsp;
msa -= msp;
qn[i] = q;
}
msq = (qn[0] << 9) + (qn[1] >> 2);
lsq = (qn[1] << 30) + (qn[2] << 19) + (qn[3] << 8) + (qn[4] >> 3);
if (msq < 0x100000) {
msq = (msq << 1) + (lsq >> 31);
lsq <<= 1;
expt--;
}
if (expt <= 0) {
lc[msw] = sign;
lc[lsw] = 0;
} else if (expt >= 0x7ff) {
raise(SIGFPE);
lc[msw] = sign + 0x7ff00000;
lc[lsw] = 0;
} else {
lc[msw] = sign + (expt << 20) + (msq & 0xfffff);
lc[lsw] = lsq;
}
return dc;
}
float
__divsf3(float a, float b)
{
return (float)((double)a / (double)b);
}
static int
compared2(const long *pa, const long *pb)
{
#define upa ((const ulong *)pa)
#define upb ((const ulong *)pb)
if (pa[msw] == pb[msw]) {
int result = (upa[lsw] < upb[lsw] ? -1 :
upa[lsw] > upb[lsw] ? 1 : 0);
return (pa[msw] < 0 ? -result : result);
}
if ((pa[msw] & pb[msw]) < 0)
return (pa[msw] < pb[msw] ? 1 : -1);
else if (!((pa[msw] | pb[msw]) & 0x7fffffff) && !(pa[lsw] | pb[lsw]))
return 0;
else
return (pa[msw] > pb[msw] ? 1 : -1);
#undef upa
#undef upb
}
int
__eqdf2(double a, double b)
{
return compared2(la, lb);
}
int
__nedf2(double a, double b)
{
return compared2(la, lb);
}
int
__gtdf2(double a, double b)
{
return compared2(la, lb);
}
int
__gedf2(double a, double b)
{
return compared2(la, lb);
}
int
__ltdf2(double a, double b)
{
return compared2(la, lb);
}
int
__ledf2(double a, double b)
{
return compared2(la, lb);
}
static int
comparef2(long va, long vb)
{
if (va == vb)
return 0;
if ((va & vb) < 0)
return (va < vb ? 1 : -1);
else if (!((va | vb) & 0x7fffffff))
return 0;
else
return (va > vb ? 1 : -1);
}
int
__eqsf2(float a, float b)
{
return comparef2(ia, ib);
}
int
__nesf2(float a, float b)
{
return comparef2(ia, ib);
}
int
__gtsf2(float a, float b)
{
return comparef2(ia, ib);
}
int
__gesf2(float a, float b)
{
return comparef2(ia, ib);
}
int
__ltsf2(float a, float b)
{
return comparef2(ia, ib);
}
int
__lesf2(float a, float b)
{
return comparef2(ia, ib);
}
long
__fixdfsi(double a)
{
long i = (la[msw] & 0xfffff) + 0x100000;
int expt = dx(la) - dx_bias;
if (expt < 0)
return 0;
if (expt <= 20)
i >>= 20 - expt;
else if (expt >= 31 &&
(expt > 31 || i != 0x100000 || la[msw] >= 0 ||
ula[lsw] >= 1L << 21)
) {
raise(SIGFPE);
i = (la[msw] < 0 ? 0x80000000 : 0x7fffffff);
} else
i = (i << (expt - 20)) + (ula[lsw] >> (52 - expt));
return (la[msw] < 0 ? -i : i);
}
long
__fixsfsi(float a)
{
long i = (ia & 0x7fffff) + 0x800000;
int expt = fx(ia) - fx_bias;
if (expt < 0)
return 0;
if (expt <= 23)
i >>= 23 - expt;
else if (expt >= 31 && (expt > 31 || i != 0x800000 || ia >= 0)) {
raise(SIGFPE);
i = (ia < 0 ? 0x80000000 : 0x7fffffff);
} else
i <<= expt - 23;
return (ia < 0 ? -i : i);
}
double
__floatsidf(long i)
{
long msc;
ulong v;
long lc[2];
if (i > 0)
msc = 0x41e00000 - 0x100000, v = i;
else if (i < 0)
msc = 0xc1e00000 - 0x100000, v = -i;
else
return 0;
while (v < 0x01000000)
v <<= 8, msc -= 0x00800000;
if (v < 0x10000000)
v <<= 4, msc -= 0x00400000;
while (v < 0x80000000)
v <<= 1, msc -= 0x00100000;
lc[msw] = msc + (v >> 11);
lc[lsw] = v << 21;
return dc;
}
float
__floatsisf(long i)
{
long lc;
if (i == 0)
lc = 0;
else {
ulong v;
if (i < 0)
lc = 0xcf000000, v = -i;
else
lc = 0x4f000000, v = i;
while (v < 0x01000000)
v <<= 8, lc -= 0x04000000;
while (v < 0x80000000)
v <<= 1, lc -= 0x00800000;
v = ((v >> 7) + 1) >> 1;
if (v > 0xffffff)
v >>= 1, lc += 0x00800000;
lc += v & 0x7fffff;
}
return fc;
}
float
__truncdfsf2(double a)
{
long lc;
if ((la[msw] & 0x7ff00000) < 0x38100000)
lc = la[msw] & 0x80000000;
else if ((la[msw] & 0x7ff00000) >= 0x47f00000) {
raise(SIGFPE);
lc = (la[msw] & 0x80000000) + 0x7f800000;
} else {
lc = (la[msw] & 0xc0000000) +
((la[msw] & 0x07ffffff) << 3) +
(ula[lsw] >> 29);
if (ula[lsw] & 0x10000000)
++lc;
}
return fc;
}
double
__extendsfdf2(float a)
{
long lc[2];
if (!(ia & 0x7fffffff))
lc[msw] = ia, lc[lsw] = 0;
else
extend(lc, ia);
return dc;
}
#ifdef TEST
#include <stdio.h>
#include <stdlib.h>
int
test(double v1)
{
double v3 = v1 * 3;
double vh = v1 / 2;
double vd = v3 - vh;
double vdn = v1 - v3;
printf("%g=1 %g=3 %g=0.5 %g=2.5 %g=-2\n", v1, v3, vh, vd, vdn);
return 0;
}
float
randf(void)
{
int v = rand();
v = (v << 16) ^ rand();
if (!(v & 0x7f800000))
return 0;
if ((v & 0x7f800000) == 0x7f800000)
return randf();
return *(float *)&v;
}
int
main(int argc, char *argv[])
{
int i;
test(1.0);
for (i = 0; i < 10; ++i) {
float a = randf(), b = randf(), r;
int c;
switch ((rand() >> 12) & 3) {
case 0:
r = a + b;
c = '+';
break;
case 1:
r = a - b;
c = '-';
break;
case 2:
r = a * b;
c = '*';
break;
case 3:
if (b == 0)
continue;
r = a / b;
c = '/';
break;
}
printf("0x%08x %c 0x%08x = 0x%08x\n",
*(int *)&a, c, *(int *)&b, *(int *)&r);
}
}
#endif