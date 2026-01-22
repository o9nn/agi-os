#include "stdpre.h"
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <setjmp.h>
private void
section(FILE * f, const char *str)
{
fprintf(f, "\n\t \n\n", str);
}
private clock_t
time_clear(char *buf, int bsize, int nreps)
{
clock_t t = clock();
int i;
for (i = 0; i < nreps; ++i)
memset(buf, 0, bsize);
return clock() - t;
}
private void
define(FILE *f, const char *str)
{
fprintf(f, "#define %s ", str);
}
private void
define_int(FILE *f, const char *str, int value)
{
fprintf(f, "#define %s %d\n", str, value);
}
private void
print_ffs(FILE *f, int nbytes)
{
int i;
for (i = 0; i < nbytes; ++i)
fprintf(f, "ff");
}
private int
ilog2(int n)
{
int i = 0, m = n;
while (m > 1)
++i, m = (m + 1) >> 1;
return i;
}
int
main(int argc, char *argv[])
{
char *fname = argv[1];
long one = 1;
struct {
char c;
short s;
} ss;
struct {
char c;
int i;
} si;
struct {
char c;
long l;
} sl;
struct {
char c;
char *p;
} sp;
struct {
char c;
float f;
} sf;
struct {
char c;
double d;
} sd;
struct {
char c;
jmp_buf j;
} sj;
long lm1 = -1;
long lr1 = lm1 >> 1, lr2 = lm1 >> 2;
unsigned long um1 = ~(unsigned long)0;
int im1 = -1;
int ir1 = im1 >> 1, ir2 = im1 >> 2;
union {
long l;
char *p;
} pl0, pl1;
int ars;
int lwidth = size_of(long) * 8;
union {
float f;
int i;
long l;
} f0, f1, fm1;
int floats_are_IEEE;
FILE *f = fopen(fname, "w");
if (f == NULL) {
fprintf(stderr, "genarch.c: can't open %s for writing\n", fname);
return exit_FAILED;
}
fprintf(f, "\n");
fprintf(f, "\n");
f0.f = 0.0, f1.f = 1.0, fm1.f = -1.0;
floats_are_IEEE =
(size_of(float) == size_of(int) ?
f0.i == 0 && f1.i == (int)0x3f800000 && fm1.i == (int)0xbf800000 :
f0.l == 0 && f1.l == 0x3f800000L && fm1.l == 0xbf800000L);
section(f, "Scalar alignments");
#define OFFSET_IN(s, e) (int)((char *)&s.e - (char *)&s)
define_int(f, "ARCH_ALIGN_SHORT_MOD", OFFSET_IN(ss, s));
define_int(f, "ARCH_ALIGN_INT_MOD", OFFSET_IN(si, i));
define_int(f, "ARCH_ALIGN_LONG_MOD", OFFSET_IN(sl, l));
define_int(f, "ARCH_ALIGN_PTR_MOD", OFFSET_IN(sp, p));
define_int(f, "ARCH_ALIGN_FLOAT_MOD", OFFSET_IN(sf, f));
define_int(f, "ARCH_ALIGN_DOUBLE_MOD", OFFSET_IN(sd, d));
define_int(f, "ARCH_ALIGN_STRUCT_MOD", OFFSET_IN(sj, j));
#undef OFFSET_IN
section(f, "Scalar sizes");
define_int(f, "ARCH_LOG2_SIZEOF_CHAR", ilog2(size_of(char)));
define_int(f, "ARCH_LOG2_SIZEOF_SHORT", ilog2(size_of(short)));
define_int(f, "ARCH_LOG2_SIZEOF_INT", ilog2(size_of(int)));
define_int(f, "ARCH_LOG2_SIZEOF_LONG", ilog2(size_of(long)));
#ifdef HAVE_LONG_LONG
define_int(f, "ARCH_LOG2_SIZEOF_LONG_LONG", ilog2(size_of(long long)));
#endif
define_int(f, "ARCH_SIZEOF_PTR", size_of(char *));
define_int(f, "ARCH_SIZEOF_FLOAT", size_of(float));
define_int(f, "ARCH_SIZEOF_DOUBLE", size_of(double));
if (floats_are_IEEE) {
define_int(f, "ARCH_FLOAT_MANTISSA_BITS", 24);
define_int(f, "ARCH_DOUBLE_MANTISSA_BITS", 53);
} else {
define_int(f, "ARCH_FLOAT_MANTISSA_BITS", sizeof(float) * 8 - 17);
define_int(f, "ARCH_DOUBLE_MANTISSA_BITS", sizeof(double) * 8 - 17);
}
section(f, "Unsigned max values");
#define PRINT_MAX(str, typ, tstr, l)\
BEGIN\
define(f, str);\
fprintf(f, "((%s)0x", tstr);\
print_ffs(f, sizeof(typ));\
fprintf(f, "%s + (%s)0)\n", l, tstr);\
END
PRINT_MAX("ARCH_MAX_UCHAR", unsigned char, "unsigned char", "");
PRINT_MAX("ARCH_MAX_USHORT", unsigned short, "unsigned short", "");
define(f, "ARCH_MAX_UINT");
fprintf(f, "((unsigned int)~0 + (unsigned int)0)\n");
define(f, "ARCH_MAX_ULONG");
fprintf(f, "((unsigned long)~0L + (unsigned long)0)\n");
#undef PRINT_MAX
section(f, "Cache sizes");
{
#define MAX_BLOCK (1 << 22)
#define MAX_NREPS (1 << 10)
static char buf[MAX_BLOCK];
int bsize = 1 << 10;
int nreps = 1;
clock_t t = 0;
clock_t t_eps;
while (nreps < MAX_NREPS && (t = time_clear(buf, bsize, nreps)) == 0)
nreps <<= 1;
t_eps = t;
while (nreps < MAX_NREPS && (t = time_clear(buf, bsize, nreps)) < t_eps * 10)
nreps <<= 1;
for (; bsize <= MAX_BLOCK;) {
clock_t dt = time_clear(buf, bsize, nreps);
if (dt > t + (t >> 1)) {
t = dt;
break;
}
bsize <<= 1;
nreps >>= 1;
if (nreps == 0)
nreps = 1, t <<= 1;
}
define_int(f, "ARCH_CACHE1_SIZE", bsize >> 1);
if (nreps > 1)
nreps >>= 1, t >>= 1;
for (; bsize <= MAX_BLOCK;) {
clock_t dt = time_clear(buf, bsize, nreps);
if (dt > t * 1.25) {
t = dt;
break;
}
bsize <<= 1;
nreps >>= 1;
if (nreps == 0)
nreps = 1, t <<= 1;
}
define_int(f, "ARCH_CACHE2_SIZE", bsize >> 1);
}
section(f, "Miscellaneous");
define_int(f, "ARCH_IS_BIG_ENDIAN", 1 - *(char *)&one);
pl0.l = 0;
pl1.l = -1;
define_int(f, "ARCH_PTRS_ARE_SIGNED", (pl1.p < pl0.p));
define_int(f, "ARCH_FLOATS_ARE_IEEE", (floats_are_IEEE ? 1 : 0));
ars = (lr2 != -1 || ir1 != -1 || ir2 != -1 ? 0 :
lr1 != -1 ? 1 :
2);
define_int(f, "ARCH_ARITH_RSHIFT", ars);
define_int(f, "ARCH_CAN_SHIFT_FULL_LONG", um1 >> lwidth == 0);
define_int(f, "ARCH_DIV_NEG_POS_TRUNCATES", im1 / 2 == 0);
fclose(f);
return exit_OK;
}