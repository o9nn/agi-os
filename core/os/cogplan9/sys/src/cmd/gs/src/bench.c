#include "stdio_.h"
#include <stdlib.h>
FILE *gs_stdout;
FILE *gs_stderr;
FILE *gs_debug_out;
const char gp_scratch_file_name_prefix[] = "gs_";
static void
capture_stdio(void)
{
gs_stdout = stdout;
gs_stderr = stderr;
gs_debug_out = stderr;
}
#include "gsio.h"
#undef gs_stdout
#undef gs_stderr
#undef stdout
#define stdout gs_stdout
#undef stderr
#define stderr gs_stderr
FILE *
gp_open_scratch_file(const char *prefix, char *fname, const char *mode)
{
return NULL;
}
void
gp_set_printer_binary(int prnfno, int binary)
{
}
void
gs_to_exit(int n)
{
}
#define eprintf_program_ident(f, pn, rn) (void)0
void
lprintf_file_and_line(FILE * f, const char *file, int line)
{
fprintf(f, "%s(%d): ", file, line);
}
#include "gp_unix.c"
#undef stdout
#define stdout gs_stdout
#undef stderr
#define stderr gs_stderr
#define do10(x) x;x;x;x;x; x;x;x;x;x
static int
iadd(int a, int n, char **msg)
{
int b = 0, i;
for (i = n / 20; --i >= 0;) {
do10((b += a, b += i));
}
*msg = "integer adds";
return b;
}
static int
imul(int a, int n, char **msg)
{
int b = 1, i;
for (i = n / 20; --i > 0;) {
do10((b *= a, b *= i));
}
*msg = "integer multiplies";
return b;
}
static int
idiv(int a, int n, char **msg)
{
int b = 1, i;
for (i = n / 20; --i > 0;) {
b += 999999;
do10((b /= a, b /= i));
}
*msg = "integer divides";
return b;
}
static int
fadd(float a, int n, char **msg)
{
float b = 0;
int i;
for (i = n / 10; --i >= 0;) {
do10((b += a));
}
*msg = "floating adds";
return b;
}
static int
fmul(float a, int n, char **msg)
{
float b = 1;
int i;
for (i = n / 10; --i >= 0;) {
do10((b *= a));
}
*msg = "floating multiplies";
return b;
}
static int
fdiv(float a, int n, char **msg)
{
float b = 1;
int i;
for (i = n / 10; --i >= 0;) {
do10((b /= a));
}
*msg = "floating divides";
return b;
}
static int
fconv(int a, int n, char **msg)
{
int b[10];
float f[10];
int i;
b[0] = a;
for (i = n / 20; --i >= 0;)
f[0] = b[0], f[1] = b[1], f[2] = b[2], f[3] = b[3], f[4] = b[4],
f[5] = b[5], f[6] = b[6], f[7] = b[7], f[8] = b[8], f[9] = b[9],
b[0] = f[1], b[1] = f[2], b[2] = f[3], b[3] = f[4], b[4] = f[5],
b[5] = f[6], b[6] = f[7], b[7] = f[8], b[8] = f[9], b[9] = f[0];
*msg = "float/int conversions";
return b[0];
}
static int
mfast(int *m, int n, char **msg)
{
int i;
m[0] = n;
for (i = n / 20; --i >= 0;)
m[9] = m[8], m[8] = m[7], m[7] = m[6], m[6] = m[5], m[5] = m[4],
m[4] = m[3], m[3] = m[2], m[2] = m[1], m[1] = m[0], m[0] = m[9];
*msg = "fast memory accesses";
return m[0];
}
static int
mslow(int *m, int n, char **msg)
{
int *p;
int i, k = 0;
m[0] = n;
for (i = n / 20; --i >= 0; k = (k + 397) & 0x3ffff)
p = m + k,
p[0] = p[100], p[20] = p[120], p[40] = p[140],
p[60] = p[160], p[80] = p[180],
p[200] = p[300], p[220] = p[320], p[240] = p[340],
p[260] = p[360], p[280] = p[380];
*msg = "slow memory accesses";
return m[0];
}
int
main(int argc, const char *argv[])
{
int i;
int *mem = malloc(1100000);
capture_stdio();
for (i = 0;; ++i) {
long t0[2], t1[2];
char *msg;
int n;
gp_get_usertime(t0);
switch (i) {
case 0:
iadd(0, n = 10000000, &msg);
break;
case 1:
imul(1, n = 1000000, &msg);
break;
case 2:
idiv(1, n = 1000000, &msg);
break;
case 3:
fadd(3.14, n = 10000000, &msg);
break;
case 4:
fmul(1.0000001, n = 10000000, &msg);
break;
case 5:
fdiv(1.0000001, n = 1000000, &msg);
break;
case 6:
fconv(12345, n = 10000000, &msg);
break;
case 7:
mfast(mem, n = 10000000, &msg);
break;
case 8:
mslow(mem, n = 1000000, &msg);
break;
default:
free(mem);
exit(0);
}
gp_get_usertime(t1);
fprintf(stdout, "Time for %9d %s = %g ms\n", n, msg,
(t1[0] - t0[0]) * 1000.0 + (t1[1] - t0[1]) / 1000000.0);
fflush(stdout);
}
}