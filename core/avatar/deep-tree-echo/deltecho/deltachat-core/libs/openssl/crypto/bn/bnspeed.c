#define BASENUM 1000000
#undef PROG
#define PROG bnspeed_main
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <openssl/crypto.h>
#include <openssl/err.h>
#if !defined(OPENSSL_SYS_MSDOS) && (!defined(OPENSSL_SYS_VMS) || defined(__DECC)) && !defined(OPENSSL_SYS_MACOSX)
# define TIMES
#endif
#ifndef _IRIX
# include <time.h>
#endif
#ifdef TIMES
# include <sys/types.h>
# include <sys/times.h>
#endif
#if defined(OPENSSL_SYS_VMS_DECC) && !defined(__TMS)
# undef TIMES
#endif
#ifndef TIMES
# include <sys/timeb.h>
#endif
#if defined(sun) || defined(__ultrix)
# define _POSIX_SOURCE
# include <limits.h>
# include <sys/param.h>
#endif
#include <openssl/bn.h>
#include <openssl/x509.h>
#ifndef HZ
# ifndef CLK_TCK
#  ifndef _BSD_CLK_TCK_
#   define HZ   100.0
#  else
#   define HZ ((double)_BSD_CLK_TCK_)
#  endif
# else
#  define HZ ((double)CLK_TCK)
# endif
#endif
#undef BUFSIZE
#define BUFSIZE ((long)1024*8)
int run = 0;
static double Time_F(int s);
#define START   0
#define STOP    1
static double Time_F(int s)
{
double ret;
#ifdef TIMES
static struct tms tstart, tend;
if (s == START) {
times(&tstart);
return (0);
} else {
times(&tend);
ret = ((double)(tend.tms_utime - tstart.tms_utime)) / HZ;
return ((ret < 1e-3) ? 1e-3 : ret);
}
#else
static struct timeb tstart, tend;
long i;
if (s == START) {
ftime(&tstart);
return (0);
} else {
ftime(&tend);
i = (long)tend.millitm - (long)tstart.millitm;
ret = ((double)(tend.time - tstart.time)) + ((double)i) / 1000.0;
return ((ret < 0.001) ? 0.001 : ret);
}
#endif
}
#define NUM_SIZES       5
static int sizes[NUM_SIZES] = { 128, 256, 512, 1024, 2048 };
void do_mul(BIGNUM *r, BIGNUM *a, BIGNUM *b, BN_CTX *ctx);
int main(int argc, char **argv)
{
BN_CTX *ctx;
BIGNUM a, b, c;
ctx = BN_CTX_new();
BN_init(&a);
BN_init(&b);
BN_init(&c);
do_mul(&a, &b, &c, ctx);
}
void do_mul(BIGNUM *r, BIGNUM *a, BIGNUM *b, BN_CTX *ctx)
{
int i, j, k;
double tm;
long num;
for (i = 0; i < NUM_SIZES; i++) {
num = BASENUM;
if (i)
num /= (i * 3);
BN_rand(a, sizes[i], 1, 0);
for (j = i; j < NUM_SIZES; j++) {
BN_rand(b, sizes[j], 1, 0);
Time_F(START);
for (k = 0; k < num; k++)
BN_mul(r, b, a, ctx);
tm = Time_F(STOP);
printf("mul %4d x %4d -> %8.3fms\n", sizes[i], sizes[j],
tm * 1000.0 / num);
}
}
for (i = 0; i < NUM_SIZES; i++) {
num = BASENUM;
if (i)
num /= (i * 3);
BN_rand(a, sizes[i], 1, 0);
Time_F(START);
for (k = 0; k < num; k++)
BN_sqr(r, a, ctx);
tm = Time_F(STOP);
printf("sqr %4d x %4d -> %8.3fms\n", sizes[i], sizes[i],
tm * 1000.0 / num);
}
for (i = 0; i < NUM_SIZES; i++) {
num = BASENUM / 10;
if (i)
num /= (i * 3);
BN_rand(a, sizes[i] - 1, 1, 0);
for (j = i; j < NUM_SIZES; j++) {
BN_rand(b, sizes[j], 1, 0);
Time_F(START);
for (k = 0; k < 100000; k++)
BN_div(r, NULL, b, a, ctx);
tm = Time_F(STOP);
printf("div %4d / %4d -> %8.3fms\n", sizes[j], sizes[i] - 1,
tm * 1000.0 / num);
}
}
}