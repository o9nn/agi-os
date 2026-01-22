#define BASENUM 5000
#define NUM_START 0
#define TEST_EXP
#undef TEST_MUL
#undef TEST_SQR
#undef TEST_GCD
#undef TEST_KRON
#undef TEST_INV
#undef TEST_SQRT
#define P_MOD_64 9
#if defined(TEST_EXP) + defined(TEST_MUL) + defined(TEST_SQR) + defined(TEST_GCD) + defined(TEST_KRON) + defined(TEST_INV) +defined(TEST_SQRT) != 1
# error "choose one test"
#endif
#if defined(TEST_INV) || defined(TEST_SQRT)
# define C_PRIME
static void genprime_cb(int p, int n, void *arg);
#endif
#undef PROG
#define PROG bnspeed_main
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <openssl/crypto.h>
#include <openssl/err.h>
#include <openssl/rand.h>
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
# ifndef _BSD_CLK_TCK_
# define HZ 100.0
# else
# define HZ ((double)_BSD_CLK_TCK_)
# endif
# else
# define HZ ((double)CLK_TCK)
# endif
#endif
#undef BUFSIZE
#define BUFSIZE ((long)1024*8)
int run = 0;
static double Time_F(int s);
#define START 0
#define STOP 1
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
#define NUM_SIZES 7
#if NUM_START > NUM_SIZES
# error "NUM_START > NUM_SIZES"
#endif
static int sizes[NUM_SIZES] = { 128, 256, 512, 1024, 2048, 4096, 8192 };
static int mul_c[NUM_SIZES] =
{ 8 * 8 * 8 * 8 * 8 * 8, 8 * 8 * 8 * 8 * 8, 8 * 8 * 8 * 8, 8 * 8 * 8,
8 * 8, 8, 1
};
#define RAND_SEED(string) { const char str[] = string; RAND_seed(string, sizeof str); }
void do_mul_exp(BIGNUM *r, BIGNUM *a, BIGNUM *b, BIGNUM *c, BN_CTX *ctx);
int main(int argc, char **argv)
{
BN_CTX *ctx;
BIGNUM *a, *b, *c, *r;
#if 1
if (!CRYPTO_set_mem_debug_functions(0, 0, 0, 0, 0))
abort();
#endif
ctx = BN_CTX_new();
a = BN_new();
b = BN_new();
c = BN_new();
r = BN_new();
while (!RAND_status())
RAND_SEED("I demand a manual recount!");
do_mul_exp(r, a, b, c, ctx);
return 0;
}
void do_mul_exp(BIGNUM *r, BIGNUM *a, BIGNUM *b, BIGNUM *c, BN_CTX *ctx)
{
int i, k;
double tm;
long num;
num = BASENUM;
for (i = NUM_START; i < NUM_SIZES; i++) {
#ifdef C_PRIME
# ifdef TEST_SQRT
if (!BN_set_word(a, 64))
goto err;
if (!BN_set_word(b, P_MOD_64))
goto err;
# define ADD a
# define REM b
# else
# define ADD NULL
# define REM NULL
# endif
if (!BN_generate_prime(c, sizes[i], 0, ADD, REM, genprime_cb, NULL))
goto err;
putc('\n', stderr);
fflush(stderr);
#endif
for (k = 0; k < num; k++) {
if (k % 50 == 0) {
if (!BN_pseudo_rand(a, sizes[i], 1, 0))
goto err;
if (!BN_pseudo_rand(b, sizes[i], 1, 0))
goto err;
#ifndef C_PRIME
if (!BN_pseudo_rand(c, sizes[i], 1, 1))
goto err;
#endif
#ifdef TEST_SQRT
if (!BN_mod_sqr(a, a, c, ctx))
goto err;
if (!BN_mod_sqr(b, b, c, ctx))
goto err;
#else
if (!BN_nnmod(a, a, c, ctx))
goto err;
if (!BN_nnmod(b, b, c, ctx))
goto err;
#endif
if (k == 0)
Time_F(START);
}
#if defined(TEST_EXP)
if (!BN_mod_exp(r, a, b, c, ctx))
goto err;
#elif defined(TEST_MUL)
{
int i = 0;
for (i = 0; i < 50; i++)
if (!BN_mod_mul(r, a, b, c, ctx))
goto err;
}
#elif defined(TEST_SQR)
{
int i = 0;
for (i = 0; i < 50; i++) {
if (!BN_mod_sqr(r, a, c, ctx))
goto err;
if (!BN_mod_sqr(r, b, c, ctx))
goto err;
}
}
#elif defined(TEST_GCD)
if (!BN_gcd(r, a, b, ctx))
goto err;
if (!BN_gcd(r, b, c, ctx))
goto err;
if (!BN_gcd(r, c, a, ctx))
goto err;
#elif defined(TEST_KRON)
if (-2 == BN_kronecker(a, b, ctx))
goto err;
if (-2 == BN_kronecker(b, c, ctx))
goto err;
if (-2 == BN_kronecker(c, a, ctx))
goto err;
#elif defined(TEST_INV)
if (!BN_mod_inverse(r, a, c, ctx))
goto err;
if (!BN_mod_inverse(r, b, c, ctx))
goto err;
#else
if (!BN_mod_sqrt(r, a, c, ctx))
goto err;
if (!BN_mod_sqrt(r, b, c, ctx))
goto err;
#endif
}
tm = Time_F(STOP);
printf(
#if defined(TEST_EXP)
"modexp %4d ^ %4d %% %4d"
#elif defined(TEST_MUL)
"50*modmul %4d %4d %4d"
#elif defined(TEST_SQR)
"100*modsqr %4d %4d %4d"
#elif defined(TEST_GCD)
"3*gcd %4d %4d %4d"
#elif defined(TEST_KRON)
"3*kronecker %4d %4d %4d"
#elif defined(TEST_INV)
"2*inv %4d %4d mod %4d"
#else
"2*sqrt [prime == %d (mod 64)] %4d %4d mod %4d"
#endif
" -> %8.6fms %5.1f (%ld)\n",
#ifdef TEST_SQRT
P_MOD_64,
#endif
sizes[i], sizes[i], sizes[i], tm * 1000.0 / num,
tm * mul_c[i] / num, num);
num /= 7;
if (num <= 0)
num = 1;
}
return;
err:
ERR_print_errors_fp(stderr);
}
#ifdef C_PRIME
static void genprime_cb(int p, int n, void *arg)
{
char c = '*';
if (p == 0)
c = '.';
if (p == 1)
c = '+';
if (p == 2)
c = '*';
if (p == 3)
c = '\n';
putc(c, stderr);
fflush(stderr);
(void)n;
(void)arg;
}
#endif