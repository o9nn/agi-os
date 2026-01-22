#include <stdio.h>
#include <openssl/dsa.h>
#define TEST
#define GENUINE_DSA
#ifdef GENUINE_DSA
# define LAST_VALUE 0xbd
#else
# define LAST_VALUE 0xd3
#endif
#ifdef TEST
unsigned char seed[20] = {
0xd5, 0x01, 0x4e, 0x4b,
0x60, 0xef, 0x2b, 0xa8,
0xb6, 0x21, 0x1b, 0x40,
0x62, 0xba, 0x32, 0x24,
0xe0, 0x42, 0x7d, LAST_VALUE
};
#endif
int cb(int p, int n)
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
printf("%c", c);
fflush(stdout);
}
main()
{
int i;
BIGNUM *n;
BN_CTX *ctx;
unsigned char seed_buf[20];
DSA *dsa;
int counter, h;
BIO *bio_err = NULL;
if (bio_err == NULL)
bio_err = BIO_new_fp(stderr, BIO_NOCLOSE);
memcpy(seed_buf, seed, 20);
dsa = DSA_generate_parameters(1024, seed, 20, &counter, &h, cb, bio_err);
if (dsa == NULL)
DSA_print(bio_err, dsa, 0);
}