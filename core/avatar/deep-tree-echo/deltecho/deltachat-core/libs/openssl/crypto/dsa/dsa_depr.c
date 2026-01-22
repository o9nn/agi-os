#undef GENUINE_DSA
#ifdef GENUINE_DSA
# define HASH    EVP_sha()
#else
# define HASH    EVP_sha1()
#endif
static void *dummy = &dummy;
#ifndef OPENSSL_NO_SHA
# include <stdio.h>
# include <time.h>
# include "cryptlib.h"
# include <openssl/evp.h>
# include <openssl/bn.h>
# include <openssl/dsa.h>
# include <openssl/rand.h>
# include <openssl/sha.h>
# ifndef OPENSSL_NO_DEPRECATED
DSA *DSA_generate_parameters(int bits,
unsigned char *seed_in, int seed_len,
int *counter_ret, unsigned long *h_ret,
void (*callback) (int, int, void *),
void *cb_arg)
{
BN_GENCB cb;
DSA *ret;
if ((ret = DSA_new()) == NULL)
return NULL;
BN_GENCB_set_old(&cb, callback, cb_arg);
if (DSA_generate_parameters_ex(ret, bits, seed_in, seed_len,
counter_ret, h_ret, &cb))
return ret;
DSA_free(ret);
return NULL;
}
# endif
#endif