#include <openssl/opensslconf.h>
#ifndef OPENSSL_NO_SEED
# include <openssl/evp.h>
# include <openssl/err.h>
# include <string.h>
# include <assert.h>
# include <openssl/seed.h>
# include "evp_locl.h"
static int seed_init_key(EVP_CIPHER_CTX *ctx, const unsigned char *key,
const unsigned char *iv, int enc);
typedef struct {
SEED_KEY_SCHEDULE ks;
} EVP_SEED_KEY;
IMPLEMENT_BLOCK_CIPHER(seed, ks, SEED, EVP_SEED_KEY, NID_seed,
16, 16, 16, 128, 0, seed_init_key, 0, 0, 0, 0)
static int seed_init_key(EVP_CIPHER_CTX *ctx, const unsigned char *key,
const unsigned char *iv, int enc)
{
SEED_set_key(key, ctx->cipher_data);
return 1;
}
#endif