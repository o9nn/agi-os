#include <stdio.h>
#include "cryptlib.h"
#include <openssl/evp.h>
#include <openssl/objects.h>
#ifndef OPENSSL_FIPS
static int null_init_key(EVP_CIPHER_CTX *ctx, const unsigned char *key,
const unsigned char *iv, int enc);
static int null_cipher(EVP_CIPHER_CTX *ctx, unsigned char *out,
const unsigned char *in, size_t inl);
static const EVP_CIPHER n_cipher = {
NID_undef,
1, 0, 0,
0,
null_init_key,
null_cipher,
NULL,
0,
NULL,
NULL,
NULL,
NULL
};
const EVP_CIPHER *EVP_enc_null(void)
{
return (&n_cipher);
}
static int null_init_key(EVP_CIPHER_CTX *ctx, const unsigned char *key,
const unsigned char *iv, int enc)
{
return 1;
}
static int null_cipher(EVP_CIPHER_CTX *ctx, unsigned char *out,
const unsigned char *in, size_t inl)
{
if (in != out)
memcpy((char *)out, (const char *)in, inl);
return 1;
}
#endif