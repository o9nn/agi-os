#include <stdio.h>
#include "cryptlib.h"
#ifndef OPENSSL_NO_RIPEMD
# include <openssl/ripemd.h>
# include <openssl/evp.h>
# include <openssl/objects.h>
# include <openssl/x509.h>
# ifndef OPENSSL_NO_RSA
#  include <openssl/rsa.h>
# endif
# include "evp_locl.h"
static int init(EVP_MD_CTX *ctx)
{
return RIPEMD160_Init(ctx->md_data);
}
static int update(EVP_MD_CTX *ctx, const void *data, size_t count)
{
return RIPEMD160_Update(ctx->md_data, data, count);
}
static int final(EVP_MD_CTX *ctx, unsigned char *md)
{
return RIPEMD160_Final(md, ctx->md_data);
}
static const EVP_MD ripemd160_md = {
NID_ripemd160,
NID_ripemd160WithRSA,
RIPEMD160_DIGEST_LENGTH,
0,
init,
update,
final,
NULL,
NULL,
EVP_PKEY_RSA_method,
RIPEMD160_CBLOCK,
sizeof(EVP_MD *) + sizeof(RIPEMD160_CTX),
};
const EVP_MD *EVP_ripemd160(void)
{
return (&ripemd160_md);
}
#endif