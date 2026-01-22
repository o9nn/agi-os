#include <stdio.h>
#include "cryptlib.h"
#include <openssl/evp.h>
#include <openssl/objects.h>
#include <openssl/x509.h>
#ifndef OPENSSL_NO_SHA
# ifndef OPENSSL_FIPS
static int init(EVP_MD_CTX *ctx)
{
return SHA1_Init(ctx->md_data);
}
static int update(EVP_MD_CTX *ctx, const void *data, size_t count)
{
return SHA1_Update(ctx->md_data, data, count);
}
static int final(EVP_MD_CTX *ctx, unsigned char *md)
{
return SHA1_Final(md, ctx->md_data);
}
static const EVP_MD ecdsa_md = {
NID_ecdsa_with_SHA1,
NID_ecdsa_with_SHA1,
SHA_DIGEST_LENGTH,
EVP_MD_FLAG_PKEY_DIGEST,
init,
update,
final,
NULL,
NULL,
EVP_PKEY_ECDSA_method,
SHA_CBLOCK,
sizeof(EVP_MD *) + sizeof(SHA_CTX),
};
const EVP_MD *EVP_ecdsa(void)
{
return (&ecdsa_md);
}
# endif
#endif