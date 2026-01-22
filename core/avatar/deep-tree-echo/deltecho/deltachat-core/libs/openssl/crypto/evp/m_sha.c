#include <stdio.h>
#include "cryptlib.h"
#if !defined(OPENSSL_NO_SHA) && !defined(OPENSSL_NO_SHA0)
# include <openssl/evp.h>
# include <openssl/objects.h>
# include <openssl/x509.h>
# ifndef OPENSSL_NO_RSA
#  include <openssl/rsa.h>
# endif
# include "evp_locl.h"
static int init(EVP_MD_CTX *ctx)
{
return SHA_Init(ctx->md_data);
}
static int update(EVP_MD_CTX *ctx, const void *data, size_t count)
{
return SHA_Update(ctx->md_data, data, count);
}
static int final(EVP_MD_CTX *ctx, unsigned char *md)
{
return SHA_Final(md, ctx->md_data);
}
static const EVP_MD sha_md = {
NID_sha,
NID_shaWithRSAEncryption,
SHA_DIGEST_LENGTH,
0,
init,
update,
final,
NULL,
NULL,
EVP_PKEY_RSA_method,
SHA_CBLOCK,
sizeof(EVP_MD *) + sizeof(SHA_CTX),
};
const EVP_MD *EVP_sha(void)
{
return (&sha_md);
}
#endif