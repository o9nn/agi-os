#include <stdio.h>
#include "cryptlib.h"
#ifndef OPENSSL_NO_MD4
# include <openssl/evp.h>
# include <openssl/objects.h>
# include <openssl/x509.h>
# include <openssl/md4.h>
# ifndef OPENSSL_NO_RSA
# include <openssl/rsa.h>
# endif
# include "evp_locl.h"
static int init(EVP_MD_CTX *ctx)
{
return MD4_Init(ctx->md_data);
}
static int update(EVP_MD_CTX *ctx, const void *data, size_t count)
{
return MD4_Update(ctx->md_data, data, count);
}
static int final(EVP_MD_CTX *ctx, unsigned char *md)
{
return MD4_Final(md, ctx->md_data);
}
static const EVP_MD md4_md = {
NID_md4,
NID_md4WithRSAEncryption,
MD4_DIGEST_LENGTH,
0,
init,
update,
final,
NULL,
NULL,
EVP_PKEY_RSA_method,
MD4_CBLOCK,
sizeof(EVP_MD *) + sizeof(MD4_CTX),
};
const EVP_MD *EVP_md4(void)
{
return (&md4_md);
}
#endif