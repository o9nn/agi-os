#include <stdio.h>
#include "cryptlib.h"
#ifndef OPENSSL_NO_MD5
# include <openssl/evp.h>
# include <openssl/objects.h>
# include <openssl/x509.h>
# include <openssl/md5.h>
# ifndef OPENSSL_NO_RSA
#  include <openssl/rsa.h>
# endif
# include "evp_locl.h"
static int init(EVP_MD_CTX *ctx)
{
return MD5_Init(ctx->md_data);
}
static int update(EVP_MD_CTX *ctx, const void *data, size_t count)
{
return MD5_Update(ctx->md_data, data, count);
}
static int final(EVP_MD_CTX *ctx, unsigned char *md)
{
return MD5_Final(md, ctx->md_data);
}
static const EVP_MD md5_md = {
NID_md5,
NID_md5WithRSAEncryption,
MD5_DIGEST_LENGTH,
0,
init,
update,
final,
NULL,
NULL,
EVP_PKEY_RSA_method,
MD5_CBLOCK,
sizeof(EVP_MD *) + sizeof(MD5_CTX),
};
const EVP_MD *EVP_md5(void)
{
return (&md5_md);
}
#endif