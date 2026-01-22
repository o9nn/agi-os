#include <stdio.h>
#include "cryptlib.h"
#ifndef OPENSSL_NO_MD2
# include <openssl/evp.h>
# include <openssl/objects.h>
# include <openssl/x509.h>
# include <openssl/md2.h>
# ifndef OPENSSL_NO_RSA
# include <openssl/rsa.h>
# endif
static int init(EVP_MD_CTX *ctx)
{
return MD2_Init(ctx->md_data);
}
static int update(EVP_MD_CTX *ctx, const void *data, size_t count)
{
return MD2_Update(ctx->md_data, data, count);
}
static int final(EVP_MD_CTX *ctx, unsigned char *md)
{
return MD2_Final(md, ctx->md_data);
}
static const EVP_MD md2_md = {
NID_md2,
NID_md2WithRSAEncryption,
MD2_DIGEST_LENGTH,
0,
init,
update,
final,
NULL,
NULL,
EVP_PKEY_RSA_method,
MD2_BLOCK,
sizeof(EVP_MD *) + sizeof(MD2_CTX),
};
const EVP_MD *EVP_md2(void)
{
return (&md2_md);
}
#endif