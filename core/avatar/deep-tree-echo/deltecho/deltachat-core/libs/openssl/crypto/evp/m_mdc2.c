#include <stdio.h>
#include "cryptlib.h"
#ifndef OPENSSL_NO_MDC2
# include <openssl/evp.h>
# include <openssl/objects.h>
# include <openssl/x509.h>
# include <openssl/mdc2.h>
# ifndef OPENSSL_NO_RSA
# include <openssl/rsa.h>
# endif
# include "evp_locl.h"
static int init(EVP_MD_CTX *ctx)
{
return MDC2_Init(ctx->md_data);
}
static int update(EVP_MD_CTX *ctx, const void *data, size_t count)
{
return MDC2_Update(ctx->md_data, data, count);
}
static int final(EVP_MD_CTX *ctx, unsigned char *md)
{
return MDC2_Final(md, ctx->md_data);
}
static const EVP_MD mdc2_md = {
NID_mdc2,
NID_mdc2WithRSA,
MDC2_DIGEST_LENGTH,
0,
init,
update,
final,
NULL,
NULL,
EVP_PKEY_RSA_ASN1_OCTET_STRING_method,
MDC2_BLOCK,
sizeof(EVP_MD *) + sizeof(MDC2_CTX),
};
const EVP_MD *EVP_mdc2(void)
{
return (&mdc2_md);
}
#endif