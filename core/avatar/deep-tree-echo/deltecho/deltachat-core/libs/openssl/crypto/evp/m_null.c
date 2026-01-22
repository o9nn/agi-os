#include <stdio.h>
#include "cryptlib.h"
#include <openssl/evp.h>
#include <openssl/objects.h>
#include <openssl/x509.h>
static int init(EVP_MD_CTX *ctx)
{
return 1;
}
static int update(EVP_MD_CTX *ctx, const void *data, size_t count)
{
return 1;
}
static int final(EVP_MD_CTX *ctx, unsigned char *md)
{
return 1;
}
static const EVP_MD null_md = {
NID_undef,
NID_undef,
0,
0,
init,
update,
final,
NULL,
NULL,
EVP_PKEY_NULL_method,
0,
sizeof(EVP_MD *),
};
const EVP_MD *EVP_md_null(void)
{
return (&null_md);
}