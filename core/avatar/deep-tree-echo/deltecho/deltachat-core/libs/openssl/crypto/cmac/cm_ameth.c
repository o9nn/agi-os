#include <stdio.h>
#include "cryptlib.h"
#include <openssl/evp.h>
#include <openssl/cmac.h>
#include "asn1_locl.h"
static int cmac_size(const EVP_PKEY *pkey)
{
return EVP_MAX_BLOCK_LENGTH;
}
static void cmac_key_free(EVP_PKEY *pkey)
{
CMAC_CTX *cmctx = (CMAC_CTX *)pkey->pkey.ptr;
if (cmctx)
CMAC_CTX_free(cmctx);
}
const EVP_PKEY_ASN1_METHOD cmac_asn1_meth = {
EVP_PKEY_CMAC,
EVP_PKEY_CMAC,
0,
"CMAC",
"OpenSSL CMAC method",
0, 0, 0, 0,
0, 0, 0,
cmac_size,
0,
0, 0, 0, 0, 0, 0, 0,
cmac_key_free,
0,
0, 0
};