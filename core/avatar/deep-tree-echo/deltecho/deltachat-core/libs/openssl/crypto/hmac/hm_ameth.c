#include <stdio.h>
#include "cryptlib.h"
#include <openssl/evp.h>
#include "asn1_locl.h"
#define HMAC_TEST_PRIVATE_KEY_FORMAT
static int hmac_size(const EVP_PKEY *pkey)
{
return EVP_MAX_MD_SIZE;
}
static void hmac_key_free(EVP_PKEY *pkey)
{
ASN1_OCTET_STRING *os = (ASN1_OCTET_STRING *)pkey->pkey.ptr;
if (os) {
if (os->data)
OPENSSL_cleanse(os->data, os->length);
ASN1_OCTET_STRING_free(os);
}
}
static int hmac_pkey_ctrl(EVP_PKEY *pkey, int op, long arg1, void *arg2)
{
switch (op) {
case ASN1_PKEY_CTRL_DEFAULT_MD_NID:
*(int *)arg2 = NID_sha1;
return 1;
default:
return -2;
}
}
#ifdef HMAC_TEST_PRIVATE_KEY_FORMAT
static int old_hmac_decode(EVP_PKEY *pkey,
const unsigned char **pder, int derlen)
{
ASN1_OCTET_STRING *os;
os = ASN1_OCTET_STRING_new();
if (!os || !ASN1_OCTET_STRING_set(os, *pder, derlen))
goto err;
if (!EVP_PKEY_assign(pkey, EVP_PKEY_HMAC, os))
goto err;
return 1;
err:
ASN1_OCTET_STRING_free(os);
return 0;
}
static int old_hmac_encode(const EVP_PKEY *pkey, unsigned char **pder)
{
int inc;
ASN1_OCTET_STRING *os = (ASN1_OCTET_STRING *)pkey->pkey.ptr;
if (pder) {
if (!*pder) {
*pder = OPENSSL_malloc(os->length);
inc = 0;
} else
inc = 1;
memcpy(*pder, os->data, os->length);
if (inc)
*pder += os->length;
}
return os->length;
}
#endif
const EVP_PKEY_ASN1_METHOD hmac_asn1_meth = {
EVP_PKEY_HMAC,
EVP_PKEY_HMAC,
0,
"HMAC",
"OpenSSL HMAC method",
0, 0, 0, 0,
0, 0, 0,
hmac_size,
0,
0, 0, 0, 0, 0, 0, 0,
hmac_key_free,
hmac_pkey_ctrl,
#ifdef HMAC_TEST_PRIVATE_KEY_FORMAT
old_hmac_decode,
old_hmac_encode
#else
0, 0
#endif
};