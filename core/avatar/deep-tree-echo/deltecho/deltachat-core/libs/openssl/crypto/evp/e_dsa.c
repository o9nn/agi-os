#include <stdio.h>
#include "cryptlib.h"
#include <openssl/evp.h>
#include <openssl/objects.h>
#include <openssl/x509.h>
static EVP_PKEY_METHOD dss_method = {
DSA_sign,
DSA_verify,
{EVP_PKEY_DSA, EVP_PKEY_DSA2, EVP_PKEY_DSA3, NULL},
};