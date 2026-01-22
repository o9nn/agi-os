#include <stdio.h>
#include "cryptlib.h"
#include <openssl/evp.h>
#include <openssl/rand.h>
#include <openssl/objects.h>
#include <openssl/x509.h>
#include <openssl/pkcs7.h>
PKCS7_in_bio(PKCS7 *p7, BIO *in);
PKCS7_out_bio(PKCS7 *p7, BIO *out);
PKCS7_add_signer(PKCS7 *p7, X509 *cert, EVP_PKEY *key);
PKCS7_cipher(PKCS7 *p7, EVP_CIPHER *cipher);
PKCS7_Init(PKCS7 *p7);
PKCS7_Update(PKCS7 *p7);
PKCS7_Finish(PKCS7 *p7);