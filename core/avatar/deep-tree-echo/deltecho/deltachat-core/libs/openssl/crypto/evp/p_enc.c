#include <stdio.h>
#include "cryptlib.h"
#include <openssl/rand.h>
#ifndef OPENSSL_NO_RSA
# include <openssl/rsa.h>
#endif
#include <openssl/evp.h>
#include <openssl/objects.h>
#include <openssl/x509.h>
int EVP_PKEY_encrypt_old(unsigned char *ek, const unsigned char *key,
int key_len, EVP_PKEY *pubk)
{
int ret = 0;
#ifndef OPENSSL_NO_RSA
if (pubk->type != EVP_PKEY_RSA) {
#endif
EVPerr(EVP_F_EVP_PKEY_ENCRYPT_OLD, EVP_R_PUBLIC_KEY_NOT_RSA);
#ifndef OPENSSL_NO_RSA
goto err;
}
ret =
RSA_public_encrypt(key_len, key, ek, pubk->pkey.rsa,
RSA_PKCS1_PADDING);
err:
#endif
return (ret);
}