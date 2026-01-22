#include <stdio.h>
#include "cryptlib.h"
#include <openssl/bn.h>
#include <openssl/rsa.h>
#include <openssl/rand.h>
static int RSA_null_public_encrypt(int flen, const unsigned char *from,
unsigned char *to, RSA *rsa, int padding);
static int RSA_null_private_encrypt(int flen, const unsigned char *from,
unsigned char *to, RSA *rsa, int padding);
static int RSA_null_public_decrypt(int flen, const unsigned char *from,
unsigned char *to, RSA *rsa, int padding);
static int RSA_null_private_decrypt(int flen, const unsigned char *from,
unsigned char *to, RSA *rsa, int padding);
#if 0
static int RSA_null_mod_exp(const BIGNUM *r0, const BIGNUM *i, RSA *rsa);
#endif
static int RSA_null_init(RSA *rsa);
static int RSA_null_finish(RSA *rsa);
static RSA_METHOD rsa_null_meth = {
"Null RSA",
RSA_null_public_encrypt,
RSA_null_public_decrypt,
RSA_null_private_encrypt,
RSA_null_private_decrypt,
NULL,
NULL,
RSA_null_init,
RSA_null_finish,
0,
NULL,
NULL,
NULL,
NULL
};
const RSA_METHOD *RSA_null_method(void)
{
return (&rsa_null_meth);
}
static int RSA_null_public_encrypt(int flen, const unsigned char *from,
unsigned char *to, RSA *rsa, int padding)
{
RSAerr(RSA_F_RSA_NULL_PUBLIC_ENCRYPT, RSA_R_RSA_OPERATIONS_NOT_SUPPORTED);
return -1;
}
static int RSA_null_private_encrypt(int flen, const unsigned char *from,
unsigned char *to, RSA *rsa, int padding)
{
RSAerr(RSA_F_RSA_NULL_PRIVATE_ENCRYPT,
RSA_R_RSA_OPERATIONS_NOT_SUPPORTED);
return -1;
}
static int RSA_null_private_decrypt(int flen, const unsigned char *from,
unsigned char *to, RSA *rsa, int padding)
{
RSAerr(RSA_F_RSA_NULL_PRIVATE_DECRYPT,
RSA_R_RSA_OPERATIONS_NOT_SUPPORTED);
return -1;
}
static int RSA_null_public_decrypt(int flen, const unsigned char *from,
unsigned char *to, RSA *rsa, int padding)
{
RSAerr(RSA_F_RSA_NULL_PUBLIC_DECRYPT, RSA_R_RSA_OPERATIONS_NOT_SUPPORTED);
return -1;
}
#if 0
static int RSA_null_mod_exp(BIGNUM *r0, BIGNUM *I, RSA *rsa)
{
... err(RSA_F_RSA_NULL_MOD_EXP, RSA_R_RSA_OPERATIONS_NOT_SUPPORTED);
return -1;
}
#endif
static int RSA_null_init(RSA *rsa)
{
return (1);
}
static int RSA_null_finish(RSA *rsa)
{
return (1);
}