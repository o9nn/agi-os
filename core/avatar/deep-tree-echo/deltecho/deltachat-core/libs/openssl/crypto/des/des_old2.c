#undef OPENSSL_DES_LIBDES_COMPATIBILITY
#include <openssl/des.h>
#include <openssl/rand.h>
void _ossl_096_des_random_seed(DES_cblock *key)
{
RAND_seed(key, sizeof(DES_cblock));
}