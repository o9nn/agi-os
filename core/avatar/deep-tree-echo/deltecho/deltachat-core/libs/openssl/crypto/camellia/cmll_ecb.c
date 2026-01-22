#ifndef CAMELLIA_DEBUG
# ifndef NDEBUG
# define NDEBUG
# endif
#endif
#include <assert.h>
#include <openssl/camellia.h>
#include "cmll_locl.h"
void Camellia_ecb_encrypt(const unsigned char *in, unsigned char *out,
const CAMELLIA_KEY *key, const int enc)
{
assert(in && out && key);
assert((CAMELLIA_ENCRYPT == enc) || (CAMELLIA_DECRYPT == enc));
if (CAMELLIA_ENCRYPT == enc)
Camellia_encrypt(in, out, key);
else
Camellia_decrypt(in, out, key);
}