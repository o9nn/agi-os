#include <openssl/opensslv.h>
#include <openssl/crypto.h>
#include <openssl/camellia.h>
#include "cmll_locl.h"
const char CAMELLIA_version[] = "CAMELLIA" OPENSSL_VERSION_PTEXT;
int private_Camellia_set_key(const unsigned char *userKey, const int bits,
CAMELLIA_KEY *key)
{
if (!userKey || !key)
return -1;
if (bits != 128 && bits != 192 && bits != 256)
return -2;
key->grand_rounds = Camellia_Ekeygen(bits, userKey, key->u.rd_key);
return 0;
}
void Camellia_encrypt(const unsigned char *in, unsigned char *out,
const CAMELLIA_KEY *key)
{
Camellia_EncryptBlock_Rounds(key->grand_rounds, in, key->u.rd_key, out);
}
void Camellia_decrypt(const unsigned char *in, unsigned char *out,
const CAMELLIA_KEY *key)
{
Camellia_DecryptBlock_Rounds(key->grand_rounds, in, key->u.rd_key, out);
}