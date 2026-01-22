#include <openssl/crypto.h>
#include <openssl/opensslconf.h>
#if !defined(OPENSSL_NO_SHA1) && !defined(OPENSSL_NO_SHA)
# undef  SHA_0
# define SHA_1
# include <openssl/opensslv.h>
const char SHA1_version[] = "SHA1" OPENSSL_VERSION_PTEXT;
# include "sha_locl.h"
#endif