#include <openssl/crypto.h>
#include <openssl/opensslconf.h>
#if !defined(OPENSSL_NO_SHA0) && !defined(OPENSSL_NO_SHA)
# undef SHA_1
# define SHA_0
# include <openssl/opensslv.h>
const char SHA_version[] = "SHA" OPENSSL_VERSION_PTEXT;
# include "sha_locl.h"
#endif