#include <stdio.h>
#include <openssl/crypto.h>
#include "cryptlib.h"
#include <openssl/conf.h>
#include <openssl/dso.h>
#include <openssl/x509.h>
#include <openssl/asn1.h>
#ifndef OPENSSL_NO_ENGINE
# include <openssl/engine.h>
#endif
static int openssl_configured = 0;
void OPENSSL_config(const char *config_name)
{
if (openssl_configured)
return;
OPENSSL_load_builtin_modules();
#ifndef OPENSSL_NO_ENGINE
ENGINE_load_builtin_engines();
#endif
ERR_clear_error();
CONF_modules_load_file(NULL, config_name,
CONF_MFLAGS_DEFAULT_SECTION |
CONF_MFLAGS_IGNORE_MISSING_FILE);
openssl_configured = 1;
}
void OPENSSL_no_config()
{
openssl_configured = 1;
}