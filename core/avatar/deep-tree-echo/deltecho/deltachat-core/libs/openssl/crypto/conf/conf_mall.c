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
void OPENSSL_load_builtin_modules(void)
{
ASN1_add_oid_module();
#ifndef OPENSSL_NO_ENGINE
ENGINE_add_conf_module();
#endif
EVP_add_alg_module();
}