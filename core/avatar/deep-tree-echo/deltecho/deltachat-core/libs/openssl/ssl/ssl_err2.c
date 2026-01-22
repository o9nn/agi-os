#include <stdio.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
void SSL_load_error_strings(void)
{
#ifndef OPENSSL_NO_ERR
ERR_load_crypto_strings();
ERR_load_SSL_strings();
#endif
}