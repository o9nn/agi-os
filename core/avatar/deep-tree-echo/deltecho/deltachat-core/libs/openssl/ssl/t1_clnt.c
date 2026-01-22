#include <stdio.h>
#include "ssl_locl.h"
#include <openssl/buffer.h>
#include <openssl/rand.h>
#include <openssl/objects.h>
#include <openssl/evp.h>
static const SSL_METHOD *tls1_get_client_method(int ver);
static const SSL_METHOD *tls1_get_client_method(int ver)
{
if (ver == TLS1_2_VERSION)
return TLSv1_2_client_method();
if (ver == TLS1_1_VERSION)
return TLSv1_1_client_method();
if (ver == TLS1_VERSION)
return TLSv1_client_method();
return NULL;
}
IMPLEMENT_tls_meth_func(TLS1_2_VERSION, TLSv1_2_client_method,
ssl_undefined_function,
ssl3_connect, tls1_get_client_method)
IMPLEMENT_tls_meth_func(TLS1_1_VERSION, TLSv1_1_client_method,
ssl_undefined_function,
ssl3_connect, tls1_get_client_method)
IMPLEMENT_tls_meth_func(TLS1_VERSION, TLSv1_client_method,
ssl_undefined_function,
ssl3_connect, tls1_get_client_method)