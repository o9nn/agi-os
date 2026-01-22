#include <stdio.h>
#include <openssl/objects.h>
#include "ssl_locl.h"
static const SSL_METHOD *tls1_get_method(int ver)
{
if (ver == TLS1_2_VERSION)
return TLSv1_2_method();
if (ver == TLS1_1_VERSION)
return TLSv1_1_method();
if (ver == TLS1_VERSION)
return TLSv1_method();
return NULL;
}
IMPLEMENT_tls_meth_func(TLS1_2_VERSION, TLSv1_2_method,
ssl3_accept, ssl3_connect, tls1_get_method)
IMPLEMENT_tls_meth_func(TLS1_1_VERSION, TLSv1_1_method,
ssl3_accept, ssl3_connect, tls1_get_method)
IMPLEMENT_tls_meth_func(TLS1_VERSION, TLSv1_method,
ssl3_accept, ssl3_connect, tls1_get_method)