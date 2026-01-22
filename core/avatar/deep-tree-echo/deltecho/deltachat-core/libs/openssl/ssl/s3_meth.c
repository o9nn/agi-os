#include <stdio.h>
#include <openssl/objects.h>
#include "ssl_locl.h"
#ifndef OPENSSL_NO_SSL3_METHOD
static const SSL_METHOD *ssl3_get_method(int ver)
{
if (ver == SSL3_VERSION)
return (SSLv3_method());
else
return (NULL);
}
IMPLEMENT_ssl3_meth_func(SSLv3_method,
ssl3_accept, ssl3_connect, ssl3_get_method)
#endif