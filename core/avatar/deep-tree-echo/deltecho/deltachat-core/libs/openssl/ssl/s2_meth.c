#include "ssl_locl.h"
#ifndef OPENSSL_NO_SSL2_METHOD
# ifndef OPENSSL_NO_SSL2
# include <stdio.h>
# include <openssl/objects.h>
static const SSL_METHOD *ssl2_get_method(int ver);
static const SSL_METHOD *ssl2_get_method(int ver)
{
if (ver == SSL2_VERSION)
return (SSLv2_method());
else
return (NULL);
}
IMPLEMENT_ssl2_meth_func(SSLv2_method,
ssl2_accept, ssl2_connect, ssl2_get_method)
# else
const SSL_METHOD *SSLv2_method(void) { return NULL; }
const SSL_METHOD *SSLv2_client_method(void) { return NULL; }
const SSL_METHOD *SSLv2_server_method(void) { return NULL; }
# endif
#else
# if PEDANTIC
static void *dummy = &dummy;
# endif
#endif