#include <stdio.h>
#include <openssl/objects.h>
#include "ssl_locl.h"
static const SSL_METHOD *dtls1_get_method(int ver);
static const SSL_METHOD *dtls1_get_method(int ver)
{
if (ver == DTLS1_VERSION)
return (DTLSv1_method());
else
return (NULL);
}
IMPLEMENT_dtls1_meth_func(DTLSv1_method,
dtls1_accept, dtls1_connect, dtls1_get_method)