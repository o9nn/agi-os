#include <stdio.h>
#include "cryptlib.h"
#include <openssl/bio.h>
#include <openssl/evp.h>
#include <openssl/x509.h>
#include <openssl/pkcs7.h>
#include <openssl/pem.h>
IMPLEMENT_PEM_rw(X509, X509, PEM_STRING_X509, X509)