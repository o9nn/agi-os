#include <stdio.h>
#include "cryptlib.h"
#include <openssl/asn1t.h>
#include <openssl/x509.h>
ASN1_SEQUENCE(X509_SIG) = {
ASN1_SIMPLE(X509_SIG, algor, X509_ALGOR),
ASN1_SIMPLE(X509_SIG, digest, ASN1_OCTET_STRING)
} ASN1_SEQUENCE_END(X509_SIG)
IMPLEMENT_ASN1_FUNCTIONS(X509_SIG)