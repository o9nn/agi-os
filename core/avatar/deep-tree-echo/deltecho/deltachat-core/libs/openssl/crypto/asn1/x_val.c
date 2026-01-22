#include <stdio.h>
#include "cryptlib.h"
#include <openssl/asn1t.h>
#include <openssl/x509.h>
ASN1_SEQUENCE(X509_VAL) = {
ASN1_SIMPLE(X509_VAL, notBefore, ASN1_TIME),
ASN1_SIMPLE(X509_VAL, notAfter, ASN1_TIME)
} ASN1_SEQUENCE_END(X509_VAL)
IMPLEMENT_ASN1_FUNCTIONS(X509_VAL)