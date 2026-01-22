#include <stdio.h>
#include "cryptlib.h"
#include <openssl/x509v3.h>
const X509V3_EXT_METHOD v3_crl_num = {
NID_crl_number, 0, ASN1_ITEM_ref(ASN1_INTEGER),
0, 0, 0, 0,
(X509V3_EXT_I2S)i2s_ASN1_INTEGER,
0,
0, 0, 0, 0, NULL
};
const X509V3_EXT_METHOD v3_delta_crl = {
NID_delta_crl, 0, ASN1_ITEM_ref(ASN1_INTEGER),
0, 0, 0, 0,
(X509V3_EXT_I2S)i2s_ASN1_INTEGER,
0,
0, 0, 0, 0, NULL
};
static void *s2i_asn1_int(X509V3_EXT_METHOD *meth, X509V3_CTX *ctx,
char *value)
{
return s2i_ASN1_INTEGER(meth, value);
}
const X509V3_EXT_METHOD v3_inhibit_anyp = {
NID_inhibit_any_policy, 0, ASN1_ITEM_ref(ASN1_INTEGER),
0, 0, 0, 0,
(X509V3_EXT_I2S)i2s_ASN1_INTEGER,
(X509V3_EXT_S2I)s2i_asn1_int,
0, 0, 0, 0, NULL
};