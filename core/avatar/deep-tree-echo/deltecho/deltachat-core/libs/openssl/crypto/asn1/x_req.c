#include <stdio.h>
#include "cryptlib.h"
#include <openssl/asn1t.h>
#include <openssl/x509.h>
static int rinf_cb(int operation, ASN1_VALUE **pval, const ASN1_ITEM *it,
void *exarg)
{
X509_REQ_INFO *rinf = (X509_REQ_INFO *)*pval;
if (operation == ASN1_OP_NEW_POST) {
rinf->attributes = sk_X509_ATTRIBUTE_new_null();
if (!rinf->attributes)
return 0;
}
return 1;
}
ASN1_SEQUENCE_enc(X509_REQ_INFO, enc, rinf_cb) = {
ASN1_SIMPLE(X509_REQ_INFO, version, ASN1_INTEGER),
ASN1_SIMPLE(X509_REQ_INFO, subject, X509_NAME),
ASN1_SIMPLE(X509_REQ_INFO, pubkey, X509_PUBKEY),
ASN1_IMP_SET_OF_OPT(X509_REQ_INFO, attributes, X509_ATTRIBUTE, 0)
} ASN1_SEQUENCE_END_enc(X509_REQ_INFO, X509_REQ_INFO)
IMPLEMENT_ASN1_FUNCTIONS(X509_REQ_INFO)
ASN1_SEQUENCE_ref(X509_REQ, 0, CRYPTO_LOCK_X509_REQ) = {
ASN1_SIMPLE(X509_REQ, req_info, X509_REQ_INFO),
ASN1_SIMPLE(X509_REQ, sig_alg, X509_ALGOR),
ASN1_SIMPLE(X509_REQ, signature, ASN1_BIT_STRING)
} ASN1_SEQUENCE_END_ref(X509_REQ, X509_REQ)
IMPLEMENT_ASN1_FUNCTIONS(X509_REQ)
IMPLEMENT_ASN1_DUP_FUNCTION(X509_REQ)