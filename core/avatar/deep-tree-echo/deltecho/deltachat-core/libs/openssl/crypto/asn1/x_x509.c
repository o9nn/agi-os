#include <stdio.h>
#include "cryptlib.h"
#include <openssl/evp.h>
#include <openssl/asn1t.h>
#include <openssl/x509.h>
#include <openssl/x509v3.h>
ASN1_SEQUENCE_enc(X509_CINF, enc, 0) = {
ASN1_EXP_OPT(X509_CINF, version, ASN1_INTEGER, 0),
ASN1_SIMPLE(X509_CINF, serialNumber, ASN1_INTEGER),
ASN1_SIMPLE(X509_CINF, signature, X509_ALGOR),
ASN1_SIMPLE(X509_CINF, issuer, X509_NAME),
ASN1_SIMPLE(X509_CINF, validity, X509_VAL),
ASN1_SIMPLE(X509_CINF, subject, X509_NAME),
ASN1_SIMPLE(X509_CINF, key, X509_PUBKEY),
ASN1_IMP_OPT(X509_CINF, issuerUID, ASN1_BIT_STRING, 1),
ASN1_IMP_OPT(X509_CINF, subjectUID, ASN1_BIT_STRING, 2),
ASN1_EXP_SEQUENCE_OF_OPT(X509_CINF, extensions, X509_EXTENSION, 3)
} ASN1_SEQUENCE_END_enc(X509_CINF, X509_CINF)
IMPLEMENT_ASN1_FUNCTIONS(X509_CINF)
extern void policy_cache_free(X509_POLICY_CACHE *cache);
static int x509_cb(int operation, ASN1_VALUE **pval, const ASN1_ITEM *it,
void *exarg)
{
X509 *ret = (X509 *)*pval;
switch (operation) {
case ASN1_OP_NEW_POST:
ret->valid = 0;
ret->name = NULL;
ret->ex_flags = 0;
ret->ex_pathlen = -1;
ret->skid = NULL;
ret->akid = NULL;
#ifndef OPENSSL_NO_RFC3779
ret->rfc3779_addr = NULL;
ret->rfc3779_asid = NULL;
#endif
ret->aux = NULL;
ret->crldp = NULL;
CRYPTO_new_ex_data(CRYPTO_EX_INDEX_X509, ret, &ret->ex_data);
break;
case ASN1_OP_D2I_POST:
if (ret->name != NULL)
OPENSSL_free(ret->name);
ret->name = X509_NAME_oneline(ret->cert_info->subject, NULL, 0);
break;
case ASN1_OP_FREE_POST:
CRYPTO_free_ex_data(CRYPTO_EX_INDEX_X509, ret, &ret->ex_data);
X509_CERT_AUX_free(ret->aux);
ASN1_OCTET_STRING_free(ret->skid);
AUTHORITY_KEYID_free(ret->akid);
CRL_DIST_POINTS_free(ret->crldp);
policy_cache_free(ret->policy_cache);
GENERAL_NAMES_free(ret->altname);
NAME_CONSTRAINTS_free(ret->nc);
#ifndef OPENSSL_NO_RFC3779
sk_IPAddressFamily_pop_free(ret->rfc3779_addr, IPAddressFamily_free);
ASIdentifiers_free(ret->rfc3779_asid);
#endif
if (ret->name != NULL)
OPENSSL_free(ret->name);
break;
}
return 1;
}
ASN1_SEQUENCE_ref(X509, x509_cb, CRYPTO_LOCK_X509) = {
ASN1_SIMPLE(X509, cert_info, X509_CINF),
ASN1_SIMPLE(X509, sig_alg, X509_ALGOR),
ASN1_SIMPLE(X509, signature, ASN1_BIT_STRING)
} ASN1_SEQUENCE_END_ref(X509, X509)
IMPLEMENT_ASN1_FUNCTIONS(X509)
IMPLEMENT_ASN1_DUP_FUNCTION(X509)
int X509_get_ex_new_index(long argl, void *argp, CRYPTO_EX_new *new_func,
CRYPTO_EX_dup *dup_func, CRYPTO_EX_free *free_func)
{
return CRYPTO_get_ex_new_index(CRYPTO_EX_INDEX_X509, argl, argp,
new_func, dup_func, free_func);
}
int X509_set_ex_data(X509 *r, int idx, void *arg)
{
return (CRYPTO_set_ex_data(&r->ex_data, idx, arg));
}
void *X509_get_ex_data(X509 *r, int idx)
{
return (CRYPTO_get_ex_data(&r->ex_data, idx));
}
X509 *d2i_X509_AUX(X509 **a, const unsigned char **pp, long length)
{
const unsigned char *q;
X509 *ret;
int freeret = 0;
q = *pp;
if (!a || *a == NULL) {
freeret = 1;
}
ret = d2i_X509(a, &q, length);
if (!ret)
return NULL;
length -= q - *pp;
if (length > 0 && !d2i_X509_CERT_AUX(&ret->aux, &q, length))
goto err;
*pp = q;
return ret;
err:
if (freeret) {
X509_free(ret);
if (a)
*a = NULL;
}
return NULL;
}
int i2d_X509_AUX(X509 *a, unsigned char **pp)
{
int length, tmplen;
unsigned char *start = pp != NULL ? *pp : NULL;
length = i2d_X509(a, pp);
if (length < 0 || a == NULL)
return length;
tmplen = i2d_X509_CERT_AUX(a->aux, pp);
if (tmplen < 0) {
if (start != NULL)
*pp = start;
return tmplen;
}
length += tmplen;
return length;
}