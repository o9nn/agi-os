#include <stdio.h>
#include "cryptlib.h"
#include <openssl/objects.h>
#include <openssl/ts.h>
#include <openssl/pkcs7.h>
static int TS_verify_cert(X509_STORE *store, STACK_OF(X509) *untrusted,
X509 *signer, STACK_OF(X509) **chain);
static int TS_check_signing_certs(PKCS7_SIGNER_INFO *si,
STACK_OF(X509) *chain);
static ESS_SIGNING_CERT *ESS_get_signing_cert(PKCS7_SIGNER_INFO *si);
static int TS_find_cert(STACK_OF(ESS_CERT_ID) *cert_ids, X509 *cert);
static int TS_issuer_serial_cmp(ESS_ISSUER_SERIAL *is, X509_CINF *cinfo);
static int int_TS_RESP_verify_token(TS_VERIFY_CTX *ctx,
PKCS7 *token, TS_TST_INFO *tst_info);
static int TS_check_status_info(TS_RESP *response);
static char *TS_get_status_text(STACK_OF(ASN1_UTF8STRING) *text);
static int TS_check_policy(ASN1_OBJECT *req_oid, TS_TST_INFO *tst_info);
static int TS_compute_imprint(BIO *data, TS_TST_INFO *tst_info,
X509_ALGOR **md_alg,
unsigned char **imprint, unsigned *imprint_len);
static int TS_check_imprints(X509_ALGOR *algor_a,
unsigned char *imprint_a, unsigned len_a,
TS_TST_INFO *tst_info);
static int TS_check_nonces(const ASN1_INTEGER *a, TS_TST_INFO *tst_info);
static int TS_check_signer_name(GENERAL_NAME *tsa_name, X509 *signer);
static int TS_find_name(STACK_OF(GENERAL_NAME) *gen_names,
GENERAL_NAME *name);
static const char *TS_status_text[] = { "granted",
"grantedWithMods",
"rejection",
"waiting",
"revocationWarning",
"revocationNotification"
};
#define TS_STATUS_TEXT_SIZE (sizeof(TS_status_text)/sizeof(*TS_status_text))
#define TS_STATUS_BUF_SIZE 256
static struct {
int code;
const char *text;
} TS_failure_info[] = {
{
TS_INFO_BAD_ALG, "badAlg"
},
{
TS_INFO_BAD_REQUEST, "badRequest"
},
{
TS_INFO_BAD_DATA_FORMAT, "badDataFormat"
},
{
TS_INFO_TIME_NOT_AVAILABLE, "timeNotAvailable"
},
{
TS_INFO_UNACCEPTED_POLICY, "unacceptedPolicy"
},
{
TS_INFO_UNACCEPTED_EXTENSION, "unacceptedExtension"
},
{
TS_INFO_ADD_INFO_NOT_AVAILABLE, "addInfoNotAvailable"
},
{
TS_INFO_SYSTEM_FAILURE, "systemFailure"
}
};
#define TS_FAILURE_INFO_SIZE (sizeof(TS_failure_info) / \
sizeof(*TS_failure_info))
int TS_RESP_verify_signature(PKCS7 *token, STACK_OF(X509) *certs,
X509_STORE *store, X509 **signer_out)
{
STACK_OF(PKCS7_SIGNER_INFO) *sinfos = NULL;
PKCS7_SIGNER_INFO *si;
STACK_OF(X509) *signers = NULL;
X509 *signer;
STACK_OF(X509) *chain = NULL;
char buf[4096];
int i, j = 0, ret = 0;
BIO *p7bio = NULL;
if (!token) {
TSerr(TS_F_TS_RESP_VERIFY_SIGNATURE, TS_R_INVALID_NULL_POINTER);
goto err;
}
if (!PKCS7_type_is_signed(token)) {
TSerr(TS_F_TS_RESP_VERIFY_SIGNATURE, TS_R_WRONG_CONTENT_TYPE);
goto err;
}
sinfos = PKCS7_get_signer_info(token);
if (!sinfos || sk_PKCS7_SIGNER_INFO_num(sinfos) != 1) {
TSerr(TS_F_TS_RESP_VERIFY_SIGNATURE, TS_R_THERE_MUST_BE_ONE_SIGNER);
goto err;
}
si = sk_PKCS7_SIGNER_INFO_value(sinfos, 0);
if (PKCS7_get_detached(token)) {
TSerr(TS_F_TS_RESP_VERIFY_SIGNATURE, TS_R_NO_CONTENT);
goto err;
}
signers = PKCS7_get0_signers(token, certs, 0);
if (!signers || sk_X509_num(signers) != 1)
goto err;
signer = sk_X509_value(signers, 0);
if (!TS_verify_cert(store, certs, signer, &chain))
goto err;
if (!TS_check_signing_certs(si, chain))
goto err;
p7bio = PKCS7_dataInit(token, NULL);
while ((i = BIO_read(p7bio, buf, sizeof(buf))) > 0) ;
j = PKCS7_signatureVerify(p7bio, token, si, signer);
if (j <= 0) {
TSerr(TS_F_TS_RESP_VERIFY_SIGNATURE, TS_R_SIGNATURE_FAILURE);
goto err;
}
if (signer_out) {
*signer_out = signer;
CRYPTO_add(&signer->references, 1, CRYPTO_LOCK_X509);
}
ret = 1;
err:
BIO_free_all(p7bio);
sk_X509_pop_free(chain, X509_free);
sk_X509_free(signers);
return ret;
}
static int TS_verify_cert(X509_STORE *store, STACK_OF(X509) *untrusted,
X509 *signer, STACK_OF(X509) **chain)
{
X509_STORE_CTX cert_ctx;
int i;
int ret = 1;
*chain = NULL;
if (!X509_STORE_CTX_init(&cert_ctx, store, signer, untrusted))
return 0;
X509_STORE_CTX_set_purpose(&cert_ctx, X509_PURPOSE_TIMESTAMP_SIGN);
i = X509_verify_cert(&cert_ctx);
if (i <= 0) {
int j = X509_STORE_CTX_get_error(&cert_ctx);
TSerr(TS_F_TS_VERIFY_CERT, TS_R_CERTIFICATE_VERIFY_ERROR);
ERR_add_error_data(2, "Verify error:",
X509_verify_cert_error_string(j));
ret = 0;
} else {
*chain = X509_STORE_CTX_get1_chain(&cert_ctx);
}
X509_STORE_CTX_cleanup(&cert_ctx);
return ret;
}
static int TS_check_signing_certs(PKCS7_SIGNER_INFO *si,
STACK_OF(X509) *chain)
{
ESS_SIGNING_CERT *ss = ESS_get_signing_cert(si);
STACK_OF(ESS_CERT_ID) *cert_ids = NULL;
X509 *cert;
int i = 0;
int ret = 0;
if (!ss)
goto err;
cert_ids = ss->cert_ids;
cert = sk_X509_value(chain, 0);
if (TS_find_cert(cert_ids, cert) != 0)
goto err;
if (sk_ESS_CERT_ID_num(cert_ids) > 1) {
for (i = 1; i < sk_X509_num(chain); ++i) {
cert = sk_X509_value(chain, i);
if (TS_find_cert(cert_ids, cert) < 0)
goto err;
}
}
ret = 1;
err:
if (!ret)
TSerr(TS_F_TS_CHECK_SIGNING_CERTS,
TS_R_ESS_SIGNING_CERTIFICATE_ERROR);
ESS_SIGNING_CERT_free(ss);
return ret;
}
static ESS_SIGNING_CERT *ESS_get_signing_cert(PKCS7_SIGNER_INFO *si)
{
ASN1_TYPE *attr;
const unsigned char *p;
attr = PKCS7_get_signed_attribute(si, NID_id_smime_aa_signingCertificate);
if (!attr)
return NULL;
p = attr->value.sequence->data;
return d2i_ESS_SIGNING_CERT(NULL, &p, attr->value.sequence->length);
}
static int TS_find_cert(STACK_OF(ESS_CERT_ID) *cert_ids, X509 *cert)
{
int i;
if (!cert_ids || !cert)
return -1;
X509_check_purpose(cert, -1, 0);
for (i = 0; i < sk_ESS_CERT_ID_num(cert_ids); ++i) {
ESS_CERT_ID *cid = sk_ESS_CERT_ID_value(cert_ids, i);
if (cid->hash->length == sizeof(cert->sha1_hash)
&& !memcmp(cid->hash->data, cert->sha1_hash,
sizeof(cert->sha1_hash))) {
ESS_ISSUER_SERIAL *is = cid->issuer_serial;
if (!is || !TS_issuer_serial_cmp(is, cert->cert_info))
return i;
}
}
return -1;
}
static int TS_issuer_serial_cmp(ESS_ISSUER_SERIAL *is, X509_CINF *cinfo)
{
GENERAL_NAME *issuer;
if (!is || !cinfo || sk_GENERAL_NAME_num(is->issuer) != 1)
return -1;
issuer = sk_GENERAL_NAME_value(is->issuer, 0);
if (issuer->type != GEN_DIRNAME
|| X509_NAME_cmp(issuer->d.dirn, cinfo->issuer))
return -1;
if (ASN1_INTEGER_cmp(is->serial, cinfo->serialNumber))
return -1;
return 0;
}
int TS_RESP_verify_response(TS_VERIFY_CTX *ctx, TS_RESP *response)
{
PKCS7 *token = TS_RESP_get_token(response);
TS_TST_INFO *tst_info = TS_RESP_get_tst_info(response);
int ret = 0;
if (!TS_check_status_info(response))
goto err;
if (!int_TS_RESP_verify_token(ctx, token, tst_info))
goto err;
ret = 1;
err:
return ret;
}
int TS_RESP_verify_token(TS_VERIFY_CTX *ctx, PKCS7 *token)
{
TS_TST_INFO *tst_info = PKCS7_to_TS_TST_INFO(token);
int ret = 0;
if (tst_info) {
ret = int_TS_RESP_verify_token(ctx, token, tst_info);
TS_TST_INFO_free(tst_info);
}
return ret;
}
static int int_TS_RESP_verify_token(TS_VERIFY_CTX *ctx,
PKCS7 *token, TS_TST_INFO *tst_info)
{
X509 *signer = NULL;
GENERAL_NAME *tsa_name = TS_TST_INFO_get_tsa(tst_info);
X509_ALGOR *md_alg = NULL;
unsigned char *imprint = NULL;
unsigned imprint_len = 0;
int ret = 0;
if ((ctx->flags & TS_VFY_SIGNATURE)
&& !TS_RESP_verify_signature(token, ctx->certs, ctx->store, &signer))
goto err;
if ((ctx->flags & TS_VFY_VERSION)
&& TS_TST_INFO_get_version(tst_info) != 1) {
TSerr(TS_F_INT_TS_RESP_VERIFY_TOKEN, TS_R_UNSUPPORTED_VERSION);
goto err;
}
if ((ctx->flags & TS_VFY_POLICY)
&& !TS_check_policy(ctx->policy, tst_info))
goto err;
if ((ctx->flags & TS_VFY_IMPRINT)
&& !TS_check_imprints(ctx->md_alg, ctx->imprint, ctx->imprint_len,
tst_info))
goto err;
if ((ctx->flags & TS_VFY_DATA)
&& (!TS_compute_imprint(ctx->data, tst_info,
&md_alg, &imprint, &imprint_len)
|| !TS_check_imprints(md_alg, imprint, imprint_len, tst_info)))
goto err;
if ((ctx->flags & TS_VFY_NONCE)
&& !TS_check_nonces(ctx->nonce, tst_info))
goto err;
if ((ctx->flags & TS_VFY_SIGNER)
&& tsa_name && !TS_check_signer_name(tsa_name, signer)) {
TSerr(TS_F_INT_TS_RESP_VERIFY_TOKEN, TS_R_TSA_NAME_MISMATCH);
goto err;
}
if ((ctx->flags & TS_VFY_TSA_NAME)
&& !TS_check_signer_name(ctx->tsa_name, signer)) {
TSerr(TS_F_INT_TS_RESP_VERIFY_TOKEN, TS_R_TSA_UNTRUSTED);
goto err;
}
ret = 1;
err:
X509_free(signer);
X509_ALGOR_free(md_alg);
OPENSSL_free(imprint);
return ret;
}
static int TS_check_status_info(TS_RESP *response)
{
TS_STATUS_INFO *info = TS_RESP_get_status_info(response);
long status = ASN1_INTEGER_get(info->status);
const char *status_text = NULL;
char *embedded_status_text = NULL;
char failure_text[TS_STATUS_BUF_SIZE] = "";
if (status == 0 || status == 1)
return 1;
if (0 <= status && status < (long)TS_STATUS_TEXT_SIZE)
status_text = TS_status_text[status];
else
status_text = "unknown code";
if (sk_ASN1_UTF8STRING_num(info->text) > 0
&& !(embedded_status_text = TS_get_status_text(info->text)))
return 0;
if (info->failure_info) {
int i;
int first = 1;
for (i = 0; i < (int)TS_FAILURE_INFO_SIZE; ++i) {
if (ASN1_BIT_STRING_get_bit(info->failure_info,
TS_failure_info[i].code)) {
if (!first)
strcat(failure_text, ",");
else
first = 0;
strcat(failure_text, TS_failure_info[i].text);
}
}
}
if (failure_text[0] == '\0')
strcpy(failure_text, "unspecified");
TSerr(TS_F_TS_CHECK_STATUS_INFO, TS_R_NO_TIME_STAMP_TOKEN);
ERR_add_error_data(6,
"status code: ", status_text,
", status text: ", embedded_status_text ?
embedded_status_text : "unspecified",
", failure codes: ", failure_text);
OPENSSL_free(embedded_status_text);
return 0;
}
static char *TS_get_status_text(STACK_OF(ASN1_UTF8STRING) *text)
{
int i;
unsigned int length = 0;
char *result = NULL;
char *p;
for (i = 0; i < sk_ASN1_UTF8STRING_num(text); ++i) {
ASN1_UTF8STRING *current = sk_ASN1_UTF8STRING_value(text, i);
length += ASN1_STRING_length(current);
length += 1;
}
if (!(result = OPENSSL_malloc(length))) {
TSerr(TS_F_TS_GET_STATUS_TEXT, ERR_R_MALLOC_FAILURE);
return NULL;
}
for (i = 0, p = result; i < sk_ASN1_UTF8STRING_num(text); ++i) {
ASN1_UTF8STRING *current = sk_ASN1_UTF8STRING_value(text, i);
length = ASN1_STRING_length(current);
if (i > 0)
*p++ = '/';
strncpy(p, (const char *)ASN1_STRING_data(current), length);
p += length;
}
*p = '\0';
return result;
}
static int TS_check_policy(ASN1_OBJECT *req_oid, TS_TST_INFO *tst_info)
{
ASN1_OBJECT *resp_oid = TS_TST_INFO_get_policy_id(tst_info);
if (OBJ_cmp(req_oid, resp_oid) != 0) {
TSerr(TS_F_TS_CHECK_POLICY, TS_R_POLICY_MISMATCH);
return 0;
}
return 1;
}
static int TS_compute_imprint(BIO *data, TS_TST_INFO *tst_info,
X509_ALGOR **md_alg,
unsigned char **imprint, unsigned *imprint_len)
{
TS_MSG_IMPRINT *msg_imprint = TS_TST_INFO_get_msg_imprint(tst_info);
X509_ALGOR *md_alg_resp = TS_MSG_IMPRINT_get_algo(msg_imprint);
const EVP_MD *md;
EVP_MD_CTX md_ctx;
unsigned char buffer[4096];
int length;
*md_alg = NULL;
*imprint = NULL;
if (!(*md_alg = X509_ALGOR_dup(md_alg_resp)))
goto err;
if (!(md = EVP_get_digestbyobj((*md_alg)->algorithm))) {
TSerr(TS_F_TS_COMPUTE_IMPRINT, TS_R_UNSUPPORTED_MD_ALGORITHM);
goto err;
}
length = EVP_MD_size(md);
if (length < 0)
goto err;
*imprint_len = length;
if (!(*imprint = OPENSSL_malloc(*imprint_len))) {
TSerr(TS_F_TS_COMPUTE_IMPRINT, ERR_R_MALLOC_FAILURE);
goto err;
}
if (!EVP_DigestInit(&md_ctx, md))
goto err;
while ((length = BIO_read(data, buffer, sizeof(buffer))) > 0) {
if (!EVP_DigestUpdate(&md_ctx, buffer, length))
goto err;
}
if (!EVP_DigestFinal(&md_ctx, *imprint, NULL))
goto err;
return 1;
err:
X509_ALGOR_free(*md_alg);
OPENSSL_free(*imprint);
*imprint_len = 0;
*imprint = NULL;
return 0;
}
static int TS_check_imprints(X509_ALGOR *algor_a,
unsigned char *imprint_a, unsigned len_a,
TS_TST_INFO *tst_info)
{
TS_MSG_IMPRINT *b = TS_TST_INFO_get_msg_imprint(tst_info);
X509_ALGOR *algor_b = TS_MSG_IMPRINT_get_algo(b);
int ret = 0;
if (algor_a) {
if (OBJ_cmp(algor_a->algorithm, algor_b->algorithm))
goto err;
if ((algor_a->parameter
&& ASN1_TYPE_get(algor_a->parameter) != V_ASN1_NULL)
|| (algor_b->parameter
&& ASN1_TYPE_get(algor_b->parameter) != V_ASN1_NULL))
goto err;
}
ret = len_a == (unsigned)ASN1_STRING_length(b->hashed_msg) &&
memcmp(imprint_a, ASN1_STRING_data(b->hashed_msg), len_a) == 0;
err:
if (!ret)
TSerr(TS_F_TS_CHECK_IMPRINTS, TS_R_MESSAGE_IMPRINT_MISMATCH);
return ret;
}
static int TS_check_nonces(const ASN1_INTEGER *a, TS_TST_INFO *tst_info)
{
const ASN1_INTEGER *b = TS_TST_INFO_get_nonce(tst_info);
if (!b) {
TSerr(TS_F_TS_CHECK_NONCES, TS_R_NONCE_NOT_RETURNED);
return 0;
}
if (ASN1_INTEGER_cmp(a, b) != 0) {
TSerr(TS_F_TS_CHECK_NONCES, TS_R_NONCE_MISMATCH);
return 0;
}
return 1;
}
static int TS_check_signer_name(GENERAL_NAME *tsa_name, X509 *signer)
{
STACK_OF(GENERAL_NAME) *gen_names = NULL;
int idx = -1;
int found = 0;
if (tsa_name->type == GEN_DIRNAME
&& X509_name_cmp(tsa_name->d.dirn, signer->cert_info->subject) == 0)
return 1;
gen_names = X509_get_ext_d2i(signer, NID_subject_alt_name, NULL, &idx);
while (gen_names != NULL
&& !(found = TS_find_name(gen_names, tsa_name) >= 0)) {
GENERAL_NAMES_free(gen_names);
gen_names = X509_get_ext_d2i(signer, NID_subject_alt_name,
NULL, &idx);
}
if (gen_names)
GENERAL_NAMES_free(gen_names);
return found;
}
static int TS_find_name(STACK_OF(GENERAL_NAME) *gen_names, GENERAL_NAME *name)
{
int i, found;
for (i = 0, found = 0; !found && i < sk_GENERAL_NAME_num(gen_names); ++i) {
GENERAL_NAME *current = sk_GENERAL_NAME_value(gen_names, i);
found = GENERAL_NAME_cmp(current, name) == 0;
}
return found ? i - 1 : -1;
}