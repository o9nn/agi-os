#include <stdio.h>
#include <ctype.h>
#include "cryptlib.h"
#include <openssl/rand.h>
#include <openssl/x509.h>
#include <openssl/asn1.h>
#include <openssl/asn1t.h>
#include "asn1_locl.h"
typedef struct {
char *param_name;
char *param_value;
} MIME_PARAM;
DECLARE_STACK_OF(MIME_PARAM)
IMPLEMENT_STACK_OF(MIME_PARAM)
typedef struct {
char *name;
char *value;
STACK_OF(MIME_PARAM) *params;
} MIME_HEADER;
DECLARE_STACK_OF(MIME_HEADER)
IMPLEMENT_STACK_OF(MIME_HEADER)
static int asn1_output_data(BIO *out, BIO *data, ASN1_VALUE *val, int flags,
const ASN1_ITEM *it);
static char *strip_ends(char *name);
static char *strip_start(char *name);
static char *strip_end(char *name);
static MIME_HEADER *mime_hdr_new(char *name, char *value);
static int mime_hdr_addparam(MIME_HEADER *mhdr, char *name, char *value);
static STACK_OF(MIME_HEADER) *mime_parse_hdr(BIO *bio);
static int mime_hdr_cmp(const MIME_HEADER *const *a,
const MIME_HEADER *const *b);
static int mime_param_cmp(const MIME_PARAM *const *a,
const MIME_PARAM *const *b);
static void mime_param_free(MIME_PARAM *param);
static int mime_bound_check(char *line, int linelen, char *bound, int blen);
static int multi_split(BIO *bio, char *bound, STACK_OF(BIO) **ret);
static int strip_eol(char *linebuf, int *plen);
static MIME_HEADER *mime_hdr_find(STACK_OF(MIME_HEADER) *hdrs, char *name);
static MIME_PARAM *mime_param_find(MIME_HEADER *hdr, char *name);
static void mime_hdr_free(MIME_HEADER *hdr);
#define MAX_SMLEN 1024
#define mime_debug(x)
int i2d_ASN1_bio_stream(BIO *out, ASN1_VALUE *val, BIO *in, int flags,
const ASN1_ITEM *it)
{
if (flags & SMIME_STREAM) {
BIO *bio, *tbio;
bio = BIO_new_NDEF(out, val, it);
if (!bio) {
ASN1err(ASN1_F_I2D_ASN1_BIO_STREAM, ERR_R_MALLOC_FAILURE);
return 0;
}
SMIME_crlf_copy(in, bio, flags);
(void)BIO_flush(bio);
do {
tbio = BIO_pop(bio);
BIO_free(bio);
bio = tbio;
} while (bio != out);
}
else
ASN1_item_i2d_bio(it, out, val);
return 1;
}
static int B64_write_ASN1(BIO *out, ASN1_VALUE *val, BIO *in, int flags,
const ASN1_ITEM *it)
{
BIO *b64;
int r;
b64 = BIO_new(BIO_f_base64());
if (!b64) {
ASN1err(ASN1_F_B64_WRITE_ASN1, ERR_R_MALLOC_FAILURE);
return 0;
}
out = BIO_push(b64, out);
r = i2d_ASN1_bio_stream(out, val, in, flags, it);
(void)BIO_flush(out);
BIO_pop(out);
BIO_free(b64);
return r;
}
int PEM_write_bio_ASN1_stream(BIO *out, ASN1_VALUE *val, BIO *in, int flags,
const char *hdr, const ASN1_ITEM *it)
{
int r;
BIO_printf(out, "-----BEGIN %s-----\n", hdr);
r = B64_write_ASN1(out, val, in, flags, it);
BIO_printf(out, "-----END %s-----\n", hdr);
return r;
}
static ASN1_VALUE *b64_read_asn1(BIO *bio, const ASN1_ITEM *it)
{
BIO *b64;
ASN1_VALUE *val;
if (!(b64 = BIO_new(BIO_f_base64()))) {
ASN1err(ASN1_F_B64_READ_ASN1, ERR_R_MALLOC_FAILURE);
return 0;
}
bio = BIO_push(b64, bio);
val = ASN1_item_d2i_bio(it, bio, NULL);
if (!val)
ASN1err(ASN1_F_B64_READ_ASN1, ASN1_R_DECODE_ERROR);
(void)BIO_flush(bio);
bio = BIO_pop(bio);
BIO_free(b64);
return val;
}
static int asn1_write_micalg(BIO *out, STACK_OF(X509_ALGOR) *mdalgs)
{
const EVP_MD *md;
int i, have_unknown = 0, write_comma, ret = 0, md_nid;
have_unknown = 0;
write_comma = 0;
for (i = 0; i < sk_X509_ALGOR_num(mdalgs); i++) {
if (write_comma)
BIO_write(out, ",", 1);
write_comma = 1;
md_nid = OBJ_obj2nid(sk_X509_ALGOR_value(mdalgs, i)->algorithm);
md = EVP_get_digestbynid(md_nid);
if (md && md->md_ctrl) {
int rv;
char *micstr;
rv = md->md_ctrl(NULL, EVP_MD_CTRL_MICALG, 0, &micstr);
if (rv > 0) {
BIO_puts(out, micstr);
OPENSSL_free(micstr);
continue;
}
if (rv != -2)
goto err;
}
switch (md_nid) {
case NID_sha1:
BIO_puts(out, "sha1");
break;
case NID_md5:
BIO_puts(out, "md5");
break;
case NID_sha256:
BIO_puts(out, "sha-256");
break;
case NID_sha384:
BIO_puts(out, "sha-384");
break;
case NID_sha512:
BIO_puts(out, "sha-512");
break;
case NID_id_GostR3411_94:
BIO_puts(out, "gostr3411-94");
goto err;
break;
default:
if (have_unknown)
write_comma = 0;
else {
BIO_puts(out, "unknown");
have_unknown = 1;
}
break;
}
}
ret = 1;
err:
return ret;
}
int SMIME_write_ASN1(BIO *bio, ASN1_VALUE *val, BIO *data, int flags,
int ctype_nid, int econt_nid,
STACK_OF(X509_ALGOR) *mdalgs, const ASN1_ITEM *it)
{
char bound[33], c;
int i;
const char *mime_prefix, *mime_eol, *cname = "smime.p7m";
const char *msg_type = NULL;
if (flags & SMIME_OLDMIME)
mime_prefix = "application/x-pkcs7-";
else
mime_prefix = "application/pkcs7-";
if (flags & SMIME_CRLFEOL)
mime_eol = "\r\n";
else
mime_eol = "\n";
if ((flags & SMIME_DETACHED) && data) {
if (RAND_pseudo_bytes((unsigned char *)bound, 32) < 0)
return 0;
for (i = 0; i < 32; i++) {
c = bound[i] & 0xf;
if (c < 10)
c += '0';
else
c += 'A' - 10;
bound[i] = c;
}
bound[32] = 0;
BIO_printf(bio, "MIME-Version: 1.0%s", mime_eol);
BIO_printf(bio, "Content-Type: multipart/signed;");
BIO_printf(bio, " protocol=\"%ssignature\";", mime_prefix);
BIO_puts(bio, " micalg=\"");
asn1_write_micalg(bio, mdalgs);
BIO_printf(bio, "\"; boundary=\"----%s\"%s%s",
bound, mime_eol, mime_eol);
BIO_printf(bio, "This is an S/MIME signed message%s%s",
mime_eol, mime_eol);
BIO_printf(bio, "------%s%s", bound, mime_eol);
if (!asn1_output_data(bio, data, val, flags, it))
return 0;
BIO_printf(bio, "%s------%s%s", mime_eol, bound, mime_eol);
BIO_printf(bio, "Content-Type: %ssignature;", mime_prefix);
BIO_printf(bio, " name=\"smime.p7s\"%s", mime_eol);
BIO_printf(bio, "Content-Transfer-Encoding: base64%s", mime_eol);
BIO_printf(bio, "Content-Disposition: attachment;");
BIO_printf(bio, " filename=\"smime.p7s\"%s%s", mime_eol, mime_eol);
B64_write_ASN1(bio, val, NULL, 0, it);
BIO_printf(bio, "%s------%s--%s%s", mime_eol, bound,
mime_eol, mime_eol);
return 1;
}
if (ctype_nid == NID_pkcs7_enveloped)
msg_type = "enveloped-data";
else if (ctype_nid == NID_pkcs7_signed) {
if (econt_nid == NID_id_smime_ct_receipt)
msg_type = "signed-receipt";
else if (sk_X509_ALGOR_num(mdalgs) >= 0)
msg_type = "signed-data";
else
msg_type = "certs-only";
} else if (ctype_nid == NID_id_smime_ct_compressedData) {
msg_type = "compressed-data";
cname = "smime.p7z";
}
BIO_printf(bio, "MIME-Version: 1.0%s", mime_eol);
BIO_printf(bio, "Content-Disposition: attachment;");
BIO_printf(bio, " filename=\"%s\"%s", cname, mime_eol);
BIO_printf(bio, "Content-Type: %smime;", mime_prefix);
if (msg_type)
BIO_printf(bio, " smime-type=%s;", msg_type);
BIO_printf(bio, " name=\"%s\"%s", cname, mime_eol);
BIO_printf(bio, "Content-Transfer-Encoding: base64%s%s",
mime_eol, mime_eol);
if (!B64_write_ASN1(bio, val, data, flags, it))
return 0;
BIO_printf(bio, "%s", mime_eol);
return 1;
}
static int asn1_output_data(BIO *out, BIO *data, ASN1_VALUE *val, int flags,
const ASN1_ITEM *it)
{
BIO *tmpbio;
const ASN1_AUX *aux = it->funcs;
ASN1_STREAM_ARG sarg;
int rv = 1;
if (!(flags & SMIME_DETACHED) || (flags & PKCS7_REUSE_DIGEST)) {
SMIME_crlf_copy(data, out, flags);
return 1;
}
if (!aux || !aux->asn1_cb) {
ASN1err(ASN1_F_ASN1_OUTPUT_DATA, ASN1_R_STREAMING_NOT_SUPPORTED);
return 0;
}
sarg.out = out;
sarg.ndef_bio = NULL;
sarg.boundary = NULL;
if (aux->asn1_cb(ASN1_OP_DETACHED_PRE, &val, it, &sarg) <= 0)
return 0;
SMIME_crlf_copy(data, sarg.ndef_bio, flags);
if (aux->asn1_cb(ASN1_OP_DETACHED_POST, &val, it, &sarg) <= 0)
rv = 0;
while (sarg.ndef_bio != out) {
tmpbio = BIO_pop(sarg.ndef_bio);
BIO_free(sarg.ndef_bio);
sarg.ndef_bio = tmpbio;
}
return rv;
}
ASN1_VALUE *SMIME_read_ASN1(BIO *bio, BIO **bcont, const ASN1_ITEM *it)
{
BIO *asnin;
STACK_OF(MIME_HEADER) *headers = NULL;
STACK_OF(BIO) *parts = NULL;
MIME_HEADER *hdr;
MIME_PARAM *prm;
ASN1_VALUE *val;
int ret;
if (bcont)
*bcont = NULL;
if (!(headers = mime_parse_hdr(bio))) {
ASN1err(ASN1_F_SMIME_READ_ASN1, ASN1_R_MIME_PARSE_ERROR);
return NULL;
}
if (!(hdr = mime_hdr_find(headers, "content-type")) || !hdr->value) {
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
ASN1err(ASN1_F_SMIME_READ_ASN1, ASN1_R_NO_CONTENT_TYPE);
return NULL;
}
if (!strcmp(hdr->value, "multipart/signed")) {
prm = mime_param_find(hdr, "boundary");
if (!prm || !prm->param_value) {
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
ASN1err(ASN1_F_SMIME_READ_ASN1, ASN1_R_NO_MULTIPART_BOUNDARY);
return NULL;
}
ret = multi_split(bio, prm->param_value, &parts);
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
if (!ret || (sk_BIO_num(parts) != 2)) {
ASN1err(ASN1_F_SMIME_READ_ASN1, ASN1_R_NO_MULTIPART_BODY_FAILURE);
sk_BIO_pop_free(parts, BIO_vfree);
return NULL;
}
asnin = sk_BIO_value(parts, 1);
if (!(headers = mime_parse_hdr(asnin))) {
ASN1err(ASN1_F_SMIME_READ_ASN1, ASN1_R_MIME_SIG_PARSE_ERROR);
sk_BIO_pop_free(parts, BIO_vfree);
return NULL;
}
if (!(hdr = mime_hdr_find(headers, "content-type")) || !hdr->value) {
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
ASN1err(ASN1_F_SMIME_READ_ASN1, ASN1_R_NO_SIG_CONTENT_TYPE);
return NULL;
}
if (strcmp(hdr->value, "application/x-pkcs7-signature") &&
strcmp(hdr->value, "application/pkcs7-signature")) {
ASN1err(ASN1_F_SMIME_READ_ASN1, ASN1_R_SIG_INVALID_MIME_TYPE);
ERR_add_error_data(2, "type: ", hdr->value);
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
sk_BIO_pop_free(parts, BIO_vfree);
return NULL;
}
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
if (!(val = b64_read_asn1(asnin, it))) {
ASN1err(ASN1_F_SMIME_READ_ASN1, ASN1_R_ASN1_SIG_PARSE_ERROR);
sk_BIO_pop_free(parts, BIO_vfree);
return NULL;
}
if (bcont) {
*bcont = sk_BIO_value(parts, 0);
BIO_free(asnin);
sk_BIO_free(parts);
} else
sk_BIO_pop_free(parts, BIO_vfree);
return val;
}
if (strcmp(hdr->value, "application/x-pkcs7-mime") &&
strcmp(hdr->value, "application/pkcs7-mime")) {
ASN1err(ASN1_F_SMIME_READ_ASN1, ASN1_R_INVALID_MIME_TYPE);
ERR_add_error_data(2, "type: ", hdr->value);
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
return NULL;
}
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
if (!(val = b64_read_asn1(bio, it))) {
ASN1err(ASN1_F_SMIME_READ_ASN1, ASN1_R_ASN1_PARSE_ERROR);
return NULL;
}
return val;
}
int SMIME_crlf_copy(BIO *in, BIO *out, int flags)
{
BIO *bf;
char eol;
int len;
char linebuf[MAX_SMLEN];
bf = BIO_new(BIO_f_buffer());
if (!bf)
return 0;
out = BIO_push(bf, out);
if (flags & SMIME_BINARY) {
while ((len = BIO_read(in, linebuf, MAX_SMLEN)) > 0)
BIO_write(out, linebuf, len);
} else {
if (flags & SMIME_TEXT)
BIO_printf(out, "Content-Type: text/plain\r\n\r\n");
while ((len = BIO_gets(in, linebuf, MAX_SMLEN)) > 0) {
eol = strip_eol(linebuf, &len);
if (len)
BIO_write(out, linebuf, len);
if (eol)
BIO_write(out, "\r\n", 2);
}
}
(void)BIO_flush(out);
BIO_pop(out);
BIO_free(bf);
return 1;
}
int SMIME_text(BIO *in, BIO *out)
{
char iobuf[4096];
int len;
STACK_OF(MIME_HEADER) *headers;
MIME_HEADER *hdr;
if (!(headers = mime_parse_hdr(in))) {
ASN1err(ASN1_F_SMIME_TEXT, ASN1_R_MIME_PARSE_ERROR);
return 0;
}
if (!(hdr = mime_hdr_find(headers, "content-type")) || !hdr->value) {
ASN1err(ASN1_F_SMIME_TEXT, ASN1_R_MIME_NO_CONTENT_TYPE);
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
return 0;
}
if (strcmp(hdr->value, "text/plain")) {
ASN1err(ASN1_F_SMIME_TEXT, ASN1_R_INVALID_MIME_TYPE);
ERR_add_error_data(2, "type: ", hdr->value);
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
return 0;
}
sk_MIME_HEADER_pop_free(headers, mime_hdr_free);
while ((len = BIO_read(in, iobuf, sizeof(iobuf))) > 0)
BIO_write(out, iobuf, len);
if (len < 0)
return 0;
return 1;
}
static int multi_split(BIO *bio, char *bound, STACK_OF(BIO) **ret)
{
char linebuf[MAX_SMLEN];
int len, blen;
int eol = 0, next_eol = 0;
BIO *bpart = NULL;
STACK_OF(BIO) *parts;
char state, part, first;
blen = strlen(bound);
part = 0;
state = 0;
first = 1;
parts = sk_BIO_new_null();
*ret = parts;
while ((len = BIO_gets(bio, linebuf, MAX_SMLEN)) > 0) {
state = mime_bound_check(linebuf, len, bound, blen);
if (state == 1) {
first = 1;
part++;
} else if (state == 2) {
sk_BIO_push(parts, bpart);
return 1;
} else if (part) {
next_eol = strip_eol(linebuf, &len);
if (first) {
first = 0;
if (bpart)
sk_BIO_push(parts, bpart);
bpart = BIO_new(BIO_s_mem());
BIO_set_mem_eof_return(bpart, 0);
} else if (eol)
BIO_write(bpart, "\r\n", 2);
eol = next_eol;
if (len)
BIO_write(bpart, linebuf, len);
}
}
return 0;
}
#define MIME_INVALID 0
#define MIME_START 1
#define MIME_TYPE 2
#define MIME_NAME 3
#define MIME_VALUE 4
#define MIME_QUOTE 5
#define MIME_COMMENT 6
static STACK_OF(MIME_HEADER) *mime_parse_hdr(BIO *bio)
{
char *p, *q, c;
char *ntmp;
char linebuf[MAX_SMLEN];
MIME_HEADER *mhdr = NULL;
STACK_OF(MIME_HEADER) *headers;
int len, state, save_state = 0;
headers = sk_MIME_HEADER_new(mime_hdr_cmp);
if (!headers)
return NULL;
while ((len = BIO_gets(bio, linebuf, MAX_SMLEN)) > 0) {
if (mhdr && isspace((unsigned char)linebuf[0]))
state = MIME_NAME;
else
state = MIME_START;
ntmp = NULL;
for (p = linebuf, q = linebuf; (c = *p) && (c != '\r') && (c != '\n');
p++) {
switch (state) {
case MIME_START:
if (c == ':') {
state = MIME_TYPE;
*p = 0;
ntmp = strip_ends(q);
q = p + 1;
}
break;
case MIME_TYPE:
if (c == ';') {
mime_debug("Found End Value\n");
*p = 0;
mhdr = mime_hdr_new(ntmp, strip_ends(q));
sk_MIME_HEADER_push(headers, mhdr);
ntmp = NULL;
q = p + 1;
state = MIME_NAME;
} else if (c == '(') {
save_state = state;
state = MIME_COMMENT;
}
break;
case MIME_COMMENT:
if (c == ')') {
state = save_state;
}
break;
case MIME_NAME:
if (c == '=') {
state = MIME_VALUE;
*p = 0;
ntmp = strip_ends(q);
q = p + 1;
}
break;
case MIME_VALUE:
if (c == ';') {
state = MIME_NAME;
*p = 0;
mime_hdr_addparam(mhdr, ntmp, strip_ends(q));
ntmp = NULL;
q = p + 1;
} else if (c == '"') {
mime_debug("Found Quote\n");
state = MIME_QUOTE;
} else if (c == '(') {
save_state = state;
state = MIME_COMMENT;
}
break;
case MIME_QUOTE:
if (c == '"') {
mime_debug("Found Match Quote\n");
state = MIME_VALUE;
}
break;
}
}
if (state == MIME_TYPE) {
mhdr = mime_hdr_new(ntmp, strip_ends(q));
sk_MIME_HEADER_push(headers, mhdr);
} else if (state == MIME_VALUE)
mime_hdr_addparam(mhdr, ntmp, strip_ends(q));
if (p == linebuf)
break;
}
return headers;
}
static char *strip_ends(char *name)
{
return strip_end(strip_start(name));
}
static char *strip_start(char *name)
{
char *p, c;
for (p = name; (c = *p); p++) {
if (c == '"') {
if (p[1])
return p + 1;
return NULL;
}
if (!isspace((unsigned char)c))
return p;
}
return NULL;
}
static char *strip_end(char *name)
{
char *p, c;
if (!name)
return NULL;
for (p = name + strlen(name) - 1; p >= name; p--) {
c = *p;
if (c == '"') {
if (p - 1 == name)
return NULL;
*p = 0;
return name;
}
if (isspace((unsigned char)c))
*p = 0;
else
return name;
}
return NULL;
}
static MIME_HEADER *mime_hdr_new(char *name, char *value)
{
MIME_HEADER *mhdr;
char *tmpname, *tmpval, *p;
int c;
if (name) {
if (!(tmpname = BUF_strdup(name)))
return NULL;
for (p = tmpname; *p; p++) {
c = (unsigned char)*p;
if (isupper(c)) {
c = tolower(c);
*p = c;
}
}
} else
tmpname = NULL;
if (value) {
if (!(tmpval = BUF_strdup(value)))
return NULL;
for (p = tmpval; *p; p++) {
c = (unsigned char)*p;
if (isupper(c)) {
c = tolower(c);
*p = c;
}
}
} else
tmpval = NULL;
mhdr = (MIME_HEADER *)OPENSSL_malloc(sizeof(MIME_HEADER));
if (!mhdr)
return NULL;
mhdr->name = tmpname;
mhdr->value = tmpval;
if (!(mhdr->params = sk_MIME_PARAM_new(mime_param_cmp)))
return NULL;
return mhdr;
}
static int mime_hdr_addparam(MIME_HEADER *mhdr, char *name, char *value)
{
char *tmpname, *tmpval, *p;
int c;
MIME_PARAM *mparam;
if (name) {
tmpname = BUF_strdup(name);
if (!tmpname)
return 0;
for (p = tmpname; *p; p++) {
c = (unsigned char)*p;
if (isupper(c)) {
c = tolower(c);
*p = c;
}
}
} else
tmpname = NULL;
if (value) {
tmpval = BUF_strdup(value);
if (!tmpval)
return 0;
} else
tmpval = NULL;
mparam = (MIME_PARAM *)OPENSSL_malloc(sizeof(MIME_PARAM));
if (!mparam)
return 0;
mparam->param_name = tmpname;
mparam->param_value = tmpval;
sk_MIME_PARAM_push(mhdr->params, mparam);
return 1;
}
static int mime_hdr_cmp(const MIME_HEADER *const *a,
const MIME_HEADER *const *b)
{
if (!(*a)->name || !(*b)->name)
return ! !(*a)->name - ! !(*b)->name;
return (strcmp((*a)->name, (*b)->name));
}
static int mime_param_cmp(const MIME_PARAM *const *a,
const MIME_PARAM *const *b)
{
if (!(*a)->param_name || !(*b)->param_name)
return ! !(*a)->param_name - ! !(*b)->param_name;
return (strcmp((*a)->param_name, (*b)->param_name));
}
static MIME_HEADER *mime_hdr_find(STACK_OF(MIME_HEADER) *hdrs, char *name)
{
MIME_HEADER htmp;
int idx;
htmp.name = name;
idx = sk_MIME_HEADER_find(hdrs, &htmp);
if (idx < 0)
return NULL;
return sk_MIME_HEADER_value(hdrs, idx);
}
static MIME_PARAM *mime_param_find(MIME_HEADER *hdr, char *name)
{
MIME_PARAM param;
int idx;
param.param_name = name;
idx = sk_MIME_PARAM_find(hdr->params, &param);
if (idx < 0)
return NULL;
return sk_MIME_PARAM_value(hdr->params, idx);
}
static void mime_hdr_free(MIME_HEADER *hdr)
{
if (hdr->name)
OPENSSL_free(hdr->name);
if (hdr->value)
OPENSSL_free(hdr->value);
if (hdr->params)
sk_MIME_PARAM_pop_free(hdr->params, mime_param_free);
OPENSSL_free(hdr);
}
static void mime_param_free(MIME_PARAM *param)
{
if (param->param_name)
OPENSSL_free(param->param_name);
if (param->param_value)
OPENSSL_free(param->param_value);
OPENSSL_free(param);
}
static int mime_bound_check(char *line, int linelen, char *bound, int blen)
{
if (linelen == -1)
linelen = strlen(line);
if (blen == -1)
blen = strlen(bound);
if (blen + 2 > linelen)
return 0;
if (!strncmp(line, "--", 2) && !strncmp(line + 2, bound, blen)) {
if (!strncmp(line + blen + 2, "--", 2))
return 2;
else
return 1;
}
return 0;
}
static int strip_eol(char *linebuf, int *plen)
{
int len = *plen;
char *p, c;
int is_eol = 0;
p = linebuf + len - 1;
for (p = linebuf + len - 1; len > 0; len--, p--) {
c = *p;
if (c == '\n')
is_eol = 1;
else if (c != '\r')
break;
}
*plen = len;
return is_eol;
}