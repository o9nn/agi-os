#include <config.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <openssl/opensslv.h>
#if (OPENSSL_VERSION_NUMBER < 0x0090700f)
#error OpenSSL 0.9.7 or later is required
#endif
#include <openssl/bn.h>
#include <openssl/dh.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <openssl/md5.h>
#include <openssl/sha.h>
#include <openssl/dsa.h>
#include <sasl.h>
#define MD5_H
#include <saslplug.h>
#include "plugin_common.h"
#ifdef macintosh
#include <sasl_passdss_plugin_decl.h>
#endif
static const char plugin_id[] = "$Id: passdss.c,v 1.5 2008/10/29 17:59:41 murch Exp $";
const char g[] = "2";
const char N[] = "FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E088A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381FFFFFFFFFFFFFFFF";
#define NO_LAYER_FLAG (1<<0)
#define INTEGRITY_LAYER_FLAG (1<<1)
#define PRIVACY_LAYER_FLAG (1<<2)
#define NO_LAYER_SSF 0
#define INTEGRITY_LAYER_SSF 1
#define PRIVACY_LAYER_SSF 112
typedef struct context {
int state;
char *authid;
char *userid;
sasl_secret_t *password;
unsigned int free_password;
DH *dh;
const sasl_utils_t *utils;
char *out_buf;
unsigned out_buf_len;
unsigned char secmask;
unsigned char padding[EVP_MAX_BLOCK_LENGTH];
HMAC_CTX hmac_send_ctx;
HMAC_CTX hmac_recv_ctx;
unsigned char send_integrity_key[4 + EVP_MAX_MD_SIZE];
unsigned char recv_integrity_key[4 + EVP_MAX_MD_SIZE];
unsigned char *cs_integrity_key;
unsigned char *sc_integrity_key;
EVP_CIPHER_CTX cipher_enc_ctx;
EVP_CIPHER_CTX cipher_dec_ctx;
unsigned blk_siz;
unsigned char cs_encryption_iv[EVP_MAX_MD_SIZE];
unsigned char sc_encryption_iv[EVP_MAX_MD_SIZE];
unsigned char cs_encryption_key[2 * EVP_MAX_MD_SIZE];
unsigned char sc_encryption_key[2 * EVP_MAX_MD_SIZE];
uint32_t pktnum_out;
uint32_t pktnum_in;
char *encode_buf, *decode_buf, *decode_pkt_buf;
unsigned encode_buf_len, decode_buf_len, decode_pkt_buf_len;
decode_context_t decode_context;
} context_t;
static int passdss_encode(void *context,
const struct iovec *invec,
unsigned numiov,
const char **output,
unsigned *outputlen)
{
context_t *text = (context_t *) context;
unsigned long inputlen;
unsigned char hmac[EVP_MAX_MD_SIZE];
unsigned i, hmaclen;
uint32_t tmpnum;
int ret;
if (!context || !invec || !numiov || !output || !outputlen) {
PARAMERROR( text->utils );
return SASL_BADPARAM;
}
for (i = 0, inputlen = 0; i < numiov; i++)
inputlen += invec[i].iov_len;
ret = _plug_buf_alloc(text->utils, &text->encode_buf,
&text->encode_buf_len,
4 +
inputlen +
EVP_MAX_MD_SIZE +
EVP_MAX_BLOCK_LENGTH - 1);
if (ret != SASL_OK) return ret;
*outputlen = 4;
tmpnum = htonl(text->pktnum_out++);
memcpy(text->send_integrity_key, &tmpnum, 4);
HMAC_Init_ex(&text->hmac_send_ctx, text->send_integrity_key,
4+SHA_DIGEST_LENGTH, EVP_sha1(), NULL);
for (i = 0; i < numiov; i++) {
HMAC_Update(&text->hmac_send_ctx, invec[i].iov_base, invec[i].iov_len);
if (text->secmask & PRIVACY_LAYER_FLAG) {
unsigned enclen;
EVP_EncryptUpdate(&text->cipher_enc_ctx,
text->encode_buf + *outputlen, &enclen,
invec[i].iov_base, invec[i].iov_len);
*outputlen += enclen;
}
else {
memcpy(text->encode_buf + *outputlen, invec[i].iov_base,
invec[i].iov_len);
*outputlen += invec[i].iov_len;
}
}
HMAC_Final(&text->hmac_send_ctx, hmac, &hmaclen);
if (text->secmask & PRIVACY_LAYER_FLAG) {
unsigned enclen;
unsigned char padlen;
EVP_EncryptUpdate(&text->cipher_enc_ctx,
text->encode_buf + *outputlen, &enclen,
hmac, hmaclen);
*outputlen += enclen;
padlen = text->blk_siz - ((inputlen + hmaclen) % text->blk_siz) - 1;
EVP_EncryptUpdate(&text->cipher_enc_ctx,
text->encode_buf + *outputlen, &enclen,
text->padding, padlen);
*outputlen += enclen;
EVP_EncryptUpdate(&text->cipher_enc_ctx,
text->encode_buf + *outputlen, &enclen,
&padlen, 1);
*outputlen += enclen;
EVP_EncryptFinal_ex(&text->cipher_enc_ctx,
text->encode_buf + *outputlen, &enclen);
*outputlen += enclen;
}
else {
memcpy(text->encode_buf + *outputlen, hmac, hmaclen);
*outputlen += hmaclen;
}
tmpnum = *outputlen - 4;
tmpnum = htonl(tmpnum);
memcpy(text->encode_buf, &tmpnum, 4);
*output = text->encode_buf;
return SASL_OK;
}
static int passdss_decode_packet(void *context,
const char *input,
unsigned inputlen,
char **output,
unsigned *outputlen)
{
context_t *text = (context_t *) context;
uint32_t tmpnum;
unsigned char hmac[EVP_MAX_MD_SIZE];
unsigned hmaclen;
int ret;
if (text->secmask & PRIVACY_LAYER_FLAG) {
unsigned declen, padlen;
ret = _plug_buf_alloc(text->utils, &(text->decode_pkt_buf),
&(text->decode_pkt_buf_len), inputlen);
if (ret != SASL_OK) return ret;
ret = EVP_DecryptUpdate(&text->cipher_dec_ctx,
text->decode_pkt_buf, &declen,
(char *) input, inputlen);
if (ret)
EVP_DecryptFinal_ex(&text->cipher_dec_ctx,
text->decode_pkt_buf + declen, &declen);
if (!ret) {
SETERROR(text->utils, "Error decrypting input");
return SASL_BADPROT;
}
input = text->decode_pkt_buf;
padlen = text->decode_pkt_buf[inputlen - 1] + 1;
inputlen -= padlen;
}
inputlen -= SHA_DIGEST_LENGTH;
tmpnum = htonl(text->pktnum_in++);
memcpy(text->recv_integrity_key, &tmpnum, 4);
HMAC(EVP_sha1(), text->recv_integrity_key, 4+SHA_DIGEST_LENGTH,
input, inputlen, hmac, &hmaclen);
if (memcmp(hmac, input+inputlen, hmaclen)) {
SETERROR(text->utils, "HMAC is incorrect\n");
return SASL_BADMAC;
}
*output = (char *) input;
*outputlen = inputlen;
return SASL_OK;
}
static int passdss_decode(void *context,
const char *input, unsigned inputlen,
const char **output, unsigned *outputlen)
{
context_t *text = (context_t *) context;
int ret;
ret = _plug_decode(&text->decode_context, input, inputlen,
&text->decode_buf, &text->decode_buf_len, outputlen,
passdss_decode_packet, text);
*output = text->decode_buf;
return ret;
}
#define MAX_MPI_LEN 2147483643
#define MAX_UTF8_LEN 2147483643
static int MakeBuffer(const sasl_utils_t *utils, char **buf, unsigned offset,
unsigned *buflen, unsigned *outlen, const char *fmt, ...)
{
va_list ap;
char *p, *out = NULL, *lptr = NULL;
int r, alloclen, len = -1, argc = 0;
BIGNUM *mpi;
char *os, *str;
uint32_t u, nl;
va_start(ap, fmt);
for (p = (char *) fmt, alloclen = offset; *p; p++) {
if (*p != '%') {
alloclen++;
continue;
}
if (*++p == '*') {
len = va_arg(ap, int);
p++;
}
else if (isdigit((int) *p)) {
len = 0;
while (isdigit((int) *p)) len = 10 * len + *p++ - '0';
}
switch (*p) {
case 'a':
alloclen += 4;
break;
case 'm':
mpi = va_arg(ap, BIGNUM *);
len = BN_num_bytes(mpi);
if (len > MAX_MPI_LEN) {
utils->log(NULL, SASL_LOG_ERR,
"String too long to create mpi string\n");
r = SASL_FAIL;
goto done;
}
alloclen += len + 4;
break;
case 'o':
alloclen += len;
os = va_arg(ap, char *);
break;
case 's':
str = va_arg(ap, char *);
if (len == -1) len = strlen(str);
if (len > MAX_UTF8_LEN) {
utils->log(NULL, SASL_LOG_ERR,
"String too long to create utf8 string\n");
r = SASL_FAIL;
goto done;
}
alloclen += len + 4;
break;
case 'u':
u = va_arg(ap, uint32_t);
if (len == -1) len = 4;
alloclen += len;
break;
default:
alloclen++;
break;
}
len = -1;
}
va_end(ap);
r = _plug_buf_alloc(utils, buf, buflen, alloclen);
if (r != SASL_OK) return r;
out = *buf + offset;
va_start(ap, fmt);
for (p = (char *) fmt; *p; p++) {
if (*p != '%') {
*out = *p;
out++;
continue;
}
if (*++p == '*') {
len = va_arg(ap, int);
p++;
}
else if (isdigit((int) *p)) {
len = 0;
while (isdigit((int) *p)) len = 10 * len + *p++ - '0';
}
switch (*p) {
case 'a':
argc = len;
len = -1;
lptr = out;
out += 4;
continue;
break;
case 'm':
mpi = va_arg(ap, BIGNUM *);
len = BN_bn2bin(mpi, out+4);
nl = htonl(len);
memcpy(out, &nl, 4);
out += len + 4;
break;
case 'o':
os = va_arg(ap, char *);
memcpy(out, os, len);
out += len;
break;
case 's':
str = va_arg(ap, char *);
if (len == -1) len = strlen(str);
nl = htonl(len);
memcpy(out, &nl, 4);
memcpy(out+4, str, len);
out += len + 4;
break;
case 'u':
u = va_arg(ap, uint32_t);
nl = htonl(u);
if (len == -1) len = 4;
memcpy(out, &nl + 4 - len, len);
out += len;
break;
default:
*out = *p;
out++;
break;
}
if (lptr && !--argc) {
len = out - lptr - 4;
nl = htonl(len);
memcpy(lptr, &nl, 4);
lptr = NULL;
}
len = -1;
}
done:
va_end(ap);
*outlen = out - *buf;
return r;
}
static int UnBuffer(const sasl_utils_t *utils, const char *buf,
unsigned buflen, const char *fmt, ...)
{
va_list ap;
char *p;
BIGNUM **mpi;
char **os, **str;
uint32_t *u, nl;
unsigned len;
enum { OCTET_REFERENCE,
OCTET_COPY,
OCTET_ALLOC
} octet_flag;
int r = SASL_OK;
va_start(ap, fmt);
for (p = (char *) fmt; *p; p++) {
if (*p != '%') {
if (*buf != *p) {
r = SASL_BADPROT;
goto done;
}
buf++;
buflen--;
continue;
}
p++;
octet_flag = OCTET_COPY;
if (*p == '-') {
octet_flag = OCTET_REFERENCE;
p++;
}
else if (*p == '+') {
octet_flag = OCTET_ALLOC;
p++;
}
len = 0;
if (*p == '*') {
len = va_arg(ap, int);
p++;
}
else if (isdigit((int) *p)) {
len = 0;
while (isdigit((int) *p)) len = 10 * len + *p++ - '0';
}
switch (*p) {
case 'm':
mpi = va_arg(ap, BIGNUM **);
if (buflen < 4) {
SETERROR(utils, "Buffer is not big enough to be PASSDSS MPI\n");
r = SASL_BADPROT;
goto done;
}
memcpy(&nl, buf, 4);
len = ntohl(nl);
buf += 4;
buflen -= 4;
if (len > buflen) {
SETERROR(utils, "Not enough data for this PASSDSS MPI\n");
r = SASL_BADPROT;
goto done;
}
if (mpi) {
if (!*mpi) *mpi = BN_new();
BN_init(*mpi);
BN_bin2bn(buf, len, *mpi);
}
break;
case 'o':
os = va_arg(ap, char **);
if (len > buflen) {
SETERROR(utils, "Not enough data for this PASSDSS os\n");
r = SASL_BADPROT;
goto done;
}
if (os) {
if (octet_flag == OCTET_REFERENCE)
*os = (char *) buf;
else {
if (octet_flag == OCTET_ALLOC &&
(*os = (char *) utils->malloc(len)) == NULL) {
r = SASL_NOMEM;
goto done;
}
memcpy(*os, buf, len);
}
}
break;
case 'p':
if (buflen < len) len = buflen;
break;
case 's':
str = va_arg(ap, char **);
if (str) *str = NULL;
if (buflen < 4) {
SETERROR(utils, "Buffer is not big enough to be PASSDSS string\n");
r = SASL_BADPROT;
goto done;
}
memcpy(&nl, buf, 4);
len = ntohl(nl);
buf += 4;
buflen -= 4;
if (len > buflen) {
SETERROR(utils, "Not enough data for this PASSDSS string\n");
r = SASL_BADPROT;
goto done;
}
if (str) {
*str = (char *) utils->malloc(len+1);
if (!*str) {
r = SASL_NOMEM;
goto done;
}
memcpy(*str, buf, len);
(*str)[len] = '\0';
}
break;
case 'u':
u = va_arg(ap, uint32_t*);
if (!len) len = 4;
if (buflen < len) {
SETERROR(utils, "Buffer is not big enough to be PASSDSS uint32\n");
r = SASL_BADPROT;
goto done;
}
if (u) {
memset(u, 0, 4);
memcpy(u + 4 - len, buf, len);
*u = ntohl(*u);
}
break;
default:
len = 1;
if (*buf != *p) {
r = SASL_BADPROT;
goto done;
}
break;
}
buf += len;
buflen -= len;
}
if (buflen != 0) {
SETERROR(utils, "Extra data in PASSDSS buffer\n");
r = SASL_BADPROT;
}
done:
va_end(ap);
return r;
}
#define DOHASH(out, in1, len1, in2, len2, in3, len3) \
EVP_DigestInit(&mdctx, EVP_sha1()); \
EVP_DigestUpdate(&mdctx, in1, len1); \
EVP_DigestUpdate(&mdctx, in2, len2); \
EVP_DigestUpdate(&mdctx, in3, len3); \
EVP_DigestFinal(&mdctx, out, NULL)
void CalcLayerParams(context_t *text, char *K, unsigned Klen,
char *hash, unsigned hashlen)
{
EVP_MD_CTX mdctx;
DOHASH(text->cs_encryption_iv, K, Klen, "A", 1, hash, hashlen);
DOHASH(text->sc_encryption_iv, K, Klen, "B", 1, hash, hashlen);
DOHASH(text->cs_encryption_key, K, Klen, "C", 1, hash, hashlen);
DOHASH(text->cs_encryption_key + hashlen, K, Klen, "", 0,
text->cs_encryption_key, hashlen);
DOHASH(text->sc_encryption_key, K, Klen, "D", 1, hash, hashlen);
DOHASH(text->sc_encryption_key + hashlen, K, Klen, "", 0,
text->sc_encryption_key, hashlen);
DOHASH(text->cs_integrity_key, K, Klen, "E", 1, hash, hashlen);
DOHASH(text->sc_integrity_key, K, Klen, "F", 1, hash, hashlen);
}
static void passdss_common_mech_dispose(void *conn_context,
const sasl_utils_t *utils)
{
context_t *text = (context_t *) conn_context;
if (!text) return;
if (text->authid) utils->free(text->authid);
if (text->userid) utils->free(text->userid);
if (text->free_password) _plug_free_secret(utils, &(text->password));
if (text->dh) DH_free(text->dh);
HMAC_CTX_cleanup(&text->hmac_send_ctx);
HMAC_CTX_cleanup(&text->hmac_recv_ctx);
EVP_CIPHER_CTX_cleanup(&text->cipher_enc_ctx);
EVP_CIPHER_CTX_cleanup(&text->cipher_dec_ctx);
_plug_decode_free(&text->decode_context);
if (text->encode_buf) utils->free(text->encode_buf);
if (text->decode_buf) utils->free(text->decode_buf);
if (text->decode_pkt_buf) utils->free(text->decode_pkt_buf);
if (text->out_buf) utils->free(text->out_buf);
utils->free(text);
}
static int passdss_server_mech_new(void *glob_context __attribute__((unused)),
sasl_server_params_t *sparams,
const char *challenge __attribute__((unused)),
unsigned challen __attribute__((unused)),
void **conn_context)
{
context_t *text;
text = sparams->utils->malloc(sizeof(context_t));
if (text == NULL) {
MEMERROR(sparams->utils);
return SASL_NOMEM;
}
memset(text, 0, sizeof(context_t));
text->state = 1;
text->utils = sparams->utils;
text->cs_integrity_key = text->recv_integrity_key + 4;
text->sc_integrity_key = text->send_integrity_key + 4;
*conn_context = text;
return SASL_OK;
}
static int
passdss_server_mech_step1(context_t *text,
sasl_server_params_t *params,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams __attribute__((unused)))
{
BIGNUM *X = NULL;
DSA *dsa = NULL;
unsigned char *K = NULL;
unsigned Klen, hashlen;
int need, musthave;
EVP_MD_CTX mdctx;
unsigned char hash[EVP_MAX_MD_SIZE];
DSA_SIG *sig = NULL;
int result;
result = UnBuffer(params->utils, clientin, clientinlen,
"%s%s%m", &text->userid, &text->authid, &X);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error UnBuffering input in step 1");
goto cleanup;
}
dsa = DSA_generate_parameters(1024, NULL, 0, NULL, NULL, NULL, NULL);
if (!dsa) {
result = SASL_FAIL;
goto cleanup;
}
DSA_generate_key(dsa);
text->dh = DH_new();
BN_hex2bn(&text->dh->p, N);
BN_hex2bn(&text->dh->g, g);
DH_generate_key(text->dh);
K = text->utils->malloc(DH_size(text->dh) + 4);
if (!K) {
params->utils->log(NULL, SASL_LOG_ERR, "Error allocing K\n");
result = SASL_NOMEM;
goto cleanup;
}
Klen = DH_compute_key(K+4, X, text->dh);
*((uint32_t *) K) = htonl(Klen);
Klen += 4;
if (params->props.maxbufsize < 32) {
need = musthave = 0;
} else {
need = params->props.max_ssf - params->external_ssf;
musthave = params->props.min_ssf - params->external_ssf;
}
if (musthave <= NO_LAYER_SSF)
text->secmask |= NO_LAYER_FLAG;
if ((musthave <= INTEGRITY_LAYER_SSF) && (INTEGRITY_LAYER_SSF <= need))
text->secmask |= INTEGRITY_LAYER_FLAG;
if ((musthave <= PRIVACY_LAYER_SSF) && (PRIVACY_LAYER_SSF <= need))
text->secmask |= PRIVACY_LAYER_FLAG;
result = MakeBuffer(text->utils, &text->out_buf, 0, &text->out_buf_len,
serveroutlen, "%5a%s%m%m%m%m%m%1o%3u",
"ssh-dss", dsa->p, dsa->q, dsa->g, dsa->pub_key,
text->dh->pub_key, &text->secmask,
(params->props.maxbufsize > 0xFFFFFF) ? 0xFFFFFF :
params->props.maxbufsize);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR, "Error making output buffer\n");
goto cleanup;
}
EVP_DigestInit(&mdctx, EVP_sha1());
EVP_DigestUpdate(&mdctx, clientin, clientinlen);
EVP_DigestUpdate(&mdctx, text->out_buf, *serveroutlen);
EVP_DigestUpdate(&mdctx, K, Klen);
EVP_DigestFinal(&mdctx, hash, &hashlen);
CalcLayerParams(text, K, Klen, hash, hashlen);
HMAC_CTX_init(&text->hmac_recv_ctx);
HMAC_Init_ex(&text->hmac_recv_ctx, text->cs_integrity_key,
SHA_DIGEST_LENGTH, EVP_sha1(), NULL);
HMAC_Update(&text->hmac_recv_ctx, clientin, clientinlen);
HMAC_Update(&text->hmac_recv_ctx, text->out_buf, *serveroutlen);
sig = DSA_do_sign(hash, hashlen, dsa);
if (!sig) {
params->utils->log(NULL, SASL_LOG_ERR,
"Error calculating DSS signature\n");
result = SASL_FAIL;
goto cleanup;
}
result = MakeBuffer(text->utils, &text->out_buf, *serveroutlen,
&text->out_buf_len, serveroutlen,
"%3a%s%m%m", "ssh-dss", sig->r, sig->s);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR, "Error making output buffer\n");
goto cleanup;
}
*serverout = text->out_buf;
text->state = 2;
result = SASL_CONTINUE;
cleanup:
if (X) BN_free(X);
if (K) text->utils->free(K);
if (dsa) DSA_free(dsa);
if (sig) DSA_SIG_free(sig);
return result;
}
static int
passdss_server_mech_step2(context_t *text,
sasl_server_params_t *params,
const char *clientin,
unsigned clientinlen,
const char **serverout __attribute__((unused)),
unsigned *serveroutlen __attribute__((unused)),
sasl_out_params_t *oparams)
{
char *password = NULL;
unsigned declen, hmaclen;
unsigned char *csecmask, *cli_hmac, hmac[EVP_MAX_MD_SIZE];
uint32_t cbufsiz;
int r, result = SASL_OK;
result = _plug_buf_alloc(text->utils, &text->decode_pkt_buf,
&text->decode_pkt_buf_len, clientinlen);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR,
"Error allocating decrypt buffer in step 2\n");
goto cleanup;
}
EVP_CIPHER_CTX_init(&text->cipher_dec_ctx);
EVP_DecryptInit_ex(&text->cipher_dec_ctx, EVP_des_ede3_cbc(), NULL,
text->cs_encryption_key, text->cs_encryption_iv);
EVP_CIPHER_CTX_set_padding(&text->cipher_dec_ctx, 0);
text->blk_siz = EVP_CIPHER_CTX_block_size(&text->cipher_dec_ctx);
r = EVP_DecryptUpdate(&text->cipher_dec_ctx, text->decode_pkt_buf, &declen,
clientin, clientinlen);
if (r)
r = EVP_DecryptFinal_ex(&text->cipher_dec_ctx,
text->decode_pkt_buf + declen, &declen);
if (!r) {
params->utils->seterror(params->utils->conn, 0,
"Error decrypting input in step 2");
result = SASL_BADPROT;
goto cleanup;
}
clientin = text->decode_pkt_buf;
result = UnBuffer(params->utils, clientin, clientinlen,
"%-1o%3u%s%-*o%*p", &csecmask, &cbufsiz, &password,
SHA_DIGEST_LENGTH, &cli_hmac, text->blk_siz - 1);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error UnBuffering input in step 2");
goto cleanup;
}
HMAC_Update(&text->hmac_recv_ctx, clientin, 4);
HMAC_Final(&text->hmac_recv_ctx, hmac, &hmaclen);
if (memcmp(cli_hmac, hmac, hmaclen)) {
params->utils->seterror(params->utils->conn, 0,
"Client HMAC verification failed");
result = SASL_BADMAC;
goto cleanup;
}
result = params->canon_user(params->utils->conn,
text->authid, 0, SASL_CU_AUTHID, oparams);
if (result != SASL_OK) {
return result;
}
result = params->utils->checkpass(params->utils->conn,
oparams->authid, oparams->alen,
password, strlen(password));
if (result != SASL_OK) {
params->utils->seterror(params->utils->conn, 0,
"Password verification failed");
goto cleanup;
}
result = params->canon_user(params->utils->conn,
*text->userid ? text->userid : text->authid, 0,
SASL_CU_AUTHZID, oparams);
if (result != SASL_OK) return result;
text->secmask &= *csecmask;
if (text->secmask & PRIVACY_LAYER_FLAG) {
oparams->mech_ssf = PRIVACY_LAYER_SSF;
} else if (text->secmask & INTEGRITY_LAYER_FLAG) {
oparams->mech_ssf = INTEGRITY_LAYER_SSF;
} else if (text->secmask & NO_LAYER_FLAG) {
oparams->mech_ssf = NO_LAYER_SSF;
} else {
oparams->mech_ssf = 2;
SETERROR(params->utils,
"unable to agree on layers with server");
return SASL_BADPROT;
}
oparams->doneflag = 1;
oparams->param_version = 0;
if (oparams->mech_ssf > 0) {
oparams->encode = &passdss_encode;
oparams->decode = &passdss_decode;
oparams->maxoutbuf = cbufsiz - 4 - SHA_DIGEST_LENGTH;
HMAC_CTX_init(&text->hmac_send_ctx);
if (oparams->mech_ssf > 1) {
oparams->maxoutbuf -= text->blk_siz-1;
EVP_CIPHER_CTX_init(&text->cipher_enc_ctx);
EVP_EncryptInit_ex(&text->cipher_enc_ctx, EVP_des_ede3_cbc(), NULL,
text->sc_encryption_key, text->sc_encryption_iv);
EVP_CIPHER_CTX_set_padding(&text->cipher_enc_ctx, 0);
}
_plug_decode_init(&text->decode_context, text->utils,
(params->props.maxbufsize > 0xFFFFFF) ? 0xFFFFFF :
params->props.maxbufsize);
}
else {
oparams->encode = NULL;
oparams->decode = NULL;
oparams->maxoutbuf = 0;
}
result = SASL_OK;
cleanup:
if (password) _plug_free_string(params->utils, &password);
return result;
}
static int passdss_server_mech_step(void *conn_context,
sasl_server_params_t *sparams,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams)
{
context_t *text = (context_t *) conn_context;
if (!sparams
|| !serverout
|| !serveroutlen
|| !oparams)
return SASL_BADPARAM;
sparams->utils->log(NULL, SASL_LOG_DEBUG,
"PASSDSS server step %d\n", text->state);
*serverout = NULL;
*serveroutlen = 0;
switch (text->state) {
case 1:
return passdss_server_mech_step1(text, sparams, clientin, clientinlen,
serverout, serveroutlen, oparams);
case 2:
return passdss_server_mech_step2(text, sparams, clientin, clientinlen,
serverout, serveroutlen, oparams);
default:
sparams->utils->seterror(sparams->utils->conn, 0,
"Invalid PASSDSS server step %d", text->state);
return SASL_FAIL;
}
return SASL_FAIL;
}
static sasl_server_plug_t passdss_server_plugins[] =
{
{
"PASSDSS-3DES-1",
112,
SASL_SEC_NOPLAINTEXT
| SASL_SEC_NOANONYMOUS
| SASL_SEC_NOACTIVE
| SASL_SEC_NODICTIONARY
| SASL_SEC_FORWARD_SECRECY
| SASL_SEC_PASS_CREDENTIALS
| SASL_SEC_MUTUAL_AUTH,
SASL_FEAT_WANT_CLIENT_FIRST
| SASL_FEAT_ALLOWS_PROXY,
NULL,
&passdss_server_mech_new,
&passdss_server_mech_step,
&passdss_common_mech_dispose,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL
}
};
int passdss_server_plug_init(const sasl_utils_t *utils,
int maxversion,
int *out_version,
sasl_server_plug_t **pluglist,
int *plugcount)
{
if (maxversion < SASL_SERVER_PLUG_VERSION) {
SETERROR(utils, "PASSDSS version mismatch");
return SASL_BADVERS;
}
*out_version = SASL_SERVER_PLUG_VERSION;
*pluglist = passdss_server_plugins;
*plugcount = 1;
return SASL_OK;
}
static int passdss_client_mech_new(void *glob_context __attribute__((unused)),
sasl_client_params_t *params,
void **conn_context)
{
context_t *text;
text = params->utils->malloc(sizeof(context_t));
if (text == NULL) {
MEMERROR(params->utils);
return SASL_NOMEM;
}
memset(text, 0, sizeof(context_t));
text->state = 1;
text->utils = params->utils;
text->cs_integrity_key = text->send_integrity_key + 4;
text->sc_integrity_key = text->recv_integrity_key + 4;
*conn_context = text;
return SASL_OK;
}
static int
passdss_client_mech_step1(context_t *text,
sasl_client_params_t *params,
const char *serverin __attribute__((unused)),
unsigned serverinlen __attribute__((unused)),
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
const char *user = NULL, *authid = NULL;
int user_result = SASL_OK;
int auth_result = SASL_OK;
int pass_result = SASL_OK;
int result;
if (serverinlen > 0) {
SETERROR(params->utils, "Invalid input to first step of PASSDSS\n");
return SASL_BADPROT;
}
if (params->props.min_ssf > PRIVACY_LAYER_SSF + params->external_ssf) {
SETERROR(params->utils,
"minimum ssf too strong for PASSDSS");
return SASL_TOOWEAK;
}
if (oparams->authid == NULL) {
auth_result = _plug_get_authid(params->utils, &authid, prompt_need);
if ((auth_result != SASL_OK) && (auth_result != SASL_INTERACT))
return auth_result;
}
if (oparams->user == NULL) {
user_result = _plug_get_userid(params->utils, &user, prompt_need);
if ((user_result != SASL_OK) && (user_result != SASL_INTERACT))
return user_result;
}
if (text->password == NULL) {
pass_result = _plug_get_password(params->utils, &text->password,
&text->free_password, prompt_need);
if ((pass_result != SASL_OK) && (pass_result != SASL_INTERACT))
return pass_result;
}
if (prompt_need && *prompt_need) {
params->utils->free(*prompt_need);
*prompt_need = NULL;
}
if ((user_result == SASL_INTERACT) || (auth_result == SASL_INTERACT) ||
(pass_result == SASL_INTERACT)) {
result =
_plug_make_prompts(params->utils, prompt_need,
user_result == SASL_INTERACT ?
"Please enter your authorization name" : NULL,
NULL,
auth_result == SASL_INTERACT ?
"Please enter your authentication name" : NULL,
NULL,
pass_result == SASL_INTERACT ?
"Please enter your password" : NULL, NULL,
NULL, NULL, NULL,
NULL, NULL, NULL);
if (result != SASL_OK) goto cleanup;
return SASL_INTERACT;
}
if (!text->password) {
PARAMERROR(params->utils);
return SASL_BADPARAM;
}
if (!user || !*user) {
result = params->canon_user(params->utils->conn, authid, 0,
SASL_CU_AUTHID | SASL_CU_AUTHZID, oparams);
}
else {
result = params->canon_user(params->utils->conn, user, 0,
SASL_CU_AUTHZID, oparams);
if (result != SASL_OK) goto cleanup;
result = params->canon_user(params->utils->conn, authid, 0,
SASL_CU_AUTHID, oparams);
}
if (result != SASL_OK) goto cleanup;
text->dh = DH_new();
BN_hex2bn(&text->dh->p, N);
BN_hex2bn(&text->dh->g, g);
DH_generate_key(text->dh);
result = MakeBuffer(text->utils, &text->out_buf, 0, &text->out_buf_len,
clientoutlen, "%s%s%m",
(user && *user) ? (char *) oparams->user : "",
(char *) oparams->authid, text->dh->pub_key);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR, "Error making output buffer\n");
goto cleanup;
}
*clientout = text->out_buf;
text->state = 2;
result = SASL_CONTINUE;
cleanup:
return result;
}
static int
passdss_client_mech_step2(context_t *text,
sasl_client_params_t *params,
const char *serverin,
unsigned serverinlen,
sasl_interact_t **prompt_need __attribute__((unused)),
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
DSA *dsa = DSA_new();
DSA_SIG *sig = DSA_SIG_new();
BIGNUM *Y = NULL;
uint32_t siglen;
unsigned char *K = NULL;
unsigned Klen, hashlen, enclen;
unsigned char *ssecmask;
uint32_t sbufsiz;
EVP_MD_CTX mdctx;
unsigned char hash[EVP_MAX_MD_SIZE];
int need, musthave;
int result, r;
result = UnBuffer(params->utils, serverin, serverinlen,
"%u%3p\7ssh-dss%m%m%m%m%m%-1o%3u%u%3p\7ssh-dss%m%m",
NULL, &dsa->p, &dsa->q, &dsa->g, &dsa->pub_key,
&Y, &ssecmask, &sbufsiz, &siglen, &sig->r, &sig->s);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error UnBuffering input in step 2");
goto cleanup;
}
K = text->utils->malloc(DH_size(text->dh) + 4);
if (!K) {
params->utils->log(NULL, SASL_LOG_ERR, "Error allocing K\n");
result = SASL_NOMEM;
goto cleanup;
}
Klen = DH_compute_key(K+4, Y, text->dh);
*((uint32_t *) K) = htonl(Klen);
Klen += 4;
EVP_DigestInit(&mdctx, EVP_sha1());
EVP_DigestUpdate(&mdctx, text->out_buf, text->out_buf_len);
EVP_DigestUpdate(&mdctx, serverin, serverinlen - siglen - 4);
EVP_DigestUpdate(&mdctx, K, Klen);
EVP_DigestFinal(&mdctx, hash, &hashlen);
result = DSA_do_verify(hash, hashlen, sig, dsa);
if (result != 1) {
params->utils->log(NULL, SASL_LOG_ERR,
(result == 0) ? "Incorrect DSS signature\n" :
"Error verifying DSS signature\n");
result = (result == 0) ? SASL_BADPROT : SASL_FAIL;
goto cleanup;
}
CalcLayerParams(text, K, Klen, hash, hashlen);
EVP_CIPHER_CTX_init(&text->cipher_enc_ctx);
EVP_EncryptInit_ex(&text->cipher_enc_ctx, EVP_des_ede3_cbc(), NULL,
text->cs_encryption_key, text->cs_encryption_iv);
EVP_CIPHER_CTX_set_padding(&text->cipher_enc_ctx, 0);
text->blk_siz = EVP_CIPHER_CTX_block_size(&text->cipher_enc_ctx);
if (params->props.maxbufsize < 32) {
need = musthave = 0;
} else {
need = params->props.max_ssf - params->external_ssf;
musthave = params->props.min_ssf - params->external_ssf;
}
if ((*ssecmask & PRIVACY_LAYER_FLAG) &&
(need >= PRIVACY_LAYER_SSF) && (musthave <= PRIVACY_LAYER_SSF)) {
text->secmask = PRIVACY_LAYER_FLAG;
oparams->mech_ssf = PRIVACY_LAYER_SSF;
} else if ((*ssecmask & INTEGRITY_LAYER_FLAG) &&
(need >= INTEGRITY_LAYER_SSF) &&
(musthave <= INTEGRITY_LAYER_SSF)) {
text->secmask =INTEGRITY_LAYER_FLAG;
oparams->mech_ssf = INTEGRITY_LAYER_SSF;
} else if ((*ssecmask & NO_LAYER_FLAG) && (musthave <= NO_LAYER_SSF)) {
text->secmask = NO_LAYER_FLAG;
oparams->mech_ssf = NO_LAYER_SSF;
} else {
oparams->mech_ssf = 2;
SETERROR(params->utils,
"unable to agree on layers with server");
return SASL_BADPROT;
}
HMAC_CTX_init(&text->hmac_send_ctx);
HMAC_Init_ex(&text->hmac_send_ctx, text->cs_integrity_key,
SHA_DIGEST_LENGTH, EVP_sha1(), NULL);
HMAC_Update(&text->hmac_send_ctx, text->out_buf, text->out_buf_len);
HMAC_Update(&text->hmac_send_ctx, serverin, serverinlen - siglen - 4);
result = MakeBuffer(text->utils, &text->out_buf, 0,
&text->out_buf_len, clientoutlen, "%1o%3u%*s",
&text->secmask,
(params->props.maxbufsize > 0xFFFFFF) ? 0xFFFFFF :
params->props.maxbufsize,
text->password->len, text->password->data);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR, "Error making output buffer\n");
goto cleanup;
}
HMAC_Update(&text->hmac_send_ctx, text->out_buf, 4);
HMAC_Final(&text->hmac_send_ctx, hash, &hashlen);
result = MakeBuffer(text->utils, &text->out_buf, *clientoutlen,
&text->out_buf_len, clientoutlen, "%*o%*o",
hashlen, hash, text->blk_siz - 1, text->padding);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR, "Error making output buffer\n");
goto cleanup;
}
result = _plug_buf_alloc(text->utils, &text->encode_buf,
&text->encode_buf_len, *clientoutlen);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR,
"Error allocating encrypt buffer in step 2\n");
goto cleanup;
}
r = EVP_EncryptUpdate(&text->cipher_enc_ctx, text->encode_buf,
clientoutlen, text->out_buf,
text->blk_siz * (*clientoutlen / text->blk_siz));
if (r)
r = EVP_EncryptFinal_ex(&text->cipher_enc_ctx,
text->encode_buf + *clientoutlen, &enclen);
if (!r) {
params->utils->seterror(params->utils->conn, 0,
"Error encrypting output in step 2");
result = SASL_FAIL;
goto cleanup;
}
*clientout = text->encode_buf;
oparams->doneflag = 1;
oparams->param_version = 0;
if (oparams->mech_ssf > 0) {
oparams->encode = &passdss_encode;
oparams->decode = &passdss_decode;
oparams->maxoutbuf = sbufsiz - 4 - SHA_DIGEST_LENGTH;
HMAC_CTX_init(&text->hmac_recv_ctx);
if (oparams->mech_ssf > 1) {
oparams->maxoutbuf -= text->blk_siz-1;
EVP_CIPHER_CTX_init(&text->cipher_dec_ctx);
EVP_DecryptInit_ex(&text->cipher_dec_ctx, EVP_des_ede3_cbc(), NULL,
text->sc_encryption_key, text->sc_encryption_iv);
EVP_CIPHER_CTX_set_padding(&text->cipher_dec_ctx, 0);
}
_plug_decode_init(&text->decode_context, text->utils,
(params->props.maxbufsize > 0xFFFFFF) ? 0xFFFFFF :
params->props.maxbufsize);
}
else {
oparams->encode = NULL;
oparams->decode = NULL;
oparams->maxoutbuf = 0;
}
result = SASL_OK;
cleanup:
if (Y) BN_free(Y);
if (K) text->utils->free(K);
if (dsa) DSA_free(dsa);
if (sig) DSA_SIG_free(sig);
return result;
}
static int passdss_client_mech_step(void *conn_context,
sasl_client_params_t *params,
const char *serverin,
unsigned serverinlen,
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
context_t *text = (context_t *) conn_context;
params->utils->log(NULL, SASL_LOG_DEBUG,
"PASSDSS client step %d\n", text->state);
*clientout = NULL;
*clientoutlen = 0;
switch (text->state) {
case 1:
return passdss_client_mech_step1(text, params, serverin, serverinlen,
prompt_need, clientout, clientoutlen,
oparams);
case 2:
return passdss_client_mech_step2(text, params, serverin, serverinlen,
prompt_need, clientout, clientoutlen,
oparams);
default:
params->utils->log(NULL, SASL_LOG_ERR,
"Invalid PASSDSS client step %d\n", text->state);
return SASL_FAIL;
}
return SASL_FAIL;
}
static sasl_client_plug_t passdss_client_plugins[] =
{
{
"PASSDSS-3DES-1",
112,
SASL_SEC_NOPLAINTEXT
| SASL_SEC_NOANONYMOUS
| SASL_SEC_NOACTIVE
| SASL_SEC_NODICTIONARY
| SASL_SEC_FORWARD_SECRECY
| SASL_SEC_PASS_CREDENTIALS
| SASL_SEC_MUTUAL_AUTH,
SASL_FEAT_WANT_CLIENT_FIRST
| SASL_FEAT_ALLOWS_PROXY,
NULL,
NULL,
&passdss_client_mech_new,
&passdss_client_mech_step,
&passdss_common_mech_dispose,
NULL,
NULL,
NULL,
NULL
}
};
int passdss_client_plug_init(sasl_utils_t *utils,
int maxversion,
int *out_version,
sasl_client_plug_t **pluglist,
int *plugcount)
{
if (maxversion < SASL_CLIENT_PLUG_VERSION) {
SETERROR(utils, "PASSDSS version mismatch");
return SASL_BADVERS;
}
*out_version = SASL_CLIENT_PLUG_VERSION;
*pluglist = passdss_client_plugins;
*plugcount = 1;
return SASL_OK;
}