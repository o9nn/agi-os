#include <config.h>
#include <assert.h>
#include <ctype.h>
#include <stdio.h>
#include <limits.h>
#include <stdarg.h>
#ifndef UINT32_MAX
#define UINT32_MAX 4294967295U
#endif
#if UINT_MAX == UINT32_MAX
typedef unsigned int uint32;
#elif ULONG_MAX == UINT32_MAX
typedef unsigned long uint32;
#elif USHRT_MAX == UINT32_MAX
typedef unsigned short uint32;
#else
#error dont know what to use for uint32
#endif
#include <openssl/bn.h>
#include <openssl/evp.h>
#include <openssl/hmac.h>
#include <openssl/md5.h>
#include <sasl.h>
#define MD5_H
#include <saslplug.h>
#include "plugin_common.h"
#ifdef macintosh
#include <sasl_srp_plugin_decl.h>
#endif
static const char plugin_id[] = "$Id: srp.c,v 1.59 2010/11/30 11:41:47 mel Exp $";
#define SRP_MAXBLOCKSIZE 16
#define SRP_MAXBUFFERSIZE 2147483643
#define DEFAULT_MDA		"SHA-1"
#define OPTION_MDA		"mda="
#define OPTION_REPLAY_DETECTION	"replay_detection"
#define OPTION_INTEGRITY	"integrity="
#define OPTION_CONFIDENTIALITY	"confidentiality="
#define OPTION_MANDATORY	"mandatory="
#define OPTION_MAXBUFFERSIZE	"maxbuffersize="
struct Ng {
char *N;
unsigned long g;
} Ng_tab[] = {
{ "115B8B692E0E045692CF280B436735C77A5A9E8A9E7ED56C965F87DB5B2A2ECE3",
2
},
{ "8025363296FB943FCE54BE717E0E2958A02A9672EF561953B2BAA3BAACC3ED5754EB764C7AB7184578C57D5949CCB41B",
2
},
{ "D4C7F8A2B32C11B8FBA9581EC4BA4F1B04215642EF7355E37C0FC0443EF756EA2C6B8EEB755A1C723027663CAA265EF785B8FF6A9B35227A52D86633DBDFCA43",
2
},
{ "C94D67EB5B1A2346E8AB422FC6A0EDAEDA8C7F894C9EEEC42F9ED250FD7F0046E5AF2CF73D6B2FA26BB08033DA4DE322E144E7A8E9B12A0E4637F6371F34A2071C4B3836CBEEAB15034460FAA7ADF483",
2
},
{ "B344C7C4F8C495031BB4E04FF8F84EE95008163940B9558276744D91F7CC9F402653BE7147F00F576B93754BCDDF71B636F2099E6FFF90E79575F3D0DE694AFF737D9BE9713CEF8D837ADA6380B1093E94B6A529A8C6C2BE33E0867C60C3262B",
2
},
{ "EEAF0AB9ADB38DD69C33F80AFA8FC5E86072618775FF3C0B9EA2314C9C256576D674DF7496EA81D3383B4813D692C6E0E0D5D8E250B98BE48E495C1D6089DAD15DC7D7B46154D6B6CE8EF4AD69B15D4982559B297BCF1885C529F566660E57EC68EDBC3C05726CC02FD4CBF4976EAA9AFD5138FE8376435B9FC61D2FC0EB06E3",
2
},
{ "D77946826E811914B39401D56A0A7843A8E7575D738C672A090AB1187D690DC43872FC06A7B6A43F3B95BEAEC7DF04B9D242EBDC481111283216CE816E004B786C5FCE856780D41837D95AD787A50BBE90BD3A9C98AC0F5FC0DE744B1CDE1891690894BC1F65E00DE15B4B2AA6D87100C9ECC2527E45EB849DEB14BB2049B163EA04187FD27C1BD9C7958CD40CE7067A9C024F9B7C5A0B4F5003686161F0605B",
2
},
{ "9DEF3CAFB939277AB1F12A8617A47BBBDBA51DF499AC4C80BEEEA9614B19CC4D5F4F5F556E27CBDE51C6A94BE4607A291558903BA0D0F84380B655BB9A22E8DCDF028A7CEC67F0D08134B1C8B97989149B609E0BE3BAB63D47548381DBC5B1FC764E3F4B53DD9DA1158BFD3E2B9C8CF56EDF019539349627DB2FD53D24B7C48665772E437D6C7F8CE442734AF7CCB7AE837C264AE3A9BEB87F8A2FE9B8B5292E5A021FFF5E91479E8CE7A28C2442C6F315180F93499A234DCF76E3FED135F9BB",
2
},
{ "AC6BDB41324A9A9BF166DE5E1389582FAF72B6651987EE07FC3192943DB56050A37329CBB4A099ED8193E0757767A13DD52312AB4B03310DCD7F48A9DA04FD50E8083969EDB767B0CF6095179A163AB3661A05FBD5FAAAE82918A9962F0B93B855F97993EC975EEAA80D740ADBF4FF747359D041D5C33EA71D281E446B14773BCA97B43A23FB801676BD207A436C6481F1D2B9078717461A5B9D32E688F87748544523B524B0D57D5EA77A2775D2ECFA032CFBDBF52FB3786160279004E57AE6AF874E7303CE53299CCC041C7BC308D82A5698F3A8D0C38271AE35F8E9DBFBB694B5C803D89F7AE435DE236D525F54759B65E372FCD68EF20FA7111F9E4AFF73",
2
}
};
#define NUM_Ng (sizeof(Ng_tab) / sizeof(struct Ng))
typedef struct layer_option_s {
const char *name;
unsigned enabled;
unsigned bit;
sasl_ssf_t ssf;
const char *evp_name;
} layer_option_t;
static layer_option_t digest_options[] = {
{ "SHA-1",		0, (1<<0), 1,	"sha1" },
{ "RIPEMD-160",	0, (1<<1), 1,	"rmd160" },
{ "MD5",		0, (1<<2), 1,	"md5" },
{ NULL,		0,      0, 0,	NULL }
};
static layer_option_t *default_digest = &digest_options[0];
static layer_option_t *server_mda = NULL;
static layer_option_t cipher_options[] = {
{ "DES",		0, (1<<0), 56,	"des-ofb" },
{ "3DES",		0, (1<<1), 112,	"des-ede-ofb" },
{ "AES",		0, (1<<2), 128,	"aes-128-ofb" },
{ "Blowfish",	0, (1<<3), 128,	"bf-ofb" },
{ "CAST-128",	0, (1<<4), 128,	"cast5-ofb" },
{ "IDEA",		0, (1<<5), 128,	"idea-ofb" },
{ NULL,		0,      0, 0,	NULL}
};
#if OPENSSL_VERSION_NUMBER < 0x00907000L
static layer_option_t *default_cipher = &cipher_options[0];
#else
static layer_option_t *default_cipher = &cipher_options[2];
#endif
enum {
BIT_REPLAY_DETECTION=	(1<<0),
BIT_INTEGRITY=		(1<<1),
BIT_CONFIDENTIALITY=	(1<<2)
};
typedef struct srp_options_s {
unsigned mda;
unsigned replay_detection;
unsigned integrity;
unsigned confidentiality;
unsigned mandatory;
unsigned long maxbufsize;
} srp_options_t;
typedef struct context {
int state;
BIGNUM N;
BIGNUM g;
BIGNUM v;
BIGNUM b;
BIGNUM B;
BIGNUM a;
BIGNUM A;
char K[EVP_MAX_MD_SIZE];
int Klen;
char M1[EVP_MAX_MD_SIZE];
int M1len;
char *authid;
char *userid;
sasl_secret_t *password;
unsigned int free_password;
char *client_options;
char *server_options;
srp_options_t client_opts;
char cIV[SRP_MAXBLOCKSIZE];
char *salt;
int saltlen;
const EVP_MD *md;
const sasl_utils_t *utils;
char *out_buf;
unsigned out_buf_len;
unsigned layer;
const EVP_MD *hmac_md;
HMAC_CTX hmac_send_ctx;
HMAC_CTX hmac_recv_ctx;
const EVP_CIPHER *cipher;
EVP_CIPHER_CTX cipher_enc_ctx;
EVP_CIPHER_CTX cipher_dec_ctx;
int seqnum_out;
int seqnum_in;
char           *encode_buf, *decode_buf, *decode_pkt_buf;
unsigned       encode_buf_len, decode_buf_len, decode_pkt_buf_len;
decode_context_t decode_context;
} context_t;
static int srp_encode(void *context,
const struct iovec *invec,
unsigned numiov,
const char **output,
unsigned *outputlen)
{
context_t *text = (context_t *) context;
unsigned i;
char *input;
unsigned long inputlen, tmpnum;
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
SRP_MAXBLOCKSIZE +
EVP_MAX_MD_SIZE);
if (ret != SASL_OK) return ret;
*outputlen = 4;
for (i = 0; i < numiov; i++) {
input = invec[i].iov_base;
inputlen = invec[i].iov_len;
if (text->layer & BIT_CONFIDENTIALITY) {
unsigned enclen;
EVP_EncryptUpdate(&text->cipher_enc_ctx,
text->encode_buf + *outputlen, &enclen,
input, inputlen);
*outputlen += enclen;
input = text->encode_buf + 4;
inputlen = *outputlen - 4;
}
else {
memcpy(text->encode_buf + *outputlen, input, inputlen);
*outputlen += inputlen;
}
}
if (text->layer & BIT_CONFIDENTIALITY) {
unsigned enclen;
EVP_EncryptFinal(&text->cipher_enc_ctx,
text->encode_buf + *outputlen, &enclen);
*outputlen += enclen;
}
if (text->layer & BIT_INTEGRITY) {
unsigned hashlen;
HMAC_Update(&text->hmac_send_ctx, text->encode_buf+4, *outputlen-4);
if (text->layer & BIT_REPLAY_DETECTION) {
tmpnum = htonl(text->seqnum_out);
HMAC_Update(&text->hmac_send_ctx, (char *) &tmpnum, 4);
text->seqnum_out++;
}
HMAC_Final(&text->hmac_send_ctx, text->encode_buf + *outputlen,
&hashlen);
*outputlen += hashlen;
}
tmpnum = *outputlen - 4;
tmpnum = htonl(tmpnum);
memcpy(text->encode_buf, &tmpnum, 4);
*output = text->encode_buf;
return SASL_OK;
}
static int srp_decode_packet(void *context,
const char *input,
unsigned inputlen,
char **output,
unsigned *outputlen)
{
context_t *text = (context_t *) context;
int ret;
if (text->layer & BIT_INTEGRITY) {
const char *hash;
char myhash[EVP_MAX_MD_SIZE];
unsigned hashlen, myhashlen, i;
unsigned long tmpnum;
hashlen = EVP_MD_size(text->hmac_md);
if (inputlen < hashlen) {
text->utils->seterror(text->utils->conn, 0,
"SRP input is smaller "
"than hash length: %d vs %d\n",
inputlen, hashlen);
return SASL_BADPROT;
}
inputlen -= hashlen;
hash = input + inputlen;
HMAC_Update(&text->hmac_recv_ctx, input, inputlen);
if (text->layer & BIT_REPLAY_DETECTION) {
tmpnum = htonl(text->seqnum_in);
HMAC_Update(&text->hmac_recv_ctx, (char *) &tmpnum, 4);
text->seqnum_in++;
}
HMAC_Final(&text->hmac_recv_ctx, myhash, &myhashlen);
for (i = 0; i < hashlen; i++) {
if ((myhashlen != hashlen) || (myhash[i] != hash[i])) {
SETERROR(text->utils, "Hash is incorrect\n");
return SASL_BADMAC;
}
}
}
ret = _plug_buf_alloc(text->utils, &(text->decode_pkt_buf),
&(text->decode_pkt_buf_len),
inputlen);
if (ret != SASL_OK) return ret;
if (text->layer & BIT_CONFIDENTIALITY) {
unsigned declen;
EVP_DecryptUpdate(&text->cipher_dec_ctx,
text->decode_pkt_buf, &declen,
(char *) input, inputlen);
*outputlen = declen;
EVP_DecryptFinal(&text->cipher_dec_ctx,
text->decode_pkt_buf + declen, &declen);
*outputlen += declen;
} else {
memcpy(text->decode_pkt_buf, input, inputlen);
*outputlen = inputlen;
}
*output = text->decode_pkt_buf;
return SASL_OK;
}
static int srp_decode(void *context,
const char *input, unsigned inputlen,
const char **output, unsigned *outputlen)
{
context_t *text = (context_t *) context;
int ret;
ret = _plug_decode(&text->decode_context, input, inputlen,
&text->decode_buf, &text->decode_buf_len, outputlen,
srp_decode_packet, text);
*output = text->decode_buf;
return ret;
}
static int BigIntToBytes(BIGNUM *num, char *out, int maxoutlen, int *outlen)
{
int len;
len = BN_num_bytes(num);
if (len > maxoutlen) return SASL_FAIL;
*outlen = BN_bn2bin(num, out);
return SASL_OK;
}
static int BigIntCmpWord(BIGNUM *a, BN_ULONG w)
{
BIGNUM *b = BN_new();
int r;
BN_set_word(b, w);
r = BN_cmp(a, b);
BN_free(b);
return r;
}
static void GetRandBigInt(BIGNUM *out)
{
BN_init(out);
BN_rand(out, SRP_MAXBLOCKSIZE*8, 0, 0);
}
#define MAX_BUFFER_LEN 2147483643
#define MAX_MPI_LEN 65535
#define MAX_UTF8_LEN 65535
#define MAX_OS_LEN 255
static int MakeBuffer(const sasl_utils_t *utils, char **buf, unsigned *buflen,
unsigned *outlen, const char *fmt, ...)
{
va_list ap;
char *p, *out = NULL;
int r, alloclen, len;
BIGNUM *mpi;
char *os, *str, c;
uint32 u;
short ns;
long totlen;
va_start(ap, fmt);
for (p = (char *) fmt, alloclen = 0; *p; p++) {
if (*p != '%') {
alloclen++;
continue;
}
switch (*++p) {
case 'm':
mpi = va_arg(ap, BIGNUM *);
len = BN_num_bytes(mpi);
if (len > MAX_MPI_LEN) {
utils->log(NULL, SASL_LOG_ERR,
"String too long to create mpi string\n");
r = SASL_FAIL;
goto done;
}
alloclen += len + 2;
break;
case 'o':
len = va_arg(ap, int);
if (len > MAX_OS_LEN) {
utils->log(NULL, SASL_LOG_ERR,
"String too long to create os string\n");
r = SASL_FAIL;
goto done;
}
alloclen += len + 1;
os = va_arg(ap, char *);
break;
case 's':
str = va_arg(ap, char *);
len = strlen(str);
if (len > MAX_UTF8_LEN) {
utils->log(NULL, SASL_LOG_ERR,
"String too long to create utf8 string\n");
r = SASL_FAIL;
goto done;
}
alloclen += len + 2;
break;
case 'u':
u = va_arg(ap, uint32);
alloclen += sizeof(uint32);
break;
case 'c':
c = va_arg(ap, int) & 0xFF;
alloclen += 1;
break;
default:
alloclen += 1;
break;
}
}
va_end(ap);
if (alloclen > MAX_BUFFER_LEN) {
utils->log(NULL, SASL_LOG_ERR,
"String too long to create SRP buffer string\n");
return SASL_FAIL;
}
alloclen += 4;
r = _plug_buf_alloc(utils, buf, buflen, alloclen);
if (r != SASL_OK) return r;
out = *buf + 4;
va_start(ap, fmt);
for (p = (char *) fmt; *p; p++) {
if (*p != '%') {
*out = *p;
out++;
continue;
}
switch (*++p) {
case 'm':
mpi = va_arg(ap, BIGNUM *);
r = BigIntToBytes(mpi, out+2, BN_num_bytes(mpi), &len);
if (r) goto done;
ns = htons(len);
memcpy(out, &ns, 2);
out += len + 2;
break;
case 'o':
len = va_arg(ap, int);
os = va_arg(ap, char *);
*out = len & 0xFF;
memcpy(out+1, os, len);
out += len+1;
break;
case 's':
str = va_arg(ap, char *);
len = strlen(str);
ns = htons(len);
memcpy(out, &ns, 2);
memcpy(out+2, str, len);
out += len + 2;
break;
case 'u':
u = va_arg(ap, uint32);
u = htonl(u);
memcpy(out, &u, sizeof(uint32));
out += sizeof(uint32);
break;
case 'c':
c = va_arg(ap, int) & 0xFF;
*out = c;
out++;
break;
default:
*out = *p;
out++;
break;
}
}
done:
va_end(ap);
*outlen = out - *buf;
totlen = htonl(*outlen - 4);
memcpy(*buf, &totlen, 4);
return r;
}
static int UnBuffer(const sasl_utils_t *utils, const char *buf,
unsigned buflen, const char *fmt, ...)
{
va_list ap;
char *p;
int r = SASL_OK, noalloc;
BIGNUM *mpi;
char **os, **str;
uint32 *u;
unsigned short ns;
unsigned len;
if (!buf || buflen < 4) {
utils->seterror(utils->conn, 0,
"Buffer is not big enough to be SRP buffer: %d\n",
buflen);
return SASL_BADPROT;
}
memcpy(&len, buf, 4);
len = ntohl(len);
buf += 4;
buflen -= 4;
if (len != buflen) {
SETERROR(utils, "SRP Buffer isn't of the right length\n");
return SASL_BADPROT;
}
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
if ((noalloc = (*++p == '-'))) ++p;
switch (*p) {
case 'm':
if (buflen < 2) {
SETERROR(utils, "Buffer is not big enough to be SRP MPI\n");
r = SASL_BADPROT;
goto done;
}
memcpy(&ns, buf, 2);
len = ntohs(ns);
buf += 2;
buflen -= 2;
if (len > buflen) {
SETERROR(utils, "Not enough data for this SRP MPI\n");
r = SASL_BADPROT;
goto done;
}
mpi = va_arg(ap, BIGNUM *);
BN_init(mpi);
BN_bin2bn(buf, len, mpi);
break;
case 'o':
if (buflen < 1) {
SETERROR(utils, "Buffer is not big enough to be SRP os\n");
r = SASL_BADPROT;
goto done;
}
len = (unsigned char) *buf;
buf++;
buflen--;
if (len > buflen) {
SETERROR(utils, "Not enough data for this SRP os\n");
r = SASL_BADPROT;
goto done;
}
*(va_arg(ap, int *)) = len;
os = va_arg(ap, char **);
if (noalloc)
*os = (char *) buf;
else {
*os = (char *) utils->malloc(len);
if (!*os) {
r = SASL_NOMEM;
goto done;
}
memcpy(*os, buf, len);
}
break;
case 's':
if (buflen < 2) {
SETERROR(utils, "Buffer is not big enough to be SRP UTF8\n");
r = SASL_BADPROT;
goto done;
}
memcpy(&ns, buf, 2);
len = ntohs(ns);
buf += 2;
buflen -= 2;
if (len > buflen) {
SETERROR(utils, "Not enough data for this SRP UTF8\n");
r = SASL_BADPROT;
goto done;
}
str = va_arg(ap, char **);
*str = (char *) utils->malloc(len+1);
if (!*str) {
r = SASL_NOMEM;
goto done;
}
memcpy(*str, buf, len);
(*str)[len] = '\0';
break;
case 'u':
if (buflen < sizeof(uint32)) {
SETERROR(utils, "Buffer is not big enough to be SRP uint\n");
r = SASL_BADPROT;
goto done;
}
len = sizeof(uint32);
u = va_arg(ap, uint32*);
memcpy(u, buf, len);
*u = ntohs(*u);
break;
case 'c':
if (buflen < 1) {
SETERROR(utils, "Buffer is not big enough to be SRP char\n");
r = SASL_BADPROT;
goto done;
}
len = 1;
*(va_arg(ap, char *)) = *buf;
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
done:
va_end(ap);
if (buflen != 0) {
SETERROR(utils, "Extra data in SRP buffer\n");
r = SASL_BADPROT;
}
return r;
}
static int MakeHash(const EVP_MD *md, unsigned char hash[], int *hashlen,
const char *fmt, ...)
{
va_list ap;
char *p, buf[4096], *in;
int inlen;
EVP_MD_CTX mdctx;
int r = 0, hflag;
EVP_DigestInit(&mdctx, md);
va_start(ap, fmt);
for (p = (char *) fmt; *p; p++) {
if (*p != '%') {
in = p;
inlen = 1;
hflag = 0;
}
else {
if ((hflag = (*++p == 'h'))) ++p;
switch (*p) {
case 'm': {
BIGNUM *mval = va_arg(ap, BIGNUM *);
in = buf;
r = BigIntToBytes(mval, buf, sizeof(buf)-1, &inlen);
if (r) goto done;
break;
}
case 'o': {
inlen = va_arg(ap, int);
in = va_arg(ap, char *);
break;
}
case 's':
in = va_arg(ap, char *);
inlen = strlen(in);
break;
case 'u': {
uint32 uval = va_arg(ap, uint32);
in = buf;
inlen = sizeof(uint32);
*((uint32 *) buf) = htonl(uval);
break;
}
default:
in = p;
inlen = 1;
break;
}
}
if (hflag) {
EVP_MD_CTX tmpctx;
EVP_DigestInit(&tmpctx, md);
EVP_DigestUpdate(&tmpctx, in, inlen);
EVP_DigestFinal(&tmpctx, buf, &inlen);
in = buf;
}
EVP_DigestUpdate(&mdctx, in, inlen);
}
done:
va_end(ap);
EVP_DigestFinal(&mdctx, hash, hashlen);
return r;
}
static int CalculateX(context_t *text, const char *salt, int saltlen,
const char *user, const char *pass, int passlen,
BIGNUM *x)
{
char hash[EVP_MAX_MD_SIZE];
int hashlen;
MakeHash(text->md, hash, &hashlen, "%s:%o", user, passlen, pass);
MakeHash(text->md, hash, &hashlen, "%o%o", saltlen, salt, hashlen, hash);
BN_init(x);
BN_bin2bn(hash, hashlen, x);
return SASL_OK;
}
static int CalculateM1(context_t *text, BIGNUM *N, BIGNUM *g,
char *U, char *salt, int saltlen,
BIGNUM *A, BIGNUM *B, char *K, int Klen,
char *I, char *L, char *M1, int *M1len)
{
int r, i, len;
unsigned char Nhash[EVP_MAX_MD_SIZE];
unsigned char ghash[EVP_MAX_MD_SIZE];
unsigned char Ng[EVP_MAX_MD_SIZE];
r = MakeHash(text->md, Nhash, &len, "%m", N);
if (r) return r;
r = MakeHash(text->md, ghash, &len, "%m", g);
if (r) return r;
for (i = 0; i < len; i++) {
Ng[i] = (Nhash[i] ^ ghash[i]);
}
r = MakeHash(text->md, M1, M1len, "%o%hs%o%m%m%o%hs%hs",
len, Ng, U, saltlen, salt, A, B, Klen, K, I, L);
return r;
}
static int CalculateM2(context_t *text, BIGNUM *A,
char *M1, int M1len, char *K, int Klen,
char *I, char *o, char *sid, uint32 ttl,
char *M2, int *M2len)
{
int r;
r = MakeHash(text->md, M2, M2len, "%m%o%o%hs%hs%s%u",
A, M1len, M1, Klen, K, I, o, sid, ttl);
return r;
}
static int ParseOption(const sasl_utils_t *utils,
char *in, char **option, char **nextptr)
{
char *comma;
int len;
int i;
if (strlen(in) == 0) {
*option = NULL;
return SASL_OK;
}
comma = strchr(in,',');
if (comma == NULL) comma = in + strlen(in);
len = comma - in;
*option = utils->malloc(len + 1);
if (!*option) return SASL_NOMEM;
for (i = 0; i < len; i++) {
(*option)[i] = tolower((int)in[i]);
}
(*option)[len] = '\0';
if (*comma) {
*nextptr = comma+1;
} else {
*nextptr = NULL;
}
return SASL_OK;
}
static int FindBit(char *name, layer_option_t *opts)
{
while (opts->name) {
if (!strcasecmp(name, opts->name)) {
return opts->bit;
}
opts++;
}
return 0;
}
static layer_option_t *FindOptionFromBit(unsigned bit, layer_option_t *opts)
{
while (opts->name) {
if (opts->bit == bit) {
return opts;
}
opts++;
}
return NULL;
}
static int ParseOptionString(const sasl_utils_t *utils,
char *str, srp_options_t *opts, int isserver)
{
if (!strncasecmp(str, OPTION_MDA, strlen(OPTION_MDA))) {
int bit = FindBit(str+strlen(OPTION_MDA), digest_options);
if (isserver && (!bit || opts->mda)) {
opts->mda = -1;
if (!bit)
utils->seterror(utils->conn, 0,
"SRP MDA %s not supported\n",
str+strlen(OPTION_MDA));
else
SETERROR(utils, "Multiple SRP MDAs given\n");
return SASL_BADPROT;
}
opts->mda |= bit;
} else if (!strcasecmp(str, OPTION_REPLAY_DETECTION)) {
if (opts->replay_detection) {
SETERROR(utils, "SRP Replay Detection option appears twice\n");
return SASL_BADPROT;
}
opts->replay_detection = 1;
} else if (!strncasecmp(str, OPTION_INTEGRITY, strlen(OPTION_INTEGRITY)) &&
!strncasecmp(str+strlen(OPTION_INTEGRITY), "HMAC-", 5)) {
int bit = FindBit(str+strlen(OPTION_INTEGRITY)+5, digest_options);
if (isserver && (!bit || opts->integrity)) {
opts->integrity = -1;
if (!bit)
utils->seterror(utils->conn, 0,
"SRP Integrity option %s not supported\n",
str+strlen(OPTION_INTEGRITY));
else
SETERROR(utils, "Multiple SRP Integrity options given\n");
return SASL_BADPROT;
}
opts->integrity |= bit;
} else if (!strncasecmp(str, OPTION_CONFIDENTIALITY,
strlen(OPTION_CONFIDENTIALITY))) {
int bit = FindBit(str+strlen(OPTION_CONFIDENTIALITY),
cipher_options);
if (isserver && (!bit || opts->confidentiality)) {
opts->confidentiality = -1;
if (!bit)
utils->seterror(utils->conn, 0,
"SRP Confidentiality option %s not supported\n",
str+strlen(OPTION_CONFIDENTIALITY));
else
SETERROR(utils,
"Multiple SRP Confidentiality options given\n");
return SASL_FAIL;
}
opts->confidentiality |= bit;
} else if (!isserver && !strncasecmp(str, OPTION_MANDATORY,
strlen(OPTION_MANDATORY))) {
char *layer = str+strlen(OPTION_MANDATORY);
if (!strcasecmp(layer, OPTION_REPLAY_DETECTION))
opts->mandatory |= BIT_REPLAY_DETECTION;
else if (!strncasecmp(layer, OPTION_INTEGRITY,
strlen(OPTION_INTEGRITY)-1))
opts->mandatory |= BIT_INTEGRITY;
else if (!strncasecmp(layer, OPTION_CONFIDENTIALITY,
strlen(OPTION_CONFIDENTIALITY)-1))
opts->mandatory |= BIT_CONFIDENTIALITY;
else {
utils->seterror(utils->conn, 0,
"Mandatory SRP option %s not supported\n", layer);
return SASL_BADPROT;
}
} else if (!strncasecmp(str, OPTION_MAXBUFFERSIZE,
strlen(OPTION_MAXBUFFERSIZE))) {
opts->maxbufsize = strtoul(str+strlen(OPTION_MAXBUFFERSIZE), NULL, 10);
if (opts->maxbufsize > SRP_MAXBUFFERSIZE) {
utils->seterror(utils->conn, 0,
"SRP Maxbuffersize %lu too big (> %lu)\n",
opts->maxbufsize, SRP_MAXBUFFERSIZE);
return SASL_BADPROT;
}
} else {
}
return SASL_OK;
}
static int ParseOptions(const sasl_utils_t *utils,
char *in, srp_options_t *out, int isserver)
{
int r;
memset(out, 0, sizeof(srp_options_t));
out->maxbufsize = SRP_MAXBUFFERSIZE;
while (in) {
char *opt;
r = ParseOption(utils, in, &opt, &in);
if (r) return r;
if (opt == NULL) return SASL_OK;
utils->log(NULL, SASL_LOG_DEBUG, "Got option: [%s]\n",opt);
r = ParseOptionString(utils, opt, out, isserver);
utils->free(opt);
if (r) return r;
}
return SASL_OK;
}
static layer_option_t *FindBest(int available, sasl_ssf_t min_ssf,
sasl_ssf_t max_ssf, layer_option_t *opts)
{
layer_option_t *best = NULL;
if (!available) return NULL;
while (opts->name) {
if (opts->enabled && (available & opts->bit) &&
(opts->ssf >= min_ssf) && (opts->ssf <= max_ssf) &&
(!best || (opts->ssf > best->ssf))) {
best = opts;
}
opts++;
}
return best;
}
static int OptionsToString(const sasl_utils_t *utils,
srp_options_t *opts, char **out)
{
char *ret = NULL;
int alloced = 0;
int first = 1;
layer_option_t *optlist;
ret = utils->malloc(1);
if (!ret) return SASL_NOMEM;
alloced = 1;
ret[0] = '\0';
optlist = digest_options;
while(optlist->name) {
if (opts->mda & optlist->bit) {
alloced += strlen(OPTION_MDA)+strlen(optlist->name)+1;
ret = utils->realloc(ret, alloced);
if (!ret) return SASL_NOMEM;
if (!first) strcat(ret, ",");
strcat(ret, OPTION_MDA);
strcat(ret, optlist->name);
first = 0;
}
optlist++;
}
if (opts->replay_detection) {
alloced += strlen(OPTION_REPLAY_DETECTION)+1;
ret = utils->realloc(ret, alloced);
if (!ret) return SASL_NOMEM;
if (!first) strcat(ret, ",");
strcat(ret, OPTION_REPLAY_DETECTION);
first = 0;
}
optlist = digest_options;
while(optlist->name) {
if (opts->integrity & optlist->bit) {
alloced += strlen(OPTION_INTEGRITY)+5+strlen(optlist->name)+1;
ret = utils->realloc(ret, alloced);
if (!ret) return SASL_NOMEM;
if (!first) strcat(ret, ",");
strcat(ret, OPTION_INTEGRITY);
strcat(ret, "HMAC-");
strcat(ret, optlist->name);
first = 0;
}
optlist++;
}
optlist = cipher_options;
while(optlist->name) {
if (opts->confidentiality & optlist->bit) {
alloced += strlen(OPTION_CONFIDENTIALITY)+strlen(optlist->name)+1;
ret = utils->realloc(ret, alloced);
if (!ret) return SASL_NOMEM;
if (!first) strcat(ret, ",");
strcat(ret, OPTION_CONFIDENTIALITY);
strcat(ret, optlist->name);
first = 0;
}
optlist++;
}
if ((opts->integrity || opts->confidentiality) &&
opts->maxbufsize < SRP_MAXBUFFERSIZE) {
alloced += strlen(OPTION_MAXBUFFERSIZE)+10+1;
ret = utils->realloc(ret, alloced);
if (!ret) return SASL_NOMEM;
if (!first) strcat(ret, ",");
strcat(ret, OPTION_MAXBUFFERSIZE);
sprintf(ret+strlen(ret), "%lu", opts->maxbufsize);
first = 0;
}
if (opts->mandatory & BIT_REPLAY_DETECTION) {
alloced += strlen(OPTION_MANDATORY)+strlen(OPTION_REPLAY_DETECTION)+1;
ret = utils->realloc(ret, alloced);
if (!ret) return SASL_NOMEM;
if (!first) strcat(ret, ",");
strcat(ret, OPTION_MANDATORY);
strcat(ret, OPTION_REPLAY_DETECTION);
first = 0;
}
if (opts->mandatory & BIT_INTEGRITY) {
alloced += strlen(OPTION_MANDATORY)+strlen(OPTION_INTEGRITY)-1+1;
ret = utils->realloc(ret, alloced);
if (!ret) return SASL_NOMEM;
if (!first) strcat(ret, ",");
strcat(ret, OPTION_MANDATORY);
strncat(ret, OPTION_INTEGRITY, strlen(OPTION_INTEGRITY)-1);
ret[alloced-1] = '\0';
first = 0;
}
if (opts->mandatory & BIT_CONFIDENTIALITY) {
alloced += strlen(OPTION_MANDATORY)+strlen(OPTION_CONFIDENTIALITY)-1+1;
ret = utils->realloc(ret, alloced);
if (!ret) return SASL_NOMEM;
if (!first) strcat(ret, ",");
strcat(ret, OPTION_MANDATORY);
strncat(ret, OPTION_CONFIDENTIALITY, strlen(OPTION_CONFIDENTIALITY)-1);
ret[alloced-1] = '\0';
first = 0;
}
*out = ret;
return SASL_OK;
}
static int SetMDA(srp_options_t *opts, context_t *text)
{
layer_option_t *opt;
opt = FindOptionFromBit(opts->mda, digest_options);
if (!opt) {
text->utils->log(NULL, SASL_LOG_ERR,
"Unable to find SRP MDA option now\n");
return SASL_FAIL;
}
text->md = EVP_get_digestbyname(opt->evp_name);
return SASL_OK;
}
static int LayerInit(srp_options_t *opts, context_t *text,
sasl_out_params_t *oparams, char *enc_IV, char *dec_IV,
unsigned maxbufsize)
{
layer_option_t *opt;
if ((opts->integrity == 0) && (opts->confidentiality == 0)) {
oparams->encode = NULL;
oparams->decode = NULL;
oparams->mech_ssf = 0;
text->utils->log(NULL, SASL_LOG_DEBUG, "Using no protection\n");
return SASL_OK;
}
oparams->encode = &srp_encode;
oparams->decode = &srp_decode;
oparams->maxoutbuf = opts->maxbufsize - 4;
_plug_decode_init(&text->decode_context, text->utils, maxbufsize);
if (opts->replay_detection) {
text->utils->log(NULL, SASL_LOG_DEBUG, "Using replay detection\n");
text->layer |= BIT_REPLAY_DETECTION;
if (!opts->integrity)
opts->integrity = default_digest->bit;
}
if (opts->integrity) {
text->utils->log(NULL, SASL_LOG_DEBUG, "Using integrity protection\n");
text->layer |= BIT_INTEGRITY;
opt = FindOptionFromBit(opts->integrity, digest_options);
if (!opt) {
text->utils->log(NULL, SASL_LOG_ERR,
"Unable to find SRP integrity layer option\n");
return SASL_FAIL;
}
oparams->mech_ssf = opt->ssf;
text->hmac_md = EVP_get_digestbyname(opt->evp_name);
HMAC_Init(&text->hmac_send_ctx, text->K, text->Klen, text->hmac_md);
HMAC_Init(&text->hmac_recv_ctx, text->K, text->Klen, text->hmac_md);
oparams->maxoutbuf -= EVP_MD_size(text->hmac_md);
}
if (opts->confidentiality) {
text->utils->log(NULL, SASL_LOG_DEBUG,
"Using confidentiality protection\n");
text->layer |= BIT_CONFIDENTIALITY;
opt = FindOptionFromBit(opts->confidentiality, cipher_options);
if (!opt) {
text->utils->log(NULL, SASL_LOG_ERR,
"Unable to find SRP confidentiality layer option\n");
return SASL_FAIL;
}
oparams->mech_ssf = opt->ssf;
text->cipher = EVP_get_cipherbyname(opt->evp_name);
EVP_CIPHER_CTX_init(&text->cipher_enc_ctx);
EVP_EncryptInit(&text->cipher_enc_ctx, text->cipher, text->K, enc_IV);
EVP_CIPHER_CTX_init(&text->cipher_dec_ctx);
EVP_DecryptInit(&text->cipher_dec_ctx, text->cipher, text->K, dec_IV);
}
return SASL_OK;
}
static void LayerCleanup(context_t *text)
{
if (text->layer & BIT_INTEGRITY) {
HMAC_cleanup(&text->hmac_send_ctx);
HMAC_cleanup(&text->hmac_recv_ctx);
}
if (text->layer & BIT_CONFIDENTIALITY) {
EVP_CIPHER_CTX_cleanup(&text->cipher_enc_ctx);
EVP_CIPHER_CTX_cleanup(&text->cipher_dec_ctx);
}
}
static void srp_common_mech_dispose(void *conn_context,
const sasl_utils_t *utils)
{
context_t *text = (context_t *) conn_context;
if (!text) return;
BN_clear_free(&text->N);
BN_clear_free(&text->g);
BN_clear_free(&text->v);
BN_clear_free(&text->b);
BN_clear_free(&text->B);
BN_clear_free(&text->a);
BN_clear_free(&text->A);
if (text->authid)		utils->free(text->authid);
if (text->userid)		utils->free(text->userid);
if (text->free_password)	_plug_free_secret(utils, &(text->password));
if (text->salt)		utils->free(text->salt);
if (text->client_options)	utils->free(text->client_options);
if (text->server_options)	utils->free(text->server_options);
LayerCleanup(text);
_plug_decode_free(&text->decode_context);
if (text->encode_buf)	utils->free(text->encode_buf);
if (text->decode_buf)	utils->free(text->decode_buf);
if (text->decode_pkt_buf)	utils->free(text->decode_pkt_buf);
if (text->out_buf)		utils->free(text->out_buf);
utils->free(text);
}
static void
srp_common_mech_free(void *global_context __attribute__((unused)),
const sasl_utils_t *utils __attribute__((unused)))
{
}
static int generate_N_and_g(BIGNUM *N, BIGNUM *g)
{
int result;
BN_init(N);
result = BN_hex2bn(&N, Ng_tab[NUM_Ng-1].N);
if (!result) return SASL_FAIL;
BN_init(g);
BN_set_word(g, Ng_tab[NUM_Ng-1].g);
return SASL_OK;
}
static int CalculateV(context_t *text,
BIGNUM *N, BIGNUM *g,
const char *user,
const char *pass, unsigned passlen,
BIGNUM *v, char **salt, int *saltlen)
{
BIGNUM x;
BN_CTX *ctx = BN_CTX_new();
int r;
*saltlen = SRP_MAXBLOCKSIZE;
*salt = (char *)text->utils->malloc(*saltlen);
if (!*salt) return SASL_NOMEM;
text->utils->rand(text->utils->rpool, *salt, *saltlen);
r = CalculateX(text, *salt, *saltlen, user, pass, passlen, &x);
if (r) {
text->utils->seterror(text->utils->conn, 0,
"Error calculating 'x'");
return r;
}
BN_init(v);
BN_mod_exp(v, g, &x, N, ctx);
BN_CTX_free(ctx);
BN_clear_free(&x);
return r;
}
static int CalculateB(context_t *text  __attribute__((unused)),
BIGNUM *v, BIGNUM *N, BIGNUM *g, BIGNUM *b, BIGNUM *B)
{
BIGNUM v3;
BN_CTX *ctx = BN_CTX_new();
GetRandBigInt(b);
BN_add_word(b, BN_num_bits(N));
BN_init(&v3);
BN_set_word(&v3, 3);
BN_mod_mul(&v3, &v3, v, N, ctx);
BN_init(B);
BN_mod_exp(B, g, b, N, ctx);
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
BN_mod_add(B, B, &v3, N, ctx);
#else
BN_add(B, B, &v3);
BN_mod(B, B, N, ctx);
#endif
BN_CTX_free(ctx);
return SASL_OK;
}
static int ServerCalculateK(context_t *text, BIGNUM *v,
BIGNUM *N, BIGNUM *A, BIGNUM *b, BIGNUM *B,
char *K, int *Klen)
{
unsigned char hash[EVP_MAX_MD_SIZE];
int hashlen;
BIGNUM u;
BIGNUM base;
BIGNUM S;
BN_CTX *ctx = BN_CTX_new();
int r;
r = MakeHash(text->md, hash, &hashlen, "%m%m", A, B);
if (r) return r;
BN_init(&u);
BN_bin2bn(hash, hashlen, &u);
BN_init(&base);
BN_mod_exp(&base, v, &u, N, ctx);
BN_mod_mul(&base, &base, A, N, ctx);
BN_init(&S);
BN_mod_exp(&S, &base, b, N, ctx);
if (BN_is_one(&base)) {
SETERROR(text->utils, "Unsafe SRP value for 'Av^u'\n");
r = SASL_BADPROT;
goto err;
}
BN_add_word(&base, 1);
if (BN_cmp(&S, N) == 0) {
SETERROR(text->utils, "Unsafe SRP value for 'Av^u'\n");
r = SASL_BADPROT;
goto err;
}
r = MakeHash(text->md, K, Klen, "%m", &S);
if (r) goto err;
r = SASL_OK;
err:
BN_CTX_free(ctx);
BN_clear_free(&u);
BN_clear_free(&base);
BN_clear_free(&S);
return r;
}
static int ParseUserSecret(const sasl_utils_t *utils,
char *secret, size_t seclen,
char **mda, BIGNUM *v, char **salt, int *saltlen)
{
int r;
r = utils->decode64(secret, seclen, secret, seclen, &seclen);
if (!r)
r = UnBuffer(utils, secret, seclen, "%s%m%o", mda, v, saltlen, salt);
if (r) {
utils->seterror(utils->conn, 0,
"Error UnBuffering user secret");
}
return r;
}
static int CreateServerOptions(sasl_server_params_t *sparams, char **out)
{
srp_options_t opts;
sasl_ssf_t limitssf, requiressf;
layer_option_t *optlist;
memset(&opts,0,sizeof(srp_options_t));
opts.mda = server_mda->bit;
if(sparams->props.maxbufsize == 0) {
limitssf = 0;
requiressf = 0;
} else {
if (sparams->props.max_ssf < sparams->external_ssf) {
limitssf = 0;
} else {
limitssf = sparams->props.max_ssf - sparams->external_ssf;
}
if (sparams->props.min_ssf < sparams->external_ssf) {
requiressf = 0;
} else {
requiressf = sparams->props.min_ssf - sparams->external_ssf;
}
}
if (default_digest->enabled) {
optlist = digest_options;
while(optlist->name) {
if (optlist->enabled &&
(limitssf >= 1)) {
opts.integrity |= optlist->bit;
}
optlist++;
}
}
if (opts.integrity) {
opts.replay_detection = 1;
}
if (default_cipher->enabled) {
optlist = cipher_options;
while(optlist->name) {
if (optlist->enabled &&
(requiressf <= optlist->ssf) &&
(limitssf >= optlist->ssf)) {
opts.confidentiality |= optlist->bit;
}
optlist++;
}
}
if (requiressf >= 1)
opts.mandatory = BIT_REPLAY_DETECTION | BIT_INTEGRITY;
if (requiressf > 1)
opts.mandatory |= BIT_CONFIDENTIALITY;
opts.maxbufsize = SRP_MAXBUFFERSIZE;
if (sparams->props.maxbufsize &&
sparams->props.maxbufsize < opts.maxbufsize)
opts.maxbufsize = sparams->props.maxbufsize;
return OptionsToString(sparams->utils, &opts, out);
}
static int
srp_server_mech_new(void *glob_context __attribute__((unused)),
sasl_server_params_t *params,
const char *challenge __attribute__((unused)),
unsigned challen __attribute__((unused)),
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
text->md = EVP_get_digestbyname(server_mda->evp_name);
*conn_context = text;
return SASL_OK;
}
static int srp_server_mech_step1(context_t *text,
sasl_server_params_t *params,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams)
{
int result;
char *sid = NULL;
char *cn = NULL;
int cnlen;
char *realm = NULL;
char *user = NULL;
const char *password_request[] = { "*cmusaslsecretSRP",
SASL_AUX_PASSWORD,
NULL };
struct propval auxprop_values[3];
result = UnBuffer(params->utils, clientin, clientinlen,
"%s%s%s%o", &text->authid, &text->userid, &sid,
&cnlen, &cn);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error UnBuffering input in step 1");
return result;
}
result = _plug_parseuser(params->utils, &user, &realm, params->user_realm,
params->serverFQDN, text->authid);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error getting realm");
goto cleanup;
}
result = generate_N_and_g(&text->N, &text->g);
if (result) {
params->utils->seterror(text->utils->conn, 0,
"Error calculating N and g");
return result;
}
result = params->utils->prop_request(params->propctx, password_request);
if (result != SASL_OK) goto cleanup;
result = params->canon_user(params->utils->conn,
text->authid, 0, SASL_CU_AUTHID, oparams);
if (result != SASL_OK) goto cleanup;
result = params->canon_user(params->utils->conn,
text->userid, 0, SASL_CU_AUTHZID, oparams);
if (result != SASL_OK) goto cleanup;
result = params->utils->prop_getnames(params->propctx, password_request,
auxprop_values);
if (result < 0 ||
((!auxprop_values[0].name || !auxprop_values[0].values) &&
(!auxprop_values[1].name || !auxprop_values[1].values))) {
params->utils->seterror(params->utils->conn,0,
"no secret in database");
result = params->transition ? SASL_TRANS : SASL_NOUSER;
goto cleanup;
}
if (auxprop_values[0].name && auxprop_values[0].values) {
char *mda = NULL;
result = ParseUserSecret(params->utils,
(char*) auxprop_values[0].values[0],
auxprop_values[0].valsize,
&mda, &text->v, &text->salt, &text->saltlen);
if (result) {
if (mda) params->utils->free(mda);
goto cleanup;
}
server_mda = digest_options;
while (server_mda->name) {
if (!strcasecmp(server_mda->name, mda))
break;
server_mda++;
}
if (!server_mda->name) {
params->utils->seterror(params->utils->conn, 0,
"unknown SRP mda '%s'", mda);
params->utils->free(mda);
result = SASL_FAIL;
goto cleanup;
}
params->utils->free(mda);
} else if (auxprop_values[1].name && auxprop_values[1].values) {
int len = strlen(auxprop_values[1].values[0]);
if (len == 0) {
params->utils->seterror(params->utils->conn,0,
"empty secret");
result = SASL_FAIL;
goto cleanup;
}
result = CalculateV(text, &text->N, &text->g, text->authid,
auxprop_values[1].values[0], len,
&text->v, &text->salt, &text->saltlen);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error calculating v");
goto cleanup;
}
} else {
params->utils->seterror(params->utils->conn, 0,
"Have neither type of secret");
result = SASL_FAIL;
goto cleanup;
}
params->utils->prop_erase(params->propctx, password_request[1]);
result = CalculateB(text, &text->v, &text->N, &text->g,
&text->b, &text->B);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error calculating B");
return result;
}
result = CreateServerOptions(params, &text->server_options);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error creating server options");
goto cleanup;
}
result = MakeBuffer(text->utils, &text->out_buf, &text->out_buf_len,
serveroutlen, "%c%m%m%o%m%s",
0x00, &text->N, &text->g, text->saltlen, text->salt,
&text->B, text->server_options);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error creating SRP buffer from data in step 1");
goto cleanup;
}
*serverout = text->out_buf;
text->state = 2;
result = SASL_CONTINUE;
cleanup:
if (sid) params->utils->free(sid);
if (cn) params->utils->free(cn);
if (user) params->utils->free(user);
if (realm) params->utils->free(realm);
return result;
}
static int srp_server_mech_step2(context_t *text,
sasl_server_params_t *params,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams)
{
int result;
char *M1 = NULL, *cIV = NULL;
int M1len, cIVlen;
srp_options_t client_opts;
char myM1[EVP_MAX_MD_SIZE];
int myM1len;
int i;
char M2[EVP_MAX_MD_SIZE];
int M2len;
char sIV[SRP_MAXBLOCKSIZE];
result = UnBuffer(params->utils, clientin, clientinlen,
"%m%-o%s%-o", &text->A, &M1len, &M1,
&text->client_options, &cIVlen, &cIV);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error UnBuffering input in step 2");
goto cleanup;
}
if (BigIntCmpWord(&text->A, 0) <= 0) {
SETERROR(params->utils, "Illegal value for 'A'\n");
result = SASL_BADPROT;
goto cleanup;
}
result = ParseOptions(params->utils, text->client_options, &client_opts, 1);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error parsing user's options");
if (client_opts.confidentiality) {
oparams->mech_ssf = 2;
}
else if (client_opts.integrity || client_opts.replay_detection) {
oparams->mech_ssf = 1;
}
return result;
}
result = SetMDA(&client_opts, text);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error setting options");
return result;
}
result = ServerCalculateK(text, &text->v, &text->N, &text->A,
&text->b, &text->B, text->K, &text->Klen);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error calculating K");
return result;
}
result = CalculateM1(text, &text->N, &text->g, text->authid,
text->salt, text->saltlen, &text->A, &text->B,
text->K, text->Klen, text->userid,
text->server_options, myM1, &myM1len);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error calculating M1");
goto cleanup;
}
if (myM1len != M1len) {
params->utils->seterror(params->utils->conn, 0,
"SRP M1 lengths do not match");
result = SASL_BADAUTH;
goto cleanup;
}
for (i = 0; i < myM1len; i++) {
if (myM1[i] != M1[i]) {
params->utils->seterror(params->utils->conn, 0,
"client evidence does not match what we "
"calculated. Probably a password error");
result = SASL_BADAUTH;
goto cleanup;
}
}
result = CalculateM2(text, &text->A, M1, M1len, text->K, text->Klen,
text->userid, text->client_options, "", 0,
M2, &M2len);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error calculating M2 (server evidence)");
goto cleanup;
}
text->utils->rand(text->utils->rpool, sIV, sizeof(sIV));
result = MakeBuffer(text->utils, &text->out_buf, &text->out_buf_len,
serveroutlen, "%o%o%s%u", M2len, M2,
sizeof(sIV), sIV, "", 0);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error making output buffer in SRP step 3");
goto cleanup;
}
*serverout = text->out_buf;
result = LayerInit(&client_opts, text, oparams, cIV, sIV,
params->props.maxbufsize);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error initializing security layer");
return result;
}
oparams->doneflag = 1;
oparams->param_version = 0;
result = SASL_OK;
cleanup:
return result;
}
static int srp_server_mech_step(void *conn_context,
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
*serverout = NULL;
*serveroutlen = 0;
if (text == NULL) {
return SASL_BADPROT;
}
sparams->utils->log(NULL, SASL_LOG_DEBUG,
"SRP server step %d\n", text->state);
switch (text->state) {
case 1:
return srp_server_mech_step1(text, sparams, clientin, clientinlen,
serverout, serveroutlen, oparams);
case 2:
return srp_server_mech_step2(text, sparams, clientin, clientinlen,
serverout, serveroutlen, oparams);
default:
sparams->utils->seterror(sparams->utils->conn, 0,
"Invalid SRP server step %d", text->state);
return SASL_FAIL;
}
return SASL_FAIL;
}
#ifdef DO_SRP_SETPASS
static int srp_setpass(void *glob_context __attribute__((unused)),
sasl_server_params_t *sparams,
const char *userstr,
const char *pass,
unsigned passlen __attribute__((unused)),
const char *oldpass __attribute__((unused)),
unsigned oldpasslen __attribute__((unused)),
unsigned flags)
{
int r;
char *user = NULL;
char *user_only = NULL;
char *realm = NULL;
sasl_secret_t *sec = NULL;
struct propctx *propctx = NULL;
const char *store_request[] = { "cmusaslsecretSRP",
NULL };
if (!sparams->utils->auxprop_store ||
sparams->utils->auxprop_store(NULL, NULL, NULL) != SASL_OK) {
SETERROR(sparams->utils, "SRP: auxprop backend can't store properties");
return SASL_NOMECH;
}
r = _plug_parseuser(sparams->utils, &user_only, &realm, sparams->user_realm,
sparams->serverFQDN, userstr);
if (r) {
sparams->utils->seterror(sparams->utils->conn, 0,
"Error parsing user");
return r;
}
r = _plug_make_fulluser(sparams->utils, &user, user_only, realm);
if (r) {
goto end;
}
if ((flags & SASL_SET_DISABLE) || pass == NULL) {
sec = NULL;
} else {
context_t *text;
BIGNUM N;
BIGNUM g;
BIGNUM v;
char *salt;
int saltlen;
char *buffer = NULL;
int bufferlen, alloclen, encodelen;
text = sparams->utils->malloc(sizeof(context_t));
if (text == NULL) {
MEMERROR(sparams->utils);
return SASL_NOMEM;
}
memset(text, 0, sizeof(context_t));
text->utils = sparams->utils;
text->md = EVP_get_digestbyname(server_mda->evp_name);
r = generate_N_and_g(&N, &g);
if (r) {
sparams->utils->seterror(sparams->utils->conn, 0,
"Error calculating N and g");
goto end;
}
r = CalculateV(text, &N, &g, user, pass, passlen, &v, &salt, &saltlen);
if (r) {
sparams->utils->seterror(sparams->utils->conn, 0,
"Error calculating v");
goto end;
}
r = MakeBuffer(text->utils, &text->out_buf, &text->out_buf_len,
&bufferlen, "%s%m%o",
server_mda->name, &v, saltlen, salt);
if (r) {
sparams->utils->seterror(sparams->utils->conn, 0,
"Error making buffer for secret");
goto end;
}
buffer = text->out_buf;
alloclen = (bufferlen/3 + 1) * 4 + 1;
sec = sparams->utils->malloc(sizeof(sasl_secret_t)+alloclen);
if (!sec) {
r = SASL_NOMEM;
goto end;
}
sparams->utils->encode64(buffer, bufferlen, sec->data, alloclen,
&encodelen);
sec->len = encodelen;
end:
if (buffer) sparams->utils->free((void *) buffer);
BN_clear_free(&N);
BN_clear_free(&g);
BN_clear_free(&v);
sparams->utils->free(text);
if (r) return r;
}
propctx = sparams->utils->prop_new(0);
if (!propctx)
r = SASL_FAIL;
if (!r)
r = sparams->utils->prop_request(propctx, store_request);
if (!r)
r = sparams->utils->prop_set(propctx, "cmusaslsecretSRP",
(sec ? sec->data : NULL),
(sec ? sec->len : 0));
if (!r)
r = sparams->utils->auxprop_store(sparams->utils->conn, propctx, user);
if (propctx)
sparams->utils->prop_dispose(&propctx);
if (r) {
sparams->utils->seterror(sparams->utils->conn, 0,
"Error putting SRP secret");
goto cleanup;
}
sparams->utils->log(NULL, SASL_LOG_DEBUG, "Setpass for SRP successful\n");
cleanup:
if (user) 	_plug_free_string(sparams->utils, &user);
if (user_only) 	_plug_free_string(sparams->utils, &user_only);
if (realm) 	_plug_free_string(sparams->utils, &realm);
if (sec)    _plug_free_secret(sparams->utils, &sec);
return r;
}
#endif
static int srp_mech_avail(void *glob_context __attribute__((unused)),
sasl_server_params_t *sparams,
void **conn_context __attribute__((unused)))
{
if (!server_mda || !server_mda->enabled) {
SETERROR(sparams->utils,
"SRP unavailable due to selected MDA unavailable");
return SASL_NOMECH;
}
return SASL_OK;
}
static sasl_server_plug_t srp_server_plugins[] =
{
{
"SRP",
0,
SASL_SEC_NOPLAINTEXT
| SASL_SEC_NOANONYMOUS
| SASL_SEC_NOACTIVE
| SASL_SEC_NODICTIONARY
| SASL_SEC_FORWARD_SECRECY
| SASL_SEC_MUTUAL_AUTH,
SASL_FEAT_WANT_CLIENT_FIRST
| SASL_FEAT_ALLOWS_PROXY,
NULL,
&srp_server_mech_new,
&srp_server_mech_step,
&srp_common_mech_dispose,
&srp_common_mech_free,
#ifdef DO_SRP_SETPASS
&srp_setpass,
#else
NULL,
#endif
NULL,
NULL,
&srp_mech_avail,
NULL
}
};
int srp_server_plug_init(const sasl_utils_t *utils,
int maxversion,
int *out_version,
const sasl_server_plug_t **pluglist,
int *plugcount,
const char *plugname __attribute__((unused)))
{
const char *mda;
unsigned int len;
layer_option_t *opts;
if (maxversion < SASL_SERVER_PLUG_VERSION) {
SETERROR(utils, "SRP version mismatch");
return SASL_BADVERS;
}
utils->getopt(utils->getopt_context, "SRP", "srp_mda", &mda, &len);
if (!mda) mda = DEFAULT_MDA;
OpenSSL_add_all_algorithms();
opts = digest_options;
while (opts->name) {
if (EVP_get_digestbyname(opts->evp_name)) {
opts->enabled = 1;
srp_server_plugins[0].max_ssf = opts->ssf;
}
if (!strcasecmp(opts->name, mda) || !strcasecmp(opts->evp_name, mda)) {
server_mda = opts;
}
opts++;
}
opts = cipher_options;
while (opts->name) {
if (EVP_get_cipherbyname(opts->evp_name)) {
opts->enabled = 1;
if (opts->ssf > srp_server_plugins[0].max_ssf) {
srp_server_plugins[0].max_ssf = opts->ssf;
}
}
opts++;
}
*out_version = SASL_SERVER_PLUG_VERSION;
*pluglist = srp_server_plugins;
*plugcount = 1;
return SASL_OK;
}
static int check_N_and_g(const sasl_utils_t *utils, BIGNUM *N, BIGNUM *g)
{
char *N_prime;
unsigned long g_prime;
unsigned i;
int r = SASL_FAIL;
N_prime = BN_bn2hex(N);
g_prime = BN_get_word(g);
for (i = 0; i < NUM_Ng; i++) {
if (!strcasecmp(N_prime, Ng_tab[i].N) && (g_prime == Ng_tab[i].g)) {
r = SASL_OK;
break;
}
}
if (N_prime) utils->free(N_prime);
return r;
}
static int CalculateA(context_t *text  __attribute__((unused)),
BIGNUM *N, BIGNUM *g, BIGNUM *a, BIGNUM *A)
{
BN_CTX *ctx = BN_CTX_new();
GetRandBigInt(a);
BN_add_word(a, BN_num_bits(N));
BN_init(A);
BN_mod_exp(A, g, a, N, ctx);
BN_CTX_free(ctx);
return SASL_OK;
}
static int ClientCalculateK(context_t *text, char *salt, int saltlen,
char *user, char *pass, int passlen,
BIGNUM *N, BIGNUM *g, BIGNUM *a, BIGNUM *A,
BIGNUM *B, char *K, int *Klen)
{
int r;
unsigned char hash[EVP_MAX_MD_SIZE];
int hashlen;
BIGNUM x;
BIGNUM u;
BIGNUM aux;
BIGNUM gx;
BIGNUM gx3;
BIGNUM base;
BIGNUM S;
BN_CTX *ctx = BN_CTX_new();
r = MakeHash(text->md, hash, &hashlen, "%m%m", A, B);
if (r) goto err;
BN_init(&u);
BN_bin2bn(hash, hashlen, &u);
if (BN_is_zero(&u)) {
SETERROR(text->utils, "SRP: Illegal value for 'u'\n");
r = SASL_BADPROT;
goto err;
}
r = CalculateX(text, salt, saltlen, user, pass, passlen, &x);
if (r) return r;
BN_init(&aux);
BN_mul(&aux, &u, &x, ctx);
BN_add(&aux, &aux, a);
BN_init(&gx);
BN_mod_exp(&gx, g, &x, N, ctx);
BN_init(&gx3);
BN_set_word(&gx3, 3);
BN_mod_mul(&gx3, &gx3, &gx, N, ctx);
BN_init(&base);
#if OPENSSL_VERSION_NUMBER >= 0x00907000L
BN_mod_sub(&base, B, &gx3, N, ctx);
#else
BN_sub(&base, B, &gx3);
BN_mod(&base, &base, N, ctx);
if (BigIntCmpWord(&base, 0) < 0) {
BN_add(&base, &base, N);
}
#endif
BN_init(&S);
BN_mod_exp(&S, &base, &aux, N, ctx);
r = MakeHash(text->md, K, Klen, "%m", &S);
if (r) goto err;
r = SASL_OK;
err:
BN_CTX_free(ctx);
BN_clear_free(&x);
BN_clear_free(&u);
BN_clear_free(&aux);
BN_clear_free(&gx);
BN_clear_free(&gx3);
BN_clear_free(&base);
BN_clear_free(&S);
return r;
}
static int CreateClientOpts(sasl_client_params_t *params,
srp_options_t *available,
srp_options_t *out)
{
layer_option_t *opt;
sasl_ssf_t external;
sasl_ssf_t limit;
sasl_ssf_t musthave;
memset(out, 0, sizeof(srp_options_t));
params->utils->log(NULL, SASL_LOG_DEBUG,
"Available MDA = %d\n", available->mda);
opt = FindBest(available->mda, 0, 256, digest_options);
if (opt) {
out->mda = opt->bit;
}
else {
SETERROR(params->utils, "Can't find an acceptable SRP MDA\n");
return SASL_BADAUTH;
}
external = params->external_ssf;
if(params->props.maxbufsize == 0) {
musthave = 0;
limit = 0;
} else {
if (params->props.max_ssf > external) {
limit = params->props.max_ssf - external;
} else {
limit = 0;
}
if (params->props.min_ssf > external) {
musthave = params->props.min_ssf - external;
} else {
musthave = 0;
}
}
params->utils->log(NULL, SASL_LOG_DEBUG,
"Available confidentiality = %d  "
"musthave = %d  limit = %d",
available->confidentiality, musthave, limit);
if (limit > 1) {
opt = FindBest(available->confidentiality, musthave, limit,
cipher_options);
if (opt) {
out->confidentiality = opt->bit;
musthave = 0;
}
else if (musthave > 1) {
SETERROR(params->utils,
"Can't find an acceptable SRP confidentiality layer\n");
return SASL_TOOWEAK;
}
}
params->utils->log(NULL, SASL_LOG_DEBUG,
"Available integrity = %d "
"musthave = %d  limit = %d",
available->integrity, musthave, limit);
if ((limit >= 1) && (musthave <= 1)) {
opt = FindBest(available->integrity, musthave, limit,
digest_options);
if (opt) {
out->integrity = opt->bit;
out->replay_detection = available->replay_detection;
}
else if (musthave > 0) {
SETERROR(params->utils,
"Can't find an acceptable SRP integrity layer\n");
return SASL_TOOWEAK;
}
}
params->utils->log(NULL, SASL_LOG_DEBUG,
"Mandatory layers = %d\n",available->mandatory);
if ((!out->replay_detection &&
(available->mandatory & BIT_REPLAY_DETECTION)) ||
(!out->integrity &&
(available->mandatory & BIT_INTEGRITY)) ||
(!out->confidentiality &&
(available->mandatory & BIT_CONFIDENTIALITY))) {
SETERROR(params->utils, "Mandatory SRP layer not supported\n");
return SASL_BADAUTH;
}
out->maxbufsize = SRP_MAXBUFFERSIZE;
if (params->props.maxbufsize && params->props.maxbufsize < out->maxbufsize)
out->maxbufsize = params->props.maxbufsize;
return SASL_OK;
}
static int srp_client_mech_new(void *glob_context __attribute__((unused)),
sasl_client_params_t *params,
void **conn_context)
{
context_t *text;
text = params->utils->malloc(sizeof(context_t));
if (text == NULL) {
MEMERROR( params->utils );
return SASL_NOMEM;
}
memset(text, 0, sizeof(context_t));
text->state = 1;
text->utils = params->utils;
*conn_context = text;
return SASL_OK;
}
static int
srp_client_mech_step1(context_t *text,
sasl_client_params_t *params,
const char *serverin __attribute__((unused)),
unsigned serverinlen,
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
const char *authid = NULL, *userid = NULL;
int auth_result = SASL_OK;
int pass_result = SASL_OK;
int user_result = SASL_OK;
int result;
if (serverinlen > 0) {
SETERROR(params->utils, "Invalid input to first step of SRP\n");
return SASL_BADPROT;
}
if (oparams->authid==NULL) {
auth_result = _plug_get_authid(params->utils, &authid, prompt_need);
if ((auth_result != SASL_OK) && (auth_result != SASL_INTERACT))
return auth_result;
}
if (oparams->user == NULL) {
user_result = _plug_get_userid(params->utils, &userid, prompt_need);
if ((user_result != SASL_OK) && (user_result != SASL_INTERACT))
return user_result;
}
if (text->password == NULL) {
pass_result=_plug_get_password(params->utils, &text->password,
&text->free_password, prompt_need);
if ((pass_result != SASL_OK) && (pass_result != SASL_INTERACT))
return pass_result;
}
if (prompt_need && *prompt_need) {
params->utils->free(*prompt_need);
*prompt_need = NULL;
}
if ((auth_result == SASL_INTERACT) || (user_result == SASL_INTERACT) ||
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
if (result != SASL_OK) return result;
return SASL_INTERACT;
}
if (!userid || !*userid) {
result = params->canon_user(params->utils->conn, authid, 0,
SASL_CU_AUTHID | SASL_CU_AUTHZID, oparams);
}
else {
result = params->canon_user(params->utils->conn, authid, 0,
SASL_CU_AUTHID, oparams);
if (result != SASL_OK) return result;
result = params->canon_user(params->utils->conn, userid, 0,
SASL_CU_AUTHZID, oparams);
}
if (result != SASL_OK) return result;
result = MakeBuffer(text->utils, &text->out_buf, &text->out_buf_len,
clientoutlen, "%s%s%s%o",
(char *) oparams->authid, (char *) oparams->user,
"", 0, "");
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
srp_client_mech_step2(context_t *text,
sasl_client_params_t *params,
const char *serverin,
unsigned serverinlen,
sasl_interact_t **prompt_need __attribute__((unused)),
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
int result;
char reuse;
srp_options_t server_opts;
result = UnBuffer(params->utils, serverin, serverinlen,
"%c%m%m%o%m%s", &reuse, &text->N, &text->g,
&text->saltlen, &text->salt, &text->B,
&text->server_options);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error UnBuffering input in step 2");
goto cleanup;
}
result = check_N_and_g(params->utils, &text->N, &text->g);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR,
"Values of 'N' and 'g' are not recommended\n");
goto cleanup;
}
if (BigIntCmpWord(&text->B, 0) <= 0 || BN_cmp(&text->B, &text->N) >= 0) {
SETERROR(params->utils, "Illegal value for 'B'\n");
result = SASL_BADPROT;
goto cleanup;
}
memset(&server_opts, 0, sizeof(srp_options_t));
result = ParseOptions(params->utils, text->server_options, &server_opts, 0);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR,
"Error parsing SRP server options\n");
goto cleanup;
}
result = CreateClientOpts(params, &server_opts, &text->client_opts);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR,
"Error creating client options\n");
goto cleanup;
}
result = OptionsToString(params->utils, &text->client_opts,
&text->client_options);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR,
"Error converting client options to an option string\n");
goto cleanup;
}
result = SetMDA(&text->client_opts, text);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error setting MDA");
goto cleanup;
}
result = CalculateA(text, &text->N, &text->g, &text->a, &text->A);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error calculating A");
return result;
}
result = ClientCalculateK(text, text->salt, text->saltlen,
(char *) oparams->authid,
text->password->data, text->password->len,
&text->N, &text->g, &text->a, &text->A, &text->B,
text->K, &text->Klen);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR,
"Error creating K\n");
goto cleanup;
}
result = CalculateM1(text, &text->N, &text->g, (char *) oparams->authid,
text->salt, text->saltlen, &text->A, &text->B,
text->K, text->Klen, (char *) oparams->user,
text->server_options, text->M1, &text->M1len);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR,
"Error creating M1\n");
goto cleanup;
}
text->utils->rand(text->utils->rpool, text->cIV, sizeof(text->cIV));
result = MakeBuffer(text->utils, &text->out_buf, &text->out_buf_len,
clientoutlen, "%m%o%s%o",
&text->A, text->M1len, text->M1, text->client_options,
sizeof(text->cIV), text->cIV);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR, "Error making output buffer\n");
goto cleanup;
}
*clientout = text->out_buf;
text->state = 3;
result = SASL_CONTINUE;
cleanup:
return result;
}
static int
srp_client_mech_step3(context_t *text,
sasl_client_params_t *params,
const char *serverin,
unsigned serverinlen,
sasl_interact_t **prompt_need __attribute__((unused)),
const char **clientout __attribute__((unused)),
unsigned *clientoutlen __attribute__((unused)),
sasl_out_params_t *oparams)
{
int result;
char *M2 = NULL, *sIV = NULL;
char *sid = NULL;
int M2len, sIVlen;
uint32 ttl;
int i;
char myM2[EVP_MAX_MD_SIZE];
int myM2len;
result = UnBuffer(params->utils, serverin, serverinlen,
"%-o%-o%s%u", &M2len, &M2, &sIVlen, &sIV,
&sid, &ttl);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error UnBuffering input in step 3");
goto cleanup;
}
result = CalculateM2(text, &text->A, text->M1, text->M1len,
text->K, text->Klen, (char *) oparams->user,
text->client_options, "", 0,
myM2, &myM2len);
if (result) {
params->utils->log(NULL, SASL_LOG_ERR,
"Error calculating our own M2 (server evidence)\n");
goto cleanup;
}
if (myM2len != M2len) {
SETERROR(params->utils, "SRP Server M2 length wrong\n");
result = SASL_BADSERV;
goto cleanup;
}
for (i = 0; i < myM2len; i++) {
if (M2[i] != myM2[i]) {
SETERROR(params->utils,
"SRP Server spoof detected. M2 incorrect\n");
result = SASL_BADSERV;
goto cleanup;
}
}
result = LayerInit(&text->client_opts, text, oparams, sIV, text->cIV,
params->props.maxbufsize);
if (result) {
params->utils->seterror(params->utils->conn, 0,
"Error initializing security layer");
return result;
}
oparams->doneflag = 1;
oparams->param_version = 0;
result = SASL_OK;
cleanup:
if (sid) params->utils->free(sid);
return result;
}
static int srp_client_mech_step(void *conn_context,
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
"SRP client step %d\n", text->state);
*clientout = NULL;
*clientoutlen = 0;
switch (text->state) {
case 1:
return srp_client_mech_step1(text, params, serverin, serverinlen,
prompt_need, clientout, clientoutlen,
oparams);
case 2:
return srp_client_mech_step2(text, params, serverin, serverinlen,
prompt_need, clientout, clientoutlen,
oparams);
case 3:
return srp_client_mech_step3(text, params, serverin, serverinlen,
prompt_need, clientout, clientoutlen,
oparams);
default:
params->utils->log(NULL, SASL_LOG_ERR,
"Invalid SRP client step %d\n", text->state);
return SASL_FAIL;
}
return SASL_FAIL;
}
static sasl_client_plug_t srp_client_plugins[] =
{
{
"SRP",
0,
SASL_SEC_NOPLAINTEXT
| SASL_SEC_NOANONYMOUS
| SASL_SEC_NOACTIVE
| SASL_SEC_NODICTIONARY
| SASL_SEC_FORWARD_SECRECY
| SASL_SEC_MUTUAL_AUTH,
SASL_FEAT_WANT_CLIENT_FIRST
| SASL_FEAT_ALLOWS_PROXY,
NULL,
NULL,
&srp_client_mech_new,
&srp_client_mech_step,
&srp_common_mech_dispose,
&srp_common_mech_free,
NULL,
NULL,
NULL
}
};
int srp_client_plug_init(const sasl_utils_t *utils __attribute__((unused)),
int maxversion,
int *out_version,
const sasl_client_plug_t **pluglist,
int *plugcount,
const char *plugname __attribute__((unused)))
{
layer_option_t *opts;
if (maxversion < SASL_CLIENT_PLUG_VERSION) {
SETERROR(utils, "SRP version mismatch");
return SASL_BADVERS;
}
OpenSSL_add_all_algorithms();
opts = digest_options;
while (opts->name) {
if (EVP_get_digestbyname(opts->evp_name)) {
opts->enabled = 1;
srp_client_plugins[0].max_ssf = opts->ssf;
}
opts++;
}
opts = cipher_options;
while (opts->name) {
if (EVP_get_cipherbyname(opts->evp_name)) {
opts->enabled = 1;
if (opts->ssf > srp_client_plugins[0].max_ssf) {
srp_client_plugins[0].max_ssf = opts->ssf;
}
}
opts++;
}
*out_version = SASL_CLIENT_PLUG_VERSION;
*pluglist = srp_client_plugins;
*plugcount=1;
return SASL_OK;
}