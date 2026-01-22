#include <config.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#ifndef macintosh
#include <sys/types.h>
#include <sys/stat.h>
#endif
#include <fcntl.h>
#include <ctype.h>
#ifdef WITH_DES
# ifdef WITH_SSL_DES
# include <openssl/des.h>
# include <openssl/opensslv.h>
# if (OPENSSL_VERSION_NUMBER >= 0x0090700f) && \
!defined(OPENSSL_ENABLE_OLD_DES_SUPPORT)
# define des_cblock DES_cblock
# define des_key_schedule DES_key_schedule
# define des_key_sched(k,ks) \
DES_key_sched((k),&(ks))
# define des_cbc_encrypt(i,o,l,k,iv,e) \
DES_cbc_encrypt((i),(o),(l),&(k),(iv),(e))
# define des_ede2_cbc_encrypt(i,o,l,k1,k2,iv,e) \
DES_ede2_cbc_encrypt((i),(o),(l),&(k1),&(k2),(iv),(e))
# endif
# else
#ifdef HAVE_DES_H
# include <des.h>
#endif
# endif
#endif
#ifdef WIN32
# include <winsock2.h>
#else
# include <netinet/in.h>
#endif
#include <sasl.h>
#include <saslplug.h>
#include "plugin_common.h"
#ifndef WIN32
extern int strcasecmp(const char *s1, const char *s2);
#endif
#ifdef macintosh
#include <sasl_md5_plugin_decl.h>
#endif
#ifdef sun
extern int gethostname(char *, int);
#endif
#define bool int
#ifndef TRUE
#define TRUE (1)
#define FALSE (0)
#endif
#define MAX_UIN32_DIV_10 429496729
#define MAX_UIN32_MOD_10 5
#define DEFAULT_BUFSIZE 0xFFFF
#define MAX_SASL_BUFSIZE 0xFFFFFF
static const char plugin_id[] = "$Id: digestmd5.c,v 1.205 2011/05/13 19:18:37 murch Exp $";
#define NONCE_SIZE (32)
#define DIGEST_NOLAYER (1)
#define DIGEST_INTEGRITY (2)
#define DIGEST_PRIVACY (4)
#define HASHLEN 16
typedef unsigned char HASH[HASHLEN + 1];
#define HASHHEXLEN 32
typedef unsigned char HASHHEX[HASHHEXLEN + 1];
#define MAC_SIZE 10
#define MAC_OFFS 2
const char *SEALING_CLIENT_SERVER="Digest H(A1) to client-to-server sealing key magic constant";
const char *SEALING_SERVER_CLIENT="Digest H(A1) to server-to-client sealing key magic constant";
const char *SIGNING_CLIENT_SERVER="Digest session key to client-to-server signing key magic constant";
const char *SIGNING_SERVER_CLIENT="Digest session key to server-to-client signing key magic constant";
#define HT (9)
#define CR (13)
#define LF (10)
#define SP (32)
#define DEL (127)
#define NEED_ESCAPING "\"\\"
#define REALM_CHAL_PREFIX "Available realms:"
static char *quote (char *str);
struct context;
typedef int cipher_function_t(struct context *,
const char *,
unsigned,
unsigned char[],
char *,
unsigned *);
typedef int cipher_init_t(struct context *, unsigned char [16],
unsigned char [16]);
typedef void cipher_free_t(struct context *);
enum Context_type { SERVER = 0, CLIENT = 1 };
typedef struct cipher_context cipher_context_t;
typedef struct reauth_entry {
char *authid;
char *realm;
unsigned char *nonce;
unsigned int nonce_count;
unsigned char *cnonce;
union {
struct {
time_t timestamp;
} s;
struct {
char *serverFQDN;
int protection;
struct digest_cipher *cipher;
unsigned long server_maxbuf;
char *algorithm;
unsigned char *opaque;
} c;
} u;
} reauth_entry_t;
typedef struct reauth_cache {
enum Context_type i_am;
time_t timeout;
void *mutex;
unsigned size;
reauth_entry_t *e;
} reauth_cache_t;
typedef struct digest_glob_context {
reauth_cache_t *reauth;
} digest_glob_context_t;
typedef struct context {
int state;
enum Context_type i_am;
int http_mode;
reauth_cache_t *reauth;
char *authid;
char *realm;
unsigned char *nonce;
unsigned int nonce_count;
unsigned char *cnonce;
char ** realms;
int realm_cnt;
char *response_value;
unsigned int seqnum;
unsigned int rec_seqnum;
HASH Ki_send;
HASH Ki_receive;
HASH HA1;
const sasl_utils_t *utils;
char *out_buf;
unsigned out_buf_len;
buffer_info_t *enc_in_buf;
char *encode_buf, *decode_buf, *decode_packet_buf;
unsigned encode_buf_len, decode_buf_len, decode_packet_buf_len;
decode_context_t decode_context;
cipher_function_t *cipher_enc;
cipher_function_t *cipher_dec;
cipher_init_t *cipher_init;
cipher_free_t *cipher_free;
struct cipher_context *cipher_enc_context;
struct cipher_context *cipher_dec_context;
} context_t;
struct digest_cipher {
char *name;
sasl_ssf_t ssf;
int n;
int flag;
cipher_function_t *cipher_enc;
cipher_function_t *cipher_dec;
cipher_init_t *cipher_init;
cipher_free_t *cipher_free;
};
#if 0
static const unsigned char *COLON = ":";
#else
static const unsigned char COLON[] = { ':', '\0' };
#endif
static unsigned hash(const char *str)
{
unsigned val = 0;
int i;
while (str && *str) {
i = (int) *str;
val ^= i;
val <<= 1;
str++;
}
return val;
}
static void CvtHex(HASH Bin, HASHHEX Hex)
{
unsigned short i;
unsigned char j;
for (i = 0; i < HASHLEN; i++) {
j = (Bin[i] >> 4) & 0xf;
if (j <= 9)
Hex[i * 2] = (j + '0');
else
Hex[i * 2] = (j + 'a' - 10);
j = Bin[i] & 0xf;
if (j <= 9)
Hex[i * 2 + 1] = (j + '0');
else
Hex[i * 2 + 1] = (j + 'a' - 10);
}
Hex[HASHHEXLEN] = '\0';
}
void
DigestCalcResponse(const sasl_utils_t * utils,
HASHHEX HA1,
unsigned char *pszNonce,
unsigned int pszNonceCount,
unsigned char *pszCNonce,
unsigned char *pszQop,
unsigned char *pszDigestUri,
unsigned char *pszMethod,
HASHHEX HEntity,
HASHHEX Response
)
{
MD5_CTX Md5Ctx;
HASH HA2;
HASH RespHash;
HASHHEX HA2Hex;
unsigned char ncvalue[10];
utils->MD5Init(&Md5Ctx);
if (pszMethod != NULL) {
utils->MD5Update(&Md5Ctx, pszMethod, (unsigned) strlen((char *) pszMethod));
}
utils->MD5Update(&Md5Ctx, (unsigned char *) COLON, 1);
utils->MD5Update(&Md5Ctx, pszDigestUri, (unsigned) strlen((char *) pszDigestUri));
if (strcasecmp((char *) pszQop, "auth") != 0) {
utils->MD5Update(&Md5Ctx, COLON, 1);
utils->MD5Update(&Md5Ctx, HEntity, HASHHEXLEN);
}
utils->MD5Final(HA2, &Md5Ctx);
CvtHex(HA2, HA2Hex);
utils->MD5Init(&Md5Ctx);
utils->MD5Update(&Md5Ctx, HA1, HASHHEXLEN);
utils->MD5Update(&Md5Ctx, COLON, 1);
utils->MD5Update(&Md5Ctx, pszNonce, (unsigned) strlen((char *) pszNonce));
utils->MD5Update(&Md5Ctx, COLON, 1);
if (*pszQop) {
sprintf((char *)ncvalue, "%08x", pszNonceCount);
utils->MD5Update(&Md5Ctx, ncvalue, (unsigned) strlen((char *)ncvalue));
utils->MD5Update(&Md5Ctx, COLON, 1);
utils->MD5Update(&Md5Ctx, pszCNonce, (unsigned) strlen((char *) pszCNonce));
utils->MD5Update(&Md5Ctx, COLON, 1);
utils->MD5Update(&Md5Ctx, pszQop, (unsigned) strlen((char *) pszQop));
utils->MD5Update(&Md5Ctx, COLON, 1);
}
utils->MD5Update(&Md5Ctx, HA2Hex, HASHHEXLEN);
utils->MD5Final(RespHash, &Md5Ctx);
CvtHex(RespHash, Response);
}
static bool UTF8_In_8859_1(const unsigned char *base, size_t len)
{
const unsigned char *scan, *end;
end = base + len;
for (scan = base; scan < end; ++scan) {
if (*scan > 0xC3)
break;
if (*scan >= 0xC0 && *scan <= 0xC3) {
if (++scan == end || *scan < 0x80 || *scan > 0xBF)
break;
}
}
return (scan >= end);
}
static void MD5_UTF8_8859_1(const sasl_utils_t * utils,
MD5_CTX * ctx,
bool In_ISO_8859_1,
const unsigned char *base,
int len)
{
const unsigned char *scan, *end;
unsigned char cbuf;
end = base + len;
if (!In_ISO_8859_1) {
utils->MD5Update(ctx, base, len);
return;
}
do {
for (scan = base; scan < end && *scan < 0xC0; ++scan);
if (scan != base)
utils->MD5Update(ctx, base, (unsigned) (scan - base));
if (scan + 1 >= end)
break;
cbuf = ((scan[0] & 0x3) << 6) | (scan[1] & 0x3f);
utils->MD5Update(ctx, &cbuf, 1);
base = scan + 2;
}
while (base < end);
}
static bool DigestCalcSecret(const sasl_utils_t * utils,
unsigned char *pszUserName,
unsigned char *pszRealm,
unsigned char *Password,
int PasswordLen,
bool Ignore_8859,
HASH HA1)
{
bool In_8859_1;
bool Any_8859_1 = FALSE;
MD5_CTX Md5Ctx;
utils->MD5Init(&Md5Ctx);
if (Ignore_8859 == FALSE) {
In_8859_1 = UTF8_In_8859_1(pszUserName, strlen((char *) pszUserName));
MD5_UTF8_8859_1(utils, &Md5Ctx, In_8859_1,
pszUserName, (unsigned) strlen((char *) pszUserName));
Any_8859_1 |= In_8859_1;
} else {
utils->MD5Update(&Md5Ctx, pszUserName, (unsigned) strlen((char *) pszUserName));
}
utils->MD5Update(&Md5Ctx, COLON, 1);
if (pszRealm != NULL && pszRealm[0] != '\0') {
if (Ignore_8859 == FALSE) {
In_8859_1 = UTF8_In_8859_1(pszRealm, strlen((char *) pszRealm));
MD5_UTF8_8859_1(utils, &Md5Ctx, In_8859_1,
pszRealm, (unsigned) strlen((char *) pszRealm));
Any_8859_1 |= In_8859_1;
} else {
utils->MD5Update(&Md5Ctx, pszRealm, (unsigned) strlen((char *) pszRealm));
}
}
utils->MD5Update(&Md5Ctx, COLON, 1);
if (Ignore_8859 == FALSE) {
In_8859_1 = UTF8_In_8859_1(Password, PasswordLen);
MD5_UTF8_8859_1(utils, &Md5Ctx, In_8859_1,
Password, PasswordLen);
Any_8859_1 |= In_8859_1;
} else {
utils->MD5Update(&Md5Ctx, Password, PasswordLen);
}
utils->MD5Final(HA1, &Md5Ctx);
return Any_8859_1;
}
static unsigned char *create_nonce(const sasl_utils_t * utils)
{
unsigned char *base64buf;
int base64len;
char *ret = (char *) utils->malloc(NONCE_SIZE);
if (ret == NULL)
return NULL;
utils->rand(utils->rpool, (char *) ret, NONCE_SIZE);
base64len = (NONCE_SIZE * 4 / 3) + (NONCE_SIZE % 3 ? 4 : 0);
base64buf = (unsigned char *) utils->malloc(base64len + 1);
if (base64buf == NULL) {
utils->seterror(utils->conn, 0, "Unable to allocate final buffer");
return NULL;
}
if (utils->encode64(ret, NONCE_SIZE,
(char *) base64buf, base64len, NULL) != SASL_OK) {
utils->free(ret);
return NULL;
}
utils->free(ret);
return base64buf;
}
static int add_to_challenge(const sasl_utils_t *utils,
char **str, unsigned *buflen, unsigned *curlen,
char *name,
unsigned char *value,
bool need_quotes)
{
size_t namesize = strlen(name);
size_t valuesize = strlen((char *) value);
unsigned newlen;
int ret;
newlen = (unsigned) (*curlen + 1 + namesize + 2 + valuesize + 2);
ret = _plug_buf_alloc(utils, str, buflen, newlen);
if(ret != SASL_OK) return ret;
if (*curlen > 0) {
strcat(*str, ",");
strcat(*str, name);
} else {
strcpy(*str, name);
}
if (need_quotes) {
strcat(*str, "=\"");
if (strpbrk ((char *)value, NEED_ESCAPING) != NULL) {
char * quoted = quote ((char *) value);
valuesize = strlen(quoted);
ret = _plug_buf_alloc(utils, str, buflen, newlen);
if (ret == SASL_OK) {
strcat(*str, quoted);
free (quoted);
} else {
free (quoted);
return ret;
}
} else {
strcat(*str, (char *) value);
}
strcat(*str, "\"");
} else {
strcat(*str, "=");
strcat(*str, (char *) value);
}
*curlen = newlen;
return SASL_OK;
}
static int is_lws_char (char c)
{
return (c == ' ' || c == HT || c == CR || c == LF);
}
static char *skip_lws (char *s)
{
if (!s) return NULL;
while (is_lws_char(s[0])) {
if (s[0] == '\0') break;
s++;
}
return s;
}
static char *skip_r_lws (char *s)
{
char *end;
size_t len;
if (!s) return NULL;
len = strlen(s);
if (len == 0) return NULL;
end = s + len - 1;
while (end > s && (end[0] == ' ' || end[0] == HT || end[0] == CR || end[0] == LF)) {
end--;
}
if (end == s && (end[0] == ' ' || end[0] == HT || end[0] == CR || end[0] == LF)) {
return NULL;
} else {
return (end + 1);
}
}
static char *skip_token (char *s, int caseinsensitive)
{
if(!s) return NULL;
while (s[0]>SP) {
if (s[0]==DEL || s[0]=='(' || s[0]==')' || s[0]=='<' || s[0]=='>' ||
s[0]=='@' || s[0]==',' || s[0]==';' || s[0]==':' || s[0]=='\\' ||
s[0]=='\'' || s[0]=='/' || s[0]=='[' || s[0]==']' || s[0]== '?' ||
s[0]=='=' || s[0]== '{' || s[0]== '}') {
if (caseinsensitive == 1) {
if (!isupper((unsigned char) s[0]))
break;
} else {
break;
}
}
s++;
}
return s;
}
static bool str2ul32 (char *str, unsigned long * value)
{
unsigned int n;
char c;
if (str == NULL) {
return (FALSE);
}
*value = 0;
str = skip_lws (str);
if (str[0] == '\0') {
return (FALSE);
}
n = 0;
while (str[0] != '\0') {
c = str[0];
if (!isdigit((int)c)) {
return (FALSE);
}
if (n > MAX_UIN32_DIV_10) {
return (FALSE);
} else if (n == MAX_UIN32_DIV_10 && ((unsigned) (c - '0') > MAX_UIN32_MOD_10)) {
return (FALSE);
}
n = n * 10 + (unsigned) (c - '0');
str++;
}
*value = n;
return (TRUE);
}
static char *unquote (char *qstr)
{
char *endvalue;
int escaped = 0;
char *outptr;
if(!qstr) return NULL;
if (qstr[0] == '"') {
qstr++;
outptr = qstr;
for (endvalue = qstr; endvalue[0] != '\0'; endvalue++, outptr++) {
if (escaped) {
outptr[0] = endvalue[0];
escaped = 0;
}
else if (endvalue[0] == '\\') {
escaped = 1;
outptr--;
}
else if (endvalue[0] == '"') {
break;
}
else {
outptr[0] = endvalue[0];
}
}
if (endvalue[0] != '"') {
return NULL;
}
while (outptr <= endvalue) {
outptr[0] = '\0';
outptr++;
}
endvalue++;
}
else {
endvalue = skip_token(qstr,0);
};
return endvalue;
}
static char *quote (char *str)
{
char *p;
char *outp;
char *result;
int num_to_escape;
if (!str) return NULL;
num_to_escape = 0;
p = strpbrk (str, NEED_ESCAPING);
while (p != NULL) {
num_to_escape++;
p = strpbrk (p + 1, NEED_ESCAPING);
}
if (num_to_escape == 0) {
return (strdup (str));
}
result = malloc (strlen(str) + num_to_escape + 1);
for (p = str, outp = result; *p; p++) {
if (*p == '"' || *p == '\\') {
*outp = '\\';
outp++;
}
*outp = *p;
outp++;
}
*outp = '\0';
return (result);
}
static void get_pair(char **in, char **name, char **value)
{
char *endpair;
char *curp = *in;
*name = NULL;
*value = NULL;
if (curp == NULL) return;
while (curp[0] != '\0') {
curp = skip_lws(curp);
if (curp[0] == ',') {
curp++;
} else {
break;
}
}
if (curp[0] == '\0') {
*name = "";
return;
}
*name = curp;
curp = skip_token(curp,1);
if (curp[0] != '=' && curp[0] != '\0') {
*curp++ = '\0';
};
curp = skip_lws(curp);
if (curp[0] != '=') {
*name = NULL;
return;
}
curp[0] = '\0';
curp++;
curp = skip_lws(curp);
*value = (curp[0] == '"') ? curp+1 : curp;
endpair = unquote (curp);
if (endpair == NULL) {
*name = NULL;
*value = NULL;
return;
}
if (is_lws_char (endpair[0])) {
*endpair++ = '\0';
endpair = skip_lws(endpair);
}
if (endpair[0] == ',') {
endpair[0] = '\0';
endpair++;
} else if (endpair[0] != '\0') {
*name = NULL;
*value = NULL;
return;
}
*in = endpair;
}
#ifdef WITH_DES
struct des_context_s {
des_key_schedule keysched;
des_cblock ivec;
des_key_schedule keysched2;
};
typedef struct des_context_s des_context_t;
static void slidebits(unsigned char *keybuf, unsigned char *inbuf)
{
keybuf[0] = inbuf[0];
keybuf[1] = (inbuf[0]<<7) | (inbuf[1]>>1);
keybuf[2] = (inbuf[1]<<6) | (inbuf[2]>>2);
keybuf[3] = (inbuf[2]<<5) | (inbuf[3]>>3);
keybuf[4] = (inbuf[3]<<4) | (inbuf[4]>>4);
keybuf[5] = (inbuf[4]<<3) | (inbuf[5]>>5);
keybuf[6] = (inbuf[5]<<2) | (inbuf[6]>>6);
keybuf[7] = (inbuf[6]<<1);
}
static int dec_3des(context_t *text,
const char *input,
unsigned inputlen,
unsigned char digest[16] __attribute__((unused)),
char *output,
unsigned *outputlen)
{
des_context_t *c = (des_context_t *) text->cipher_dec_context;
int padding, p;
des_ede2_cbc_encrypt((void *) input,
(void *) output,
inputlen,
c->keysched,
c->keysched2,
&c->ivec,
DES_DECRYPT);
padding = output[inputlen - 11];
if (padding < 1 || padding > 8) {
return SASL_FAIL;
}
for (p = 1; p <= padding; p++) {
if (output[inputlen - 10 - p] != padding) {
return SASL_FAIL;
}
}
*outputlen = inputlen - padding - 10;
return SASL_OK;
}
static int enc_3des(context_t *text,
const char *input,
unsigned inputlen,
unsigned char digest[16],
char *output,
unsigned *outputlen)
{
des_context_t *c = (des_context_t *) text->cipher_enc_context;
int len;
int paddinglen;
paddinglen = 8 - ((inputlen + 10) % 8);
memcpy(output, input, inputlen);
memset(output+inputlen, paddinglen, paddinglen);
memcpy(output+inputlen+paddinglen, digest, 10);
len=inputlen+paddinglen+10;
des_ede2_cbc_encrypt((void *) output,
(void *) output,
len,
c->keysched,
c->keysched2,
&c->ivec,
DES_ENCRYPT);
*outputlen=len;
return SASL_OK;
}
static int init_3des(context_t *text,
unsigned char enckey[16],
unsigned char deckey[16])
{
des_context_t *c;
unsigned char keybuf[8];
c = (des_context_t *) text->utils->malloc(2 * sizeof(des_context_t));
if (c == NULL) return SASL_NOMEM;
slidebits(keybuf, enckey);
if (des_key_sched((des_cblock *) keybuf, c->keysched) < 0)
return SASL_FAIL;
slidebits(keybuf, enckey + 7);
if (des_key_sched((des_cblock *) keybuf, c->keysched2) < 0)
return SASL_FAIL;
memcpy(c->ivec, ((char *) enckey) + 8, 8);
text->cipher_enc_context = (cipher_context_t *) c;
c++;
slidebits(keybuf, deckey);
if (des_key_sched((des_cblock *) keybuf, c->keysched) < 0)
return SASL_FAIL;
slidebits(keybuf, deckey + 7);
if (des_key_sched((des_cblock *) keybuf, c->keysched2) < 0)
return SASL_FAIL;
memcpy(c->ivec, ((char *) deckey) + 8, 8);
text->cipher_dec_context = (cipher_context_t *) c;
return SASL_OK;
}
static int dec_des(context_t *text,
const char *input,
unsigned inputlen,
unsigned char digest[16] __attribute__((unused)),
char *output,
unsigned *outputlen)
{
des_context_t *c = (des_context_t *) text->cipher_dec_context;
int p, padding = 0;
des_cbc_encrypt((void *) input,
(void *) output,
inputlen,
c->keysched,
&c->ivec,
DES_DECRYPT);
memcpy(c->ivec, input + (inputlen - 8), 8);
padding = output[inputlen - 11];
if (padding < 1 || padding > 8) {
return SASL_FAIL;
}
for (p = 1; p <= padding; p++) {
if (output[inputlen - 10 - p] != padding) {
return SASL_FAIL;
}
}
*outputlen = inputlen - padding - 10;
return SASL_OK;
}
static int enc_des(context_t *text,
const char *input,
unsigned inputlen,
unsigned char digest[16],
char *output,
unsigned *outputlen)
{
des_context_t *c = (des_context_t *) text->cipher_enc_context;
int len;
int paddinglen;
paddinglen = 8 - ((inputlen+10) % 8);
memcpy(output, input, inputlen);
memset(output+inputlen, paddinglen, paddinglen);
memcpy(output+inputlen+paddinglen, digest, 10);
len = inputlen + paddinglen + 10;
des_cbc_encrypt((void *) output,
(void *) output,
len,
c->keysched,
&c->ivec,
DES_ENCRYPT);
memcpy(c->ivec, output + (len - 8), 8);
*outputlen = len;
return SASL_OK;
}
static int init_des(context_t *text,
unsigned char enckey[16],
unsigned char deckey[16])
{
des_context_t *c;
unsigned char keybuf[8];
c = (des_context_t *) text->utils->malloc(2 * sizeof(des_context_t));
if (c == NULL) return SASL_NOMEM;
slidebits(keybuf, enckey);
des_key_sched((des_cblock *) keybuf, c->keysched);
memcpy(c->ivec, ((char *) enckey) + 8, 8);
text->cipher_enc_context = (cipher_context_t *) c;
c++;
slidebits(keybuf, deckey);
des_key_sched((des_cblock *) keybuf, c->keysched);
memcpy(c->ivec, ((char *) deckey) + 8, 8);
text->cipher_dec_context = (cipher_context_t *) c;
return SASL_OK;
}
static void free_des(context_t *text)
{
if (text->cipher_enc_context) text->utils->free(text->cipher_enc_context);
}
#endif
#ifdef WITH_RC4
struct rc4_context_s {
unsigned char sbox[256];
int i, j;
};
typedef struct rc4_context_s rc4_context_t;
static void rc4_init(rc4_context_t *text,
const unsigned char *key,
unsigned keylen)
{
int i, j;
for (i=0;i<256;i++)
text->sbox[i]=i;
j=0;
for (i = 0; i < 256; i++) {
unsigned char tmp;
j = (j + text->sbox[i] + key[i % keylen]) % 256;
tmp = text->sbox[i];
text->sbox[i] = text->sbox[j];
text->sbox[j] = tmp;
}
text->i = 0;
text->j = 0;
}
static void rc4_encrypt(rc4_context_t *text,
const char *input,
char *output,
unsigned len)
{
int tmp;
int i = text->i;
int j = text->j;
int t;
int K;
const char *input_end = input + len;
while (input < input_end) {
i = (i + 1) % 256;
j = (j + text->sbox[i]) % 256;
tmp = text->sbox[i];
text->sbox[i] = text->sbox[j];
text->sbox[j] = tmp;
t = (text->sbox[i] + text->sbox[j]) % 256;
K = text->sbox[t];
*output++ = *input++ ^ K;
}
text->i = i;
text->j = j;
}
static void rc4_decrypt(rc4_context_t *text,
const char *input,
char *output,
unsigned len)
{
int tmp;
int i = text->i;
int j = text->j;
int t;
int K;
const char *input_end = input + len;
while (input < input_end) {
i = (i + 1) % 256;
j = (j + text->sbox[i]) % 256;
tmp = text->sbox[i];
text->sbox[i] = text->sbox[j];
text->sbox[j] = tmp;
t = (text->sbox[i] + text->sbox[j]) % 256;
K = text->sbox[t];
*output++ = *input++ ^ K;
}
text->i = i;
text->j = j;
}
static void free_rc4(context_t *text)
{
if(text->cipher_enc_context) text->utils->free(text->cipher_enc_context);
if(text->cipher_dec_context) text->utils->free(text->cipher_dec_context);
}
static int init_rc4(context_t *text,
unsigned char enckey[16],
unsigned char deckey[16])
{
text->cipher_enc_context=
(cipher_context_t *) text->utils->malloc(sizeof(rc4_context_t));
if (text->cipher_enc_context == NULL) return SASL_NOMEM;
text->cipher_dec_context=
(cipher_context_t *) text->utils->malloc(sizeof(rc4_context_t));
if (text->cipher_dec_context == NULL) return SASL_NOMEM;
rc4_init((rc4_context_t *) text->cipher_enc_context,
(const unsigned char *) enckey, 16);
rc4_init((rc4_context_t *) text->cipher_dec_context,
(const unsigned char *) deckey, 16);
return SASL_OK;
}
static int dec_rc4(context_t *text,
const char *input,
unsigned inputlen,
unsigned char digest[16] __attribute__((unused)),
char *output,
unsigned *outputlen)
{
rc4_decrypt((rc4_context_t *) text->cipher_dec_context,
input, output, inputlen);
*outputlen = inputlen - 10;
return SASL_OK;
}
static int enc_rc4(context_t *text,
const char *input,
unsigned inputlen,
unsigned char digest[16],
char *output,
unsigned *outputlen)
{
*outputlen = inputlen+10;
rc4_encrypt((rc4_context_t *) text->cipher_enc_context,
input,
output,
inputlen);
rc4_encrypt((rc4_context_t *) text->cipher_enc_context,
(const char *) digest,
(output)+inputlen, 10);
return SASL_OK;
}
#endif
struct digest_cipher available_ciphers[] =
{
#ifdef WITH_RC4
{ "rc4-40", 40, 5, 0x01, &enc_rc4, &dec_rc4, &init_rc4, &free_rc4 },
{ "rc4-56", 56, 7, 0x02, &enc_rc4, &dec_rc4, &init_rc4, &free_rc4 },
{ "rc4", 128, 16, 0x04, &enc_rc4, &dec_rc4, &init_rc4, &free_rc4 },
#endif
#ifdef WITH_DES
{ "des", 55, 16, 0x08, &enc_des, &dec_des, &init_des, &free_des },
{ "3des", 112, 16, 0x10, &enc_3des, &dec_3des, &init_3des, &free_des },
#endif
{ NULL, 0, 0, 0, NULL, NULL, NULL, NULL }
};
static int create_layer_keys(context_t *text,
const sasl_utils_t *utils,
HASH key, int keylen,
unsigned char enckey[16],
unsigned char deckey[16])
{
MD5_CTX Md5Ctx;
utils->log(utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 create_layer_keys()");
utils->MD5Init(&Md5Ctx);
utils->MD5Update(&Md5Ctx, key, keylen);
if (text->i_am == SERVER) {
utils->MD5Update(&Md5Ctx, (const unsigned char *) SEALING_SERVER_CLIENT,
(unsigned) strlen(SEALING_SERVER_CLIENT));
} else {
utils->MD5Update(&Md5Ctx, (const unsigned char *) SEALING_CLIENT_SERVER,
(unsigned) strlen(SEALING_CLIENT_SERVER));
}
utils->MD5Final(enckey, &Md5Ctx);
utils->MD5Init(&Md5Ctx);
utils->MD5Update(&Md5Ctx, key, keylen);
if (text->i_am != SERVER) {
utils->MD5Update(&Md5Ctx, (const unsigned char *) SEALING_SERVER_CLIENT,
(unsigned) strlen(SEALING_SERVER_CLIENT));
} else {
utils->MD5Update(&Md5Ctx, (const unsigned char *) SEALING_CLIENT_SERVER,
(unsigned) strlen(SEALING_CLIENT_SERVER));
}
utils->MD5Final(deckey, &Md5Ctx);
utils->MD5Init(&Md5Ctx);
utils->MD5Update(&Md5Ctx, text->HA1, HASHLEN);
if (text->i_am == SERVER) {
utils->MD5Update(&Md5Ctx, (const unsigned char *)SIGNING_SERVER_CLIENT,
(unsigned) strlen(SIGNING_SERVER_CLIENT));
} else {
utils->MD5Update(&Md5Ctx, (const unsigned char *)SIGNING_CLIENT_SERVER,
(unsigned) strlen(SIGNING_CLIENT_SERVER));
}
utils->MD5Final(text->Ki_send, &Md5Ctx);
utils->MD5Init(&Md5Ctx);
utils->MD5Update(&Md5Ctx, text->HA1, HASHLEN);
if (text->i_am != SERVER) {
utils->MD5Update(&Md5Ctx, (const unsigned char *)SIGNING_SERVER_CLIENT,
(unsigned) strlen(SIGNING_SERVER_CLIENT));
} else {
utils->MD5Update(&Md5Ctx, (const unsigned char *)SIGNING_CLIENT_SERVER,
(unsigned) strlen(SIGNING_CLIENT_SERVER));
}
utils->MD5Final(text->Ki_receive, &Md5Ctx);
return SASL_OK;
}
static const unsigned short version = 1;
static int digestmd5_encode(void *context,
const struct iovec *invec,
unsigned numiov,
const char **output,
unsigned *outputlen)
{
context_t *text = (context_t *) context;
int tmp;
unsigned int tmpnum;
unsigned short int tmpshort;
int ret;
char *out;
struct buffer_info *inblob, bufinfo;
if(!context || !invec || !numiov || !output || !outputlen) {
PARAMERROR(text->utils);
return SASL_BADPARAM;
}
if (numiov > 1) {
ret = _plug_iovec_to_buf(text->utils, invec, numiov, &text->enc_in_buf);
if (ret != SASL_OK) return ret;
inblob = text->enc_in_buf;
} else {
bufinfo.data = invec[0].iov_base;
bufinfo.curlen = invec[0].iov_len;
inblob = &bufinfo;
}
ret = _plug_buf_alloc(text->utils, &(text->encode_buf),
&(text->encode_buf_len),
(4 +
inblob->curlen +
10 +
8 +
6));
if(ret != SASL_OK) return ret;
out = (text->encode_buf)+4;
tmpnum = htonl(text->seqnum);
memcpy(text->encode_buf, &tmpnum, 4);
memcpy(text->encode_buf + 4, inblob->data, inblob->curlen);
if (text->cipher_enc) {
unsigned char digest[16];
text->utils->hmac_md5((const unsigned char *) text->encode_buf,
inblob->curlen + 4,
text->Ki_send, HASHLEN, digest);
text->cipher_enc(text, inblob->data, inblob->curlen,
digest, out, outputlen);
out+=(*outputlen);
}
else {
text->utils->hmac_md5((const unsigned char *) text->encode_buf,
inblob->curlen + 4,
text->Ki_send, HASHLEN,
(unsigned char *) text->encode_buf +
inblob->curlen + 4);
*outputlen = inblob->curlen + 10;
out+=inblob->curlen + 10;
}
tmpshort = htons(version);
memcpy(out, &tmpshort, 2);
out+=2;
(*outputlen)+=2;
tmpnum = htonl(text->seqnum);
memcpy(out, &tmpnum, 4);
(*outputlen)+=4;
tmp=htonl(*outputlen);
memcpy(text->encode_buf, &tmp, 4);
(*outputlen)+=4;
*output = text->encode_buf;
text->seqnum++;
return SASL_OK;
}
static int digestmd5_decode_packet(void *context,
const char *input,
unsigned inputlen,
char **output,
unsigned *outputlen)
{
context_t *text = (context_t *) context;
int result;
unsigned char *digest;
int tmpnum;
int lup;
unsigned short ver;
unsigned int seqnum;
unsigned char checkdigest[16];
if (inputlen < 16) {
text->utils->seterror(text->utils->conn, 0, "DIGEST-MD5 SASL packets must be at least 16 bytes long");
return SASL_FAIL;
}
memcpy(&ver, input+inputlen-6, 2);
ver = ntohs(ver);
if (ver != version) {
text->utils->seterror(text->utils->conn, 0, "Wrong Version");
return SASL_FAIL;
}
memcpy(&seqnum, input+inputlen-4, 4);
seqnum = ntohl(seqnum);
if (seqnum != text->rec_seqnum) {
text->utils->seterror(text->utils->conn, 0,
"Incorrect Sequence Number: received %u, expected %u",
seqnum,
text->rec_seqnum);
return SASL_FAIL;
}
result = _plug_buf_alloc(text->utils, &text->decode_packet_buf,
&text->decode_packet_buf_len,
inputlen
- 6
+ 4);
if (result != SASL_OK) return result;
tmpnum = htonl(text->rec_seqnum);
memcpy(text->decode_packet_buf, &tmpnum, 4);
text->rec_seqnum++;
*output = text->decode_packet_buf + 4;
if (text->cipher_dec) {
result = text->cipher_dec(text, input, inputlen-6, NULL,
*output, outputlen);
if (result != SASL_OK) return result;
}
else {
memcpy(*output, input, inputlen - 6);
*outputlen = inputlen - 16;
}
digest = (unsigned char *) *output + (inputlen - 16);
text->utils->hmac_md5((const unsigned char *) text->decode_packet_buf,
(*outputlen) + 4,
text->Ki_receive, HASHLEN, checkdigest);
for (lup = 0; lup < 10; lup++)
if (checkdigest[lup] != digest[lup]) {
text->utils->seterror(text->utils->conn, 0,
"CMAC doesn't match at byte %d!", lup);
return SASL_FAIL;
}
return SASL_OK;
}
static int digestmd5_decode(void *context,
const char *input, unsigned inputlen,
const char **output, unsigned *outputlen)
{
context_t *text = (context_t *) context;
int ret;
ret = _plug_decode(&text->decode_context, input, inputlen,
&text->decode_buf, &text->decode_buf_len, outputlen,
digestmd5_decode_packet, text);
*output = text->decode_buf;
return ret;
}
static void digestmd5_common_mech_dispose(void *conn_context,
const sasl_utils_t *utils)
{
context_t *text = (context_t *) conn_context;
int lup;
if (!text || !utils) return;
utils->log(utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 common mech dispose");
if (text->authid) utils->free(text->authid);
if (text->realm) utils->free(text->realm);
if (text->realms) {
for (lup = 0; lup < text->realm_cnt; lup++)
utils->free (text->realms[lup]);
utils->free(text->realms);
}
if (text->nonce) utils->free(text->nonce);
if (text->cnonce) utils->free(text->cnonce);
if (text->cipher_free) text->cipher_free(text);
if (text->response_value) utils->free(text->response_value);
_plug_decode_free(&text->decode_context);
if (text->encode_buf) utils->free(text->encode_buf);
if (text->decode_buf) utils->free(text->decode_buf);
if (text->decode_packet_buf) utils->free(text->decode_packet_buf);
if (text->out_buf) utils->free(text->out_buf);
if (text->enc_in_buf) {
if (text->enc_in_buf->data) utils->free(text->enc_in_buf->data);
utils->free(text->enc_in_buf);
}
utils->free(conn_context);
}
static void clear_reauth_entry(reauth_entry_t *reauth, enum Context_type type,
const sasl_utils_t *utils)
{
if (!reauth) return;
if (reauth->authid) utils->free(reauth->authid);
if (reauth->realm) utils->free(reauth->realm);
if (reauth->nonce) utils->free(reauth->nonce);
if (reauth->cnonce) utils->free(reauth->cnonce);
if (type == CLIENT) {
if (reauth->u.c.serverFQDN) utils->free(reauth->u.c.serverFQDN);
}
memset(reauth, 0, sizeof(reauth_entry_t));
}
static void digestmd5_common_mech_free(void *glob_context,
const sasl_utils_t *utils)
{
digest_glob_context_t *my_glob_context =
(digest_glob_context_t *) glob_context;
reauth_cache_t *reauth_cache = my_glob_context->reauth;
size_t n;
utils->log(utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 common mech free");
my_glob_context->reauth = NULL;
if (!reauth_cache) return;
for (n = 0; n < reauth_cache->size; n++) {
clear_reauth_entry(&reauth_cache->e[n], reauth_cache->i_am, utils);
}
if (reauth_cache->e) utils->free(reauth_cache->e);
if (reauth_cache->mutex) {
utils->mutex_free(reauth_cache->mutex);
reauth_cache->mutex = NULL;
}
utils->free(reauth_cache);
}
typedef struct server_context {
context_t common;
time_t timestamp;
int stale;
sasl_ssf_t limitssf, requiressf;
} server_context_t;
static digest_glob_context_t server_glob_context;
static void DigestCalcHA1FromSecret(context_t * text,
const sasl_utils_t * utils,
HASH HA1,
unsigned char *authorization_id,
unsigned char *pszNonce,
unsigned char *pszCNonce,
HASHHEX SessionKey)
{
MD5_CTX Md5Ctx;
utils->MD5Init(&Md5Ctx);
if (text->http_mode) {
HASHHEX HA1Hex;
CvtHex(HA1, HA1Hex);
utils->MD5Update(&Md5Ctx, HA1Hex, HASHHEXLEN);
}
else {
utils->MD5Update(&Md5Ctx, HA1, HASHLEN);
}
utils->MD5Update(&Md5Ctx, COLON, 1);
utils->MD5Update(&Md5Ctx, pszNonce, (unsigned) strlen((char *) pszNonce));
utils->MD5Update(&Md5Ctx, COLON, 1);
utils->MD5Update(&Md5Ctx, pszCNonce, (unsigned) strlen((char *) pszCNonce));
if (authorization_id != NULL) {
utils->MD5Update(&Md5Ctx, COLON, 1);
utils->MD5Update(&Md5Ctx, authorization_id,
(unsigned) strlen((char *) authorization_id));
}
utils->MD5Final(HA1, &Md5Ctx);
CvtHex(HA1, SessionKey);
memcpy(text->HA1, HA1, sizeof(HASH));
}
static char *create_response(context_t * text,
const sasl_utils_t * utils,
unsigned char *nonce,
unsigned int ncvalue,
unsigned char *cnonce,
char *qop,
const sasl_http_request_t *request,
HASH Secret,
char *authorization_id,
char **response_value)
{
HASHHEX SessionKey;
HASH EntityHash;
HASHHEX HEntity;
HASHHEX Response;
char *result;
if (qop == NULL) qop = "auth";
DigestCalcHA1FromSecret(text,
utils,
Secret,
(unsigned char *) authorization_id,
nonce,
cnonce,
SessionKey);
if (text->http_mode) {
MD5_CTX Md5Ctx;
utils->MD5Init(&Md5Ctx);
utils->MD5Update(&Md5Ctx, request->entity, request->elen);
utils->MD5Final(EntityHash, &Md5Ctx);
}
else {
memset(EntityHash, 0, HASHLEN);
}
CvtHex(EntityHash, HEntity);
DigestCalcResponse(utils,
SessionKey,
nonce,
ncvalue,
cnonce,
(unsigned char *) qop,
(unsigned char *) request->uri,
(unsigned char *) request->method,
HEntity,
Response
);
result = utils->malloc(HASHHEXLEN + 1);
memcpy(result, Response, HASHHEXLEN);
result[HASHHEXLEN] = 0;
if (response_value != NULL) {
char * new_response_value;
DigestCalcResponse(utils,
SessionKey,
nonce,
ncvalue,
cnonce,
(unsigned char *) qop,
(unsigned char *) request->uri,
NULL,
HEntity,
Response
);
new_response_value = utils->realloc(*response_value, HASHHEXLEN + 1);
if (new_response_value == NULL) {
free (*response_value);
*response_value = NULL;
return NULL;
}
*response_value = new_response_value;
memcpy(*response_value, Response, HASHHEXLEN);
(*response_value)[HASHHEXLEN] = 0;
}
return result;
}
static int get_server_realm(sasl_server_params_t * params, char **realm)
{
if (params->user_realm != NULL) {
if(params->user_realm[0] != '\0') {
*realm = (char *) params->user_realm;
} else {
params->utils->seterror(params->utils->conn, 0,
"user_realm is an empty string!");
return SASL_BADPARAM;
}
} else if (params->serverFQDN != NULL) {
*realm = (char *) params->serverFQDN;
} else {
params->utils->seterror(params->utils->conn, 0,
"no way to obtain DIGEST-MD5 realm");
return SASL_FAIL;
}
return SASL_OK;
}
static int htoi(unsigned char *hexin, unsigned int *res)
{
size_t lup, inlen;
inlen = strlen((char *) hexin);
*res = 0;
for (lup = 0; lup < inlen; lup++) {
switch (hexin[lup]) {
case '0':
case '1':
case '2':
case '3':
case '4':
case '5':
case '6':
case '7':
case '8':
case '9':
*res = (*res << 4) + (hexin[lup] - '0');
break;
case 'a':
case 'b':
case 'c':
case 'd':
case 'e':
case 'f':
*res = (*res << 4) + (hexin[lup] - 'a' + 10);
break;
case 'A':
case 'B':
case 'C':
case 'D':
case 'E':
case 'F':
*res = (*res << 4) + (hexin[lup] - 'A' + 10);
break;
default:
return SASL_BADPARAM;
}
}
return SASL_OK;
}
static int digestmd5_server_mech_new(void *glob_context,
sasl_server_params_t * sparams,
const char *challenge __attribute__((unused)),
unsigned challen __attribute__((unused)),
void **conn_context)
{
context_t *text;
text = sparams->utils->malloc(sizeof(server_context_t));
if (text == NULL)
return SASL_NOMEM;
memset(text, 0, sizeof(server_context_t));
text->state = 1;
text->i_am = SERVER;
text->http_mode = (sparams->flags & SASL_NEED_HTTP);
text->reauth = ((digest_glob_context_t *) glob_context)->reauth;
*conn_context = text;
return SASL_OK;
}
static int
digestmd5_server_mech_step1(server_context_t *stext,
sasl_server_params_t *sparams,
const char *clientin __attribute__((unused)),
unsigned clientinlen __attribute__((unused)),
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t * oparams __attribute__((unused)))
{
context_t *text = (context_t *) stext;
int result;
char *realm;
unsigned char *nonce;
char *charset = "utf-8";
char qop[1024], cipheropts[1024];
struct digest_cipher *cipher;
unsigned resplen;
int added_conf = 0;
char maxbufstr[64];
sparams->utils->log(sparams->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 server step 1");
result = get_server_realm(sparams, &realm);
if(result != SASL_OK) return result;
qop[0] = '\0';
cipheropts[0] = '\0';
if (stext->requiressf == 0) {
if (*qop) strcat(qop, ",");
strcat(qop, "auth");
}
if (stext->requiressf <= 1 && stext->limitssf >= 1) {
if (*qop) strcat(qop, ",");
strcat(qop, "auth-int");
}
cipher = available_ciphers;
while (cipher->name) {
if (stext->requiressf <= cipher->ssf &&
stext->limitssf >= cipher->ssf) {
if (!added_conf) {
if (*qop) strcat(qop, ",");
strcat(qop, "auth-conf");
added_conf = 1;
}
if (*cipheropts) strcat(cipheropts, ",");
strcat(cipheropts, cipher->name);
}
cipher++;
}
if (*qop == '\0') {
return SASL_TOOWEAK;
}
nonce = create_nonce(sparams->utils);
if (nonce == NULL) {
SETERROR(sparams->utils, "internal erorr: failed creating a nonce");
return SASL_FAIL;
}
resplen = 0;
text->out_buf = NULL;
text->out_buf_len = 0;
if (add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"nonce", (unsigned char *) nonce,
TRUE) != SASL_OK) {
SETERROR(sparams->utils, "internal error: add_to_challenge failed");
return SASL_FAIL;
}
if (realm && add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"realm", (unsigned char *) realm,
TRUE) != SASL_OK) {
SETERROR(sparams->utils, "internal error: add_to_challenge failed");
return SASL_FAIL;
}
if (add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"qop",
(unsigned char *) qop, TRUE) != SASL_OK) {
SETERROR(sparams->utils, "internal error: add_to_challenge 3 failed");
return SASL_FAIL;
}
if (strcmp(cipheropts,"")!=0)
{
if (add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"cipher", (unsigned char *) cipheropts,
TRUE) != SASL_OK) {
SETERROR(sparams->utils,
"internal error: add_to_challenge 4 failed");
return SASL_FAIL;
}
}
if (stext->stale &&
add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"stale", (unsigned char *) "true", FALSE) != SASL_OK) {
SETERROR(sparams->utils, "internal error: add_to_challenge failed");
return SASL_FAIL;
}
if(sparams->props.maxbufsize) {
snprintf(maxbufstr, sizeof(maxbufstr), "%u",
sparams->props.maxbufsize);
if (add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"maxbuf",
(unsigned char *) maxbufstr, FALSE) != SASL_OK) {
SETERROR(sparams->utils,
"internal error: add_to_challenge 5 failed");
return SASL_FAIL;
}
}
if (add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"charset",
(unsigned char *) charset, FALSE) != SASL_OK) {
SETERROR(sparams->utils, "internal error: add_to_challenge 6 failed");
return SASL_FAIL;
}
if (add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"algorithm",
(unsigned char *) "md5-sess", FALSE)!=SASL_OK) {
SETERROR(sparams->utils, "internal error: add_to_challenge 7 failed");
return SASL_FAIL;
}
if (*serveroutlen > 2048) {
SETERROR(sparams->utils,
"internal error: challenge larger than 2048 bytes");
return SASL_FAIL;
}
text->authid = NULL;
if (_plug_strdup(sparams->utils, realm, &text->realm, NULL) != SASL_OK) {
SETERROR(sparams->utils,
"internal error: out of memory when saving realm");
return SASL_FAIL;
}
if (text->http_mode &&
sparams->utils->mutex_lock(text->reauth->mutex) == SASL_OK) {
unsigned val = hash((char *) nonce) % text->reauth->size;
clear_reauth_entry(&text->reauth->e[val], SERVER, sparams->utils);
text->reauth->e[val].authid = NULL;
text->reauth->e[val].realm = text->realm; text->realm = NULL;
text->reauth->e[val].nonce = nonce;
text->reauth->e[val].nonce_count = 1;
text->reauth->e[val].cnonce = NULL;
text->reauth->e[val].u.s.timestamp = time(0);
sparams->utils->mutex_unlock(text->reauth->mutex);
}
else {
text->nonce = nonce;
text->nonce_count = 1;
text->cnonce = NULL;
stext->timestamp = time(0);
}
*serveroutlen = (unsigned) strlen(text->out_buf);
*serverout = text->out_buf;
text->state = 2;
return SASL_CONTINUE;
}
static int digestmd5_server_mech_step2(server_context_t *stext,
sasl_server_params_t *sparams,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t * oparams)
{
context_t *text = (context_t *) stext;
sasl_secret_t *sec = NULL;
int result;
char *serverresponse = NULL;
char *username = NULL;
char *authorization_id = NULL;
char *realm = NULL;
unsigned char *nonce = NULL, *cnonce = NULL;
unsigned int noncecount = 0;
char *qop = NULL;
char *digesturi = NULL;
sasl_http_request_t rfc2831_request;
const sasl_http_request_t *request;
char *response = NULL;
unsigned long client_maxbuf = 65536;
int maxbuf_count = 0;
char *charset = NULL;
char *cipher = NULL;
unsigned int n = 0;
HASH Secret;
HASH SecretBogus;
bool Try_8859_1 = FALSE;
int client_ignores_realm = 0;
char *full_username = NULL;
char *internal_username = NULL;
int canon_flags;
const char *password_request[] = { SASL_AUX_PASSWORD,
"*cmusaslsecretDIGEST-MD5",
NULL };
size_t len;
struct propval auxprop_values[2];
char *in_start = NULL;
char *in = NULL;
cipher_free_t *old_cipher_free = NULL;
sparams->utils->log(sparams->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 server step 2");
if (clientinlen == 0) {
SETERROR(sparams->utils, "input expected in DIGEST-MD5, step 2");
result = SASL_BADAUTH;
goto FreeAllMem;
}
if (text->http_mode) {
request = sparams->http_request;
if (!request) {
SETERROR(sparams->utils,
"missing HTTP request in DIGEST-MD5, step 2");
result = SASL_BADPARAM;
goto FreeAllMem;
}
}
else {
rfc2831_request.method = "AUTHENTICATE";
rfc2831_request.uri = NULL;
rfc2831_request.entity = NULL;
rfc2831_request.elen = 0;
rfc2831_request.non_persist = 0;
request = &rfc2831_request;
}
in = sparams->utils->malloc(clientinlen + 1);
memcpy(in, clientin, clientinlen);
in[clientinlen] = 0;
in_start = in;
while (in[0] != '\0') {
char *name = NULL, *value = NULL;
get_pair(&in, &name, &value);
if (name == NULL) {
SETERROR(sparams->utils,
"Parse error");
result = SASL_BADAUTH;
goto FreeAllMem;
}
if (*name == '\0') {
break;
}
if (strcasecmp(name, "username") == 0) {
_plug_strdup(sparams->utils, value, &username, NULL);
} else if (strcasecmp(name, "authzid") == 0) {
_plug_strdup(sparams->utils, value, &authorization_id, NULL);
} else if (strcasecmp(name, "cnonce") == 0) {
_plug_strdup(sparams->utils, value, (char **) &cnonce, NULL);
} else if (strcasecmp(name, "nc") == 0) {
if (htoi((unsigned char *) value, &noncecount) != SASL_OK) {
SETERROR(sparams->utils,
"error converting hex to int");
result = SASL_BADAUTH;
goto FreeAllMem;
}
} else if (strcasecmp(name, "realm") == 0) {
if (realm) {
SETERROR(sparams->utils,
"duplicate realm: authentication aborted");
result = SASL_FAIL;
goto FreeAllMem;
}
_plug_strdup(sparams->utils, value, &realm, NULL);
} else if (strcasecmp(name, "nonce") == 0) {
_plug_strdup(sparams->utils, value, (char **) &nonce, NULL);
} else if (strcasecmp(name, "qop") == 0) {
if (qop) {
SETERROR(sparams->utils,
"duplicate qop: authentication aborted");
result = SASL_FAIL;
goto FreeAllMem;
}
_plug_strdup(sparams->utils, value, &qop, NULL);
} else if (strcasecmp(name, "digest-uri") == 0 ||
(text->http_mode &&
strcasecmp(name, "uri") == 0)) {
size_t service_len;
if (digesturi) {
SETERROR(sparams->utils,
"duplicate digest-uri: authentication aborted");
result = SASL_FAIL;
goto FreeAllMem;
}
_plug_strdup(sparams->utils, value, &digesturi, NULL);
if (text->http_mode && request && request->uri) {
if (strcmp(digesturi, request->uri)) {
result = SASL_BADAUTH;
SETERROR(sparams->utils,
"bad digest-uri: doesn't match HTTP request");
goto FreeAllMem;
}
}
else {
service_len = strlen(sparams->service);
if (strncasecmp(digesturi, sparams->service, service_len) ||
digesturi[service_len] != '/') {
result = SASL_BADAUTH;
SETERROR(sparams->utils,
"bad digest-uri: doesn't match service");
goto FreeAllMem;
}
rfc2831_request.uri = digesturi;
}
} else if (strcasecmp(name, "response") == 0) {
_plug_strdup(sparams->utils, value, &response, NULL);
} else if (strcasecmp(name, "cipher") == 0) {
_plug_strdup(sparams->utils, value, &cipher, NULL);
} else if (strcasecmp(name, "maxbuf") == 0) {
maxbuf_count++;
if (maxbuf_count != 1) {
result = SASL_BADAUTH;
SETERROR(sparams->utils,
"duplicate maxbuf: authentication aborted");
goto FreeAllMem;
} else if (str2ul32 (value, &client_maxbuf) == FALSE) {
result = SASL_BADAUTH;
SETERROR(sparams->utils, "invalid maxbuf parameter");
goto FreeAllMem;
} else {
if (client_maxbuf <= 16) {
result = SASL_BADAUTH;
SETERROR(sparams->utils,
"maxbuf parameter too small");
goto FreeAllMem;
}
if (client_maxbuf > MAX_SASL_BUFSIZE) {
result = SASL_BADAUTH;
SETERROR(sparams->utils,
"maxbuf parameter too big");
goto FreeAllMem;
}
}
} else if (strcasecmp(name, "charset") == 0) {
if (strcasecmp(value, "utf-8") != 0) {
SETERROR(sparams->utils, "client doesn't support UTF-8");
result = SASL_FAIL;
goto FreeAllMem;
}
_plug_strdup(sparams->utils, value, &charset, NULL);
} else if (strcasecmp(name,"algorithm") == 0) {
if (text->http_mode && strcasecmp(value, "md5-sess") != 0) {
SETERROR(sparams->utils, "'algorithm' isn't 'md5-sess'");
result = SASL_FAIL;
goto FreeAllMem;
}
} else {
sparams->utils->log(sparams->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 unrecognized pair %s/%s: ignoring",
name, value);
}
}
if ((username == NULL)) {
SETERROR(sparams->utils, "required parameters missing: username");
result = SASL_BADAUTH;
goto FreeAllMem;
}
if ((nonce == NULL)) {
SETERROR(sparams->utils, "required parameters missing: nonce");
result = SASL_BADAUTH;
goto FreeAllMem;
}
if ((noncecount == 0)) {
SETERROR(sparams->utils, "required parameters missing: noncecount");
result = SASL_BADAUTH;
goto FreeAllMem;
}
if ((cnonce == NULL)) {
SETERROR(sparams->utils, "required parameters missing: cnonce");
result = SASL_BADAUTH;
goto FreeAllMem;
}
if ((digesturi == NULL)) {
SETERROR(sparams->utils, "required parameters missing: digesturi");
result = SASL_BADAUTH;
goto FreeAllMem;
}
if ((response == NULL)) {
SETERROR(sparams->utils, "required parameters missing: response");
result = SASL_BADAUTH;
goto FreeAllMem;
}
if (realm == NULL) {
_plug_strdup(sparams->utils, "", &realm, NULL);
sparams->utils->log(sparams->utils->conn, SASL_LOG_DEBUG,
"The client didn't send a realm, assuming empty string.");
#if 0
if (text->realm[0] != '\0') {
SETERROR(sparams->utils,
"realm changed: authentication aborted");
result = SASL_BADAUTH;
goto FreeAllMem;
}
#endif
}
if (!text->nonce) {
unsigned val = hash((char *) nonce) % text->reauth->size;
if (sparams->utils->mutex_lock(text->reauth->mutex) == SASL_OK) {
if (text->reauth->e[val].realm &&
!strcmp(realm, text->reauth->e[val].realm) &&
((text->reauth->e[val].nonce_count == 1) ||
(text->reauth->e[val].authid &&
!strcmp(username, text->reauth->e[val].authid)))) {
_plug_strdup(sparams->utils, text->reauth->e[val].realm,
&text->realm, NULL);
_plug_strdup(sparams->utils, (char *) text->reauth->e[val].nonce,
(char **) &text->nonce, NULL);
text->nonce_count = text->reauth->e[val].nonce_count;
#if 0
_plug_strdup(sparams->utils, (char *) text->reauth->e[val].cnonce,
(char **) &text->cnonce, NULL);
#endif
stext->timestamp = text->reauth->e[val].u.s.timestamp;
}
sparams->utils->mutex_unlock(text->reauth->mutex);
}
if (!text->nonce) {
sparams->utils->log(sparams->utils->conn, SASL_LOG_DEBUG,
"No reauth info for '%s' found", nonce);
}
}
if (text->nonce) {
if (text->realm == NULL) {
sparams->utils->log(sparams->utils->conn, SASL_LOG_DEBUG,
"The client specifies a realm when the server hasn't provided one. Using client's realm.");
_plug_strdup(sparams->utils, realm, &text->realm, NULL);
} else if ((strcmp(realm, text->realm) != 0) &&
(text->realm[0] != 0)) {
client_ignores_realm = 1;
sparams->utils->log(sparams->utils->conn, SASL_LOG_DEBUG,
"The client tries to override server provided realm");
if (text->realm) sparams->utils->free(text->realm);
_plug_strdup(sparams->utils, realm, &text->realm, NULL);
}
if (strcmp((char *) nonce, (char *) text->nonce) != 0) {
SETERROR(sparams->utils,
"nonce changed: authentication aborted");
result = SASL_BADAUTH;
goto FreeAllMem;
}
#if 0
if (noncecount != text->nonce_count) {
SETERROR(sparams->utils,
"incorrect nonce-count: authentication aborted");
result = SASL_BADAUTH;
goto FreeAllMem;
}
#endif
#if 0
if (text->cnonce && strcmp((char *) cnonce, (char *) text->cnonce) != 0) {
SETERROR(sparams->utils,
"cnonce changed: authentication aborted");
result = SASL_BADAUTH;
goto FreeAllMem;
}
#endif
}
result = sparams->utils->prop_request(sparams->propctx, password_request);
if(result != SASL_OK) {
SETERROR(sparams->utils, "unable to obtain user password");
goto FreeAllMem;
}
if (client_ignores_realm) {
if (strlen(text->realm) == 0) {
_plug_strdup(sparams->utils, username, &full_username, NULL);
} else {
full_username = (char *) sparams->utils->malloc(strlen(username) +
strlen(text->realm) + 2);
full_username[0] = '\0';
sprintf (full_username, "%s@%s", username, text->realm);
}
internal_username = full_username;
} else {
internal_username = username;
}
canon_flags = SASL_CU_AUTHID;
if (!authorization_id || !*authorization_id) {
canon_flags |= SASL_CU_AUTHZID;
}
result = sparams->canon_user(sparams->utils->conn,
internal_username,
0,
canon_flags,
oparams);
if (result != SASL_OK) {
SETERROR(sparams->utils, "unable to canonify user and get auxprops");
goto FreeAllMem;
}
if (authorization_id != NULL && *authorization_id != '\0') {
result = sparams->canon_user(sparams->utils->conn,
authorization_id, 0, SASL_CU_AUTHZID,
oparams);
}
if (result != SASL_OK) {
SETERROR(sparams->utils, "unable to canonify authorization ID");
goto FreeAllMem;
}
result = sparams->utils->prop_getnames(sparams->propctx, password_request,
auxprop_values);
if (result < 0 ||
((!auxprop_values[0].name || !auxprop_values[0].values) &&
(!auxprop_values[1].name || !auxprop_values[1].values))) {
sparams->utils->seterror(sparams->utils->conn, 0,
"no secret in database");
result = sparams->transition ? SASL_TRANS : SASL_NOUSER;
goto FreeAllMem;
}
if (auxprop_values[0].name && auxprop_values[0].values) {
len = strlen(auxprop_values[0].values[0]);
if (len == 0) {
sparams->utils->seterror(sparams->utils->conn,0,
"empty secret");
result = SASL_FAIL;
goto FreeAllMem;
}
sec = sparams->utils->malloc(sizeof(sasl_secret_t) + len);
if (!sec) {
SETERROR(sparams->utils, "unable to allocate secret");
result = SASL_FAIL;
goto FreeAllMem;
}
sec->len = (unsigned) len;
strncpy((char *) sec->data, auxprop_values[0].values[0], len + 1);
{
Try_8859_1 = DigestCalcSecret(sparams->utils,
(unsigned char *) username,
(unsigned char *) realm,
sec->data,
sec->len,
FALSE,
Secret);
Secret[HASHLEN] = '\0';
}
if (Try_8859_1) {
DigestCalcSecret(sparams->utils,
(unsigned char *) username,
(unsigned char *) realm,
sec->data,
sec->len,
TRUE,
SecretBogus);
SecretBogus[HASHLEN] = '\0';
}
_plug_free_secret(sparams->utils, &sec);
} else if (auxprop_values[1].name && auxprop_values[1].values) {
memcpy(Secret, auxprop_values[1].values[0], HASHLEN);
Secret[HASHLEN] = '\0';
} else {
sparams->utils->seterror(sparams->utils->conn, 0,
"Have neither type of secret");
return SASL_FAIL;
}
sparams->utils->prop_erase(sparams->propctx, password_request[0]);
if (qop == NULL) {
_plug_strdup(sparams->utils, "auth", &qop, NULL);
}
if (oparams->mech_ssf > 1) {
old_cipher_free = text->cipher_free;
}
if ((!strcasecmp(qop, "auth-conf")) && (cipher != NULL)) {
struct digest_cipher *cptr;
cptr = available_ciphers;
while (cptr->name) {
if (!strcasecmp(cipher, cptr->name) &&
stext->requiressf <= cptr->ssf &&
stext->limitssf >= cptr->ssf) {
break;
}
cptr++;
}
if (cptr->name) {
text->cipher_enc = cptr->cipher_enc;
text->cipher_dec = cptr->cipher_dec;
text->cipher_init = cptr->cipher_init;
text->cipher_free = cptr->cipher_free;
oparams->mech_ssf = cptr->ssf;
n = cptr->n;
} else {
sparams->utils->log(sparams->utils->conn, SASL_LOG_WARN,
"protocol violation: client requested invalid cipher");
SETERROR(sparams->utils, "client requested invalid cipher");
oparams->mech_ssf = 2;
result = SASL_FAIL;
goto FreeAllMem;
}
oparams->encode=&digestmd5_encode;
oparams->decode=&digestmd5_decode;
} else if (!strcasecmp(qop, "auth-int") &&
stext->requiressf <= 1 && stext->limitssf >= 1) {
oparams->encode = &digestmd5_encode;
oparams->decode = &digestmd5_decode;
oparams->mech_ssf = 1;
} else if (!strcasecmp(qop, "auth") && stext->requiressf == 0) {
oparams->encode = NULL;
oparams->decode = NULL;
oparams->mech_ssf = 0;
} else {
SETERROR(sparams->utils,
"protocol violation: client requested invalid qop");
result = SASL_FAIL;
goto FreeAllMem;
}
serverresponse = create_response(text,
sparams->utils,
nonce,
noncecount,
cnonce,
qop,
request,
Secret,
authorization_id,
&text->response_value);
if (serverresponse == NULL) {
SETERROR(sparams->utils, "internal error: unable to create response");
result = SASL_NOMEM;
goto FreeAllMem;
}
if (strcmp(serverresponse, response) != 0) {
if (Try_8859_1) {
serverresponse = create_response(text,
sparams->utils,
nonce,
noncecount,
cnonce,
qop,
request,
SecretBogus,
authorization_id,
&text->response_value);
if (serverresponse == NULL) {
SETERROR(sparams->utils, "internal error: unable to create response");
result = SASL_NOMEM;
goto FreeAllMem;
}
if (strcmp(serverresponse, response) != 0) {
SETERROR(sparams->utils,
"client response doesn't match what we generated (tried bogus)");
result = SASL_BADAUTH;
goto FreeAllMem;
}
} else {
SETERROR(sparams->utils,
"client response doesn't match what we generated");
result = SASL_BADAUTH;
goto FreeAllMem;
}
}
if (!text->nonce ||
(noncecount != text->nonce_count) ||
(text->reauth->timeout &&
time(0) - stext->timestamp > text->reauth->timeout)) {
if (!text->nonce) SETERROR(sparams->utils, "no cached server nonce");
else if (noncecount != text->nonce_count)
SETERROR(sparams->utils, "incorrect nonce-count");
else SETERROR(sparams->utils, "server nonce expired");
stext->stale = 1;
result = SASL_BADAUTH;
goto FreeAllMem;
}
oparams->doneflag = 1;
oparams->maxoutbuf = client_maxbuf - 4;
if (oparams->mech_ssf > 1) {
oparams->maxoutbuf -= 25;
} else if(oparams->mech_ssf == 1) {
oparams->maxoutbuf -= 16;
}
oparams->param_version = 0;
text->seqnum = 0;
text->rec_seqnum = 0;
text->utils = sparams->utils;
if (old_cipher_free) old_cipher_free(text);
_plug_decode_init(&text->decode_context, text->utils,
sparams->props.maxbufsize ? sparams->props.maxbufsize :
DEFAULT_BUFSIZE);
if (oparams->mech_ssf > 0) {
unsigned char enckey[16];
unsigned char deckey[16];
create_layer_keys(text, sparams->utils,text->HA1,n,enckey,deckey);
if (text->cipher_init) {
if (text->cipher_init(text, enckey, deckey) != SASL_OK) {
sparams->utils->seterror(sparams->utils->conn, 0,
"couldn't init cipher");
}
}
}
{
unsigned resplen = 0;
if (add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"rspauth", (unsigned char *) text->response_value,
text->http_mode ? TRUE : FALSE) != SASL_OK) {
SETERROR(sparams->utils, "internal error: add_to_challenge failed");
result = SASL_FAIL;
goto FreeAllMem;
}
if (text->http_mode) {
char ncvalue[10];
if (add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"cnonce", cnonce, TRUE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllMem;
}
snprintf(ncvalue, sizeof(ncvalue), "%08x", text->nonce_count);
if (add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"nc", (unsigned char *) ncvalue, FALSE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllMem;
}
if (add_to_challenge(sparams->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"qop", (unsigned char *) qop, TRUE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllMem;
}
}
if (strlen(text->out_buf) > 2048) {
result = SASL_FAIL;
goto FreeAllMem;
}
}
*serveroutlen = (unsigned) strlen(text->out_buf);
*serverout = text->out_buf;
result = SASL_OK;
FreeAllMem:
if (clientinlen > 0 &&
text->reauth->timeout &&
sparams->utils->mutex_lock(text->reauth->mutex) == SASL_OK) {
unsigned val = hash((char *) nonce) % text->reauth->size;
switch (result) {
case SASL_OK:
if (text->nonce_count == 1) {
clear_reauth_entry(&text->reauth->e[val], SERVER, sparams->utils);
text->reauth->e[val].authid = username; username = NULL;
text->reauth->e[val].realm = text->realm; text->realm = NULL;
text->reauth->e[val].nonce = text->nonce; text->nonce = NULL;
text->reauth->e[val].cnonce = cnonce; cnonce = NULL;
}
if (text->nonce_count < text->reauth->e[val].nonce_count) {
clear_reauth_entry(&text->reauth->e[val], SERVER, sparams->utils);
}
else {
text->reauth->e[val].nonce_count = ++text->nonce_count;
text->reauth->e[val].u.s.timestamp = time(0);
}
break;
default:
if (text->nonce_count > 1) {
clear_reauth_entry(&text->reauth->e[val], SERVER, sparams->utils);
}
else {
}
}
sparams->utils->mutex_unlock(text->reauth->mutex);
}
if (in_start) sparams->utils->free (in_start);
if (full_username != NULL)
sparams->utils->free (full_username);
if (username != NULL)
sparams->utils->free (username);
if (authorization_id != NULL)
sparams->utils->free (authorization_id);
if (realm != NULL)
sparams->utils->free (realm);
if (nonce != NULL)
sparams->utils->free (nonce);
if (cnonce != NULL)
sparams->utils->free (cnonce);
if (response != NULL)
sparams->utils->free (response);
if (cipher != NULL)
sparams->utils->free (cipher);
if (serverresponse != NULL)
sparams->utils->free(serverresponse);
if (charset != NULL)
sparams->utils->free (charset);
if (digesturi != NULL)
sparams->utils->free (digesturi);
if (qop!=NULL)
sparams->utils->free (qop);
if (sec)
_plug_free_secret(sparams->utils, &sec);
return result;
}
static int digestmd5_server_mech_step(void *conn_context,
sasl_server_params_t *sparams,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams)
{
context_t *text = (context_t *) conn_context;
server_context_t *stext = (server_context_t *) conn_context;
*serverout = NULL;
*serveroutlen = 0;
if (clientinlen > 4096) return SASL_BADPROT;
if (text == NULL) {
return SASL_BADPROT;
}
switch (text->state) {
case 1:
if (!text->http_mode &&
!sparams->props.maxbufsize) {
stext->limitssf = 0;
stext->requiressf = 0;
} else {
if (sparams->props.max_ssf < sparams->external_ssf) {
stext->limitssf = 0;
} else {
stext->limitssf =
sparams->props.max_ssf - sparams->external_ssf;
}
if (sparams->props.min_ssf < sparams->external_ssf) {
stext->requiressf = 0;
} else {
stext->requiressf =
sparams->props.min_ssf - sparams->external_ssf;
}
}
if (clientin && text->reauth->timeout) {
if (digestmd5_server_mech_step2(stext, sparams,
clientin, clientinlen,
serverout, serveroutlen,
oparams) == SASL_OK) {
return SASL_OK;
}
sparams->utils->log(NULL, SASL_LOG_WARN,
"DIGEST-MD5 reauth failed\n");
memset(oparams, 0, sizeof(sasl_out_params_t));
if (text->nonce) sparams->utils->free(text->nonce);
if (text->realm) sparams->utils->free(text->realm);
text->nonce = text->realm = NULL;
}
return digestmd5_server_mech_step1(stext, sparams,
clientin, clientinlen,
serverout, serveroutlen, oparams);
case 2:
return digestmd5_server_mech_step2(stext, sparams,
clientin, clientinlen,
serverout, serveroutlen, oparams);
default:
sparams->utils->log(NULL, SASL_LOG_ERR,
"Invalid DIGEST-MD5 server step %d\n", text->state);
return SASL_FAIL;
}
return SASL_FAIL;
}
static void digestmd5_server_mech_dispose(void *conn_context,
const sasl_utils_t *utils)
{
server_context_t *stext = (server_context_t *) conn_context;
if (!stext || !utils) return;
digestmd5_common_mech_dispose(conn_context, utils);
}
static sasl_server_plug_t digestmd5_server_plugins[] =
{
{
"DIGEST-MD5",
#ifdef WITH_RC4
128,
#elif defined(WITH_DES)
112,
#else
1,
#endif
SASL_SEC_NOPLAINTEXT
| SASL_SEC_NOANONYMOUS
| SASL_SEC_MUTUAL_AUTH,
SASL_FEAT_ALLOWS_PROXY
| SASL_FEAT_SUPPORTS_HTTP,
&server_glob_context,
&digestmd5_server_mech_new,
&digestmd5_server_mech_step,
&digestmd5_server_mech_dispose,
&digestmd5_common_mech_free,
NULL,
NULL,
NULL,
NULL,
NULL
}
};
int digestmd5_server_plug_init(sasl_utils_t *utils,
int maxversion,
int *out_version,
sasl_server_plug_t **pluglist,
int *plugcount)
{
reauth_cache_t *reauth_cache;
const char *timeout = NULL;
unsigned int len;
if (maxversion < SASL_SERVER_PLUG_VERSION) {
return SASL_BADVERS;
}
reauth_cache = utils->malloc(sizeof(reauth_cache_t));
if (reauth_cache == NULL) {
return SASL_NOMEM;
}
memset(reauth_cache, 0, sizeof(reauth_cache_t));
reauth_cache->i_am = SERVER;
utils->getopt(utils->getopt_context, "DIGEST-MD5", "reauth_timeout",
&timeout, &len);
if (timeout) {
reauth_cache->timeout = (time_t) 60 * strtol(timeout, NULL, 10);
}
if (reauth_cache->timeout < 0) {
reauth_cache->timeout = 0;
}
if (reauth_cache->timeout) {
reauth_cache->mutex = utils->mutex_alloc();
if (!reauth_cache->mutex) {
utils->free(reauth_cache);
return SASL_FAIL;
}
reauth_cache->size = 100;
reauth_cache->e = utils->malloc(reauth_cache->size *
sizeof(reauth_entry_t));
if (reauth_cache->e == NULL) {
utils->mutex_free(reauth_cache->mutex);
utils->free(reauth_cache);
return SASL_NOMEM;
}
memset(reauth_cache->e, 0, reauth_cache->size * sizeof(reauth_entry_t));
}
((digest_glob_context_t *) digestmd5_server_plugins[0].glob_context)->reauth = reauth_cache;
*out_version = SASL_SERVER_PLUG_VERSION;
*pluglist = digestmd5_server_plugins;
*plugcount = 1;
return SASL_OK;
}
typedef struct client_context {
context_t common;
sasl_secret_t *password;
unsigned int free_password;
int protection;
struct digest_cipher *cipher;
unsigned long server_maxbuf;
char *algorithm;
unsigned char *opaque;
} client_context_t;
static digest_glob_context_t client_glob_context;
static void DigestCalcHA1(context_t * text,
const sasl_utils_t * utils,
char *pszAlg,
unsigned char *pszUserName,
unsigned char *pszRealm,
sasl_secret_t * pszPassword,
unsigned char *pszAuthorization_id,
unsigned char *pszNonce,
unsigned char *pszCNonce,
HASHHEX SessionKey)
{
MD5_CTX Md5Ctx;
HASH HA1;
DigestCalcSecret(utils,
pszUserName,
pszRealm,
(unsigned char *) pszPassword->data,
pszPassword->len,
FALSE,
HA1);
if (!text->http_mode ||
(pszAlg && strcasecmp(pszAlg, "md5-sess") == 0)) {
utils->MD5Init(&Md5Ctx);
if (text->http_mode) {
HASHHEX HA1Hex;
CvtHex(HA1, HA1Hex);
utils->MD5Update(&Md5Ctx, HA1Hex, HASHHEXLEN);
}
else {
utils->MD5Update(&Md5Ctx, HA1, HASHLEN);
}
utils->MD5Update(&Md5Ctx, COLON, 1);
utils->MD5Update(&Md5Ctx, pszNonce, (unsigned) strlen((char *) pszNonce));
utils->MD5Update(&Md5Ctx, COLON, 1);
utils->MD5Update(&Md5Ctx, pszCNonce, (unsigned) strlen((char *) pszCNonce));
if (pszAuthorization_id != NULL) {
utils->MD5Update(&Md5Ctx, COLON, 1);
utils->MD5Update(&Md5Ctx, pszAuthorization_id,
(unsigned) strlen((char *) pszAuthorization_id));
}
utils->MD5Final(HA1, &Md5Ctx);
}
CvtHex(HA1, SessionKey);
memcpy(text->HA1, HA1, sizeof(HASH));
}
static char *calculate_response(context_t * text,
const sasl_utils_t * utils,
char *algorithm,
unsigned char *username,
unsigned char *realm,
unsigned char *nonce,
unsigned int ncvalue,
unsigned char *cnonce,
char *qop,
const sasl_http_request_t *request,
sasl_secret_t * passwd,
unsigned char *authorization_id,
char **response_value)
{
HASHHEX SessionKey;
HASH EntityHash;
HASHHEX HEntity;
HASHHEX Response;
char *result;
if(!username || !cnonce || !nonce || !ncvalue || !request || !passwd) {
PARAMERROR( utils );
return NULL;
}
if (realm == NULL) {
realm = (unsigned char *) "";
}
if (qop == NULL) {
qop = "auth";
}
DigestCalcHA1(text,
utils,
algorithm,
username,
realm,
passwd,
authorization_id,
nonce,
cnonce,
SessionKey);
if (text->http_mode) {
MD5_CTX Md5Ctx;
utils->MD5Init(&Md5Ctx);
utils->MD5Update(&Md5Ctx, request->entity, request->elen);
utils->MD5Final(EntityHash, &Md5Ctx);
}
else {
memset(EntityHash, 0, HASHLEN);
}
CvtHex(EntityHash, HEntity);
DigestCalcResponse(utils,
SessionKey,
nonce,
ncvalue,
cnonce,
(unsigned char *) qop,
(unsigned char *) request->uri,
(unsigned char *) request->method,
HEntity,
Response
);
result = utils->malloc(HASHHEXLEN + 1);
memcpy(result, Response, HASHHEXLEN);
result[HASHHEXLEN] = 0;
if (response_value != NULL) {
char * new_response_value;
DigestCalcResponse(utils,
SessionKey,
nonce,
ncvalue,
cnonce,
(unsigned char *) qop,
(unsigned char *) request->uri,
NULL,
HEntity,
Response
);
new_response_value = utils->realloc(*response_value, HASHHEXLEN + 1);
if (new_response_value == NULL) {
free (*response_value);
*response_value = NULL;
return NULL;
}
*response_value = new_response_value;
memcpy(*response_value, Response, HASHHEXLEN);
(*response_value)[HASHHEXLEN] = 0;
}
return result;
}
static int make_client_response(context_t *text,
sasl_client_params_t *params,
sasl_out_params_t *oparams)
{
client_context_t *ctext = (client_context_t *) text;
char *qop = NULL;
unsigned nbits = 0;
char *digesturi = NULL;
bool IsUTF8 = FALSE;
char ncvalue[10];
char maxbufstr[64];
char *response = NULL;
unsigned resplen = 0;
int result = SASL_OK;
cipher_free_t *old_cipher_free = NULL;
sasl_http_request_t rfc2831_request;
const sasl_http_request_t *request;
params->utils->log(params->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 make_client_response()");
if (oparams->mech_ssf > 1) {
old_cipher_free = text->cipher_free;
}
switch (ctext->protection) {
case DIGEST_PRIVACY:
qop = "auth-conf";
oparams->encode = &digestmd5_encode;
oparams->decode = &digestmd5_decode;
oparams->mech_ssf = ctext->cipher->ssf;
nbits = ctext->cipher->n;
text->cipher_enc = ctext->cipher->cipher_enc;
text->cipher_dec = ctext->cipher->cipher_dec;
text->cipher_free = ctext->cipher->cipher_free;
text->cipher_init = ctext->cipher->cipher_init;
break;
case DIGEST_INTEGRITY:
qop = "auth-int";
oparams->encode = &digestmd5_encode;
oparams->decode = &digestmd5_decode;
oparams->mech_ssf = 1;
break;
case DIGEST_NOLAYER:
default:
qop = "auth";
oparams->encode = NULL;
oparams->decode = NULL;
oparams->mech_ssf = 0;
}
if (text->http_mode) {
request = params->http_request;
}
else {
digesturi = params->utils->malloc(strlen(params->service) + 1 +
strlen(params->serverFQDN) + 1 +
1);
if (digesturi == NULL) {
result = SASL_NOMEM;
goto FreeAllocatedMem;
}
strcpy(digesturi, params->service);
strcat(digesturi, "/");
strcat(digesturi, params->serverFQDN);
rfc2831_request.method = "AUTHENTICATE";
rfc2831_request.uri = digesturi;
rfc2831_request.entity = NULL;
rfc2831_request.elen = 0;
rfc2831_request.non_persist = 0;
request = &rfc2831_request;
}
response =
calculate_response(text,
params->utils,
ctext->algorithm,
(unsigned char *) oparams->authid,
(unsigned char *) text->realm,
text->nonce,
text->nonce_count,
text->cnonce,
qop,
request,
ctext->password,
strcmp(oparams->user, oparams->authid) ?
(unsigned char *) oparams->user : NULL,
&text->response_value);
resplen = 0;
if (text->out_buf) params->utils->free(text->out_buf);
text->out_buf = NULL;
text->out_buf_len = 0;
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"username", (unsigned char *) oparams->authid,
TRUE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"realm", (unsigned char *) text->realm,
TRUE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
if (strcmp(oparams->user, oparams->authid)) {
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"authzid", (unsigned char *) oparams->user, TRUE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
}
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"nonce", text->nonce, TRUE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"cnonce", text->cnonce, TRUE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
snprintf(ncvalue, sizeof(ncvalue), "%08x", text->nonce_count);
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"nc", (unsigned char *) ncvalue, FALSE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"qop", (unsigned char *) qop, FALSE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
if (ctext->cipher != NULL) {
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"cipher",
(unsigned char *) ctext->cipher->name,
FALSE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
}
if (params->props.maxbufsize) {
snprintf(maxbufstr, sizeof(maxbufstr), "%d", params->props.maxbufsize);
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"maxbuf", (unsigned char *) maxbufstr,
FALSE) != SASL_OK) {
SETERROR(params->utils,
"internal error: add_to_challenge maxbuf failed");
goto FreeAllocatedMem;
}
}
if (IsUTF8) {
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"charset", (unsigned char *) "utf-8",
FALSE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
}
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
text->http_mode ? "uri"
: "digest-uri",
(unsigned char *) request->uri,
TRUE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
if (text->http_mode) {
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"algorithm", (unsigned char *) ctext->algorithm,
FALSE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
if (ctext->opaque) {
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"opaque", ctext->opaque, TRUE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
}
}
if (add_to_challenge(params->utils,
&text->out_buf, &text->out_buf_len, &resplen,
"response", (unsigned char *) response,
FALSE) != SASL_OK) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
if (strlen(text->out_buf) > 2048) {
result = SASL_FAIL;
goto FreeAllocatedMem;
}
oparams->maxoutbuf = ctext->server_maxbuf;
if(oparams->mech_ssf > 1) {
oparams->maxoutbuf -= 25;
} else if(oparams->mech_ssf == 1) {
oparams->maxoutbuf -= 16;
}
text->seqnum = 0;
text->rec_seqnum = 0;
text->utils = params->utils;
if (old_cipher_free) old_cipher_free(text);
_plug_decode_init(&text->decode_context, text->utils,
params->props.maxbufsize ? params->props.maxbufsize :
DEFAULT_BUFSIZE);
if (oparams->mech_ssf > 0) {
unsigned char enckey[16];
unsigned char deckey[16];
create_layer_keys(text, params->utils, text->HA1, nbits,
enckey, deckey);
if (text->cipher_init) {
text->cipher_init(text, enckey, deckey);
}
}
result = SASL_OK;
FreeAllocatedMem:
if (digesturi) params->utils->free(digesturi);
if (response) params->utils->free(response);
return result;
}
static int parse_server_challenge(client_context_t *ctext,
sasl_client_params_t *params,
const char *serverin, unsigned serverinlen,
char ***outrealms, int *noutrealm)
{
context_t *text = (context_t *) ctext;
int result = SASL_OK;
char *in_start = NULL;
char *in = NULL;
char **realms = NULL;
int nrealm = 0;
sasl_ssf_t limit, musthave = 0;
sasl_ssf_t external;
int protection = 0;
int saw_qop = 0;
int ciphers = 0;
int maxbuf_count = 0;
bool IsUTF8 = FALSE;
int algorithm_count = 0;
int opaque_count = 0;
params->utils->log(params->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 parse_server_challenge()");
if (!serverin || !serverinlen) {
params->utils->seterror(params->utils->conn, 0,
"no server challenge");
return SASL_FAIL;
}
in_start = in = params->utils->malloc(serverinlen + 1);
if (in == NULL) return SASL_NOMEM;
memcpy(in, serverin, serverinlen);
in[serverinlen] = 0;
ctext->server_maxbuf = 65536;
text->cnonce = create_nonce(params->utils);
if (text->cnonce == NULL) {
params->utils->seterror(params->utils->conn, 0,
"failed to create cnonce");
result = SASL_FAIL;
goto FreeAllocatedMem;
}
while (in[0] != '\0') {
char *name, *value;
get_pair(&in, &name, &value);
if (name == NULL) {
params->utils->seterror(params->utils->conn, 0, "Parse error");
result = SASL_BADAUTH;
goto FreeAllocatedMem;
}
if (*name == '\0') {
break;
}
if (strcasecmp(name, "realm") == 0) {
nrealm++;
if(!realms)
realms = params->utils->malloc(sizeof(char *) * (nrealm + 1));
else
realms = params->utils->realloc(realms,
sizeof(char *) * (nrealm + 1));
if (realms == NULL) {
result = SASL_NOMEM;
goto FreeAllocatedMem;
}
_plug_strdup(params->utils, value, &realms[nrealm-1], NULL);
realms[nrealm] = NULL;
} else if (strcasecmp(name, "nonce") == 0) {
_plug_strdup(params->utils, value, (char **) &text->nonce,
NULL);
text->nonce_count = 1;
} else if (strcasecmp(name, "qop") == 0) {
saw_qop = 1;
while (value && *value) {
char *comma;
char *end_val;
SKIP_SPACES_IN_QOP:
value = skip_lws(value);
if (*value == '\0') {
break;
}
if (*value == ',') {
value++;
goto SKIP_SPACES_IN_QOP;
}
comma = strchr(value, ',');
if (comma != NULL) {
*comma++ = '\0';
}
end_val = skip_r_lws (value);
if (end_val == NULL) {
value = comma;
continue;
} else {
*end_val = '\0';
}
if (strcasecmp(value, "auth-conf") == 0) {
protection |= DIGEST_PRIVACY;
} else if (strcasecmp(value, "auth-int") == 0) {
protection |= DIGEST_INTEGRITY;
} else if (strcasecmp(value, "auth") == 0) {
protection |= DIGEST_NOLAYER;
} else {
params->utils->log(params->utils->conn, SASL_LOG_DEBUG,
"Server supports unknown layer: %s\n",
value);
}
value = comma;
}
} else if (strcasecmp(name, "cipher") == 0) {
while (value && *value) {
struct digest_cipher *cipher = available_ciphers;
char *comma;
char *end_val;
SKIP_SPACES_IN_CIPHER:
value = skip_lws(value);
if (*value == '\0') {
break;
}
if (*value == ',') {
value++;
goto SKIP_SPACES_IN_CIPHER;
}
comma = strchr(value, ',');
if (comma != NULL) {
*comma++ = '\0';
}
end_val = skip_r_lws (value);
if (end_val == NULL) {
value = comma;
continue;
} else {
*end_val = '\0';
}
while (cipher->name) {
if (!strcasecmp(value, cipher->name)) break;
cipher++;
}
if (cipher->name) {
ciphers |= cipher->flag;
} else {
params->utils->log(params->utils->conn, SASL_LOG_DEBUG,
"Server supports unknown cipher: %s\n",
value);
}
value = comma;
}
} else if (strcasecmp(name, "stale") == 0 && ctext->password) {
if (ctext->free_password)
_plug_free_secret(params->utils, &ctext->password);
ctext->password = NULL;
} else if (strcasecmp(name, "maxbuf") == 0) {
maxbuf_count++;
if (maxbuf_count != 1) {
result = SASL_BADAUTH;
params->utils->seterror(params->utils->conn, 0,
"At least two maxbuf directives found. Authentication aborted");
goto FreeAllocatedMem;
}
if (str2ul32 (value, &ctext->server_maxbuf) == FALSE) {
result = SASL_BADAUTH;
params->utils->seterror(params->utils->conn, 0,
"Invalid maxbuf parameter received from server (%s)", value);
goto FreeAllocatedMem;
}
if (ctext->server_maxbuf <= 16) {
result = SASL_BADAUTH;
params->utils->seterror(params->utils->conn, 0,
"Invalid maxbuf parameter received from server (too small: %s)", value);
goto FreeAllocatedMem;
}
if (ctext->server_maxbuf > MAX_SASL_BUFSIZE) {
result = SASL_BADAUTH;
params->utils->seterror(params->utils->conn, 0,
"Invalid maxbuf parameter received from server (too big: %s)", value);
goto FreeAllocatedMem;
}
} else if (strcasecmp(name, "charset") == 0) {
if (strcasecmp(value, "utf-8") != 0) {
result = SASL_BADAUTH;
params->utils->seterror(params->utils->conn, 0,
"Charset must be UTF-8");
goto FreeAllocatedMem;
} else {
IsUTF8 = TRUE;
}
} else if (strcasecmp(name,"algorithm")==0) {
if (text->http_mode && strcasecmp(value, "md5") == 0) {
}
else if (strcasecmp(value, "md5-sess") != 0)
{
params->utils->seterror(params->utils->conn, 0,
"'algorithm' isn't 'md5-sess'");
result = SASL_FAIL;
goto FreeAllocatedMem;
}
if (text->http_mode) {
_plug_strdup(params->utils, value, (char **) &ctext->algorithm,
NULL);
}
algorithm_count++;
if (algorithm_count > 1)
{
params->utils->seterror(params->utils->conn, 0,
"Must see 'algorithm' only once");
result = SASL_FAIL;
goto FreeAllocatedMem;
}
} else if (strcasecmp(name,"opaque")==0) {
if (text->http_mode) {
_plug_strdup(params->utils, value, (char **) &ctext->opaque,
NULL);
opaque_count++;
if (opaque_count > 1)
{
params->utils->seterror(params->utils->conn, 0,
"Must see 'opaque' only once");
result = SASL_FAIL;
goto FreeAllocatedMem;
}
}
} else {
params->utils->log(params->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 unrecognized pair %s/%s: ignoring",
name, value);
}
}
if (protection == 0) {
if (saw_qop == 0) {
protection = DIGEST_NOLAYER;
} else {
result = SASL_BADAUTH;
params->utils->seterror(params->utils->conn, 0,
"Server doesn't support any known qop level");
goto FreeAllocatedMem;
}
}
if (algorithm_count != 1) {
params->utils->seterror(params->utils->conn, 0,
"Must see 'algorithm' once. Didn't see at all");
result = SASL_FAIL;
goto FreeAllocatedMem;
}
if (text->nonce == NULL) {
params->utils->seterror(params->utils->conn, 0,
"Don't have nonce.");
result = SASL_FAIL;
goto FreeAllocatedMem;
}
external = params->external_ssf;
if (!text->http_mode &&
params->props.maxbufsize == 0) {
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
if ((limit > 1) && (protection & DIGEST_PRIVACY)) {
struct digest_cipher *cipher;
cipher = available_ciphers;
while (cipher->name) {
if ((limit >= cipher->ssf) && (musthave <= cipher->ssf) &&
(ciphers & cipher->flag) &&
(!ctext->cipher || (cipher->ssf > ctext->cipher->ssf))) {
ctext->cipher = cipher;
}
cipher++;
}
if (ctext->cipher) {
ctext->protection = DIGEST_PRIVACY;
} else {
params->utils->seterror(params->utils->conn, 0,
"No good privacy layers");
}
}
if (ctext->cipher == NULL) {
if ((limit >= 1) && (musthave <= 1)
&& (protection & DIGEST_INTEGRITY)) {
ctext->protection = DIGEST_INTEGRITY;
} else if (musthave <= 0) {
ctext->protection = DIGEST_NOLAYER;
if ((protection & DIGEST_NOLAYER) != DIGEST_NOLAYER) {
params->utils->seterror(params->utils->conn, 0,
"Server doesn't support \"no layer\"");
result = SASL_FAIL;
goto FreeAllocatedMem;
}
} else {
params->utils->seterror(params->utils->conn, 0,
"Can't find an acceptable layer");
result = SASL_TOOWEAK;
goto FreeAllocatedMem;
}
}
*outrealms = realms;
*noutrealm = nrealm;
FreeAllocatedMem:
if (in_start) params->utils->free(in_start);
if (result != SASL_OK && realms) {
int lup;
for (lup = 0;lup < nrealm; lup++)
params->utils->free(realms[lup]);
params->utils->free(realms);
}
return result;
}
static int ask_user_info(client_context_t *ctext,
sasl_client_params_t *params,
char **realms, int nrealm,
sasl_interact_t **prompt_need,
sasl_out_params_t *oparams)
{
context_t *text = (context_t *) ctext;
int result = SASL_OK;
const char *authid = NULL, *userid = NULL, *realm = NULL;
char *realm_chal = NULL;
int user_result = SASL_OK;
int auth_result = SASL_OK;
int pass_result = SASL_OK;
int realm_result = SASL_FAIL;
int i;
size_t len;
params->utils->log(params->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 ask_user_info()");
if (oparams->authid == NULL) {
auth_result = _plug_get_authid(params->utils, &authid, prompt_need);
if ((auth_result != SASL_OK) && (auth_result != SASL_INTERACT)) {
return auth_result;
}
}
if (oparams->user == NULL) {
user_result = _plug_get_userid(params->utils, &userid, prompt_need);
if ((user_result != SASL_OK) && (user_result != SASL_INTERACT)) {
return user_result;
}
}
if (ctext->password == NULL) {
pass_result = _plug_get_password(params->utils, &ctext->password,
&ctext->free_password, prompt_need);
if ((pass_result != SASL_OK) && (pass_result != SASL_INTERACT)) {
return pass_result;
}
}
if (text->realm == NULL) {
if (realms) {
if(nrealm == 1) {
realm = realms[0];
realm_result = SASL_OK;
} else {
realm_result = _plug_get_realm(params->utils,
(const char **) realms,
(const char **) &realm,
prompt_need);
}
}
if ((realm_result != SASL_OK) && (realm_result != SASL_INTERACT)) {
if (params->serverFQDN) {
realm = params->serverFQDN;
} else {
return realm_result;
}
}
}
if (prompt_need && *prompt_need) {
params->utils->free(*prompt_need);
*prompt_need = NULL;
}
if ((user_result == SASL_INTERACT) || (auth_result == SASL_INTERACT) ||
(pass_result == SASL_INTERACT) || (realm_result == SASL_INTERACT)) {
if (realm_result == SASL_INTERACT) {
if (realms) {
len = strlen(REALM_CHAL_PREFIX);
for (i = 0; i < nrealm; i++) {
len += strlen(realms[i]) + 4 ;
}
realm_chal = params->utils->malloc(len + 1);
strcpy (realm_chal, REALM_CHAL_PREFIX);
for (i = 0; i < nrealm; i++) {
strcat (realm_chal, " {");
strcat (realm_chal, realms[i]);
strcat (realm_chal, "},");
}
realm_chal[len-1] = '.';
} else if (params->serverFQDN) {
realm_chal = params->utils->malloc(3+strlen(params->serverFQDN));
if (realm_chal) {
sprintf(realm_chal, "{%s}", params->serverFQDN);
} else {
return SASL_NOMEM;
}
}
}
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
realm_chal ? realm_chal : "{}",
realm_result == SASL_INTERACT ?
"Please enter your realm" : NULL,
params->serverFQDN ? params->serverFQDN : NULL);
if (result == SASL_OK) return SASL_INTERACT;
return result;
}
if (oparams->authid == NULL) {
if (!userid || !*userid) {
result = params->canon_user(params->utils->conn, authid, 0,
SASL_CU_AUTHID | SASL_CU_AUTHZID,
oparams);
}
else {
result = params->canon_user(params->utils->conn,
authid, 0, SASL_CU_AUTHID, oparams);
if (result != SASL_OK) return result;
result = params->canon_user(params->utils->conn,
userid, 0, SASL_CU_AUTHZID, oparams);
}
if (result != SASL_OK) return result;
}
if (realm && text->realm == NULL) {
_plug_strdup(params->utils, realm, (char **) &text->realm, NULL);
}
return result;
}
static int digestmd5_client_mech_new(void *glob_context,
sasl_client_params_t * params,
void **conn_context)
{
context_t *text;
if ((params->flags & SASL_NEED_HTTP) && !params->http_request) {
SETERROR(params->utils,
"DIGEST-MD5 unavailable due to lack of HTTP request");
return SASL_BADPARAM;
}
text = params->utils->malloc(sizeof(client_context_t));
if (text == NULL)
return SASL_NOMEM;
memset(text, 0, sizeof(client_context_t));
text->state = 1;
text->i_am = CLIENT;
text->http_mode = (params->flags & SASL_NEED_HTTP);
text->reauth = ((digest_glob_context_t *) glob_context)->reauth;
*conn_context = text;
return SASL_OK;
}
static int
digestmd5_client_mech_step1(client_context_t *ctext,
sasl_client_params_t *params,
const char *serverin __attribute__((unused)),
unsigned serverinlen __attribute__((unused)),
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
context_t *text = (context_t *) ctext;
int result = SASL_FAIL;
unsigned val;
params->utils->log(params->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 client step 1");
result = ask_user_info(ctext, params, NULL, 0, prompt_need, oparams);
if (result != SASL_OK) return result;
val = hash(params->serverFQDN) % text->reauth->size;
if (params->utils->mutex_lock(text->reauth->mutex) == SASL_OK) {
if (text->reauth->e[val].u.c.serverFQDN &&
!strcasecmp(text->reauth->e[val].u.c.serverFQDN,
params->serverFQDN) &&
!strcmp(text->reauth->e[val].authid, oparams->authid)) {
if (text->realm) params->utils->free(text->realm);
_plug_strdup(params->utils, text->reauth->e[val].realm,
&text->realm, NULL);
_plug_strdup(params->utils, (char *) text->reauth->e[val].nonce,
(char **) &text->nonce, NULL);
text->nonce_count = ++text->reauth->e[val].nonce_count;
_plug_strdup(params->utils, (char *) text->reauth->e[val].cnonce,
(char **) &text->cnonce, NULL);
if (text->http_mode) {
_plug_strdup(params->utils,
(char *) text->reauth->e[val].u.c.algorithm,
(char **) &ctext->algorithm, NULL);
if (text->reauth->e[val].u.c.opaque) {
_plug_strdup(params->utils,
(char *) text->reauth->e[val].u.c.opaque,
(char **) &ctext->opaque, NULL);
}
}
ctext->protection = text->reauth->e[val].u.c.protection;
ctext->cipher = text->reauth->e[val].u.c.cipher;
ctext->server_maxbuf = text->reauth->e[val].u.c.server_maxbuf;
}
params->utils->mutex_unlock(text->reauth->mutex);
}
if (!text->nonce) {
text->state = 2;
return SASL_CONTINUE;
}
result = make_client_response(text, params, oparams);
if (result != SASL_OK) return result;
*clientoutlen = (unsigned) strlen(text->out_buf);
*clientout = text->out_buf;
text->state = 3;
return SASL_CONTINUE;
}
static int digestmd5_client_mech_step2(client_context_t *ctext,
sasl_client_params_t *params,
const char *serverin,
unsigned serverinlen,
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
context_t *text = (context_t *) ctext;
int result = SASL_FAIL;
char **realms = NULL;
int nrealm = 0;
params->utils->log(params->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 client step 2");
if (params->props.min_ssf > params->props.max_ssf) {
return SASL_BADPARAM;
}
if (text->nonce == NULL) {
result = parse_server_challenge(ctext, params, serverin, serverinlen,
&realms, &nrealm);
if (result != SASL_OK) goto FreeAllocatedMem;
if (nrealm == 1) {
text->realm = realms[0];
params->utils->free(realms);
realms = NULL;
} else {
text->realms = realms;
text->realm_cnt = nrealm;
}
} else {
realms = text->realms;
nrealm = text->realm_cnt;
}
result = ask_user_info(ctext, params, realms, nrealm,
prompt_need, oparams);
if (result != SASL_OK) goto FreeAllocatedMem;
result = make_client_response(text, params, oparams);
if (result != SASL_OK) goto FreeAllocatedMem;
*clientoutlen = (unsigned) strlen(text->out_buf);
*clientout = text->out_buf;
text->state = 3;
result = SASL_CONTINUE;
FreeAllocatedMem:
return result;
}
static int
digestmd5_client_mech_step3(client_context_t *ctext,
sasl_client_params_t *params,
const char *serverin,
unsigned serverinlen,
sasl_interact_t **prompt_need __attribute__((unused)),
const char **clientout __attribute__((unused)),
unsigned *clientoutlen __attribute__((unused)),
sasl_out_params_t *oparams)
{
context_t *text = (context_t *) ctext;
char *in = NULL;
char *in_start;
int result = SASL_FAIL;
params->utils->log(params->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 client step 3");
in_start = in = params->utils->malloc(serverinlen + 1);
if (in == NULL) return SASL_NOMEM;
memcpy(in, serverin, serverinlen);
in[serverinlen] = 0;
while (in[0] != '\0') {
char *name, *value;
get_pair(&in, &name, &value);
if (name == NULL) {
params->utils->seterror(params->utils->conn, 0,
"DIGEST-MD5 Received Garbage");
result = SASL_BADAUTH;
break;
}
if (*name == '\0') {
break;
}
if (strcasecmp(name, "rspauth") == 0) {
if (strcmp(text->response_value, value) != 0) {
params->utils->seterror(params->utils->conn, 0,
"DIGEST-MD5: This server wants us to believe that he knows shared secret");
result = SASL_BADSERV;
} else {
oparams->doneflag = 1;
oparams->param_version = 0;
result = SASL_OK;
}
break;
} else {
params->utils->log(params->utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 unrecognized pair %s/%s: ignoring",
name, value);
}
}
params->utils->free(in_start);
if (params->utils->mutex_lock(text->reauth->mutex) == SASL_OK) {
unsigned val = hash(params->serverFQDN) % text->reauth->size;
switch (result) {
case SASL_OK:
if (text->nonce_count == 1) {
clear_reauth_entry(&text->reauth->e[val], CLIENT, params->utils);
_plug_strdup(params->utils, oparams->authid,
&text->reauth->e[val].authid, NULL);
text->reauth->e[val].realm = text->realm; text->realm = NULL;
text->reauth->e[val].nonce = text->nonce; text->nonce = NULL;
text->reauth->e[val].nonce_count = text->nonce_count;
text->reauth->e[val].cnonce = text->cnonce; text->cnonce = NULL;
_plug_strdup(params->utils, params->serverFQDN,
&text->reauth->e[val].u.c.serverFQDN, NULL);
if (text->http_mode) {
text->reauth->e[val].u.c.algorithm = ctext->algorithm;
ctext->algorithm = NULL;
text->reauth->e[val].u.c.opaque = ctext->opaque;
ctext->opaque = NULL;
}
text->reauth->e[val].u.c.protection = ctext->protection;
text->reauth->e[val].u.c.cipher = ctext->cipher;
text->reauth->e[val].u.c.server_maxbuf = ctext->server_maxbuf;
}
else {
}
break;
default:
if (text->nonce_count > 1) {
clear_reauth_entry(&text->reauth->e[val], CLIENT, params->utils);
}
else {
}
}
params->utils->mutex_unlock(text->reauth->mutex);
}
return result;
}
static int digestmd5_client_mech_step(void *conn_context,
sasl_client_params_t *params,
const char *serverin,
unsigned serverinlen,
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
context_t *text = (context_t *) conn_context;
client_context_t *ctext = (client_context_t *) conn_context;
unsigned val = hash(params->serverFQDN) % text->reauth->size;
if (serverinlen > 2048) return SASL_BADPROT;
*clientout = NULL;
*clientoutlen = 0;
switch (text->state) {
case 1:
if (!serverin) {
int reauth = 0;
if (params->utils->mutex_lock(text->reauth->mutex) == SASL_OK) {
reauth = text->reauth->e[val].u.c.serverFQDN &&
!strcasecmp(text->reauth->e[val].u.c.serverFQDN,
params->serverFQDN);
params->utils->mutex_unlock(text->reauth->mutex);
}
if (reauth) {
return digestmd5_client_mech_step1(ctext, params,
serverin, serverinlen,
prompt_need,
clientout, clientoutlen,
oparams);
}
else {
text->state = 2;
return SASL_CONTINUE;
}
}
case 3:
if (serverin && !strncasecmp(serverin, "rspauth=", 8)) {
return digestmd5_client_mech_step3(ctext, params,
serverin, serverinlen,
prompt_need,
clientout, clientoutlen,
oparams);
}
text->state = 2;
if (params->utils->mutex_lock(text->reauth->mutex) == SASL_OK) {
clear_reauth_entry(&text->reauth->e[val], CLIENT, params->utils);
params->utils->mutex_unlock(text->reauth->mutex);
}
if (text->realm) params->utils->free(text->realm);
if (text->nonce) params->utils->free(text->nonce);
if (text->cnonce) params->utils->free(text->cnonce);
text->realm = NULL;
text->nonce = text->cnonce = NULL;
ctext->cipher = NULL;
case 2:
return digestmd5_client_mech_step2(ctext, params,
serverin, serverinlen,
prompt_need,
clientout, clientoutlen,
oparams);
default:
params->utils->log(NULL, SASL_LOG_ERR,
"Invalid DIGEST-MD5 client step %d\n", text->state);
return SASL_FAIL;
}
return SASL_FAIL;
}
static void digestmd5_client_mech_dispose(void *conn_context,
const sasl_utils_t *utils)
{
client_context_t *ctext = (client_context_t *) conn_context;
if (!ctext || !utils) return;
utils->log(utils->conn, SASL_LOG_DEBUG,
"DIGEST-MD5 client mech dispose");
if (ctext->free_password) _plug_free_secret(utils, &ctext->password);
digestmd5_common_mech_dispose(conn_context, utils);
}
static sasl_client_plug_t digestmd5_client_plugins[] =
{
{
"DIGEST-MD5",
#ifdef WITH_RC4
128,
#elif defined(WITH_DES)
112,
#else
1,
#endif
SASL_SEC_NOPLAINTEXT
| SASL_SEC_NOANONYMOUS
| SASL_SEC_MUTUAL_AUTH,
SASL_FEAT_NEEDSERVERFQDN
| SASL_FEAT_ALLOWS_PROXY
| SASL_FEAT_SUPPORTS_HTTP,
NULL,
&client_glob_context,
&digestmd5_client_mech_new,
&digestmd5_client_mech_step,
&digestmd5_client_mech_dispose,
&digestmd5_common_mech_free,
NULL,
NULL,
NULL
}
};
int digestmd5_client_plug_init(sasl_utils_t *utils,
int maxversion,
int *out_version,
sasl_client_plug_t **pluglist,
int *plugcount)
{
reauth_cache_t *reauth_cache;
if (maxversion < SASL_CLIENT_PLUG_VERSION)
return SASL_BADVERS;
reauth_cache = utils->malloc(sizeof(reauth_cache_t));
if (reauth_cache == NULL)
return SASL_NOMEM;
memset(reauth_cache, 0, sizeof(reauth_cache_t));
reauth_cache->i_am = CLIENT;
reauth_cache->mutex = utils->mutex_alloc();
if (!reauth_cache->mutex)
return SASL_FAIL;
reauth_cache->size = 10;
reauth_cache->e = utils->malloc(reauth_cache->size *
sizeof(reauth_entry_t));
if (reauth_cache->e == NULL)
return SASL_NOMEM;
memset(reauth_cache->e, 0, reauth_cache->size * sizeof(reauth_entry_t));
((digest_glob_context_t *) digestmd5_client_plugins[0].glob_context)->reauth = reauth_cache;
*out_version = SASL_CLIENT_PLUG_VERSION;
*pluglist = digestmd5_client_plugins;
*plugcount = 1;
return SASL_OK;
}