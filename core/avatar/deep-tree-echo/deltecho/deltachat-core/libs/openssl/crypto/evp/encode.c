#include <stdio.h>
#include <limits.h>
#include "cryptlib.h"
#include <openssl/evp.h>
static unsigned char conv_ascii2bin(unsigned char a);
#ifndef CHARSET_EBCDIC
# define conv_bin2ascii(a)       (data_bin2ascii[(a)&0x3f])
#else
# define conv_bin2ascii(a)       (data_bin2ascii[(a)&0x3f])
#endif
#define BIN_PER_LINE    (64/4*3)
#define CHUNKS_PER_LINE (64/4)
#define CHAR_PER_LINE   (64+1)
static const unsigned char data_bin2ascii[65] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
abcdefghijklmnopqrstuvwxyz0123456789+/";
#define B64_EOLN                0xF0
#define B64_CR                  0xF1
#define B64_EOF                 0xF2
#define B64_WS                  0xE0
#define B64_ERROR               0xFF
#define B64_NOT_BASE64(a)       (((a)|0x13) == 0xF3)
#define B64_BASE64(a)           !B64_NOT_BASE64(a)
static const unsigned char data_ascii2bin[128] = {
0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xE0, 0xF0, 0xFF, 0xFF, 0xF1, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
0xE0, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0x3E, 0xFF, 0xF2, 0xFF, 0x3F,
0x34, 0x35, 0x36, 0x37, 0x38, 0x39, 0x3A, 0x3B,
0x3C, 0x3D, 0xFF, 0xFF, 0xFF, 0x00, 0xFF, 0xFF,
0xFF, 0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06,
0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C, 0x0D, 0x0E,
0x0F, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16,
0x17, 0x18, 0x19, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0x1A, 0x1B, 0x1C, 0x1D, 0x1E, 0x1F, 0x20,
0x21, 0x22, 0x23, 0x24, 0x25, 0x26, 0x27, 0x28,
0x29, 0x2A, 0x2B, 0x2C, 0x2D, 0x2E, 0x2F, 0x30,
0x31, 0x32, 0x33, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
};
#ifndef CHARSET_EBCDIC
static unsigned char conv_ascii2bin(unsigned char a)
{
if (a & 0x80)
return B64_ERROR;
return data_ascii2bin[a];
}
#else
static unsigned char conv_ascii2bin(unsigned char a)
{
a = os_toascii[a];
if (a & 0x80)
return B64_ERROR;
return data_ascii2bin[a];
}
#endif
void EVP_EncodeInit(EVP_ENCODE_CTX *ctx)
{
ctx->length = 48;
ctx->num = 0;
ctx->line_num = 0;
}
void EVP_EncodeUpdate(EVP_ENCODE_CTX *ctx, unsigned char *out, int *outl,
const unsigned char *in, int inl)
{
int i, j;
size_t total = 0;
*outl = 0;
if (inl <= 0)
return;
OPENSSL_assert(ctx->length <= (int)sizeof(ctx->enc_data));
if (ctx->length - ctx->num > inl) {
memcpy(&(ctx->enc_data[ctx->num]), in, inl);
ctx->num += inl;
return;
}
if (ctx->num != 0) {
i = ctx->length - ctx->num;
memcpy(&(ctx->enc_data[ctx->num]), in, i);
in += i;
inl -= i;
j = EVP_EncodeBlock(out, ctx->enc_data, ctx->length);
ctx->num = 0;
out += j;
*(out++) = '\n';
*out = '\0';
total = j + 1;
}
while (inl >= ctx->length && total <= INT_MAX) {
j = EVP_EncodeBlock(out, in, ctx->length);
in += ctx->length;
inl -= ctx->length;
out += j;
*(out++) = '\n';
*out = '\0';
total += j + 1;
}
if (total > INT_MAX) {
*outl = 0;
return;
}
if (inl != 0)
memcpy(&(ctx->enc_data[0]), in, inl);
ctx->num = inl;
*outl = total;
}
void EVP_EncodeFinal(EVP_ENCODE_CTX *ctx, unsigned char *out, int *outl)
{
unsigned int ret = 0;
if (ctx->num != 0) {
ret = EVP_EncodeBlock(out, ctx->enc_data, ctx->num);
out[ret++] = '\n';
out[ret] = '\0';
ctx->num = 0;
}
*outl = ret;
}
int EVP_EncodeBlock(unsigned char *t, const unsigned char *f, int dlen)
{
int i, ret = 0;
unsigned long l;
for (i = dlen; i > 0; i -= 3) {
if (i >= 3) {
l = (((unsigned long)f[0]) << 16L) |
(((unsigned long)f[1]) << 8L) | f[2];
*(t++) = conv_bin2ascii(l >> 18L);
*(t++) = conv_bin2ascii(l >> 12L);
*(t++) = conv_bin2ascii(l >> 6L);
*(t++) = conv_bin2ascii(l);
} else {
l = ((unsigned long)f[0]) << 16L;
if (i == 2)
l |= ((unsigned long)f[1] << 8L);
*(t++) = conv_bin2ascii(l >> 18L);
*(t++) = conv_bin2ascii(l >> 12L);
*(t++) = (i == 1) ? '=' : conv_bin2ascii(l >> 6L);
*(t++) = '=';
}
ret += 4;
f += 3;
}
*t = '\0';
return (ret);
}
void EVP_DecodeInit(EVP_ENCODE_CTX *ctx)
{
ctx->num = 0;
ctx->length = 0;
ctx->line_num = 0;
ctx->expect_nl = 0;
}
int EVP_DecodeUpdate(EVP_ENCODE_CTX *ctx, unsigned char *out, int *outl,
const unsigned char *in, int inl)
{
int seof = 0, eof = 0, rv = -1, ret = 0, i, v, tmp, n, decoded_len;
unsigned char *d;
n = ctx->num;
d = ctx->enc_data;
if (n > 0 && d[n - 1] == '=') {
eof++;
if (n > 1 && d[n - 2] == '=')
eof++;
}
if (inl == 0) {
rv = 0;
goto end;
}
for (i = 0; i < inl; i++) {
tmp = *(in++);
v = conv_ascii2bin(tmp);
if (v == B64_ERROR) {
rv = -1;
goto end;
}
if (tmp == '=') {
eof++;
} else if (eof > 0 && B64_BASE64(v)) {
rv = -1;
goto end;
}
if (eof > 2) {
rv = -1;
goto end;
}
if (v == B64_EOF) {
seof = 1;
goto tail;
}
if (B64_BASE64(v)) {
if (n >= 64) {
rv = -1;
goto end;
}
OPENSSL_assert(n < (int)sizeof(ctx->enc_data));
d[n++] = tmp;
}
if (n == 64) {
decoded_len = EVP_DecodeBlock(out, d, n);
n = 0;
if (decoded_len < 0 || eof > decoded_len) {
rv = -1;
goto end;
}
ret += decoded_len - eof;
out += decoded_len - eof;
}
}
tail:
if (n > 0) {
if ((n & 3) == 0) {
decoded_len = EVP_DecodeBlock(out, d, n);
n = 0;
if (decoded_len < 0 || eof > decoded_len) {
rv = -1;
goto end;
}
ret += (decoded_len - eof);
} else if (seof) {
rv = -1;
goto end;
}
}
rv = seof || (n == 0 && eof) ? 0 : 1;
end:
*outl = ret;
ctx->num = n;
return (rv);
}
int EVP_DecodeBlock(unsigned char *t, const unsigned char *f, int n)
{
int i, ret = 0, a, b, c, d;
unsigned long l;
while ((conv_ascii2bin(*f) == B64_WS) && (n > 0)) {
f++;
n--;
}
while ((n > 3) && (B64_NOT_BASE64(conv_ascii2bin(f[n - 1]))))
n--;
if (n % 4 != 0)
return (-1);
for (i = 0; i < n; i += 4) {
a = conv_ascii2bin(*(f++));
b = conv_ascii2bin(*(f++));
c = conv_ascii2bin(*(f++));
d = conv_ascii2bin(*(f++));
if ((a & 0x80) || (b & 0x80) || (c & 0x80) || (d & 0x80))
return (-1);
l = ((((unsigned long)a) << 18L) |
(((unsigned long)b) << 12L) |
(((unsigned long)c) << 6L) | (((unsigned long)d)));
*(t++) = (unsigned char)(l >> 16L) & 0xff;
*(t++) = (unsigned char)(l >> 8L) & 0xff;
*(t++) = (unsigned char)(l) & 0xff;
ret += 3;
}
return (ret);
}
int EVP_DecodeFinal(EVP_ENCODE_CTX *ctx, unsigned char *out, int *outl)
{
int i;
*outl = 0;
if (ctx->num != 0) {
i = EVP_DecodeBlock(out, ctx->enc_data, ctx->num);
if (i < 0)
return (-1);
ctx->num = 0;
*outl = i;
return (1);
} else
return (1);
}
#ifdef undef
int EVP_DecodeValid(unsigned char *buf, int len)
{
int i, num = 0, bad = 0;
if (len == 0)
return (-1);
while (conv_ascii2bin(*buf) == B64_WS) {
buf++;
len--;
if (len == 0)
return (-1);
}
for (i = len; i >= 4; i -= 4) {
if ((conv_ascii2bin(buf[0]) >= 0x40) ||
(conv_ascii2bin(buf[1]) >= 0x40) ||
(conv_ascii2bin(buf[2]) >= 0x40) ||
(conv_ascii2bin(buf[3]) >= 0x40))
return (-1);
buf += 4;
num += 1 + (buf[2] != '=') + (buf[3] != '=');
}
if ((i == 1) && (conv_ascii2bin(buf[0]) == B64_EOLN))
return (num);
if ((i == 2) && (conv_ascii2bin(buf[0]) == B64_EOLN) &&
(conv_ascii2bin(buf[0]) == B64_EOLN))
return (num);
return (1);
}
#endif