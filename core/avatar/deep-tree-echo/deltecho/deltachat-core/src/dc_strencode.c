#include <ctype.h>
#include <libetpan/libetpan.h>
#include "dc_context.h"
#include "dc_strencode.h"
static char int_2_uppercase_hex(char code)
{
static const char hex[] = "0123456789ABCDEF";
return hex[code & 15];
}
static char hex_2_int(char ch)
{
return isdigit(ch) ? ch - '0' : tolower(ch) - 'a' + 10;
}
char* dc_urlencode(const char *to_encode)
{
const char *pstr = to_encode;
if (to_encode==NULL) {
return dc_strdup("");
}
char *buf = malloc(strlen(to_encode) * 3 + 1), *pbuf = buf;
if (buf==NULL) {
exit(46);
}
while (*pstr)
{
if (isalnum(*pstr) || *pstr=='-' || *pstr=='_' || *pstr=='.' || *pstr=='~') {
*pbuf++ = *pstr;
}
else if (*pstr==' ') {
*pbuf++ = '+';
}
else {
*pbuf++ = '%', *pbuf++ = int_2_uppercase_hex(*pstr >> 4), *pbuf++ = int_2_uppercase_hex(*pstr & 15);
}
pstr++;
}
*pbuf = '\0';
return buf;
}
char* dc_urldecode(const char* to_decode)
{
const char *pstr = to_decode;
if (to_decode==NULL) {
return dc_strdup("");
}
char *buf = malloc(strlen(to_decode) + 1), *pbuf = buf;
if (buf==NULL) {
exit(50);
}
while (*pstr)
{
if (*pstr=='%') {
if (pstr[1] && pstr[2]) {
*pbuf++ = hex_2_int(pstr[1]) << 4 | hex_2_int(pstr[2]);
pstr += 2;
}
}
else if (*pstr=='+') {
*pbuf++ = ' ';
}
else {
*pbuf++ = *pstr;
}
pstr++;
}
*pbuf = '\0';
return buf;
}
#define DEF_INCOMING_CHARSET "iso-8859-1"
#define DEF_DISPLAY_CHARSET "utf-8"
#define MAX_IMF_LINE 666
static int to_be_quoted(const char * word, size_t size)
{
const char* cur = word;
size_t i = 0;
for (i = 0; i < size; i++)
{
switch (*cur)
{
case ',':
case ':':
case '!':
case '"':
case '#':
case '$':
case '@':
case '[':
case '\\':
case ']':
case '^':
case '`':
case '{':
case '|':
case '}':
case '~':
case '=':
case '?':
case '_':
return 1;
default:
if (((unsigned char)*cur) >= 128) {
return 1;
}
break;
}
cur++;
}
return 0;
}
static int quote_word(const char* display_charset, MMAPString* mmapstr, const char* word, size_t size)
{
const char* cur = NULL;
size_t i = 0;
char hex[4];
int col = 0;
if (mmap_string_append(mmapstr, "=?")==NULL) {
return 0;
}
if (mmap_string_append(mmapstr, display_charset)==NULL) {
return 0;
}
if (mmap_string_append(mmapstr, "?Q?")==NULL) {
return 0;
}
col = mmapstr->len;
cur = word;
for(i = 0 ; i < size ; i ++)
{
int do_quote_char;
#if MAX_IMF_LINE != 666
if (col + 2
+ 3
+ 1 >= MAX_IMF_LINE)
{
int old_pos;
if (mmap_string_append(mmapstr, "?=")==NULL) {
return 0;
}
if (mmap_string_append(mmapstr, " ")==NULL) {
return 0;
}
old_pos = mmapstr->len;
if (mmap_string_append(mmapstr, "=?")==NULL) {
return 0;
}
if (mmap_string_append(mmapstr, display_charset)==NULL) {
return 0;
}
if (mmap_string_append(mmapstr, "?Q?")==NULL) {
return 0;
}
col = mmapstr->len - old_pos;
}
#endif
do_quote_char = 0;
switch (*cur)
{
case ',':
case ':':
case '!':
case '"':
case '#':
case '$':
case '@':
case '[':
case '\\':
case ']':
case '^':
case '`':
case '{':
case '|':
case '}':
case '~':
case '=':
case '?':
case '_':
do_quote_char = 1;
break;
default:
if (((unsigned char) * cur) >= 128) {
do_quote_char = 1;
}
break;
}
if (do_quote_char)
{
snprintf(hex, 4, "=%2.2X", (unsigned char) * cur);
if (mmap_string_append(mmapstr, hex)==NULL) {
return 0;
}
col += 3;
}
else
{
if (* cur==' ') {
if (mmap_string_append_c(mmapstr, '_')==NULL) {
return 0;
}
}
else {
if (mmap_string_append_c(mmapstr, * cur)==NULL) {
return 0;
}
}
col += 3;
}
cur++;
}
if (mmap_string_append(mmapstr, "?=")==NULL) {
return 0;
}
return 1;
}
static void get_word(const char* begin, const char** pend, int* pto_be_quoted)
{
const char* cur = begin;
while ((* cur != ' ') && (* cur != '\t') && (* cur != '\0')) {
cur ++;
}
#if MAX_IMF_LINE != 666
if (cur - begin +
1 > MAX_IMF_LINE)
*pto_be_quoted = 1;
else
#endif
*pto_be_quoted = to_be_quoted(begin, cur - begin);
*pend = cur;
}
char* dc_encode_header_words(const char* to_encode)
{
char* ret_str = NULL;
const char* cur = to_encode;
MMAPString* mmapstr = mmap_string_new("");
if (to_encode==NULL || mmapstr==NULL) {
goto cleanup;
}
while (* cur != '\0')
{
const char * begin;
const char * end;
int do_quote;
int quote_words;
begin = cur;
end = begin;
quote_words = 0;
do_quote = 1;
while (* cur != '\0')
{
get_word(cur, &cur, &do_quote);
if (do_quote) {
quote_words = 1;
end = cur;
}
else {
break;
}
if (* cur != '\0') {
cur ++;
}
}
if (quote_words)
{
if ( !quote_word(DEF_DISPLAY_CHARSET, mmapstr, begin, end - begin)) {
goto cleanup;
}
if ((* end==' ') || (* end=='\t')) {
if (mmap_string_append_c(mmapstr, * end)==0) {
goto cleanup;
}
end ++;
}
if (* end != '\0') {
if (mmap_string_append_len(mmapstr, end, cur - end)==NULL) {
goto cleanup;
}
}
}
else
{
if (mmap_string_append_len(mmapstr, begin, cur - begin)==NULL) {
goto cleanup;
}
}
if ((* cur==' ') || (* cur=='\t')) {
if (mmap_string_append_c(mmapstr, * cur)==0) {
goto cleanup;
}
cur ++;
}
}
ret_str = strdup(mmapstr->str);
cleanup:
if (mmapstr) {
mmap_string_free(mmapstr);
}
return ret_str;
}
char* dc_decode_header_words(const char* in)
{
if (in==NULL) {
return NULL;
}
char* out = NULL;
size_t cur_token = 0;
int r = mailmime_encoded_phrase_parse(DEF_INCOMING_CHARSET, in, strlen(in), &cur_token, DEF_DISPLAY_CHARSET, &out);
if (r != MAILIMF_NO_ERROR || out==NULL) {
out = dc_strdup(in);
}
return out;
}
static const char base64chars[] =
"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+,";
char* dc_encode_modified_utf7(const char* to_encode, int change_spaces)
{
#define UTF16MASK 0x03FFUL
#define UTF16SHIFT 10
#define UTF16BASE 0x10000UL
#define UTF16HIGHSTART 0xD800UL
#define UTF16HIGHEND 0xDBFFUL
#define UTF16LOSTART 0xDC00UL
#define UTF16LOEND 0xDFFFUL
#define UNDEFINED 64
unsigned int utf8pos = 0;
unsigned int utf8total = 0;
unsigned int c = 0;
unsigned int utf7mode = 0;
unsigned int bitstogo = 0;
unsigned int utf16flag = 0;
unsigned long ucs4 = 0;
unsigned long bitbuf = 0;
char* dst = NULL;
char* res = NULL;
if (!to_encode) {
return dc_strdup("");
}
res = (char*)malloc(2*strlen(to_encode)+1);
dst = res;
if(!dst) {
exit(51);
}
utf7mode = 0;
utf8total = 0;
bitstogo = 0;
utf8pos = 0;
while ((c = (unsigned char)*to_encode) != '\0')
{
++to_encode;
if (c >= ' ' && c <= '~' && (c != '_' || !change_spaces)) {
if (utf7mode) {
if (bitstogo) {
*dst++ = base64chars[(bitbuf << (6 - bitstogo)) & 0x3F];
}
*dst++ = '-';
utf7mode = 0;
utf8pos = 0;
bitstogo = 0;
utf8total= 0;
}
if (change_spaces && c==' ') {
*dst++ = '_';
}
else {
*dst++ = c;
}
if (c=='&') {
*dst++ = '-';
}
continue;
}
if (!utf7mode) {
*dst++ = '&';
utf7mode = 1;
}
if (c < 0x80) {
ucs4 = c;
}
else if (utf8total) {
ucs4 = (ucs4 << 6) | (c & 0x3FUL);
if (++utf8pos < utf8total) {
continue;
}
}
else {
utf8pos = 1;
if (c < 0xE0) {
utf8total = 2;
ucs4 = c & 0x1F;
}
else if (c < 0xF0) {
utf8total = 3;
ucs4 = c & 0x0F;
}
else {
utf8total = 4;
ucs4 = c & 0x03;
}
continue;
}
utf8total = 0;
do {
if (ucs4 >= UTF16BASE) {
ucs4 -= UTF16BASE;
bitbuf = (bitbuf << 16) | ((ucs4 >> UTF16SHIFT)
+ UTF16HIGHSTART);
ucs4 = (ucs4 & UTF16MASK) + UTF16LOSTART;
utf16flag = 1;
} else {
bitbuf = (bitbuf << 16) | ucs4;
utf16flag = 0;
}
bitstogo += 16;
while (bitstogo >= 6) {
bitstogo -= 6;
*dst++ = base64chars[(bitstogo ? (bitbuf >> bitstogo)
: bitbuf)
& 0x3F];
}
} while (utf16flag);
}
if (utf7mode) {
if (bitstogo) {
*dst++ = base64chars[(bitbuf << (6 - bitstogo)) & 0x3F];
}
*dst++ = '-';
}
*dst = '\0';
return res;
}
char* dc_decode_modified_utf7(const char *to_decode, int change_spaces)
{
unsigned c = 0;
unsigned i = 0;
unsigned bitcount = 0;
unsigned long ucs4 = 0;
unsigned long utf16 = 0;
unsigned long bitbuf = 0;
unsigned char base64[256];
const char* src = NULL;
char* dst = NULL;
char* res = NULL;
if (to_decode==NULL) {
return dc_strdup("");
}
res = (char*)malloc(4*strlen(to_decode)+1);
dst = res;
src = to_decode;
if(!dst) {
exit(52);
}
memset(base64, UNDEFINED, sizeof (base64));
for (i = 0; i < sizeof (base64chars); ++i) {
base64[(unsigned)base64chars[i]] = i;
}
while (*src != '\0')
{
c = *src++;
if (c != '&' || *src=='-') {
if (change_spaces && c=='_') {
*dst++ = ' ';
}
else {
*dst++ = c;
}
if (c=='&') ++src;
}
else {
bitbuf = 0;
bitcount = 0;
ucs4 = 0;
while ((c = base64[(unsigned char) *src]) != UNDEFINED) {
++src;
bitbuf = (bitbuf << 6) | c;
bitcount += 6;
if (bitcount >= 16)
{
bitcount -= 16;
utf16 = (bitcount ? bitbuf >> bitcount : bitbuf) & 0xffff;
if (utf16 >= UTF16HIGHSTART && utf16 <= UTF16HIGHEND) {
ucs4 = (utf16 - UTF16HIGHSTART) << UTF16SHIFT;
continue;
}
else if (utf16 >= UTF16LOSTART && utf16 <= UTF16LOEND) {
ucs4 += utf16 - UTF16LOSTART + UTF16BASE;
}
else {
ucs4 = utf16;
}
if (ucs4 <= 0x7fUL) {
dst[0] = ucs4;
dst += 1;
}
else if (ucs4 <= 0x7ffUL) {
dst[0] = 0xc0 | (ucs4 >> 6);
dst[1] = 0x80 | (ucs4 & 0x3f);
dst += 2;
}
else if (ucs4 <= 0xffffUL) {
dst[0] = 0xe0 | (ucs4 >> 12);
dst[1] = 0x80 | ((ucs4 >> 6) & 0x3f);
dst[2] = 0x80 | (ucs4 & 0x3f);
dst += 3;
}
else {
dst[0] = 0xf0 | (ucs4 >> 18);
dst[1] = 0x80 | ((ucs4 >> 12) & 0x3f);
dst[2] = 0x80 | ((ucs4 >> 6) & 0x3f);
dst[3] = 0x80 | (ucs4 & 0x3f);
dst += 4;
}
}
}
if (*src=='-') {
++src;
}
}
}
*dst = '\0';
return res;
}
int dc_needs_ext_header(const char* to_check)
{
if (to_check) {
while (*to_check)
{
if (!isalnum(*to_check) && *to_check!='-' && *to_check!='_' && *to_check!='.' && *to_check!='~') {
return 1;
}
to_check++;
}
}
return 0;
}
char* dc_encode_ext_header(const char* to_encode)
{
#define PREFIX "utf-8''"
const char *pstr = to_encode;
if (to_encode==NULL) {
return dc_strdup(PREFIX);
}
char *buf = malloc(strlen(PREFIX) + strlen(to_encode) * 3 + 1);
if (buf==NULL) {
exit(46);
}
char* pbuf = buf;
strcpy(pbuf, PREFIX);
pbuf += strlen(pbuf);
while (*pstr)
{
if (isalnum(*pstr) || *pstr=='-' || *pstr=='_' || *pstr=='.' || *pstr=='~') {
*pbuf++ = *pstr;
}
else {
*pbuf++ = '%', *pbuf++ = int_2_uppercase_hex(*pstr >> 4), *pbuf++ = int_2_uppercase_hex(*pstr & 15);
}
pstr++;
}
*pbuf = '\0';
return buf;
}
char* dc_decode_ext_header(const char* to_decode)
{
char* decoded = NULL;
char* charset = NULL;
const char* p2 = NULL;
if (to_decode==NULL) {
goto cleanup;
}
if ((p2=strchr(to_decode, '\''))==NULL
|| (p2==to_decode) ) {
goto cleanup;
}
charset = dc_null_terminate(to_decode, p2-to_decode);
p2++;
if ((p2=strchr(p2, '\''))==NULL) {
goto cleanup;
}
p2++;
decoded = dc_urldecode(p2);
if (charset!=NULL && strcmp(charset, "utf-8")!=0 && strcmp(charset, "UTF-8")!=0) {
char* converted = NULL;
int r = charconv("utf-8", charset, decoded, strlen(decoded), &converted);
if (r==MAIL_CHARCONV_NO_ERROR && converted != NULL) {
free(decoded);
decoded = converted;
}
else {
free(converted);
}
}
cleanup:
free(charset);
return decoded? decoded : dc_strdup(to_decode);
}