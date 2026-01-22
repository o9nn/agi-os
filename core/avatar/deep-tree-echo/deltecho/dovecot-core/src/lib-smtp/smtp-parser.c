#include "lib.h"
#include "net.h"
#include "str.h"
#include "strescape.h"
#include "smtp-parser.h"
#include <ctype.h>
const uint16_t smtp_xtext_char_mask = (1<<0);
const uint16_t smtp_atext_char_mask = (1<<1);
const uint16_t smtp_dcontent_char_mask = (1<<1)|(1<<2)|(1<<9);
const uint16_t smtp_qtext_char_mask = (1<<1)|(1<<2)|(1<<3)|(1<<4);
const uint16_t smtp_textstr_char_mask = (1<<1)|(1<<2)|(1<<9)|(1<<3)|(1<<5);
const uint16_t smtp_esmtp_value_char_mask = (1<<0)|(1<<6);
const uint16_t smtp_ehlo_param_char_mask = (1<<0)|(1<<6)|(1<<7);
const uint16_t smtp_ehlo_greet_char_mask = (1<<0)|(1<<6)|(1<<7)|(1<<8);
const uint16_t smtp_qpair_char_mask = (1<<0)|(1<<3)|(1<<6)|(1<<7);
const uint16_t smtp_char_lookup[256] = {
0x100, 0x100, 0x100, 0x100, 0x100, 0x100, 0x100, 0x100,
0x100, 0x120, 0x000, 0x100, 0x100, 0x000, 0x100, 0x100,
0x100, 0x100, 0x100, 0x100, 0x100, 0x100, 0x100, 0x100,
0x100, 0x100, 0x100, 0x100, 0x100, 0x100, 0x100, 0x100,
0x108, 0x003, 0x201, 0x003, 0x003, 0x003, 0x003, 0x003,
0x005, 0x005, 0x003, 0x042, 0x005, 0x003, 0x005, 0x003,
0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003,
0x003, 0x003, 0x005, 0x005, 0x005, 0x082, 0x005, 0x003,
0x005, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003,
0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003,
0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003,
0x003, 0x003, 0x003, 0x031, 0x021, 0x031, 0x003, 0x003,
0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003,
0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003,
0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003,
0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x003, 0x100,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000, 0x000,
};
void smtp_parser_init(struct smtp_parser *parser,
pool_t pool, const char *data)
{
parser->pool = pool;
parser->begin = parser->cur = (const unsigned char *)data;
parser->end = parser->begin + strlen(data);
parser->error = NULL;
}
static int
smtp_parser_parse_ldh_str(struct smtp_parser *parser,
string_t *out)
{
const unsigned char *pbegin = parser->cur, *palnum;
palnum = NULL;
while (parser->cur < parser->end) {
if (i_isalnum(*parser->cur))
palnum = parser->cur;
else if (*parser->cur != '-')
break;
parser->cur++;
}
if (parser->cur == pbegin || palnum == NULL) {
parser->cur = pbegin;
return 0;
}
parser->cur = palnum+1;
if (out != NULL)
str_append_data(out, pbegin, parser->cur - pbegin);
return 1;
}
int smtp_parser_parse_domain(struct smtp_parser *parser,
const char **value_r)
{
string_t *value = NULL;
if (parser->cur >= parser->end ||
(!i_isalnum(*parser->cur) && *parser->cur != '-' &&
*parser->cur != '_'))
return 0;
if (value_r != NULL)
value = t_str_new(256);
for (;;) {
if (parser->cur >= parser->end || *parser->cur == '.') {
parser->error = "Empty sub-domain";
return -1;
}
if (!i_isalnum(*parser->cur) && *parser->cur != '-' &&
*parser->cur != '_') {
parser->error = "Invalid character in domain";
return -1;
}
if (value_r != NULL)
str_append_c(value, *parser->cur);
parser->cur++;
while (parser->cur < parser->end) {
if (!i_isalnum(*parser->cur) && *parser->cur != '-' &&
*parser->cur != '_')
break;
if (value_r != NULL)
str_append_c(value, *parser->cur);
parser->cur++;
}
if (parser->cur >= parser->end || *parser->cur != '.')
break;
if (value_r != NULL)
str_append_c(value, '.');
parser->cur++;
}
if (value_r != NULL)
*value_r = str_c(value);
return 1;
}
static int
smtp_parser_parse_snum(struct smtp_parser *parser, string_t *literal,
uint8_t *octet_r)
{
const unsigned char *pbegin = parser->cur;
uint8_t octet = 0;
if (*parser->cur < '0' || *parser->cur > '9')
return 0;
do {
if (octet >= ((uint8_t)-1 / 10)) {
if (octet > (uint8_t)-1 / 10)
return -1;
if ((uint8_t)(*parser->cur - '0') > ((uint8_t)-1 % 10))
return -1;
}
octet = octet * 10 + (*parser->cur - '0');
parser->cur++;
} while (*parser->cur >= '0' && *parser->cur <= '9');
if (literal != NULL)
str_append_data(literal, pbegin, parser->cur - pbegin);
*octet_r = octet;
return 1;
}
static int
smtp_parser_parse_ipv4_address(struct smtp_parser *parser,
string_t *literal, struct in_addr *ip4_r)
{
uint8_t octet;
uint32_t ip = 0;
int ret;
int i;
if ((ret = smtp_parser_parse_snum(parser, literal, &octet)) <= 0)
return ret;
ip = octet;
for (i = 0; i < 3 && parser->cur < parser->end; i++) {
if (*parser->cur != '.')
return -1;
if (literal != NULL)
str_append_c(literal, '.');
parser->cur++;
if (smtp_parser_parse_snum(parser, literal, &octet) <= 0)
return -1;
ip = (ip << 8) + octet;
}
if (ip4_r != NULL)
ip4_r->s_addr = htonl(ip);
return 1;
}
int smtp_parser_parse_address_literal(struct smtp_parser *parser,
const char **value_r, struct ip_addr *ip_r)
{
const unsigned char *pblock;
struct in_addr ip4;
bool ipv6 = FALSE;
string_t *value = NULL, *tagbuf;
int ret;
if (parser->cur >= parser->end || *parser->cur != '[')
return 0;
parser->cur++;
if (value_r != NULL) {
value = t_str_new(128);
str_append_c(value, '[');
}
if (ip_r != NULL)
i_zero(ip_r);
i_zero(&ip4);
if ((ret=smtp_parser_parse_ipv4_address(parser, value, &ip4)) != 0) {
if (ret < 0) {
parser->error = "Invalid IPv4 address literal";
return -1;
}
if (ip_r != NULL) {
ip_r->family = AF_INET;
ip_r->u.ip4 = ip4;
}
} else {
if (value_r != NULL) {
tagbuf = value;
} else {
tagbuf = t_str_new(16);
str_append_c(tagbuf, '[');
}
if (smtp_parser_parse_ldh_str(parser, tagbuf) <= 0 ||
parser->cur >= parser->end || *parser->cur != ':') {
parser->error = "Invalid address literal";
return -1;
}
if (strcasecmp(str_c(tagbuf)+1, "IPv6") == 0)
ipv6 = TRUE;
else if (value_r == NULL) {
parser->error = t_strdup_printf(
"Unsupported %s address literal",
str_c(tagbuf)+1);
return -1;
}
parser->cur++;
if (value_r != NULL)
str_append_c(value, ':');
pblock = parser->cur;
while (parser->cur < parser->end &&
smtp_char_is_dcontent(*parser->cur))
parser->cur++;
if (parser->cur == pblock) {
parser->error = "Empty address literal";
return -1;
}
if (value_r != NULL)
str_append_data(value, pblock, parser->cur - pblock);
if (ipv6) {
struct ip_addr ip;
if (net_addr2ip(t_strndup(pblock, parser->cur - pblock),
&ip) < 0) {
parser->error = "Invalid IPv6 address literal";
return -1;
}
if (ip_r != NULL)
*ip_r = ip;
}
}
if (parser->cur >= parser->end) {
parser->error = "Missing ']' at end of address literal";
return -1;
} else if (*parser->cur != ']') {
parser->error = "Invalid character in address literal";
return -1;
}
parser->cur++;
if (value_r != NULL) {
str_append_c(value, ']');
*value_r = str_c(value);
}
return 1;
}
int smtp_parser_parse_quoted_string(struct smtp_parser *parser,
const char **value_r)
{
string_t *value = NULL;
const unsigned char *pbegin;
if (parser->cur >= parser->end || *parser->cur != '"')
return 0;
parser->cur++;
if (value_r != NULL)
value = t_str_new(256);
while (parser->cur < parser->end) {
pbegin = parser->cur;
while (parser->cur < parser->end &&
smtp_char_is_qtext(*parser->cur)) {
parser->cur++;
}
if (value_r != NULL)
str_append_data(value, pbegin, parser->cur - pbegin);
if (parser->cur >= parser->end || *parser->cur != '\\')
break;
parser->cur++;
if (parser->cur >= parser->end ||
!smtp_char_is_qpair(*parser->cur)) {
parser->error =
"Invalid character after '\\' in quoted string";
return -1;
}
if (value_r != NULL)
str_append_c(value, *parser->cur);
parser->cur++;
}
if (parser->cur >= parser->end) {
parser->error = "Premature end of quoted string";
return -1;
}
if (*parser->cur != '"') {
parser->error = "Invalid character in quoted string";
return -1;
}
parser->cur++;
if (value_r != NULL)
*value_r = str_c(value);
return 1;
}
static int
smtp_parser_skip_atom(struct smtp_parser *parser)
{
if (parser->cur >= parser->end || !smtp_char_is_atext(*parser->cur))
return 0;
parser->cur++;
while (parser->cur < parser->end && smtp_char_is_atext(*parser->cur))
parser->cur++;
return 1;
}
int smtp_parser_parse_atom(struct smtp_parser *parser,
const char **value_r)
{
const unsigned char *pbegin = parser->cur;
int ret;
if ((ret=smtp_parser_skip_atom(parser)) <= 0)
return ret;
if (value_r != NULL)
*value_r = t_strndup(pbegin, parser->cur - pbegin);
return 1;
}
int smtp_parser_parse_string(struct smtp_parser *parser,
const char **value_r)
{
int ret;
if ((ret=smtp_parser_parse_quoted_string(parser, value_r)) != 0)
return ret;
return smtp_parser_parse_atom(parser, value_r);
}
static bool
smtp_parse_xtext_hexdigit(const unsigned char digit,
unsigned char *hexvalue)
{
switch (digit) {
case '0': case '1': case '2': case '3': case '4':
case '5': case '6': case '7': case '8': case '9':
*hexvalue = (*hexvalue) << 4;
*hexvalue += digit - '0';
break;
case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
*hexvalue = (*hexvalue) << 4;
*hexvalue += digit - 'A' + 10;
break;
default:
return FALSE;
}
return TRUE;
}
int smtp_parser_parse_xtext(struct smtp_parser *parser,
string_t *out)
{
unsigned char hexchar;
if (parser->cur >= parser->end ||
(!smtp_char_is_xtext(*parser->cur) && *parser->cur != '+'))
return 0;
while (parser->cur < parser->end) {
const unsigned char *pbegin = parser->cur;
while (parser->cur < parser->end &&
smtp_char_is_xtext(*parser->cur))
parser->cur++;
if (out != NULL)
str_append_data(out, pbegin, parser->cur - pbegin);
if (parser->cur >= parser->end || *parser->cur != '+')
break;
parser->cur++;
hexchar = 0;
if (smtp_parse_xtext_hexdigit(*parser->cur, &hexchar)) {
parser->cur++;
if (smtp_parse_xtext_hexdigit(*parser->cur, &hexchar)) {
parser->cur++;
if (out != NULL)
str_append_c(out, hexchar);
continue;
}
}
parser->error = "Invalid hexchar after '+' in xtext";
return -1;
}
return 1;
}