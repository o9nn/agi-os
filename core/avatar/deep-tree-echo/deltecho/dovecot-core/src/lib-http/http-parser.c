#include "lib.h"
#include "net.h"
#include "str.h"
#include "strescape.h"
#include "http-url.h"
#include "http-parser.h"
const unsigned char _http_token_char_mask   = (1<<0)|(1<<1);
const unsigned char _http_value_char_mask   = (1<<0)|(1<<1)|(1<<2);
const unsigned char _http_text_char_mask    = (1<<0)|(1<<1)|(1<<2)|(1<<6);
const unsigned char _http_qdtext_char_mask  = (1<<3)|(1<<4)|(1<<6);
const unsigned char _http_ctext_char_mask   = (1<<3)|(1<<5)|(1<<6);
const unsigned char _http_token68_char_mask = (1<<0)|(1<<7);
const unsigned char _http_char_lookup[256] = {
0,  0,  0,  0,  0,  0,  0,  0,  0, 64,  0,  0,  0,  0,  0,   0,
0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,   0,
64, 10, 36, 50, 50, 50, 50, 50, 20, 20, 10,  9, 12,  9,  9, 140,
9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 12, 12, 12, 12, 12,  12,
12,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,   9,
9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 12,  4, 12, 10,   9,
10,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,   9,
9,  9,  9,  9,  9,  9,  9,  9,  9,  9,  9, 12, 10, 12,  9,   0,
64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,  64,
64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,  64,
64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,  64,
64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,  64,
64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,  64,
64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,  64,
64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,  64,
64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64, 64,  64,
};
void http_parser_init(struct http_parser *parser,
const unsigned char *data, size_t size)
{
i_zero(parser);
parser->begin = data;
parser->cur = data;
parser->end = data + size;
}
void http_parse_ows(struct http_parser *parser)
{
if (parser->cur >= parser->end)
return;
while (parser->cur < parser->end &&
(parser->cur[0] == ' ' || parser->cur[0] == '\t')) {
parser->cur++;
}
}
int http_parser_skip_token(struct http_parser *parser)
{
if (parser->cur >= parser->end || !http_char_is_token(*parser->cur))
return 0;
parser->cur++;
while (parser->cur < parser->end && http_char_is_token(*parser->cur))
parser->cur++;
return 1;
}
int http_parse_token(struct http_parser *parser, const char **token_r)
{
const unsigned char *first = parser->cur;
int ret;
if ((ret=http_parser_skip_token(parser)) <= 0)
return ret;
*token_r = t_strndup(first, parser->cur - first);
return 1;
}
int http_parse_token_list_next(struct http_parser *parser,
const char **token_r)
{
for (;;) {
if (http_parse_token(parser, token_r) > 0)
break;
http_parse_ows(parser);
if (parser->cur >= parser->end || parser->cur[0] != ',')
return 0;
parser->cur++;
http_parse_ows(parser);
}
return 1;
}
int http_parse_quoted_string(struct http_parser *parser, const char **str_r)
{
string_t *str;
if (parser->cur >= parser->end || parser->cur[0] != '"')
return 0;
parser->cur++;
str = t_str_new(256);
for (;;) {
const unsigned char *first;
first = parser->cur;
while (parser->cur < parser->end && http_char_is_qdtext(*parser->cur))
parser->cur++;
if (parser->cur >= parser->end)
return -1;
str_append_data(str, first, parser->cur - first);
if (*parser->cur == '"') {
parser->cur++;
break;
} else if (*parser->cur == '\\') {
parser->cur++;
if (parser->cur >= parser->end || !http_char_is_text(*parser->cur))
return -1;
str_append_c(str, *parser->cur);
parser->cur++;
} else {
return -1;
}
}
*str_r = str_c(str);
return 1;
}
int http_parse_token_or_qstring(struct http_parser *parser,
const char **word_r)
{
if (parser->cur >= parser->end)
return 0;
if (parser->cur[0] == '"')
return http_parse_quoted_string(parser, word_r);
return http_parse_token(parser, word_r);
}