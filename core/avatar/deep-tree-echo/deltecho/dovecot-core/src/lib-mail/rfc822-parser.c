#include "lib.h"
#include "str.h"
#include "strescape.h"
#include "rfc822-parser.h"
unsigned char rfc822_atext_chars[256] = {
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 1, 0, 1, 1, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 4,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 4, 0, 4,
0, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 0, 0, 0, 1, 1,
1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2
};
void rfc822_parser_init(struct rfc822_parser_context *ctx,
const unsigned char *data, size_t size,
string_t *last_comment)
{
i_zero(ctx);
ctx->data = data;
ctx->end = data + size;
ctx->last_comment = last_comment;
}
int rfc822_skip_comment(struct rfc822_parser_context *ctx)
{
const unsigned char *start;
size_t len;
int level = 1;
i_assert(*ctx->data == '(');
if (ctx->last_comment != NULL)
str_truncate(ctx->last_comment, 0);
start = ++ctx->data;
for (; ctx->data < ctx->end; ctx->data++) {
switch (*ctx->data) {
case '\0':
if (ctx->last_comment != NULL &&
ctx->nul_replacement_str != NULL) {
str_append_data(ctx->last_comment, start,
ctx->data - start);
str_append(ctx->last_comment,
ctx->nul_replacement_str);
start = ctx->data + 1;
}
break;
case '(':
level++;
break;
case ')':
if (--level == 0) {
if (ctx->last_comment != NULL) {
str_append_data(ctx->last_comment, start,
ctx->data - start);
}
ctx->data++;
return ctx->data < ctx->end ? 1 : 0;
}
break;
case '\n':
if (ctx->last_comment == NULL)
break;
len = ctx->data - start;
if (len > 0 && start[len-1] == '\r')
len--;
str_append_data(ctx->last_comment, start, len);
start = ctx->data + 1;
break;
case '\\':
ctx->data++;
if (ctx->data >= ctx->end)
return -1;
if (*ctx->data == '\r' || *ctx->data == '\n' ||
*ctx->data == '\0') {
ctx->data--;
break;
}
if (ctx->last_comment != NULL) {
str_append_data(ctx->last_comment, start,
ctx->data - start - 1);
}
start = ctx->data;
break;
}
}
return -1;
}
int rfc822_skip_lwsp(struct rfc822_parser_context *ctx)
{
for (; ctx->data < ctx->end;) {
if (*ctx->data == ' ' || *ctx->data == '\t' ||
*ctx->data == '\r' || *ctx->data == '\n') {
ctx->data++;
continue;
}
if (*ctx->data != '(')
break;
if (rfc822_skip_comment(ctx) < 0)
return -1;
}
return ctx->data < ctx->end ? 1 : 0;
}
int rfc822_parse_atom(struct rfc822_parser_context *ctx, string_t *str)
{
const unsigned char *start;
if (ctx->data >= ctx->end || !IS_ATEXT(*ctx->data))
return -1;
for (start = ctx->data++; ctx->data < ctx->end; ctx->data++) {
if (IS_ATEXT(*ctx->data))
continue;
str_append_data(str, start, ctx->data - start);
return rfc822_skip_lwsp(ctx);
}
str_append_data(str, start, ctx->data - start);
return 0;
}
int rfc822_parse_dot_atom(struct rfc822_parser_context *ctx, string_t *str)
{
const unsigned char *start;
int ret;
if (ctx->data >= ctx->end || !IS_ATEXT(*ctx->data))
return -1;
for (start = ctx->data++; ctx->data < ctx->end; ) {
if (IS_ATEXT(*ctx->data)) {
ctx->data++;
continue;
}
if (start == ctx->data)
return -1;
str_append_data(str, start, ctx->data - start);
if ((ret = rfc822_skip_lwsp(ctx)) <= 0)
return ret;
if (*ctx->data != '.')
return 1;
ctx->data++;
str_append_c(str, '.');
if (rfc822_skip_lwsp(ctx) <= 0)
return -1;
start = ctx->data;
}
i_assert(start != ctx->data);
str_append_data(str, start, ctx->data - start);
return 0;
}
int rfc822_parse_mime_token(struct rfc822_parser_context *ctx, string_t *str)
{
const unsigned char *start;
for (start = ctx->data; ctx->data < ctx->end; ctx->data++) {
if (IS_ATEXT_NON_TSPECIAL(*ctx->data) || *ctx->data == '.')
continue;
str_append_data(str, start, ctx->data - start);
return rfc822_skip_lwsp(ctx);
}
str_append_data(str, start, ctx->data - start);
return 0;
}
int rfc822_parse_quoted_string(struct rfc822_parser_context *ctx, string_t *str)
{
const unsigned char *start;
size_t len;
i_assert(ctx->data < ctx->end);
i_assert(*ctx->data == '"');
ctx->data++;
for (start = ctx->data; ctx->data < ctx->end; ctx->data++) {
switch (*ctx->data) {
case '\0':
if (ctx->nul_replacement_str != NULL) {
str_append_data(str, start, ctx->data - start);
str_append(str, ctx->nul_replacement_str);
start = ctx->data + 1;
}
break;
case '"':
str_append_data(str, start, ctx->data - start);
ctx->data++;
return rfc822_skip_lwsp(ctx);
case '\n':
len = ctx->data - start;
if (len > 0 && start[len-1] == '\r')
len--;
str_append_data(str, start, len);
start = ctx->data + 1;
break;
case '\\':
ctx->data++;
if (ctx->data >= ctx->end)
return -1;
if (*ctx->data == '\r' || *ctx->data == '\n' ||
*ctx->data == '\0') {
ctx->data--;
break;
}
str_append_data(str, start, ctx->data - start - 1);
start = ctx->data;
break;
}
}
return -1;
}
static int
rfc822_parse_atom_or_dot(struct rfc822_parser_context *ctx, string_t *str)
{
const unsigned char *start;
for (start = ctx->data; ctx->data < ctx->end; ctx->data++) {
if (IS_ATEXT(*ctx->data) || *ctx->data == '.')
continue;
str_append_data(str, start, ctx->data - start);
return rfc822_skip_lwsp(ctx);
}
str_append_data(str, start, ctx->data - start);
return 0;
}
int rfc822_parse_phrase(struct rfc822_parser_context *ctx, string_t *str)
{
int ret;
if (ctx->data >= ctx->end)
return 0;
if (*ctx->data == '.')
return -1;
for (;;) {
if (*ctx->data == '"')
ret = rfc822_parse_quoted_string(ctx, str);
else
ret = rfc822_parse_atom_or_dot(ctx, str);
if (ret <= 0)
return ret;
if (!IS_ATEXT(*ctx->data) && *ctx->data != '"'
&& *ctx->data != '.')
break;
str_append_c(str, ' ');
}
return rfc822_skip_lwsp(ctx);
}
static int
rfc822_parse_domain_literal(struct rfc822_parser_context *ctx, string_t *str)
{
const unsigned char *start;
size_t len;
i_assert(ctx->data < ctx->end);
i_assert(*ctx->data == '[');
for (start = ctx->data++; ctx->data < ctx->end; ctx->data++) {
switch (*ctx->data) {
case '\0':
if (ctx->nul_replacement_str != NULL) {
str_append_data(str, start, ctx->data - start);
str_append(str, ctx->nul_replacement_str);
start = ctx->data + 1;
}
break;
case '[':
return -1;
case ']':
str_append_data(str, start, ctx->data - start + 1);
ctx->data++;
return rfc822_skip_lwsp(ctx);
case '\n':
len = ctx->data - start;
if (len > 0 && start[len-1] == '\r')
len--;
str_append_data(str, start, len);
start = ctx->data + 1;
break;
case '\\':
ctx->data++;
if (ctx->data >= ctx->end)
return -1;
if (*ctx->data == '\r' || *ctx->data == '\n' ||
*ctx->data == '\0') {
str_append_data(str, start, ctx->data - start);
start = ctx->data;
ctx->data--;
break;
}
}
}
return -1;
}
int rfc822_parse_domain(struct rfc822_parser_context *ctx, string_t *str)
{
i_assert(ctx->data < ctx->end);
i_assert(*ctx->data == '@');
ctx->data++;
if (rfc822_skip_lwsp(ctx) <= 0)
return -1;
if (*ctx->data == '[')
return rfc822_parse_domain_literal(ctx, str);
else
return rfc822_parse_dot_atom(ctx, str);
}
int rfc822_parse_content_type(struct rfc822_parser_context *ctx, string_t *str)
{
size_t str_pos_0 = str->used;
if (rfc822_skip_lwsp(ctx) <= 0)
return -1;
if (rfc822_parse_mime_token(ctx, str) <= 0 ||
str->used == str_pos_0)
return -1;
if (*ctx->data != '/') {
str_truncate(str, str_pos_0);
return -1;
}
ctx->data++;
if (rfc822_skip_lwsp(ctx) <= 0) {
str_truncate(str, str_pos_0);
return -1;
}
str_append_c(str, '/');
size_t str_pos = str->used;
int ret;
if ((ret = rfc822_parse_mime_token(ctx, str)) < 0 ||
str->used == str_pos ||
(ctx->data != ctx->end && *ctx->data != ';')) {
str_truncate(str, str_pos_0);
return -1;
}
return ret;
}
int rfc822_parse_content_param(struct rfc822_parser_context *ctx,
const char **key_r, string_t *value)
{
string_t *key;
int ret;
*key_r = NULL;
str_truncate(value, 0);
if (ctx->data >= ctx->end)
return 0;
if (*ctx->data != ';')
return -1;
ctx->data++;
if (rfc822_skip_lwsp(ctx) <= 0)
return -1;
key = t_str_new(64);
if (rfc822_parse_mime_token(ctx, key) <= 0)
return -1;
if (*ctx->data != '=')
return -1;
ctx->data++;
if ((ret = rfc822_skip_lwsp(ctx)) <= 0) {
} else if (*ctx->data == '"') {
ret = rfc822_parse_quoted_string(ctx, value);
} else if (ctx->data < ctx->end && *ctx->data == '=') {
while (ctx->data < ctx->end && *ctx->data != ';' &&
*ctx->data != ' ' && *ctx->data != '\t' &&
*ctx->data != '\r' && *ctx->data != '\n') {
str_append_c(value, *ctx->data);
ctx->data++;
}
} else {
ret = rfc822_parse_mime_token(ctx, value);
}
*key_r = str_c(key);
return ret < 0 ? -1 : 1;
}