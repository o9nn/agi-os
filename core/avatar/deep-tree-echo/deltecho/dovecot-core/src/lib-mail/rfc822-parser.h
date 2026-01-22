#ifndef RFC822_PARSER_H
#define RFC822_PARSER_H
#include "unichar.h"
#define RFC822_NUL_REPLACEMENT_STR UNICODE_REPLACEMENT_CHAR_UTF8
struct rfc822_parser_context {
const unsigned char *data, *end;
string_t *last_comment;
const char *nul_replacement_str;
};
#define IS_ATEXT(c) \
(rfc822_atext_chars[(int)(unsigned char)(c)] != 0)
#define IS_ATEXT_NON_TSPECIAL(c) \
((rfc822_atext_chars[(int)(unsigned char)(c)] & 3) != 0)
extern unsigned char rfc822_atext_chars[256];
void rfc822_parser_init(struct rfc822_parser_context *ctx,
const unsigned char *data, size_t size,
string_t *last_comment) ATTR_NULL(4);
static inline void rfc822_parser_deinit(struct rfc822_parser_context *ctx)
{
i_assert(ctx->data <= ctx->end);
ctx->data = ctx->end = NULL;
}
int rfc822_skip_comment(struct rfc822_parser_context *ctx);
int ATTR_NOWARN_UNUSED_RESULT
rfc822_skip_lwsp(struct rfc822_parser_context *ctx);
int rfc822_parse_atom(struct rfc822_parser_context *ctx, string_t *str);
int rfc822_parse_dot_atom(struct rfc822_parser_context *ctx, string_t *str);
int rfc822_parse_mime_token(struct rfc822_parser_context *ctx, string_t *str);
int rfc822_parse_quoted_string(struct rfc822_parser_context *ctx,
string_t *str);
int rfc822_parse_phrase(struct rfc822_parser_context *ctx, string_t *str);
int rfc822_parse_domain(struct rfc822_parser_context *ctx, string_t *str);
int rfc822_parse_content_type(struct rfc822_parser_context *ctx, string_t *str);
int rfc822_parse_content_param(struct rfc822_parser_context *ctx,
const char **key_r, string_t *value);
#endif