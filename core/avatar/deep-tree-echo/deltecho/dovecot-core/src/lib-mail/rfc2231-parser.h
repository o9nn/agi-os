#ifndef RFC2231_PARSER_H
#define RFC2231_PARSER_H
int ATTR_NOWARN_UNUSED_RESULT
rfc2231_parse(struct rfc822_parser_context *ctx,
const char *const **result_r);
#endif