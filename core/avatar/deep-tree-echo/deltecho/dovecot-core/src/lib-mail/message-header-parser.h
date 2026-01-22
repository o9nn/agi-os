#ifndef MESSAGE_HEADER_PARSER_H
#define MESSAGE_HEADER_PARSER_H
#define IS_LWSP(c) \
((c) == ' ' || (c) == '\t')
struct message_size;
struct message_header_parser_ctx;
enum message_header_parser_flags {
MESSAGE_HEADER_PARSER_FLAG_SKIP_INITIAL_LWSP	= 0x01,
MESSAGE_HEADER_PARSER_FLAG_DROP_CR		= 0x02,
MESSAGE_HEADER_PARSER_FLAG_CLEAN_ONELINE	= 0x04
};
struct message_header_line {
const char *name;
size_t name_len;
const unsigned char *value;
size_t value_len;
const unsigned char *full_value;
size_t full_value_len;
const unsigned char *middle;
size_t middle_len;
uoff_t name_offset, full_value_offset;
bool continues:1;
bool continued:1;
bool eoh:1;
bool no_newline:1;
bool crlf_newline:1;
bool use_full_value:1;
};
typedef void message_header_callback_t(struct message_header_line *hdr,
void *context);
struct message_header_parser_ctx *
message_parse_header_init(struct istream *input, struct message_size *hdr_size,
enum message_header_parser_flags flags) ATTR_NULL(2);
void message_parse_header_deinit(struct message_header_parser_ctx **ctx);
int message_parse_header_next(struct message_header_parser_ctx *ctx,
struct message_header_line **hdr_r);
bool message_parse_header_has_nuls(const struct message_header_parser_ctx *ctx)
ATTR_PURE;
void message_parse_header(struct istream *input, struct message_size *hdr_size,
enum message_header_parser_flags flags,
message_header_callback_t *callback, void *context)
ATTR_NULL(2);
#define message_parse_header(input, hdr_size, flags, callback, context) \
message_parse_header(input, hdr_size, flags - \
CALLBACK_TYPECHECK(callback, void (*)( \
struct message_header_line *hdr, typeof(context))), \
(message_header_callback_t *)callback, context)
void message_header_line_write(buffer_t *output,
const struct message_header_line *hdr);
const char *
message_header_strdup(pool_t pool, const unsigned char *data, size_t size);
bool message_header_name_is_valid(const char *name);
#endif