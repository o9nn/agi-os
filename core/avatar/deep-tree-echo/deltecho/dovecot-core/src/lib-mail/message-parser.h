#ifndef MESSAGE_PARSER_H
#define MESSAGE_PARSER_H
#include "message-header-parser.h"
#include "message-part.h"
enum message_parser_flags {
MESSAGE_PARSER_FLAG_SKIP_BODY_BLOCK		= 0x01,
MESSAGE_PARSER_FLAG_MIME_VERSION_STRICT		= 0x02,
MESSAGE_PARSER_FLAG_INCLUDE_MULTIPART_BLOCKS	= 0x04,
MESSAGE_PARSER_FLAG_INCLUDE_BOUNDARIES		= 0x08
};
#define MESSAGE_PARSER_DEFAULT_MAX_NESTED_MIME_PARTS 100
#define MESSAGE_PARSER_DEFAULT_MAX_TOTAL_MIME_PARTS 10000
struct message_parser_settings {
enum message_header_parser_flags hdr_flags;
enum message_parser_flags flags;
unsigned int max_nested_mime_parts;
unsigned int max_total_mime_parts;
};
struct message_parser_ctx;
struct message_block {
struct message_part *part;
struct message_header_line *hdr;
const unsigned char *data;
size_t size;
};
typedef void message_part_header_callback_t(struct message_part *part,
struct message_header_line *hdr,
void *context);
extern message_part_header_callback_t *null_message_part_header_callback;
struct message_parser_ctx *
message_parser_init(pool_t part_pool, struct istream *input,
const struct message_parser_settings *set);
void message_parser_deinit(struct message_parser_ctx **ctx,
struct message_part **parts_r);
struct message_parser_ctx *
message_parser_init_from_parts(struct message_part *parts,
struct istream *input,
const struct message_parser_settings *set);
int message_parser_deinit_from_parts(struct message_parser_ctx **_ctx,
struct message_part **parts_r,
const char **error_r);
int message_parser_parse_next_block(struct message_parser_ctx *ctx,
struct message_block *block_r);
void message_parser_parse_header(struct message_parser_ctx *ctx,
struct message_size *hdr_size,
message_part_header_callback_t *callback,
void *context) ATTR_NULL(4);
#define message_parser_parse_header(ctx, hdr_size, callback, context) \
message_parser_parse_header(ctx, hdr_size - \
CALLBACK_TYPECHECK(callback, void (*)( \
struct message_part *, \
struct message_header_line *, typeof(context))), \
(message_part_header_callback_t *)callback, context)
void message_parser_parse_body(struct message_parser_ctx *ctx,
message_part_header_callback_t *hdr_callback,
void *context) ATTR_NULL(3);
#define message_parser_parse_body(ctx, callback, context) \
message_parser_parse_body(ctx, \
(message_part_header_callback_t *)callback, \
(void *)((uintptr_t)context - CALLBACK_TYPECHECK(callback, \
void (*)(struct message_part *, \
struct message_header_line *, typeof(context)))))
#endif