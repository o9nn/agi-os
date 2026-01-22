#ifndef MESSAGE_DECODER_H
#define MESSAGE_DECODER_H
#include "unichar.h"
struct message_header_line;
enum message_cte {
MESSAGE_CTE_UNKNOWN = 0,
MESSAGE_CTE_78BIT,
MESSAGE_CTE_BINARY,
MESSAGE_CTE_QP,
MESSAGE_CTE_BASE64
};
enum message_decoder_flags {
MESSAGE_DECODER_FLAG_RETURN_BINARY	= 0x02
};
struct message_block;
struct message_decoder_context *
message_decoder_init(normalizer_func_t *normalizer,
enum message_decoder_flags flags);
void message_decoder_deinit(struct message_decoder_context **ctx);
void message_decoder_set_return_binary(struct message_decoder_context *ctx,
bool set);
bool message_decoder_decode_next_block(struct message_decoder_context *ctx,
struct message_block *input,
struct message_block *output);
const char *
message_decoder_current_content_type(struct message_decoder_context *ctx);
void message_decoder_decode_reset(struct message_decoder_context *ctx);
enum message_cte message_decoder_parse_cte(const struct message_header_line *hdr);
#endif