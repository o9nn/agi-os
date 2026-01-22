#ifndef MESSAGE_SEARCH_H
#define MESSAGE_SEARCH_H
struct message_block;
struct message_part;
struct message_search_context;
enum message_search_flags {
MESSAGE_SEARCH_FLAG_SKIP_HEADERS	= 0x01
};
struct message_search_context *
message_search_init(const char *normalized_key_utf8,
normalizer_func_t *normalizer,
enum message_search_flags flags);
void message_search_deinit(struct message_search_context **ctx);
bool message_search_more(struct message_search_context *ctx,
struct message_block *raw_block);
bool message_search_more_get_decoded(struct message_search_context *ctx,
struct message_block *raw_block,
struct message_block *decoded_block_r);
bool message_search_more_decoded(struct message_search_context *ctx,
struct message_block *block);
void message_search_reset(struct message_search_context *ctx);
int message_search_msg(struct message_search_context *ctx,
struct istream *input, struct message_part *parts,
const char **error_r)
ATTR_NULL(3);
#endif