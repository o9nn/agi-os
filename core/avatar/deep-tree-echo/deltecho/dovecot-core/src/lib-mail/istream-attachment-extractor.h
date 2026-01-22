#ifndef ISTREAM_ATTACHMENT_H
#define ISTREAM_ATTACHMENT_H
struct istream_attachment_header {
struct message_part *part;
const char *content_type, *content_disposition;
};
struct istream_attachment_info {
const char *hash;
uoff_t start_offset;
uoff_t encoded_size;
unsigned int base64_blocks_per_line;
bool base64_have_crlf;
const struct message_part *part;
};
struct istream_attachment_settings {
uoff_t min_size;
struct hash_format *hash_format;
bool drain_parent_input;
bool (*want_attachment)(const struct istream_attachment_header *hdr,
void *context);
int (*open_temp_fd)(void *context);
int (*open_attachment_ostream)(struct istream_attachment_info *info,
struct ostream **output_r,
const char **error_r, void *context);
int (*close_attachment_ostream)(struct ostream *output, bool success,
const char **error, void *context);
};
struct istream *
i_stream_create_attachment_extractor(struct istream *input,
struct istream_attachment_settings *set,
void *context) ATTR_NULL(3);
bool i_stream_attachment_extractor_can_retry(struct istream *input);
#endif