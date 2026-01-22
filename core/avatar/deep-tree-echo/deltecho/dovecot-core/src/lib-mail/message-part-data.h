#ifndef MESSAGE_PART_DATA_H
#define MESSAGE_PART_DATA_H
#include "message-part.h"
#define MESSAGE_PART_DEFAULT_CHARSET "us-ascii"
struct message_header_line;
struct message_part_param {
const char *name;
const char *value;
};
struct message_part_envelope {
const char *date, *subject;
struct message_address *from, *sender, *reply_to;
struct message_address *to, *cc, *bcc;
const char *in_reply_to, *message_id;
};
struct message_part_data {
const char *content_type, *content_subtype;
const struct message_part_param *content_type_params;
unsigned int content_type_params_count;
const char *content_transfer_encoding;
const char *content_id;
const char *content_description;
const char *content_disposition;
const struct message_part_param *content_disposition_params;
unsigned int content_disposition_params_count;
const char *content_md5;
const char *const *content_language;
const char *content_location;
struct message_part_envelope *envelope;
};
struct message_part_attachment_settings {
const char *const *content_type_filter;
bool exclude_inlined;
};
extern const char *message_part_envelope_headers[];
bool message_part_data_is_plain_7bit(const struct message_part *part)
ATTR_PURE;
bool message_part_data_get_filename(const struct message_part *part,
const char **filename_r);
bool message_part_has_content_types(struct message_part *part, const char *const *types);
bool message_part_has_parameter(struct message_part *part, const char *parameter,
bool has_value);
bool message_part_is_attachment(struct message_part *part,
const struct message_part_attachment_settings *set);
void message_part_envelope_parse_from_header(pool_t pool,
struct message_part_envelope **_data,
struct message_header_line *hdr);
void message_part_data_parse_from_header(pool_t pool,
struct message_part *part,
struct message_header_line *hdr);
#endif