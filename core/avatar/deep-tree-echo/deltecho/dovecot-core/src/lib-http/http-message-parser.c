#include "lib.h"
#include "array.h"
#include "istream.h"
#include "iostream.h"
#include "istream-sized.h"
#include "http-parser.h"
#include "http-header.h"
#include "http-header-parser.h"
#include "http-date.h"
#include "http-transfer.h"
#include "http-message-parser.h"
#include <ctype.h>
void http_message_parser_init(struct http_message_parser *parser,
struct istream *input,
const struct http_header_limits *hdr_limits,
uoff_t max_payload_size,
enum http_message_parse_flags flags)
{
i_zero(parser);
parser->input = input;
i_stream_ref(parser->input);
if (hdr_limits != NULL)
parser->header_limits = *hdr_limits;
parser->max_payload_size = max_payload_size;
parser->flags = flags;
}
void http_message_parser_deinit(struct http_message_parser *parser)
{
if (parser->header_parser != NULL)
http_header_parser_deinit(&parser->header_parser);
pool_unref(&parser->msg.pool);
i_stream_unref(&parser->payload);
i_stream_unref(&parser->input);
}
void http_message_parser_restart(struct http_message_parser *parser,
pool_t pool)
{
i_assert(parser->payload == NULL);
if (parser->header_parser == NULL) {
enum http_header_parse_flags hdr_flags = 0;
if ((parser->flags & HTTP_MESSAGE_PARSE_FLAG_STRICT) != 0)
hdr_flags |= HTTP_HEADER_PARSE_FLAG_STRICT;
parser->header_parser = http_header_parser_init(
parser->input, &parser->header_limits, hdr_flags);
} else {
http_header_parser_reset(parser->header_parser);
}
pool_unref(&parser->msg.pool);
i_zero(&parser->msg);
if (pool != NULL) {
parser->msg.pool = pool;
pool_ref(pool);
}
parser->msg.date = (time_t)-1;
}
pool_t http_message_parser_get_pool(struct http_message_parser *parser)
{
if (parser->msg.pool == NULL)
parser->msg.pool = pool_alloconly_create("http_message", 4096);
return parser->msg.pool;
}
int http_message_parse_version(struct http_message_parser *parser)
{
const unsigned char *p = parser->cur;
const size_t size = parser->end - parser->cur;
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_NONE;
parser->error = NULL;
if (size < 8)
return 0;
if (memcmp(p, "HTTP/", 5) != 0 ||
!i_isdigit(p[5]) || p[6] != '.' || !i_isdigit(p[7])) {
parser->error = "Bad HTTP version";
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
return -1;
}
parser->msg.version_major = p[5] - '0';
parser->msg.version_minor = p[7] - '0';
parser->cur += 8;
return 1;
}
static void
http_message_parse_finish_payload_error(struct http_message_parser *parser)
{
if (parser->payload->stream_errno == EMSGSIZE) {
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_PAYLOAD_TOO_LARGE;
parser->error = "Payload is too large";
} else if (parser->payload->stream_errno == EIO) {
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
parser->error = t_strdup_printf(
"Invalid payload: %s",
i_stream_get_error(parser->payload));
} else {
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BROKEN_STREAM;
parser->error = t_strdup_printf(
"Stream error while skipping payload: %s",
i_stream_get_error(parser->payload));
}
}
int http_message_parse_finish_payload(struct http_message_parser *parser)
{
const unsigned char *data;
size_t size;
int ret;
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_NONE;
parser->error = NULL;
if (parser->payload == NULL)
return 1;
while ((ret = i_stream_read_more(parser->payload, &data, &size)) > 0)
i_stream_skip(parser->payload, size);
if (ret == 0 || parser->payload->stream_errno != 0) {
if (ret < 0)
http_message_parse_finish_payload_error(parser);
return ret;
}
i_stream_destroy(&parser->payload);
return 1;
}
static int
http_message_parse_hdr_connection(struct http_message_parser *parser,
const unsigned char *data, size_t size)
{
pool_t pool = http_message_parser_get_pool(parser);
struct http_parser hparser;
const char **opt_idx;
const char *option;
unsigned int num_tokens = 0;
http_parser_init(&hparser, data, size);
for (;;) {
if (http_parse_token_list_next(&hparser, &option) <= 0)
break;
num_tokens++;
if (strcasecmp(option, "close") == 0)
parser->msg.connection_close = TRUE;
if (!array_is_created(&parser->msg.connection_options))
p_array_init(&parser->msg.connection_options, pool, 4);
opt_idx = array_append_space(&parser->msg.connection_options);
*opt_idx = p_strdup(pool, option);
}
if (hparser.cur < hparser.end || num_tokens == 0) {
parser->error = "Invalid Connection header";
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
return -1;
}
return 0;
}
static int
http_message_parse_hdr_content_length(struct http_message_parser *parser,
const struct http_header_field *hdr)
{
if (parser->msg.have_content_length) {
parser->error = "Duplicate Content-Length header";
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
return -1;
}
if (str_to_uoff(hdr->value, &parser->msg.content_length) < 0) {
parser->error = "Invalid Content-Length header";
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
return -1;
}
parser->msg.have_content_length = TRUE;
return 0;
}
static int
http_message_parse_hdr_date(struct http_message_parser *parser,
const unsigned char *data, size_t size)
{
if (parser->msg.date != (time_t)-1) {
if ((parser->flags & HTTP_MESSAGE_PARSE_FLAG_STRICT) != 0) {
parser->error = "Duplicate Date header";
parser->error_code =
HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
return -1;
}
}
if (!http_date_parse(data, size, &parser->msg.date) &&
(parser->flags & HTTP_MESSAGE_PARSE_FLAG_STRICT) != 0) {
parser->error = "Invalid Date header";
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
return -1;
}
return 0;
}
static int
http_message_parse_hdr_location(struct http_message_parser *parser,
const struct http_header_field *hdr)
{
parser->msg.location = hdr->value;
return 0;
}
static int
http_message_parse_hdr_transfer_encoding(struct http_message_parser *parser,
const unsigned char *data, size_t size)
{
pool_t pool = http_message_parser_get_pool(parser);
struct http_parser hparser;
const char *trenc = NULL;
if (!array_is_created(&parser->msg.transfer_encoding))
p_array_init(&parser->msg.transfer_encoding, pool, 4);
http_parser_init(&hparser, data, size);
for (;;) {
if (http_parse_token(&hparser, &trenc) > 0) {
struct http_transfer_coding *coding;
bool parse_error;
coding = array_append_space(
&parser->msg.transfer_encoding);
coding->name = p_strdup(pool, trenc);
parse_error = FALSE;
for (;;) {
struct http_transfer_param *param;
const char *attribute, *value;
http_parse_ows(&hparser);
if (hparser.cur >= hparser.end ||
*hparser.cur != ';')
break;
hparser.cur++;
http_parse_ows(&hparser);
if (http_parse_token(&hparser,
&attribute) <= 0) {
parse_error = TRUE;
break;
}
http_parse_ows(&hparser);
if (hparser.cur >= hparser.end ||
*hparser.cur != '=') {
parse_error = TRUE;
break;
}
hparser.cur++;
http_parse_ows(&hparser);
if (http_parse_token_or_qstring(&hparser,
&value) <= 0) {
parse_error = TRUE;
break;
}
if (!array_is_created(&coding->parameters)) {
p_array_init(&coding->parameters,
pool, 2);
}
param = array_append_space(&coding->parameters);
param->attribute = p_strdup(pool, attribute);
param->value = p_strdup(pool, value);
}
if (parse_error)
break;
} else {
}
http_parse_ows(&hparser);
if (hparser.cur >= hparser.end || *hparser.cur != ',')
break;
hparser.cur++;
http_parse_ows(&hparser);
}
if (hparser.cur < hparser.end ||
array_count(&parser->msg.transfer_encoding) == 0) {
parser->error = "Invalid Transfer-Encoding header";
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
return -1;
}
return 0;
}
static int
http_message_parse_header(struct http_message_parser *parser,
const char *name, const unsigned char *data,
size_t size)
{
const struct http_header_field *hdr;
pool_t pool;
pool = http_message_parser_get_pool(parser);
if (parser->msg.header == NULL)
parser->msg.header = http_header_create(pool, 32);
hdr = http_header_field_add(parser->msg.header, name, data, size);
switch (name[0]) {
case 'C': case 'c':
if (strcasecmp(name, "Connection") == 0) {
return http_message_parse_hdr_connection(
parser, data, size);
}
if (strcasecmp(name, "Content-Length") == 0)
return http_message_parse_hdr_content_length(
parser, hdr);
break;
case 'D': case 'd':
if (strcasecmp(name, "Date") == 0)
return http_message_parse_hdr_date(parser, data, size);
break;
case 'L': case 'l':
if (strcasecmp(name, "Location") == 0)
return http_message_parse_hdr_location(parser, hdr);
break;
case 'T': case 't':
if (strcasecmp(name, "Transfer-Encoding") == 0) {
return http_message_parse_hdr_transfer_encoding(
parser, data, size);
}
break;
default:
break;
}
return 0;
}
static int http_message_parse_eoh(struct http_message_parser *parser)
{
struct http_message *msg = &parser->msg;
pool_t pool;
pool = http_message_parser_get_pool(parser);
if (msg->header == NULL)
msg->header = http_header_create(pool, 1);
if (msg->version_major == 1 && msg->version_minor == 0 &&
!msg->connection_close) {
const char *option;
msg->connection_close = TRUE;
if (array_is_created(&msg->connection_options)) {
array_foreach_elem(&msg->connection_options, option) {
if (strcasecmp(option, "Keep-Alive") == 0) {
msg->connection_close = FALSE;
break;
}
}
}
}
return 1;
}
int http_message_parse_headers(struct http_message_parser *parser)
{
const unsigned char *field_data;
const char *field_name, *error;
size_t field_size;
int ret;
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_NONE;
parser->error = NULL;
while ((ret = http_header_parse_next_field(
parser->header_parser, &field_name, &field_data, &field_size,
&error)) > 0) {
if (field_name == NULL)
return http_message_parse_eoh(parser);
if (http_message_parse_header(parser,
field_name, field_data, field_size) < 0)
return -1;
}
if (ret < 0) {
if (parser->input->eof || parser->input->stream_errno != 0) {
parser->error_code =
HTTP_MESSAGE_PARSE_ERROR_BROKEN_STREAM;
parser->error = "Broken stream";
} else {
parser->error_code =
HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
parser->error = t_strdup_printf(
"Failed to parse header: %s", error);
}
}
return ret;
}
static const char *
http_istream_error_callback(const struct istream_sized_error_data *data,
struct istream *input)
{
i_assert(data->eof);
i_assert(data->v_offset + data->new_bytes < data->wanted_size);
return t_strdup_printf(
"Disconnected while reading message payload at offset %"PRIuUOFF_T
" (wanted %"PRIuUOFF_T"): %s", data->v_offset + data->new_bytes,
data->wanted_size, io_stream_get_disconnect_reason(input, NULL));
}
static int
http_message_parse_body_coding(struct http_message_parser *parser,
const struct http_transfer_coding *coding,
bool *seen_chunked)
{
if (strcasecmp(coding->name, "chunked") == 0) {
*seen_chunked = TRUE;
if ((parser->error_code == HTTP_MESSAGE_PARSE_ERROR_NONE)
&& array_is_created(&coding->parameters)
&& array_count(&coding->parameters) > 0) {
const struct http_transfer_param *param =
array_front(&coding->parameters);
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BAD_MESSAGE;
parser->error = t_strdup_printf(
"Unexpected parameter `%s' specified"
"for the `%s' transfer coding",
param->attribute, coding->name);
}
} else if (*seen_chunked) {
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
parser->error = "Chunked Transfer-Encoding must be last";
return -1;
} else if (parser->error_code == HTTP_MESSAGE_PARSE_ERROR_NONE) {
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_NOT_IMPLEMENTED;
parser->error = t_strdup_printf(
"Unknown transfer coding `%s'", coding->name);
}
return 0;
}
static int
http_message_parse_body_encoded(struct http_message_parser *parser,
bool request)
{
const struct http_transfer_coding *coding;
bool seen_chunked = FALSE;
array_foreach(&parser->msg.transfer_encoding, coding) {
if (http_message_parse_body_coding(parser, coding,
&seen_chunked) < 0)
return -1;
}
if (seen_chunked) {
parser->payload = http_transfer_chunked_istream_create(
parser->input, parser->max_payload_size);
} else if (!request) {
parser->payload =
i_stream_create_limit(parser->input, UOFF_T_MAX);
} else {
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_BROKEN_MESSAGE;
parser->error =
"Final Transfer-Encoding in request is not chunked";
return -1;
}
if (parser->msg.have_content_length)
http_header_field_delete(parser->msg.header, "Content-Length");
return 0;
}
static int http_message_parse_body_sized(struct http_message_parser *parser)
{
struct istream *input;
if (parser->max_payload_size > 0
&& parser->msg.content_length > parser->max_payload_size) {
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_PAYLOAD_TOO_LARGE;
parser->error = "Payload is too large";
return -1;
}
input = i_stream_create_limit(parser->input,
parser->msg.content_length);
parser->payload = i_stream_create_sized_with_callback(input,
parser->msg.content_length,
http_istream_error_callback, input);
i_stream_unref(&input);
return 0;
}
static int http_message_parse_body_closed(struct http_message_parser *parser)
{
parser->payload = i_stream_create_limit(parser->input, UOFF_T_MAX);
return 0;
}
int http_message_parse_body(struct http_message_parser *parser, bool request)
{
i_assert(parser->payload == NULL);
parser->error_code = HTTP_MESSAGE_PARSE_ERROR_NONE;
parser->error = NULL;
if (array_is_created(&parser->msg.transfer_encoding)) {
if (http_message_parse_body_encoded(parser, request) < 0)
return -1;
} else if (parser->msg.content_length > 0) {
if (http_message_parse_body_sized(parser) < 0)
return -1;
} else if (!parser->msg.have_content_length && !request) {
if (http_message_parse_body_closed(parser) < 0)
return -1;
}
if (parser->error_code != HTTP_MESSAGE_PARSE_ERROR_NONE)
return -1;
return 0;
}