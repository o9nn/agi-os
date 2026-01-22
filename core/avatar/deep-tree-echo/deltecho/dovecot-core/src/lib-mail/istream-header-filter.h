#ifndef ISTREAM_HEADER_FILTER_H
#define ISTREAM_HEADER_FILTER_H
struct header_filter_istream;
enum header_filter_flags {
HEADER_FILTER_INCLUDE = 0x01,
HEADER_FILTER_EXCLUDE = 0x02,
HEADER_FILTER_NO_CR = 0x04,
HEADER_FILTER_HIDE_BODY = 0x08,
HEADER_FILTER_ADD_MISSING_EOH = 0x10,
HEADER_FILTER_END_BODY_WITH_LF = 0x20,
HEADER_FILTER_CRLF_PRESERVE = 0x40
};
struct message_header_line;
typedef void header_filter_callback(struct header_filter_istream *input,
struct message_header_line *hdr,
bool *matched, void *context);
extern header_filter_callback *null_header_filter_callback;
struct istream *
i_stream_create_header_filter(struct istream *input,
enum header_filter_flags flags,
const char *const *headers,
unsigned int headers_count,
header_filter_callback *callback, void *context)
ATTR_NULL(6);
#define i_stream_create_header_filter(input, flags, headers, headers_count, \
callback, context) \
i_stream_create_header_filter(input, flags, headers, headers_count - \
CALLBACK_TYPECHECK(callback, void (*)( \
struct header_filter_istream *, \
struct message_header_line *, bool *, typeof(context))), \
(header_filter_callback *)callback, context)
void i_stream_header_filter_add(struct header_filter_istream *input,
const void *data, size_t size);
#endif