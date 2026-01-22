#ifndef ISTREAM_ATTACHMENT_CONNECTOR_H
#define ISTREAM_ATTACHMENT_CONNECTOR_H
struct istream_attachment_connector *
istream_attachment_connector_begin(struct istream *base_input, uoff_t msg_size);
int istream_attachment_connector_add(struct istream_attachment_connector *conn,
struct istream *decoded_input,
uoff_t start_offset, uoff_t encoded_size,
unsigned int base64_blocks_per_line,
bool base64_have_crlf,
const char **error_r);
struct istream *
istream_attachment_connector_finish(struct istream_attachment_connector **conn);
void istream_attachment_connector_abort(struct istream_attachment_connector **conn);
#endif