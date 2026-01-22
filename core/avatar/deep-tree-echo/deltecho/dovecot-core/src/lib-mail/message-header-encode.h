#ifndef MESSAGE_HEADER_ENCODE_H
#define MESSAGE_HEADER_ENCODE_H
void message_header_encode(const char *input, string_t *output);
void message_header_encode_data(const unsigned char *input, size_t len,
string_t *output);
void message_header_encode_q(const unsigned char *input, size_t len,
string_t *output, size_t first_line_len);
void message_header_encode_b(const unsigned char *input, size_t len,
string_t *output, size_t first_line_len);
#endif