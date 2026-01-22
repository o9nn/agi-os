#ifndef MESSAGE_HEADER_DECODE_H
#define MESSAGE_HEADER_DECODE_H
#include "unichar.h"
typedef bool message_header_decode_callback_t(const unsigned char *data,
size_t size, const char *charset,
void *context);
void message_header_decode(const unsigned char *data, size_t size,
message_header_decode_callback_t *callback,
void *context);
void message_header_decode_utf8(const unsigned char *data, size_t size,
buffer_t *dest, normalizer_func_t *normalizer);
#endif