#ifndef ISTREAM_METAWRAP_H
#define ISTREAM_METAWRAP_H
typedef void
metawrap_callback_t(const char *key, const char *value, void *context);
struct istream *
i_stream_create_metawrap(struct istream *input,
metawrap_callback_t *callback, void *context);
#endif