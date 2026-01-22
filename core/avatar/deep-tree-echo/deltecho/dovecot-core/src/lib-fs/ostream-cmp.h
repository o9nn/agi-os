#ifndef OSTREAM_CMP_H
#define OSTREAM_CMP_H
struct ostream *
o_stream_create_cmp(struct ostream *output, struct istream *input);
bool o_stream_cmp_equals(struct ostream *output);
bool stream_cmp_block(struct istream *input,
const unsigned char *data, size_t size);
#endif