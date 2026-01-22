#ifndef COMPRESSION_H
#define COMPRESSION_H
enum istream_decompress_flags {
ISTREAM_DECOMPRESS_FLAG_TRY = BIT(0),
};
struct compression_handler {
const char *name;
const char *ext;
bool (*is_compressed)(struct istream *input);
struct istream *(*create_istream)(struct istream *input);
struct ostream *(*create_ostream)(struct ostream *output, int level);
int (*get_min_level)(void);
int (*get_default_level)(void);
int (*get_max_level)(void);
};
extern const struct compression_handler compression_handlers[];
int compression_lookup_handler(const char *name,
const struct compression_handler **handler_r);
const struct compression_handler *
compression_detect_handler(struct istream *input);
int compression_lookup_handler_from_ext(const char *path,
const struct compression_handler **handler_r);
struct istream *
i_stream_create_decompress(struct istream *input,
enum istream_decompress_flags flags);
#endif