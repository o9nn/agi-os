#ifndef MAILSTREAM_COMPRESS_H
#define MAILSTREAM_COMPRESS_H
#define USE_DEFLATE 1
#include <libetpan/mailstream.h>
#ifdef __cplusplus
extern "C" {
#endif
extern mailstream_low_driver * mailstream_compress_driver;
struct mailstream_compress_context;
LIBETPAN_EXPORT
mailstream_low * mailstream_low_compress_open(mailstream_low * ms);
LIBETPAN_EXPORT
int mailstream_low_compress_wait_idle(mailstream_low * low,
struct mailstream_cancel * idle,
int max_idle_delay);
#ifdef __cplusplus
}
#endif
#endif