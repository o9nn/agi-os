#ifndef MAILSTREAM_H
#define MAILSTREAM_H
#ifndef _MSC_VER
#	include <sys/time.h>
#endif
#include <libetpan/mailstream_low.h>
#include <libetpan/mailstream_helper.h>
#include <libetpan/mailstream_socket.h>
#include <libetpan/mailstream_ssl.h>
#include <libetpan/mailstream_cfstream.h>
#include <libetpan/mailstream_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
mailstream * mailstream_new(mailstream_low * low, size_t buffer_size);
LIBETPAN_EXPORT
ssize_t mailstream_write(mailstream * s, const void * buf, size_t count);
LIBETPAN_EXPORT
ssize_t mailstream_read(mailstream * s, void * buf, size_t count);
LIBETPAN_EXPORT
int mailstream_close(mailstream * s);
LIBETPAN_EXPORT
int mailstream_flush(mailstream * s);
LIBETPAN_EXPORT
ssize_t mailstream_feed_read_buffer(mailstream * s);
LIBETPAN_EXPORT
void mailstream_log_error(mailstream * s, char * buf, size_t count);
LIBETPAN_EXPORT
mailstream_low * mailstream_get_low(mailstream * s);
LIBETPAN_EXPORT
void mailstream_set_low(mailstream * s, mailstream_low * low);
LIBETPAN_EXPORT
void mailstream_cancel(mailstream * s);
LIBETPAN_EXPORT
void mailstream_set_privacy(mailstream * s, int can_be_public);
#ifdef LIBETPAN_MAILSTREAM_DEBUG
LIBETPAN_EXPORT
extern int mailstream_debug;
LIBETPAN_EXPORT
extern void (* mailstream_logger)(int direction,
const char * str, size_t size);
LIBETPAN_EXPORT
extern void (* mailstream_logger_id)(mailstream_low * s, int is_stream_data, int direction,
const char * str, size_t size);
#endif
LIBETPAN_EXPORT
void mailstream_set_logger(mailstream * s, void (* logger)(mailstream * s, int log_type,
const char * str, size_t size, void * context), void * logger_context);
LIBETPAN_EXPORT
int mailstream_wait_idle(mailstream * s, int max_idle_delay);
LIBETPAN_EXPORT
int mailstream_setup_idle(mailstream * s);
LIBETPAN_EXPORT
void mailstream_unsetup_idle(mailstream * s);
LIBETPAN_EXPORT
void mailstream_interrupt_idle(mailstream * s);
LIBETPAN_EXPORT
carray * mailstream_get_certificate_chain(mailstream * s);
LIBETPAN_EXPORT
void mailstream_certificate_chain_free(carray * certificate_chain);
#define LIBETPAN_MAILSTREAM_NETWORK_DELAY
LIBETPAN_EXPORT
extern struct timeval mailstream_network_delay;
#ifdef __cplusplus
}
#endif
#endif