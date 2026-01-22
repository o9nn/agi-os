#ifndef MAILSTREAM_LOW_H
#define MAILSTREAM_LOW_H
#include <sys/types.h>
#include <libetpan/mailstream_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
mailstream_low * mailstream_low_new(void * data,
mailstream_low_driver * driver);
ssize_t mailstream_low_write(mailstream_low * s,
const void * buf, size_t count);
ssize_t mailstream_low_read(mailstream_low * s, void * buf, size_t count);
LIBETPAN_EXPORT
int mailstream_low_close(mailstream_low * s);
LIBETPAN_EXPORT
int mailstream_low_get_fd(mailstream_low * s);
LIBETPAN_EXPORT
struct mailstream_cancel * mailstream_low_get_cancel(mailstream_low * s);
LIBETPAN_EXPORT
void mailstream_low_free(mailstream_low * s);
LIBETPAN_EXPORT
void mailstream_low_cancel(mailstream_low * s);
LIBETPAN_EXPORT
void mailstream_low_log_error(mailstream_low * s,
const void * buf, size_t count);
LIBETPAN_EXPORT
void mailstream_low_set_privacy(mailstream_low * s, int can_be_public);
LIBETPAN_EXPORT
int mailstream_low_set_identifier(mailstream_low * s,
char * identifier);
LIBETPAN_EXPORT
const char * mailstream_low_get_identifier(mailstream_low * s);
LIBETPAN_EXPORT
void mailstream_low_set_timeout(mailstream_low * s,
time_t timeout);
LIBETPAN_EXPORT
time_t mailstream_low_get_timeout(mailstream_low * s);
LIBETPAN_EXPORT
void mailstream_low_set_logger(mailstream_low * s, void (* logger)(mailstream_low * s, int log_type,
const char * str, size_t size, void * context), void * logger_context);
LIBETPAN_EXPORT
carray * mailstream_low_get_certificate_chain(mailstream_low * s);
LIBETPAN_EXPORT
int mailstream_low_wait_idle(mailstream_low * low, struct mailstream_cancel * cancel,
int max_idle_delay);
LIBETPAN_EXPORT
int mailstream_low_setup_idle(mailstream_low * low);
LIBETPAN_EXPORT
int mailstream_low_unsetup_idle(mailstream_low * low);
LIBETPAN_EXPORT
int mailstream_low_interrupt_idle(mailstream_low * low);
#ifdef __cplusplus
}
#endif
#endif