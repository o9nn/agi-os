#ifndef MAILSTREAM_TYPES_H
#define MAILSTREAM_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#define LIBETPAN_MAILSTREAM_DEBUG
#ifndef LIBETPAN_CONFIG_H
# include <libetpan/libetpan-config.h>
#endif
#include <libetpan/carray.h>
struct _mailstream;
typedef struct _mailstream mailstream;
struct _mailstream_low;
typedef struct _mailstream_low mailstream_low;
enum {
MAILSTREAM_LOG_TYPE_INFO_RECEIVED,
MAILSTREAM_LOG_TYPE_INFO_SENT,
MAILSTREAM_LOG_TYPE_ERROR_PARSE,
MAILSTREAM_LOG_TYPE_ERROR_RECEIVED,
MAILSTREAM_LOG_TYPE_ERROR_SENT,
MAILSTREAM_LOG_TYPE_DATA_RECEIVED,
MAILSTREAM_LOG_TYPE_DATA_SENT,
MAILSTREAM_LOG_TYPE_DATA_SENT_PRIVATE,
};
struct _mailstream {
size_t buffer_max_size;
char * write_buffer;
size_t write_buffer_len;
char * read_buffer;
size_t read_buffer_len;
mailstream_low * low;
struct mailstream_cancel * idle;
int idling;
void (* logger)(mailstream * s, int log_type,
const char * str, size_t size, void * logger_context);
void * logger_context;
};
struct mailstream_low_driver {
ssize_t (* mailstream_read)(mailstream_low *, void *, size_t);
ssize_t (* mailstream_write)(mailstream_low *, const void *, size_t);
int (* mailstream_close)(mailstream_low *);
int (* mailstream_get_fd)(mailstream_low *);
void (* mailstream_free)(mailstream_low *);
void (* mailstream_cancel)(mailstream_low *);
struct mailstream_cancel * (* mailstream_get_cancel)(mailstream_low *);
carray * (* mailstream_get_certificate_chain)(mailstream_low *);
int (* mailstream_setup_idle)(mailstream_low *);
int (* mailstream_unsetup_idle)(mailstream_low *);
int (* mailstream_interrupt_idle)(mailstream_low *);
};
typedef struct mailstream_low_driver mailstream_low_driver;
struct _mailstream_low {
void * data;
mailstream_low_driver * driver;
int privacy;
char * identifier;
unsigned long timeout;
void (* logger)(mailstream_low * s, int log_type,
const char * str, size_t size, void * logger_context);
void * logger_context;
};
typedef void progress_function(size_t current, size_t maximum);
typedef void mailprogress_function(size_t current, size_t maximum, void * context);
enum {
MAILSTREAM_IDLE_ERROR,
MAILSTREAM_IDLE_INTERRUPTED,
MAILSTREAM_IDLE_HASDATA,
MAILSTREAM_IDLE_TIMEOUT,
MAILSTREAM_IDLE_CANCELLED
};
#ifdef __cplusplus
}
#endif
#endif