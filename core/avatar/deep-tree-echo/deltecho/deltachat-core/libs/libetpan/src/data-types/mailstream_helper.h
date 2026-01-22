#ifndef MAILSTREAM_HELPER_H
#define MAILSTREAM_HELPER_H
#include <libetpan/mmapstring.h>
#include <libetpan/mailstream.h>
#ifdef __cplusplus
extern "C" {
#endif
char * mailstream_read_line(mailstream * stream, MMAPString * line);
char * mailstream_read_line_append(mailstream * stream, MMAPString * line);
char * mailstream_read_line_remove_eol(mailstream * stream, MMAPString * line);
char * mailstream_read_multiline(mailstream * s, size_t size,
MMAPString * stream_buffer,
MMAPString * multiline_buffer,
size_t progr_rate,
progress_function * progr_fun,
mailprogress_function * body_progr_fun, void * context);
int mailstream_is_end_multiline(const char * line);
int mailstream_send_data_crlf(mailstream * s, const char * message,
size_t size,
size_t progr_rate,
progress_function * progr_fun);
int mailstream_send_data_crlf_with_context(mailstream * s, const char * message,
size_t size,
mailprogress_function * progr_fun,
void * context);
int mailstream_send_data(mailstream * s, const char * message,
size_t size,
size_t progr_rate,
progress_function * progr_fun);
int mailstream_send_data_with_context(mailstream * s, const char * message,
size_t size,
mailprogress_function * progr_fun,
void * context);
size_t mailstream_get_data_crlf_size(const char * message, size_t size);
#ifdef __cplusplus
}
#endif
#endif