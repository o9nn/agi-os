#include <libetpan/mailmessage_types.h>
#ifndef MAILMESSAGE_H
#define MAILMESSAGE_H
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
mailmessage * mailmessage_new(void);
LIBETPAN_EXPORT
void mailmessage_free(mailmessage * info);
LIBETPAN_EXPORT
int mailmessage_init(mailmessage * msg_info,
mailsession * session,
mailmessage_driver * driver,
uint32_t indx, size_t size);
LIBETPAN_EXPORT
int mailmessage_flush(mailmessage * info);
LIBETPAN_EXPORT
int mailmessage_check(mailmessage * info);
LIBETPAN_EXPORT
int mailmessage_fetch_result_free(mailmessage * msg_info,
char * msg);
LIBETPAN_EXPORT
int mailmessage_fetch(mailmessage * msg_info,
char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailmessage_fetch_header(mailmessage * msg_info,
char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailmessage_fetch_body(mailmessage * msg_info,
char ** result, size_t * result_len);
LIBETPAN_EXPORT
int mailmessage_fetch_size(mailmessage * msg_info,
size_t * result);
LIBETPAN_EXPORT
int mailmessage_get_bodystructure(mailmessage * msg_info,
struct mailmime ** result);
LIBETPAN_EXPORT
int mailmessage_fetch_section(mailmessage * msg_info,
struct mailmime * mime,
char ** result, size_t * result_len);
LIBETPAN_EXPORT
int mailmessage_fetch_section_header(mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailmessage_fetch_section_mime(mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailmessage_fetch_section_body(mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailmessage_fetch_envelope(mailmessage * msg_info,
struct mailimf_fields ** result);
LIBETPAN_EXPORT
int mailmessage_get_flags(mailmessage * msg_info,
struct mail_flags ** result);
LIBETPAN_EXPORT
void mailmessage_resolve_single_fields(mailmessage * msg_info);
#ifdef __cplusplus
}
#endif
#endif