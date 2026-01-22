#ifndef MAILMESSAGE_TOOLS_H
#define MAILMESSAGE_TOOLS_H
#include "mailmessage_types.h"
#ifdef __cplusplus
extern "C" {
#endif
int
mailmessage_generic_initialize(mailmessage *
msg_info);
void mailmessage_generic_uninitialize(mailmessage *
msg_info);
void mailmessage_generic_flush(mailmessage * msg_info);
void mailmessage_generic_fetch_result_free(mailmessage * msg_info,
char * msg);
int mailmessage_generic_fetch(mailmessage * msg_info,
char ** result,
size_t * result_len);
int mailmessage_generic_fetch_header(mailmessage * msg_info,
char ** result,
size_t * result_len);
int mailmessage_generic_fetch_body(mailmessage * msg_info,
char ** result, size_t * result_len);
int mailmessage_generic_get_bodystructure(mailmessage *
msg_info,
struct mailmime ** result);
int
mailmessage_generic_fetch_section(mailmessage * msg_info,
struct mailmime * mime,
char ** result, size_t * result_len);
int
mailmessage_generic_fetch_section_header(mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
int
mailmessage_generic_fetch_section_mime(mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
int
mailmessage_generic_fetch_section_body(mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
int mailmessage_generic_fetch_envelope(mailmessage * msg_info,
struct mailimf_fields ** result);
#ifdef __cplusplus
}
#endif
#endif