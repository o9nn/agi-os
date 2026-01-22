#ifndef MAILPRIVACY_H
#define MAILPRIVACY_H
#include <libetpan/mailmessage.h>
#include <libetpan/mailprivacy_types.h>
#include <libetpan/mailprivacy_tools.h>
LIBETPAN_EXPORT
struct mailprivacy * mailprivacy_new(char * tmp_dir, int make_alternative);
LIBETPAN_EXPORT
void mailprivacy_free(struct mailprivacy * privacy);
LIBETPAN_EXPORT
int mailprivacy_msg_get_bodystructure(struct mailprivacy * privacy,
mailmessage * msg_info,
struct mailmime ** result);
LIBETPAN_EXPORT
void mailprivacy_msg_flush(struct mailprivacy * privacy,
mailmessage * msg_info);
LIBETPAN_EXPORT
int mailprivacy_msg_fetch_section(struct mailprivacy * privacy,
mailmessage * msg_info,
struct mailmime * mime,
char ** result, size_t * result_len);
LIBETPAN_EXPORT
int mailprivacy_msg_fetch_section_header(struct mailprivacy * privacy,
mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailprivacy_msg_fetch_section_mime(struct mailprivacy * privacy,
mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailprivacy_msg_fetch_section_body(struct mailprivacy * privacy,
mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
LIBETPAN_EXPORT
void mailprivacy_msg_fetch_result_free(struct mailprivacy * privacy,
mailmessage * msg_info,
char * msg);
LIBETPAN_EXPORT
int mailprivacy_msg_fetch(struct mailprivacy * privacy,
mailmessage * msg_info,
char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailprivacy_msg_fetch_header(struct mailprivacy * privacy,
mailmessage * msg_info,
char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailprivacy_register(struct mailprivacy * privacy,
struct mailprivacy_protocol * protocol);
LIBETPAN_EXPORT
void mailprivacy_unregister(struct mailprivacy * privacy,
struct mailprivacy_protocol * protocol);
LIBETPAN_EXPORT
char * mailprivacy_get_encryption_name(struct mailprivacy * privacy,
char * privacy_driver, char * privacy_encryption);
LIBETPAN_EXPORT
int mailprivacy_encrypt(struct mailprivacy * privacy,
char * privacy_driver, char * privacy_encryption,
struct mailmime * mime,
struct mailmime ** result);
LIBETPAN_EXPORT
int mailprivacy_encrypt_msg(struct mailprivacy * privacy,
char * privacy_driver, char * privacy_encryption,
mailmessage * msg,
struct mailmime * mime,
struct mailmime ** result);
LIBETPAN_EXPORT
void mailprivacy_debug(struct mailprivacy * privacy, FILE * f);
LIBETPAN_EXPORT
carray * mailprivacy_get_protocols(struct mailprivacy * privacy);
LIBETPAN_EXPORT
int mailprivacy_is_encrypted(struct mailprivacy * privacy,
mailmessage * msg,
struct mailmime * mime);
LIBETPAN_EXPORT
void mailprivacy_recursive_unregister_mime(struct mailprivacy * privacy,
struct mailmime * mime);
#endif