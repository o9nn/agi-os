#ifndef MAIL_PRIVACY_TOOLS_H
#define MAIL_PRIVACY_TOOLS_H
#include <libetpan/mailmessage.h>
#include <libetpan/mailprivacy_types.h>
LIBETPAN_EXPORT
void mailprivacy_mime_clear(struct mailmime * mime);
LIBETPAN_EXPORT
FILE * mailprivacy_get_tmp_file(struct mailprivacy * privacy,
char * filename, size_t size);
LIBETPAN_EXPORT
int mailprivacy_get_tmp_filename(struct mailprivacy * privacy,
char * filename, size_t size);
LIBETPAN_EXPORT
struct mailmime *
mailprivacy_new_file_part(struct mailprivacy * privacy,
char * filename,
char * default_content_type, int default_encoding);
LIBETPAN_EXPORT
int mailmime_substitute(struct mailmime * old_mime,
struct mailmime * new_mime);
LIBETPAN_EXPORT
int mailprivacy_fetch_mime_body_to_file(struct mailprivacy * privacy,
char * filename, size_t size,
mailmessage * msg, struct mailmime * mime);
LIBETPAN_EXPORT
int mailprivacy_get_part_from_file(struct mailprivacy * privacy,
int check_privacy, int reencode,
char * filename,
struct mailmime ** result_mime);
LIBETPAN_EXPORT
int mail_quote_filename(char * result, size_t size, char * path);
LIBETPAN_EXPORT
void mailprivacy_prepare_mime(struct mailmime * mime);
LIBETPAN_EXPORT
char * mailprivacy_dup_imf_file(struct mailprivacy * privacy,
char * source_filename);
LIBETPAN_EXPORT
struct mailmime_fields *
mailprivacy_mime_fields_dup(struct mailprivacy * privacy,
struct mailmime_fields * mime_fields);
LIBETPAN_EXPORT
struct mailmime_parameter *
mailmime_parameter_dup(struct mailmime_parameter * param);
LIBETPAN_EXPORT
struct mailmime_composite_type *
mailmime_composite_type_dup(struct mailmime_composite_type * composite_type);
LIBETPAN_EXPORT
struct mailmime_discrete_type *
mailmime_discrete_type_dup(struct mailmime_discrete_type * discrete_type);
LIBETPAN_EXPORT
struct mailmime_type * mailmime_type_dup(struct mailmime_type * type);
LIBETPAN_EXPORT
struct mailmime_content *
mailmime_content_dup(struct mailmime_content * content);
LIBETPAN_EXPORT
int mailprivacy_fetch_decoded_to_file(struct mailprivacy * privacy,
char * filename, size_t size,
mailmessage * msg, struct mailmime * mime);
LIBETPAN_EXPORT
int mailprivacy_get_mime(struct mailprivacy * privacy,
int check_privacy, int reencode,
char * content, size_t content_len,
struct mailmime ** result_mime);
#endif