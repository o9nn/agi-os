#ifndef MAILMIME_CONTENT_H
#define MAILMIME_CONTENT_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailmime_types.h>
LIBETPAN_EXPORT
char * mailmime_content_charset_get(struct mailmime_content * content);
LIBETPAN_EXPORT
char * mailmime_content_param_get(struct mailmime_content * content,
char * name);
LIBETPAN_EXPORT
int mailmime_parse(const char * message, size_t length,
size_t * indx, struct mailmime ** result);
LIBETPAN_EXPORT
int mailmime_get_section(struct mailmime * mime,
struct mailmime_section * section,
struct mailmime ** result);
LIBETPAN_EXPORT
char * mailmime_extract_boundary(struct mailmime_content * content_type);
LIBETPAN_EXPORT
int mailmime_base64_body_parse(const char * message, size_t length,
size_t * indx, char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailmime_quoted_printable_body_parse(const char * message, size_t length,
size_t * indx, char ** result,
size_t * result_len, int in_header);
LIBETPAN_EXPORT
int mailmime_binary_body_parse(const char * message, size_t length,
size_t * indx, char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailmime_part_parse(const char * message, size_t length,
size_t * indx,
int encoding, char ** result, size_t * result_len);
LIBETPAN_EXPORT
int mailmime_part_parse_partial(const char * message, size_t length,
size_t * indx,
int encoding, char ** result, size_t * result_len);
LIBETPAN_EXPORT
int mailmime_get_section_id(struct mailmime * mime,
struct mailmime_section ** result);
#ifdef __cplusplus
}
#endif
#endif