#ifndef MAILMIME_H
#define MAILMIME_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailimf.h>
#include <libetpan/mailmime_types.h>
#include <libetpan/mailmime_types_helper.h>
#include <libetpan/mailmime_content.h>
#include <libetpan/mailmime_decode.h>
#include <libetpan/mailmime_disposition.h>
#include <libetpan/mailmime_write_file.h>
#include <libetpan/mailmime_write_mem.h>
#include <libetpan/mailmime_write_generic.h>
LIBETPAN_EXPORT
int mailmime_content_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_content ** result);
LIBETPAN_EXPORT
int mailmime_description_parse(const char * message, size_t length,
size_t * indx,
char ** result);
LIBETPAN_EXPORT
int mailmime_location_parse(const char * message, size_t length,
size_t * indx,
char ** result);
LIBETPAN_EXPORT
int mailmime_encoding_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_mechanism ** result);
LIBETPAN_EXPORT
int
mailmime_field_parse(struct mailimf_optional_field * field,
struct mailmime_field ** result);
LIBETPAN_EXPORT
int mailmime_id_parse(const char * message, size_t length,
size_t * indx, char ** result);
LIBETPAN_EXPORT
int
mailmime_fields_parse(struct mailimf_fields *
fields,
struct mailmime_fields **
result);
LIBETPAN_EXPORT
int mailmime_version_parse(const char * message, size_t length,
size_t * indx,
uint32_t * result);
LIBETPAN_EXPORT
int
mailmime_extension_token_parse(const char * message, size_t length,
size_t * indx, char ** result);
LIBETPAN_EXPORT
int mailmime_parameter_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_parameter ** result);
LIBETPAN_EXPORT
int mailmime_value_parse(const char * message, size_t length,
size_t * indx, char ** result);
LIBETPAN_EXPORT
int mailmime_language_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_language ** result);
#ifdef __cplusplus
}
#endif
#endif