#ifndef MAILMIME_WRITE_FILE_H
#define MAILMIME_WRITE_FILE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailmime_types.h>
#include <stdio.h>
#define MAILMIME_WRITE_COMPATIBILITY
LIBETPAN_EXPORT
int mailmime_fields_write_file(FILE * f, int * col,
struct mailmime_fields * fields);
LIBETPAN_EXPORT
int mailmime_content_write_file(FILE * f, int * col,
struct mailmime_content * content);
LIBETPAN_EXPORT
int mailmime_content_type_write_file(FILE * f, int * col,
struct mailmime_content * content);
LIBETPAN_EXPORT
int mailmime_write_file(FILE * f, int * col,
struct mailmime * build_info);
LIBETPAN_EXPORT
int mailmime_quoted_printable_write_file(FILE * f, int * col, int istext,
const char * text, size_t size);
LIBETPAN_EXPORT
int mailmime_base64_write_file(FILE * f, int * col,
const char * text, size_t size);
LIBETPAN_EXPORT
int mailmime_data_write_file(FILE * f, int * col,
struct mailmime_data * data,
int istext);
#ifdef MAILMIME_WRITE_COMPATIBILITY
LIBETPAN_EXPORT
int mailmime_fields_write(FILE * f, int * col,
struct mailmime_fields * fields);
LIBETPAN_EXPORT
int mailmime_content_write(FILE * f, int * col,
struct mailmime_content * content);
LIBETPAN_EXPORT
int mailmime_content_type_write(FILE * f, int * col,
struct mailmime_content * content);
LIBETPAN_EXPORT
int mailmime_write(FILE * f, int * col,
struct mailmime * build_info);
LIBETPAN_EXPORT
int mailmime_quoted_printable_write(FILE * f, int * col, int istext,
const char * text, size_t size);
LIBETPAN_EXPORT
int mailmime_base64_write(FILE * f, int * col,
const char * text, size_t size);
LIBETPAN_EXPORT
int mailmime_data_write(FILE * f, int * col,
struct mailmime_data * data,
int istext);
#endif
#ifdef __cplusplus
}
#endif
#endif