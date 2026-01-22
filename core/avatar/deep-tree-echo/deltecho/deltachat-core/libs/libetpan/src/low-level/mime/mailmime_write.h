#ifndef MAILMIME_WRITE_H
#define  MAILMIME_WRITE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailmime_types.h>
#include <stdio.h>
int mailmime_fields_write(FILE * f, int * col,
struct mailmime_fields * fields);
int mailmime_content_write(FILE * f, int * col,
struct mailmime_content * content);
int mailmime_content_type_write(FILE * f, int * col,
struct mailmime_content * content);
int mailmime_write(FILE * f, int * col,
struct mailmime * build_info);
int mailmime_quoted_printable_write(FILE * f, int * col, int istext,
const char * text, size_t size);
int mailmime_base64_write(FILE * f, int * col,
const char * text, size_t size);
int mailmime_data_write(FILE * f, int * col,
struct mailmime_data * data,
int istext);
#ifdef __cplusplus
}
#endif
#endif