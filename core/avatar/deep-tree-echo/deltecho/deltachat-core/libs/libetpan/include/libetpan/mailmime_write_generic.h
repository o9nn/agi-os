#ifndef MAILMIME_WRITE_GENERIC_H
#define MAILMIME_WRITE_GENERIC_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailmime_types.h>
#include <stdio.h>
int mailmime_fields_write_driver(int (* do_write)(void *, const char *, size_t), void * data, int * col,
struct mailmime_fields * fields);
int mailmime_content_write_driver(int (* do_write)(void *, const char *, size_t), void * data, int * col,
struct mailmime_content * content);
int mailmime_content_type_write_driver(int (* do_write)(void *, const char *, size_t), void * data, int * col,
struct mailmime_content * content);
int mailmime_write_driver(int (* do_write)(void *, const char *, size_t), void * data, int * col,
struct mailmime * build_info);
int mailmime_quoted_printable_write_driver(int (* do_write)(void *, const char *, size_t), void * data, int * col, int istext,
const char * text, size_t size);
int mailmime_base64_write_driver(int (* do_write)(void *, const char *, size_t), void * data, int * col,
const char * text, size_t size);
int mailmime_data_write_driver(int (* do_write)(void *, const char *, size_t), void * data, int * col,
struct mailmime_data * mime_data,
int istext);
#ifdef __cplusplus
}
#endif
#endif