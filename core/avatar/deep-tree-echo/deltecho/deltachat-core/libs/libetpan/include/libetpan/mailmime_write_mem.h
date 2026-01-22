#ifndef MAILMIME_WRITE_MEM_H
#define  MAILMIME_WRITE_MEM_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailmime_types.h>
#include <libetpan/mmapstring.h>
LIBETPAN_EXPORT
int mailmime_fields_write_mem(MMAPString * f, int * col,
struct mailmime_fields * fields);
LIBETPAN_EXPORT
int mailmime_content_write_mem(MMAPString * f, int * col,
struct mailmime_content * content);
LIBETPAN_EXPORT
int mailmime_content_type_write_mem(MMAPString * f, int * col,
struct mailmime_content * content);
LIBETPAN_EXPORT
int mailmime_write_mem(MMAPString * f, int * col,
struct mailmime * build_info);
LIBETPAN_EXPORT
int mailmime_quoted_printable_write_mem(MMAPString * f, int * col, int istext,
const char * text, size_t size);
LIBETPAN_EXPORT
int mailmime_base64_write_mem(MMAPString * f, int * col,
const char * text, size_t size);
LIBETPAN_EXPORT
int mailmime_data_write_mem(MMAPString * f, int * col,
struct mailmime_data * data,
int istext);
#ifdef __cplusplus
}
#endif
#endif