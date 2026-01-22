#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "mailmime_write_mem.h"
#include <stdlib.h>
#include <string.h>
#include <time.h>
#ifdef HAVE_UNISTD_H
#	include <unistd.h>
#endif
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifdef HAVE_SYS_MMAN_H
#	include <sys/mman.h>
#endif
#include "mailmime_content.h"
#include "mailmime_types_helper.h"
#include "mailmime_write_generic.h"
static int do_write(void * data, const char * str, size_t length)
{
MMAPString * f;
f = data;
if (mmap_string_append_len(f, str, length) == NULL)
return 0;
else
return (int) length;
}
int mailmime_fields_write_mem(MMAPString * f, int * col,
struct mailmime_fields * fields)
{
return mailmime_fields_write_driver(do_write, f, col, fields);
}
int mailmime_content_write_mem(MMAPString * f, int * col,
struct mailmime_content * content)
{
return mailmime_content_write_driver(do_write, f, col, content);
}
int mailmime_content_type_write_mem(MMAPString * f, int * col,
struct mailmime_content * content)
{
return mailmime_content_type_write_driver(do_write, f, col, content);
}
int mailmime_write_mem(MMAPString * f, int * col,
struct mailmime * build_info)
{
return mailmime_write_driver(do_write, f, col, build_info);
}
int mailmime_quoted_printable_write_mem(MMAPString * f, int * col, int istext,
const char * text, size_t size)
{
return mailmime_quoted_printable_write_driver(do_write, f, col,
istext, text, size);
}
int mailmime_base64_write_mem(MMAPString * f, int * col,
const char * text, size_t size)
{
return mailmime_base64_write_driver(do_write, f, col, text, size);
}
int mailmime_data_write_mem(MMAPString * f, int * col,
struct mailmime_data * data,
int istext)
{
return mailmime_data_write_driver(do_write, f, col, data, istext);
}