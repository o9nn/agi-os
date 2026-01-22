#include "mailimf_write_mem.h"
#include "mailimf_write_generic.h"
static int do_write(void * data, const char * str, size_t length)
{
MMAPString * f;
f = data;
if (mmap_string_append_len(f, str, length) == NULL)
return 0;
else
return (int) length;
}
LIBETPAN_EXPORT
int mailimf_string_write_mem(MMAPString * f, int * col,
const char * str, size_t length)
{
return mailimf_string_write_driver(do_write, f, col, str, length);
}
LIBETPAN_EXPORT
int mailimf_fields_write_mem(MMAPString * f, int * col,
struct mailimf_fields * fields)
{
return mailimf_fields_write_driver(do_write, f, col, fields);
}
LIBETPAN_EXPORT
int mailimf_envelope_fields_write_mem(MMAPString * f, int * col,
struct mailimf_fields * fields)
{
return mailimf_envelope_fields_write_driver(do_write, f, col, fields);
}
LIBETPAN_EXPORT
int mailimf_field_write_mem(MMAPString * f, int * col,
struct mailimf_field * field)
{
return mailimf_field_write_driver(do_write, f, col, field);
}
LIBETPAN_EXPORT
int mailimf_quoted_string_write_mem(MMAPString * f, int * col,
const char * string, size_t len)
{
return mailimf_quoted_string_write_driver(do_write, f, col, string, len);
}
LIBETPAN_EXPORT
int mailimf_address_list_write_mem(MMAPString * f, int * col,
struct mailimf_address_list * addr_list)
{
return mailimf_address_list_write_driver(do_write, f, col, addr_list);
}
LIBETPAN_EXPORT
int mailimf_mailbox_list_write_mem(MMAPString * f, int * col,
struct mailimf_mailbox_list * mb_list)
{
return mailimf_mailbox_list_write_driver(do_write, f, col, mb_list);
}
LIBETPAN_EXPORT
int mailimf_header_string_write_mem(MMAPString * f, int * col,
const char * str, size_t length)
{
return mailimf_header_string_write_driver(do_write, f, col, str, length);
}