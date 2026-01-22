#include "mailimf_write_file.h"
#include "mailimf_write_generic.h"
static int do_write(void * data, const char * str, size_t length)
{
FILE * f;
f = data;
return (int) fwrite(str, 1, length, f);
}
LIBETPAN_EXPORT
int mailimf_string_write_file(FILE * f, int * col,
const char * str, size_t length)
{
return mailimf_string_write_driver(do_write, f, col, str, length);
}
LIBETPAN_EXPORT
int mailimf_fields_write_file(FILE * f, int * col,
struct mailimf_fields * fields)
{
return mailimf_fields_write_driver(do_write, f, col, fields);
}
LIBETPAN_EXPORT
int mailimf_envelope_fields_write_file(FILE * f, int * col,
struct mailimf_fields * fields)
{
return mailimf_envelope_fields_write_driver(do_write, f, col, fields);
}
LIBETPAN_EXPORT
int mailimf_field_write_file(FILE * f, int * col,
struct mailimf_field * field)
{
return mailimf_field_write_driver(do_write, f, col, field);
}
LIBETPAN_EXPORT
int mailimf_quoted_string_write_file(FILE * f, int * col,
const char * string, size_t len)
{
return mailimf_quoted_string_write_driver(do_write, f, col, string, len);
}
LIBETPAN_EXPORT
int mailimf_address_list_write_file(FILE * f, int * col,
struct mailimf_address_list * addr_list)
{
return mailimf_address_list_write_driver(do_write, f, col, addr_list);
}
LIBETPAN_EXPORT
int mailimf_mailbox_list_write_file(FILE * f, int * col,
struct mailimf_mailbox_list * mb_list)
{
return mailimf_mailbox_list_write_driver(do_write, f, col, mb_list);
}
LIBETPAN_EXPORT
int mailimf_header_string_write_file(FILE * f, int * col,
const char * str, size_t length)
{
return mailimf_header_string_write_driver(do_write, f, col, str, length);
}
#ifdef MAILIMF_WRITE_COMPATIBILITY
int mailimf_string_write(FILE * f, int * col,
const char * str, size_t length)
{
return mailimf_string_write_file(f, col, str, length);
}
int mailimf_fields_write(FILE * f, int * col,
struct mailimf_fields * fields)
{
return mailimf_fields_write_file(f, col, fields);
}
int mailimf_envelope_fields_write(FILE * f, int * col,
struct mailimf_fields * fields)
{
return mailimf_envelope_fields_write_file(f, col, fields);
}
int mailimf_field_write(FILE * f, int * col,
struct mailimf_field * field)
{
return mailimf_field_write_file(f, col, field);
}
int mailimf_quoted_string_write(FILE * f, int * col,
const char * string, size_t len)
{
return mailimf_quoted_string_write_file(f, col, string, len);
}
int mailimf_address_list_write(FILE * f, int * col,
struct mailimf_address_list * addr_list)
{
return mailimf_address_list_write_file(f, col, addr_list);
}
int mailimf_mailbox_list_write(FILE * f, int * col,
struct mailimf_mailbox_list * mb_list)
{
return mailimf_mailbox_list_write_file(f, col, mb_list);
}
int mailimf_header_string_write(FILE * f, int * col,
const char * str, size_t length)
{
return mailimf_header_string_write_file(f, col, str, length);
}
#endif