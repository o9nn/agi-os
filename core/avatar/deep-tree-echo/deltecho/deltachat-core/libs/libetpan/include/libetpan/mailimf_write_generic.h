#ifndef MAILIMF_WRITE_GENERIC_H
#define MAILIMF_WRITE_GENERIC_H
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <libetpan/mailimf_types.h>
LIBETPAN_EXPORT
int mailimf_string_write_driver(int (* do_write)(void *, const char *, size_t), void * data,
int * col,
const char * str, size_t length);
LIBETPAN_EXPORT
int mailimf_fields_write_driver(int (* do_write)(void *, const char *, size_t), void * data,
int * col,
struct mailimf_fields * fields);
LIBETPAN_EXPORT
int mailimf_envelope_fields_write_driver(int (* do_write)(void *, const char *, size_t), void * data,
int * col,
struct mailimf_fields * fields);
LIBETPAN_EXPORT
int mailimf_field_write_driver(int (* do_write)(void *, const char *, size_t), void * data,
int * col,
struct mailimf_field * field);
LIBETPAN_EXPORT
int mailimf_quoted_string_write_driver(int (* do_write)(void *, const char *, size_t), void * data,
int * col,
const char * string, size_t len);
LIBETPAN_EXPORT
int mailimf_address_list_write_driver(int (* do_write)(void *, const char *, size_t), void * data,
int * col,
struct mailimf_address_list * addr_list);
LIBETPAN_EXPORT
int mailimf_mailbox_list_write_driver(int (* do_write)(void *, const char *, size_t), void * data,
int * col,
struct mailimf_mailbox_list * mb_list);
LIBETPAN_EXPORT
int mailimf_header_string_write_driver(int (* do_write)(void *, const char *, size_t), void * data,
int * col,
const char * str, size_t length);
#ifdef __cplusplus
}
#endif
#endif