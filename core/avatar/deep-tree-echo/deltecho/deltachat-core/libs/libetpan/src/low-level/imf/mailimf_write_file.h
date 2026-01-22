#ifndef MAILIMF_WRITE_H
#define MAILIMF_WRITE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <libetpan/mailimf_types.h>
#define MAILIMF_WRITE_COMPATIBILITY
LIBETPAN_EXPORT
int mailimf_string_write_file(FILE * f, int * col,
const char * str, size_t length);
LIBETPAN_EXPORT
int mailimf_fields_write_file(FILE * f, int * col,
struct mailimf_fields * fields);
LIBETPAN_EXPORT
int mailimf_envelope_fields_write_file(FILE * f, int * col,
struct mailimf_fields * fields);
LIBETPAN_EXPORT
int mailimf_field_write_file(FILE * f, int * col,
struct mailimf_field * field);
LIBETPAN_EXPORT
int mailimf_quoted_string_write_file(FILE * f, int * col,
const char * string, size_t len);
LIBETPAN_EXPORT
int mailimf_address_list_write_file(FILE * f, int * col,
struct mailimf_address_list * addr_list);
LIBETPAN_EXPORT
int mailimf_mailbox_list_write_file(FILE * f, int * col,
struct mailimf_mailbox_list * mb_list);
LIBETPAN_EXPORT
int mailimf_header_string_write_file(FILE * f, int * col,
const char * str, size_t length);
#ifdef MAILIMF_WRITE_COMPATIBILITY
LIBETPAN_EXPORT
int mailimf_string_write(FILE * f, int * col,
const char * str, size_t length);
LIBETPAN_EXPORT
int mailimf_fields_write(FILE * f, int * col,
struct mailimf_fields * fields);
LIBETPAN_EXPORT
int mailimf_envelope_fields_write(FILE * f, int * col,
struct mailimf_fields * fields);
LIBETPAN_EXPORT
int mailimf_field_write(FILE * f, int * col,
struct mailimf_field * field);
LIBETPAN_EXPORT
int mailimf_quoted_string_write(FILE * f, int * col,
const char * string, size_t len);
LIBETPAN_EXPORT
int mailimf_address_list_write(FILE * f, int * col,
struct mailimf_address_list * addr_list);
LIBETPAN_EXPORT
int mailimf_mailbox_list_write(FILE * f, int * col,
struct mailimf_mailbox_list * mb_list);
LIBETPAN_EXPORT
int mailimf_header_string_write(FILE * f, int * col,
const char * str, size_t length);
#endif
#ifdef __cplusplus
}
#endif
#endif