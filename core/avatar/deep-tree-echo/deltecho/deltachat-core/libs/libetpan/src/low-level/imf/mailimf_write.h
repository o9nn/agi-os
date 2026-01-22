#ifndef MAILIMF_WRITE_H
#define MAILIMF_WRITE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <stdio.h>
#include <libetpan/mailimf_types.h>
int mailimf_string_write(FILE * f, int * col,
const char * str, size_t length);
int mailimf_fields_write(FILE * f, int * col,
struct mailimf_fields * fields);
int mailimf_envelope_fields_write(FILE * f, int * col,
struct mailimf_fields * fields);
int mailimf_field_write(FILE * f, int * col,
struct mailimf_field * field);
int mailimf_quoted_string_write(FILE * f, int * col,
const char * string, size_t len);
int mailimf_address_list_write(FILE * f, int * col,
struct mailimf_address_list * addr_list);
int mailimf_mailbox_list_write(FILE * f, int * col,
struct mailimf_mailbox_list * mb_list);
int mailimf_header_string_write(FILE * f, int * col,
const char * str, size_t length);
#ifdef __cplusplus
}
#endif
#endif