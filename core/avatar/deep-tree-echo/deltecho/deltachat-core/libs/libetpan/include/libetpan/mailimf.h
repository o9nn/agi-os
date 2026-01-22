#ifndef MAILIMF_H
#define MAILIMF_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailimf_types.h>
#include <libetpan/mailimf_write_generic.h>
#include <libetpan/mailimf_write_file.h>
#include <libetpan/mailimf_write_mem.h>
#include <libetpan/mailimf_types_helper.h>
#ifdef HAVE_INTTYPES_H
#	include <inttypes.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#	include <sys/types.h>
#endif
LIBETPAN_EXPORT
int mailimf_message_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_message ** result);
LIBETPAN_EXPORT
int mailimf_body_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_body ** result);
LIBETPAN_EXPORT
int mailimf_fields_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_fields ** result);
LIBETPAN_EXPORT
int
mailimf_mailbox_list_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_mailbox_list ** result);
LIBETPAN_EXPORT
int
mailimf_address_list_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_address_list ** result);
LIBETPAN_EXPORT
int mailimf_address_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_address ** result);
LIBETPAN_EXPORT
int mailimf_mailbox_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_mailbox ** result);
LIBETPAN_EXPORT
int mailimf_date_time_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_date_time ** result);
LIBETPAN_EXPORT
int mailimf_envelope_fields_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_fields ** result);
LIBETPAN_EXPORT
int mailimf_ignore_field_parse(const char * message, size_t length,
size_t * indx);
LIBETPAN_EXPORT
int
mailimf_envelope_and_optional_fields_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_fields ** result);
LIBETPAN_EXPORT
int
mailimf_optional_fields_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_fields ** result);
LIBETPAN_EXPORT
int mailimf_fws_parse(const char * message, size_t length, size_t * indx);
LIBETPAN_EXPORT
int mailimf_cfws_parse(const char * message, size_t length,
size_t * indx);
LIBETPAN_EXPORT
int mailimf_char_parse(const char * message, size_t length,
size_t * indx, char token);
LIBETPAN_EXPORT
int mailimf_unstrict_char_parse(const char * message, size_t length,
size_t * indx, char token);
LIBETPAN_EXPORT
int mailimf_crlf_parse(const char * message, size_t length, size_t * indx);
LIBETPAN_EXPORT
int
mailimf_custom_string_parse(const char * message, size_t length,
size_t * indx, char ** result,
int (* is_custom_char)(char));
LIBETPAN_EXPORT
int
mailimf_token_case_insensitive_len_parse(const char * message, size_t length,
size_t * indx, char * token,
size_t token_length);
#define mailimf_token_case_insensitive_parse(message, length, indx, token) \
mailimf_token_case_insensitive_len_parse(message, length, indx, token, \
strlen(token))
LIBETPAN_EXPORT
int mailimf_quoted_string_parse(const char * message, size_t length,
size_t * indx, char ** result);
LIBETPAN_EXPORT
int
mailimf_number_parse(const char * message, size_t length,
size_t * indx, uint32_t * result);
LIBETPAN_EXPORT
int mailimf_msg_id_parse(const char * message, size_t length,
size_t * indx,
char ** result);
LIBETPAN_EXPORT
int mailimf_msg_id_list_parse(const char * message, size_t length,
size_t * indx, clist ** result);
LIBETPAN_EXPORT
int mailimf_word_parse(const char * message, size_t length,
size_t * indx, char ** result);
LIBETPAN_EXPORT
int mailimf_atom_parse(const char * message, size_t length,
size_t * indx, char ** result);
LIBETPAN_EXPORT
int mailimf_fws_atom_parse(const char * message, size_t length,
size_t * indx, char ** result);
LIBETPAN_EXPORT
int mailimf_fws_word_parse(const char * message, size_t length,
size_t * indx, char ** result, int * p_missing_closing_quote);
LIBETPAN_EXPORT
int mailimf_fws_quoted_string_parse(const char * message, size_t length,
size_t * indx, char ** result);
LIBETPAN_EXPORT
int mailimf_references_parse(const char * message, size_t length,
size_t * indx,
struct mailimf_references ** result);
#ifdef __cplusplus
}
#endif
#endif