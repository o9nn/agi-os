#ifndef MAILMIME_DECODE_H
#define MAILMIME_DECODE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailmime_types.h>
LIBETPAN_EXPORT
int mailmime_encoded_phrase_parse(const char * default_fromcode,
const char * message, size_t length,
size_t * indx, const char * tocode,
char ** result);
int
mailmime_encoded_word_parse(const char * message, size_t length,
size_t * indx,
struct mailmime_encoded_word ** result,
int * p_has_fwd, int * p_missing_closing_quote);
#ifdef __cplusplus
}
#endif
#endif