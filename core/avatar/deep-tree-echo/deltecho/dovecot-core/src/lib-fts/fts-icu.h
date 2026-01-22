#ifndef HAVE_FTS_ICU_H
#define HAVE_FTS_ICU_H
#include <unicode/ustring.h>
#include <unicode/utrans.h>
ARRAY_DEFINE_TYPE(icu_utf16, UChar);
void fts_icu_utf8_to_utf16(ARRAY_TYPE(icu_utf16) *dest_utf16,
const char *src_utf8);
void fts_icu_utf16_to_utf8(string_t *dest_utf8, const UChar *src_utf16,
unsigned int src_len);
int fts_icu_translate(ARRAY_TYPE(icu_utf16) *dest_utf16, const UChar *src_utf16,
unsigned int src_len, UTransliterator *transliterator,
const char **error_r);
void fts_icu_lcase(string_t *dest_utf8, const char *src_utf8);
void fts_icu_deinit(void);
int fts_icu_transliterator_create(const char *id,
UTransliterator **transliterator_r,
const char **error_r) ;
#endif