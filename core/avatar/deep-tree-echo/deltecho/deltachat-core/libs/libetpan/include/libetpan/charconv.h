#ifndef CHARCONV_H
#define CHARCONV_H
#ifdef __cplusplus
extern "C" {
#endif
#include <sys/types.h>
#ifndef LIBETPAN_CONFIG_H
#	include <libetpan/libetpan-config.h>
#endif
enum {
MAIL_CHARCONV_NO_ERROR = 0,
MAIL_CHARCONV_ERROR_UNKNOWN_CHARSET,
MAIL_CHARCONV_ERROR_MEMORY,
MAIL_CHARCONV_ERROR_CONV
};
LIBETPAN_EXPORT
extern int (*extended_charconv)(const char * tocode, const char * fromcode, const char * str, size_t length,
char * result, size_t* result_len);
LIBETPAN_EXPORT
int charconv(const char * tocode, const char * fromcode,
const char * str, size_t length,
char ** result);
LIBETPAN_EXPORT
int charconv_buffer(const char * tocode, const char * fromcode,
const char * str, size_t length,
char ** result, size_t * result_len);
LIBETPAN_EXPORT
void charconv_buffer_free(char * str);
#ifdef __cplusplus
}
#endif
#endif