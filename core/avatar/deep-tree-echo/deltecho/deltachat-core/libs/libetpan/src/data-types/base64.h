#ifndef BASE64_H
#define BASE64_H
#ifdef __cplusplus
extern "C" {
#endif
#ifndef LIBETPAN_CONFIG_H
#	include "libetpan-config.h"
#endif
LIBETPAN_EXPORT
char * encode_base64(const char * in, int len);
LIBETPAN_EXPORT
char * decode_base64(const char * in, int len);
#ifdef __cplusplus
}
#endif
#endif