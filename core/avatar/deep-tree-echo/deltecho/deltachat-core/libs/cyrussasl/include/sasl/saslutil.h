#ifndef SASLUTIL_H
#define SASLUTIL_H 1
#ifndef SASL_H
#include "sasl.h"
#endif
#ifdef __cplusplus
extern "C" {
#endif
LIBSASL_API int sasl_decode64(const char *in, unsigned inlen,
char *out, unsigned outmax, unsigned *outlen);
LIBSASL_API int sasl_encode64(const char *in, unsigned inlen,
char *out, unsigned outmax, unsigned *outlen);
LIBSASL_API int sasl_mkchal(sasl_conn_t *conn, char *buf,
unsigned maxlen, unsigned hostflag);
LIBSASL_API int sasl_utf8verify(const char *str, unsigned len);
LIBSASL_API int sasl_randcreate(sasl_rand_t **rpool);
LIBSASL_API void sasl_randfree(sasl_rand_t **rpool);
LIBSASL_API void sasl_randseed(sasl_rand_t *rpool, const char *seed,
unsigned len);
LIBSASL_API void sasl_rand(sasl_rand_t *rpool, char *buf, unsigned len);
LIBSASL_API void sasl_churn(sasl_rand_t *rpool, const char *data,
unsigned len);
LIBSASL_API void sasl_erasebuffer(char *pass, unsigned len);
LIBSASL_API char *sasl_strlower (char *val);
LIBSASL_API int sasl_config_init(const char *filename);
LIBSASL_API void sasl_config_done(void);
#ifdef WIN32
#if defined(NEED_GETOPT)
LIBSASL_API int getopt(int argc, char **argv, char *optstring);
#endif
LIBSASL_API char * getpass(const char *prompt);
#endif
#ifdef __cplusplus
}
#endif
#endif