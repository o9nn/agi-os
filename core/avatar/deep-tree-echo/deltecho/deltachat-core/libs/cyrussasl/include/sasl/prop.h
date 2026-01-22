#ifndef PROP_H
#define PROP_H 1
#ifdef WIN32
# ifdef LIBSASL_EXPORTS
#  define LIBSASL_API  extern __declspec(dllexport)
# else
#  define LIBSASL_API  extern __declspec(dllimport)
# endif
#else
# define LIBSASL_API extern
#endif
#ifdef WIN32
# ifdef LIBSASL_EXPORTS
#  define LIBSASL_VAR  extern __declspec(dllexport)
# else
#  define LIBSASL_VAR  extern __declspec(dllimport)
# endif
#else
# define LIBSASL_VAR extern
#endif
struct propval {
const char *name;
const char **values;
unsigned nvalues;
unsigned valsize;
};
#define PROP_DEFAULT 4
struct propctx;
#ifdef __cplusplus
extern "C" {
#endif
LIBSASL_API struct propctx *prop_new(unsigned estimate);
LIBSASL_API int prop_dup(struct propctx *src_ctx, struct propctx **dst_ctx);
LIBSASL_API int prop_request(struct propctx *ctx, const char **names);
LIBSASL_API const struct propval *prop_get(struct propctx *ctx);
LIBSASL_API int prop_getnames(struct propctx *ctx, const char **names,
struct propval *vals);
LIBSASL_API void prop_clear(struct propctx *ctx, int requests);
LIBSASL_API void prop_erase(struct propctx *ctx, const char *name);
LIBSASL_API void prop_dispose(struct propctx **ctx);
LIBSASL_API int prop_format(struct propctx *ctx, const char *sep, int seplen,
char *outbuf, unsigned outmax, unsigned *outlen);
LIBSASL_API int prop_set(struct propctx *ctx, const char *name,
const char *value, int vallen);
LIBSASL_API int prop_setvals(struct propctx *ctx, const char *name,
const char **values);
#ifdef __cplusplus
}
#endif
#endif