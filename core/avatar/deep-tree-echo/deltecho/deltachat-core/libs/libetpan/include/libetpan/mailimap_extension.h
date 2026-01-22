#ifndef MAILIMAP_EXTENSION_H
#define MAILIMAP_EXTENSION_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailimap_types.h>
#include <libetpan/mailimap_extension_types.h>
LIBETPAN_EXPORT
int
mailimap_extension_register(struct mailimap_extension_api * extension);
LIBETPAN_EXPORT
void
mailimap_extension_unregister_all(void);
LIBETPAN_EXPORT
int
mailimap_extension_data_parse(int calling_parser,
mailstream * fd, MMAPString * buffer, struct mailimap_parser_context * parser_ctx,
size_t * indx, struct mailimap_extension_data ** result,
size_t progr_rate,
progress_function * progr_fun);
LIBETPAN_EXPORT
struct mailimap_extension_data *
mailimap_extension_data_new(struct mailimap_extension_api * extension,
int type, void * data);
LIBETPAN_EXPORT
void
mailimap_extension_data_free(struct
mailimap_extension_data * data);
void mailimap_extension_data_store(mailimap * session,
struct mailimap_extension_data ** ext_data);
LIBETPAN_EXPORT
int mailimap_has_extension(mailimap * session, const char * extension_name);
LIBETPAN_EXPORT
int mailimap_has_authentication(mailimap * session, const char * authentication_name);
#ifdef __cplusplus
}
#endif
#endif