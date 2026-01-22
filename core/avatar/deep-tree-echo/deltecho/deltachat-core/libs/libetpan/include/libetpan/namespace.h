#ifndef NAMESPACE_H
#define NAMESPACE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/namespace_types.h>
#include <libetpan/mailimap_extension.h>
LIBETPAN_EXPORT
extern struct mailimap_extension_api mailimap_extension_namespace;
LIBETPAN_EXPORT
int mailimap_namespace(mailimap * session, struct mailimap_namespace_data ** result);
LIBETPAN_EXPORT
int mailimap_has_namespace(mailimap * session);
#ifdef __cplusplus
}
#endif
#endif