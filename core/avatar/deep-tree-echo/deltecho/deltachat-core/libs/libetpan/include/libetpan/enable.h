#ifndef ENABLE_H
#define ENABLE_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailimap_extension.h>
LIBETPAN_EXPORT
extern struct mailimap_extension_api mailimap_extension_enable;
LIBETPAN_EXPORT
int mailimap_enable(mailimap * session, struct mailimap_capability_data * capabilities,
struct mailimap_capability_data ** result);
LIBETPAN_EXPORT
int mailimap_has_enable(mailimap * session);
#ifdef __cplusplus
}
#endif
#endif