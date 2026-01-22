#ifndef XLIST_H
#define XLIST_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/mailimap_extension.h>
LIBETPAN_EXPORT
extern struct mailimap_extension_api mailimap_extension_xlist;
LIBETPAN_EXPORT
int mailimap_xlist(mailimap * session, const char * mb,
const char * list_mb, clist ** result);
LIBETPAN_EXPORT
int mailimap_has_xlist(mailimap * session);
#ifdef __cplusplus
}
#endif
#endif