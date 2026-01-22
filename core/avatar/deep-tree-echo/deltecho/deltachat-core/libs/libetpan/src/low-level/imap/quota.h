#ifndef QUOTA_H
#define QUOTA_H
#include <libetpan/libetpan-config.h>
#include <libetpan/mailimap_extension.h>
#include <libetpan/quota_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
extern struct mailimap_extension_api mailimap_extension_quota;
void
mailimap_quota_free(struct mailimap_extension_data * ext_data);
LIBETPAN_EXPORT
int mailimap_quota_getquotaroot(mailimap * session,
const char * list_mb,
struct mailimap_quota_complete_data ** result);
#ifdef __cplusplus
}
#endif
#endif