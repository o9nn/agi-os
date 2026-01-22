#ifndef ACL_H
#define ACL_H
#include <libetpan/libetpan-config.h>
#include <libetpan/mailimap_extension.h>
#include <libetpan/acl_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
extern struct mailimap_extension_api mailimap_extension_acl;
LIBETPAN_EXPORT
int mailimap_acl_setacl(mailimap * session,
const char * mailbox,
const char * identifier,
const char * mod_rights);
LIBETPAN_EXPORT
int mailimap_acl_deleteacl(mailimap * session,
const char * mailbox,
const char * identifier);
LIBETPAN_EXPORT
int mailimap_acl_getacl(mailimap * session,
const char * mailbox,
clist ** result);
LIBETPAN_EXPORT
int mailimap_acl_listrights(mailimap * session,
const char * mailbox,
const char * identifier,
struct mailimap_acl_listrights_data ** result);
LIBETPAN_EXPORT
int mailimap_acl_myrights(mailimap * session,
const char * mailbox,
struct mailimap_acl_myrights_data ** result);
LIBETPAN_EXPORT
int mailimap_has_acl(mailimap * session);
#ifdef __cplusplus
}
#endif
#endif