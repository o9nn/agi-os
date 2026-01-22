#ifndef MAILIMAP_HELPER_H
#define MAILIMAP_HELPER_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailimap_types.h>
LIBETPAN_EXPORT
int mailimap_fetch_rfc822(mailimap * session,
uint32_t msgid, char ** result);
LIBETPAN_EXPORT
int mailimap_fetch_rfc822_header(mailimap * session,
uint32_t msgid, char ** result);
LIBETPAN_EXPORT
int mailimap_fetch_envelope(mailimap * session,
uint32_t first, uint32_t last,
clist ** result);
LIBETPAN_EXPORT
int mailimap_append_simple(mailimap * session, const char * mailbox,
const char * content, size_t size);
LIBETPAN_EXPORT
int mailimap_login_simple(mailimap * session,
const char * userid, const char * password);
#ifdef __cplusplus
}
#endif
#endif