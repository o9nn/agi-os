#ifndef MAILIMAP_OAUTH2_H
#define MAILIMAP_OAUTH2_H
#include <libetpan/mailimap_types.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
int mailimap_oauth2_authenticate(mailimap * session, const char * auth_user,
const char * access_token);
LIBETPAN_EXPORT
int mailimap_has_xoauth2(mailimap * session);
#ifdef __cplusplus
}
#endif
#endif