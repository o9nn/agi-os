#include "auth-common.h"
#include "userdb.h"
#ifdef USERDB_PREFETCH
#include "str.h"
#include "var-expand.h"
static void prefetch_lookup(struct auth_request *auth_request,
userdb_callback_t *callback)
{
if (!auth_request->userdb_prefetch_set) {
if (auth_request_get_auth(auth_request)->userdbs->next == NULL) {
if (auth_request->userdb_lookup) {
e_error(authdb_event(auth_request),
"userdb lookup not possible with only userdb prefetch");
} else {
e_error(authdb_event(auth_request),
"passdb didn't return userdb entries");
}
callback(USERDB_RESULT_INTERNAL_FAILURE, auth_request);
return;
}
e_debug(authdb_event(auth_request),
"passdb didn't return userdb entries, "
"trying the next userdb");
callback(USERDB_RESULT_USER_UNKNOWN, auth_request);
return;
}
e_debug(authdb_event(auth_request), "success");
callback(USERDB_RESULT_OK, auth_request);
}
struct userdb_module_interface userdb_prefetch = {
"prefetch",
NULL,
NULL,
NULL,
prefetch_lookup,
NULL,
NULL,
NULL
};
#else
struct userdb_module_interface userdb_prefetch = {
.name = "prefetch"
};
#endif