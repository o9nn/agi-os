#include <config.h>
#include <stdio.h>
#include "sasl.h"
#include "saslutil.h"
#include "saslplug.h"
#include "../sasldb/sasldb.h"
#include "plugin_common.h"
static int sasldb_auxprop_lookup(void *glob_context __attribute__((unused)),
sasl_server_params_t *sparams,
unsigned flags,
const char *user,
unsigned ulen)
{
char *userid = NULL;
char *realm = NULL;
const char *user_realm = NULL;
int ret;
const struct propval *to_fetch, *cur;
char value[8192];
size_t value_len;
char *user_buf;
int verify_against_hashed_password;
int saw_user_password = 0;
if (!sparams || !user) return SASL_BADPARAM;
user_buf = sparams->utils->malloc(ulen + 1);
if(!user_buf) {
ret = SASL_NOMEM;
goto done;
}
memcpy(user_buf, user, ulen);
user_buf[ulen] = '\0';
if(sparams->user_realm) {
user_realm = sparams->user_realm;
} else {
user_realm = sparams->serverFQDN;
}
ret = _plug_parseuser(sparams->utils, &userid, &realm, user_realm,
sparams->serverFQDN, user_buf);
if(ret != SASL_OK) goto done;
to_fetch = sparams->utils->prop_get(sparams->propctx);
if (!to_fetch) {
ret = SASL_NOMEM;
goto done;
}
verify_against_hashed_password = flags & SASL_AUXPROP_VERIFY_AGAINST_HASH;
ret = SASL_CONTINUE;
for(cur = to_fetch; cur->name; cur++) {
int cur_ret;
const char *realname = cur->name;
if(cur->name[0] == '*' && (flags & SASL_AUXPROP_AUTHZID)) continue;
if(!(flags & SASL_AUXPROP_AUTHZID)) {
if(cur->name[0] != '*') continue;
else realname = cur->name + 1;
}
if (cur->values && !(flags & SASL_AUXPROP_OVERRIDE) &&
(verify_against_hashed_password == 0 ||
strcasecmp(realname, SASL_AUX_PASSWORD_PROP) != 0)) {
continue;
} else if (cur->values) {
sparams->utils->prop_erase(sparams->propctx, cur->name);
}
if (strcasecmp(realname, SASL_AUX_PASSWORD_PROP) == 0) {
saw_user_password = 1;
}
cur_ret = _sasldb_getdata(sparams->utils,
sparams->utils->conn, userid, realm,
realname, value, sizeof(value), &value_len);
if (ret == SASL_CONTINUE || ret == SASL_NOUSER) {
ret = cur_ret;
} else if (ret == SASL_OK) {
if (cur_ret != SASL_NOUSER) {
ret = cur_ret;
}
}
if (cur_ret != SASL_OK) {
if (cur_ret != SASL_NOUSER) {
break;
}
continue;
}
sparams->utils->prop_set(sparams->propctx, cur->name,
value, (unsigned) value_len);
}
if (ret == SASL_CONTINUE) {
ret = SASL_OK;
}
if (flags & SASL_AUXPROP_AUTHZID) {
if (ret == SASL_NOUSER) {
ret = SASL_OK;
}
} else {
if (ret == SASL_NOUSER && saw_user_password == 0) {
ret = _sasldb_getdata(sparams->utils,
sparams->utils->conn,
userid,
realm,
SASL_AUX_PASSWORD_PROP,
value,
sizeof(value),
&value_len);
}
}
done:
if (userid) sparams->utils->free(userid);
if (realm)  sparams->utils->free(realm);
if (user_buf) sparams->utils->free(user_buf);
return ret;
}
static int sasldb_auxprop_store(void *glob_context __attribute__((unused)),
sasl_server_params_t *sparams,
struct propctx *ctx,
const char *user,
unsigned ulen)
{
char *userid = NULL;
char *realm = NULL;
const char *user_realm = NULL;
int ret = SASL_FAIL;
const struct propval *to_store, *cur;
char *user_buf;
if(!ctx) return SASL_OK;
if(!sparams || !user) return SASL_BADPARAM;
user_buf = sparams->utils->malloc(ulen + 1);
if(!user_buf) {
ret = SASL_NOMEM;
goto done;
}
memcpy(user_buf, user, ulen);
user_buf[ulen] = '\0';
if(sparams->user_realm) {
user_realm = sparams->user_realm;
} else {
user_realm = sparams->serverFQDN;
}
ret = _plug_parseuser(sparams->utils, &userid, &realm, user_realm,
sparams->serverFQDN, user_buf);
if(ret != SASL_OK) goto done;
to_store = sparams->utils->prop_get(ctx);
if(!to_store) {
ret = SASL_BADPARAM;
goto done;
}
ret = SASL_OK;
for (cur = to_store; cur->name; cur++) {
char * value = (cur->values && cur->values[0]) ? cur->values[0] : NULL;
if (cur->name[0] == '*') {
continue;
}
ret = _sasldb_putdata(sparams->utils,
sparams->utils->conn,
userid,
realm,
cur->name,
value,
value ? strlen(value) : 0);
if (value == NULL && ret == SASL_NOUSER) {
ret = SASL_OK;
}
if (ret != SASL_OK) {
break;
}
}
done:
if (userid) sparams->utils->free(userid);
if (realm)  sparams->utils->free(realm);
if (user_buf) sparams->utils->free(user_buf);
return ret;
}
static sasl_auxprop_plug_t sasldb_auxprop_plugin = {
0,
0,
NULL,
sasldb_auxprop_free,
sasldb_auxprop_lookup,
"sasldb",
sasldb_auxprop_store
};
int sasldb_auxprop_plug_init(const sasl_utils_t *utils,
int max_version,
int *out_version,
sasl_auxprop_plug_t **plug,
const char *plugname __attribute__((unused)))
{
if(!out_version || !plug) return SASL_BADPARAM;
if(_sasl_check_db(utils, NULL) != SASL_OK)
return SASL_NOMECH;
if(max_version < SASL_AUXPROP_PLUG_VERSION) return SASL_BADVERS;
*out_version = SASL_AUXPROP_PLUG_VERSION;
*plug = &sasldb_auxprop_plugin;
return SASL_OK;
}