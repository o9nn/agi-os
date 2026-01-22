#include <config.h>
#include <stdio.h>
#include <string.h>
#include <sasl.h>
#include <saslplug.h>
#include "plugin_common.h"
#ifdef macintosh
#include <sasl_plain_plugin_decl.h>
#endif
static const char plugin_id[] = "$Id: plain.c,v 1.67 2009/06/10 16:05:19 mel Exp $";
static int plain_server_mech_new(void *glob_context __attribute__((unused)),
sasl_server_params_t *sparams,
const char *challenge __attribute__((unused)),
unsigned challen __attribute__((unused)),
void **conn_context)
{
if (!conn_context) {
PARAMERROR( sparams->utils );
return SASL_BADPARAM;
}
*conn_context = NULL;
return SASL_OK;
}
static int plain_server_mech_step(void *conn_context __attribute__((unused)),
sasl_server_params_t *params,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams)
{
const char *author;
const char *authen;
const char *password;
unsigned password_len;
unsigned lup = 0;
int result;
char *passcopy;
unsigned canon_flags = 0;
*serverout = NULL;
*serveroutlen = 0;
author = clientin;
while ((lup < clientinlen) && (clientin[lup] != 0)) ++lup;
if (lup >= clientinlen) {
SETERROR(params->utils, "Can only find author (no password)");
return SASL_BADPROT;
}
++lup;
authen = clientin + lup;
while ((lup < clientinlen) && (clientin[lup] != 0)) ++lup;
if (lup >= clientinlen) {
params->utils->seterror(params->utils->conn, 0,
"Can only find author/en (no password)");
return SASL_BADPROT;
}
lup++;
password = clientin + lup;
while ((lup < clientinlen) && (clientin[lup] != 0)) ++lup;
password_len = (unsigned) (clientin + lup - password);
if (lup != clientinlen) {
SETERROR(params->utils,
"Got more data than we were expecting in the PLAIN plugin\n");
return SASL_BADPROT;
}
passcopy = params->utils->malloc(password_len + 1);
if (passcopy == NULL) {
MEMERROR(params->utils);
return SASL_NOMEM;
}
strncpy(passcopy, password, password_len);
passcopy[password_len] = '\0';
if (!author || !*author) {
author = authen;
canon_flags = SASL_CU_AUTHZID;
} else if (strcmp(author, authen) == 0) {
canon_flags = SASL_CU_AUTHZID;
}
result = params->canon_user(params->utils->conn,
authen,
0,
SASL_CU_AUTHID | canon_flags | SASL_CU_EXTERNALLY_VERIFIED,
oparams);
if (result != SASL_OK) {
_plug_free_string(params->utils, &passcopy);
return result;
}
result = params->utils->checkpass(params->utils->conn,
oparams->authid,
oparams->alen,
passcopy,
password_len);
_plug_free_string(params->utils, &passcopy);
if (result != SASL_OK) {
params->utils->seterror(params->utils->conn, 0,
"Password verification failed");
return result;
}
if (canon_flags == 0) {
const struct propval *pr;
int i;
pr = params->utils->prop_get(params->propctx);
if (!pr) {
return SASL_FAIL;
}
for (i = 0; pr[i].name; i++) {
if (pr[i].name[0] == '*') {
continue;
}
if (pr[i].values) {
params->utils->prop_erase(params->propctx, pr[i].name);
}
}
result = params->canon_user(params->utils->conn,
author,
0,
SASL_CU_AUTHZID,
oparams);
if (result != SASL_OK) {
return result;
}
}
oparams->doneflag = 1;
oparams->mech_ssf = 0;
oparams->maxoutbuf = 0;
oparams->encode_context = NULL;
oparams->encode = NULL;
oparams->decode_context = NULL;
oparams->decode = NULL;
oparams->param_version = 0;
return SASL_OK;
}
static sasl_server_plug_t plain_server_plugins[] =
{
{
"PLAIN",
0,
SASL_SEC_NOANONYMOUS
| SASL_SEC_PASS_CREDENTIALS,
SASL_FEAT_WANT_CLIENT_FIRST
| SASL_FEAT_ALLOWS_PROXY,
NULL,
&plain_server_mech_new,
&plain_server_mech_step,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL
}
};
int plain_server_plug_init(const sasl_utils_t *utils,
int maxversion,
int *out_version,
sasl_server_plug_t **pluglist,
int *plugcount)
{
if (maxversion < SASL_SERVER_PLUG_VERSION) {
SETERROR(utils, "PLAIN version mismatch");
return SASL_BADVERS;
}
*out_version = SASL_SERVER_PLUG_VERSION;
*pluglist = plain_server_plugins;
*plugcount = 1;
return SASL_OK;
}
typedef struct client_context {
char *out_buf;
unsigned out_buf_len;
} client_context_t;
static int plain_client_mech_new(void *glob_context __attribute__((unused)),
sasl_client_params_t *params,
void **conn_context)
{
client_context_t *text;
text = params->utils->malloc(sizeof(client_context_t));
if (text == NULL) {
MEMERROR( params->utils );
return SASL_NOMEM;
}
memset(text, 0, sizeof(client_context_t));
*conn_context = text;
return SASL_OK;
}
static int plain_client_mech_step(void *conn_context,
sasl_client_params_t *params,
const char *serverin __attribute__((unused)),
unsigned serverinlen __attribute__((unused)),
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
client_context_t *text = (client_context_t *) conn_context;
const char *user = NULL, *authid = NULL;
sasl_secret_t *password = NULL;
unsigned int free_password = 0;
int user_result = SASL_OK;
int auth_result = SASL_OK;
int pass_result = SASL_OK;
int result;
char *p;
*clientout = NULL;
*clientoutlen = 0;
if (params->props.min_ssf > params->external_ssf) {
SETERROR( params->utils, "SSF requested of PLAIN plugin");
return SASL_TOOWEAK;
}
if (oparams->authid == NULL) {
auth_result = _plug_get_authid(params->utils, &authid, prompt_need);
if ((auth_result != SASL_OK) && (auth_result != SASL_INTERACT))
return auth_result;
}
if (oparams->user == NULL) {
user_result = _plug_get_userid(params->utils, &user, prompt_need);
if ((user_result != SASL_OK) && (user_result != SASL_INTERACT))
return user_result;
}
if (password == NULL) {
pass_result = _plug_get_password(params->utils, &password,
&free_password, prompt_need);
if ((pass_result != SASL_OK) && (pass_result != SASL_INTERACT))
return pass_result;
}
if (prompt_need && *prompt_need) {
params->utils->free(*prompt_need);
*prompt_need = NULL;
}
if ((user_result == SASL_INTERACT) || (auth_result == SASL_INTERACT) ||
(pass_result == SASL_INTERACT)) {
result =
_plug_make_prompts(params->utils, prompt_need,
user_result == SASL_INTERACT ?
"Please enter your authorization name" : NULL,
NULL,
auth_result == SASL_INTERACT ?
"Please enter your authentication name" : NULL,
NULL,
pass_result == SASL_INTERACT ?
"Please enter your password" : NULL, NULL,
NULL, NULL, NULL,
NULL, NULL, NULL);
if (result != SASL_OK) goto cleanup;
return SASL_INTERACT;
}
if (!password) {
PARAMERROR(params->utils);
return SASL_BADPARAM;
}
if (!user || !*user) {
result = params->canon_user(params->utils->conn, authid, 0,
SASL_CU_AUTHID | SASL_CU_AUTHZID, oparams);
}
else {
result = params->canon_user(params->utils->conn, user, 0,
SASL_CU_AUTHZID, oparams);
if (result != SASL_OK) goto cleanup;
result = params->canon_user(params->utils->conn, authid, 0,
SASL_CU_AUTHID, oparams);
}
if (result != SASL_OK) goto cleanup;
*clientoutlen = ((user && *user ? oparams->ulen : 0) +
1 + oparams->alen +
1 + password->len);
result = _plug_buf_alloc(params->utils, &(text->out_buf),
&(text->out_buf_len), *clientoutlen + 1);
if (result != SASL_OK) goto cleanup;
memset(text->out_buf, 0, *clientoutlen + 1);
p = text->out_buf;
if (user && *user) {
memcpy(p, oparams->user, oparams->ulen);
p += oparams->ulen;
}
memcpy(++p, oparams->authid, oparams->alen);
p += oparams->alen;
memcpy(++p, password->data, password->len);
*clientout = text->out_buf;
oparams->doneflag = 1;
oparams->mech_ssf = 0;
oparams->maxoutbuf = 0;
oparams->encode_context = NULL;
oparams->encode = NULL;
oparams->decode_context = NULL;
oparams->decode = NULL;
oparams->param_version = 0;
result = SASL_OK;
cleanup:
if (free_password) _plug_free_secret(params->utils, &password);
return result;
}
static void plain_client_mech_dispose(void *conn_context,
const sasl_utils_t *utils)
{
client_context_t *text = (client_context_t *) conn_context;
if (!text) return;
if (text->out_buf) utils->free(text->out_buf);
utils->free(text);
}
static sasl_client_plug_t plain_client_plugins[] =
{
{
"PLAIN",
0,
SASL_SEC_NOANONYMOUS
| SASL_SEC_PASS_CREDENTIALS,
SASL_FEAT_WANT_CLIENT_FIRST
| SASL_FEAT_ALLOWS_PROXY,
NULL,
NULL,
&plain_client_mech_new,
&plain_client_mech_step,
&plain_client_mech_dispose,
NULL,
NULL,
NULL,
NULL
}
};
int plain_client_plug_init(sasl_utils_t *utils,
int maxversion,
int *out_version,
sasl_client_plug_t **pluglist,
int *plugcount)
{
if (maxversion < SASL_CLIENT_PLUG_VERSION) {
SETERROR(utils, "PLAIN version mismatch");
return SASL_BADVERS;
}
*out_version = SASL_CLIENT_PLUG_VERSION;
*pluglist = plain_client_plugins;
*plugcount = 1;
return SASL_OK;
}