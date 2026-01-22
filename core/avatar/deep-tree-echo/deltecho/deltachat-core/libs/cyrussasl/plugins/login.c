#include <config.h>
#include <stdio.h>
#include <ctype.h>
#include <sasl.h>
#include <saslplug.h>
#include "plugin_common.h"
static const char plugin_id[] = "$Id: login.c,v 1.31 2010/11/30 11:41:47 mel Exp $";
typedef struct context {
int state;
char *username;
unsigned username_len;
} server_context_t;
static int login_server_mech_new(void *glob_context __attribute__((unused)),
sasl_server_params_t *sparams,
const char *challenge __attribute__((unused)),
unsigned challen __attribute__((unused)),
void **conn_context)
{
server_context_t *text;
text = sparams->utils->malloc(sizeof(server_context_t));
if (text == NULL) {
MEMERROR( sparams->utils );
return SASL_NOMEM;
}
memset(text, 0, sizeof(server_context_t));
text->state = 1;
*conn_context = text;
return SASL_OK;
}
#define USERNAME_CHALLENGE "Username:"
#define PASSWORD_CHALLENGE "Password:"
static int login_server_mech_step(void *conn_context,
sasl_server_params_t *params,
const char *clientin,
unsigned clientinlen,
const char **serverout,
unsigned *serveroutlen,
sasl_out_params_t *oparams)
{
server_context_t *text = (server_context_t *) conn_context;
*serverout = NULL;
*serveroutlen = 0;
if (text == NULL) {
return SASL_BADPROT;
}
switch (text->state) {
case 1:
text->state = 2;
if (clientinlen == 0) {
*serveroutlen = (unsigned) strlen(USERNAME_CHALLENGE);
*serverout = USERNAME_CHALLENGE;
return SASL_CONTINUE;
}
case 2:
if (clientinlen > 1024) {
SETERROR(params->utils, "username too long (>1024 characters)");
return SASL_BADPROT;
}
text->username =
params->utils->malloc(sizeof(sasl_secret_t) + clientinlen + 1);
if (!text->username) {
MEMERROR( params->utils );
return SASL_NOMEM;
}
strncpy(text->username, clientin, clientinlen);
text->username_len = clientinlen;
text->username[clientinlen] = '\0';
*serveroutlen = (unsigned) strlen(PASSWORD_CHALLENGE);
*serverout = PASSWORD_CHALLENGE;
text->state = 3;
return SASL_CONTINUE;
case 3: {
sasl_secret_t *password;
int result;
if (clientinlen > 1024) {
SETERROR(params->utils,
"clientinlen is > 1024 characters in LOGIN plugin");
return SASL_BADPROT;
}
password =
params->utils->malloc(sizeof(sasl_secret_t) + clientinlen + 1);
if (!password) {
MEMERROR(params->utils);
return SASL_NOMEM;
}
strncpy((char *) password->data, clientin, clientinlen);
password->data[clientinlen] = '\0';
password->len = clientinlen;
result = params->canon_user(params->utils->conn,
text->username,
text->username_len,
SASL_CU_AUTHID | SASL_CU_AUTHZID | SASL_CU_EXTERNALLY_VERIFIED,
oparams);
if (result != SASL_OK) return result;
result = params->utils->checkpass(params->utils->conn,
oparams->authid, oparams->alen,
(char *) password->data, password->len);
if (result != SASL_OK) {
_plug_free_secret(params->utils, &password);
return result;
}
_plug_free_secret(params->utils, &password);
*serverout = NULL;
*serveroutlen = 0;
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
default:
params->utils->log(NULL, SASL_LOG_ERR,
"Invalid LOGIN server step %d\n", text->state);
return SASL_FAIL;
}
return SASL_FAIL;
}
static void login_server_mech_dispose(void *conn_context,
const sasl_utils_t *utils)
{
server_context_t *text = (server_context_t *) conn_context;
if (!text) return;
if (text->username) utils->free(text->username);
utils->free(text);
}
static sasl_server_plug_t login_server_plugins[] =
{
{
"LOGIN",
0,
SASL_SEC_NOANONYMOUS
| SASL_SEC_PASS_CREDENTIALS,
0,
NULL,
&login_server_mech_new,
&login_server_mech_step,
&login_server_mech_dispose,
NULL,
NULL,
NULL,
NULL,
NULL,
NULL
}
};
int login_server_plug_init(sasl_utils_t *utils,
int maxversion,
int *out_version,
sasl_server_plug_t **pluglist,
int *plugcount)
{
if (maxversion < SASL_SERVER_PLUG_VERSION) {
SETERROR(utils, "LOGIN version mismatch");
return SASL_BADVERS;
}
*out_version = SASL_SERVER_PLUG_VERSION;
*pluglist = login_server_plugins;
*plugcount = 1;
return SASL_OK;
}
typedef struct client_context {
int state;
sasl_secret_t *password;
unsigned int free_password;
} client_context_t;
static int login_client_mech_new(void *glob_context __attribute__((unused)),
sasl_client_params_t *params,
void **conn_context)
{
client_context_t *text;
text = params->utils->malloc(sizeof(client_context_t));
if (text == NULL) {
MEMERROR(params->utils);
return SASL_NOMEM;
}
memset(text, 0, sizeof(client_context_t));
text->state = 1;
*conn_context = text;
return SASL_OK;
}
static int login_client_mech_step(void *conn_context,
sasl_client_params_t *params,
const char *serverin __attribute__((unused)),
unsigned serverinlen __attribute__((unused)),
sasl_interact_t **prompt_need,
const char **clientout,
unsigned *clientoutlen,
sasl_out_params_t *oparams)
{
client_context_t *text = (client_context_t *) conn_context;
*clientout = NULL;
*clientoutlen = 0;
switch (text->state) {
case 1: {
const char *user = NULL;
int auth_result = SASL_OK;
int pass_result = SASL_OK;
int result;
if (params->props.min_ssf > params->external_ssf) {
SETERROR( params->utils, "SSF requested of LOGIN plugin");
return SASL_TOOWEAK;
}
if (oparams->user == NULL) {
auth_result = _plug_get_authid(params->utils, &user, prompt_need);
if ((auth_result != SASL_OK) && (auth_result != SASL_INTERACT))
return auth_result;
}
if (text->password == NULL) {
pass_result = _plug_get_password(params->utils, &text->password,
&text->free_password, prompt_need);
if ((pass_result != SASL_OK) && (pass_result != SASL_INTERACT))
return pass_result;
}
if (prompt_need && *prompt_need) {
params->utils->free(*prompt_need);
*prompt_need = NULL;
}
if ((auth_result == SASL_INTERACT) || (pass_result == SASL_INTERACT)) {
result =
_plug_make_prompts(params->utils, prompt_need,
NULL, NULL,
auth_result == SASL_INTERACT ?
"Please enter your authentication name" : NULL,
NULL,
pass_result == SASL_INTERACT ?
"Please enter your password" : NULL, NULL,
NULL, NULL, NULL,
NULL, NULL, NULL);
if (result != SASL_OK) return result;
return SASL_INTERACT;
}
if (!text->password) {
PARAMERROR(params->utils);
return SASL_BADPARAM;
}
result = params->canon_user(params->utils->conn, user, 0,
SASL_CU_AUTHID | SASL_CU_AUTHZID, oparams);
if (result != SASL_OK) return result;
if (!serverin) {
SETERROR( params->utils,
"Server didn't issue challenge for USERNAME");
return SASL_BADPROT;
}
if (!clientout) {
PARAMERROR( params->utils );
return SASL_BADPARAM;
}
if (clientoutlen) *clientoutlen = oparams->alen;
*clientout = oparams->authid;
text->state = 2;
return SASL_CONTINUE;
}
case 2:
if (!serverin) {
SETERROR( params->utils,
"Server didn't issue challenge for PASSWORD");
return SASL_BADPROT;
}
if (!clientout) {
PARAMERROR(params->utils);
return SASL_BADPARAM;
}
if (clientoutlen) *clientoutlen = text->password->len;
*clientout = (char *) text->password->data;
oparams->doneflag = 1;
oparams->mech_ssf = 0;
oparams->maxoutbuf = 0;
oparams->encode_context = NULL;
oparams->encode = NULL;
oparams->decode_context = NULL;
oparams->decode = NULL;
oparams->param_version = 0;
return SASL_OK;
default:
params->utils->log(NULL, SASL_LOG_ERR,
"Invalid LOGIN client step %d\n", text->state);
return SASL_FAIL;
}
return SASL_FAIL;
}
static void login_client_mech_dispose(void *conn_context,
const sasl_utils_t *utils)
{
client_context_t *text = (client_context_t *) conn_context;
if (!text) return;
if (text->free_password) _plug_free_secret(utils, &(text->password));
utils->free(text);
}
static sasl_client_plug_t login_client_plugins[] =
{
{
"LOGIN",
0,
SASL_SEC_NOANONYMOUS
| SASL_SEC_PASS_CREDENTIALS,
SASL_FEAT_SERVER_FIRST,
NULL,
NULL,
&login_client_mech_new,
&login_client_mech_step,
&login_client_mech_dispose,
NULL,
NULL,
NULL,
NULL
}
};
int login_client_plug_init(sasl_utils_t *utils,
int maxversion,
int *out_version,
sasl_client_plug_t **pluglist,
int *plugcount)
{
if (maxversion < SASL_CLIENT_PLUG_VERSION) {
SETERROR(utils, "Version mismatch in LOGIN");
return SASL_BADVERS;
}
*out_version = SASL_CLIENT_PLUG_VERSION;
*pluglist = login_client_plugins;
*plugcount = 1;
return SASL_OK;
}