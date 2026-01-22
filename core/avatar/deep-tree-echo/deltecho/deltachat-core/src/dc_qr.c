#include <stdarg.h>
#include <unistd.h>
#include "dc_context.h"
#include "dc_apeerstate.h"
#define MAILTO_SCHEME "mailto:"
#define MATMSG_SCHEME "MATMSG:"
#define VCARD_BEGIN "BEGIN:VCARD"
#define SMTP_SCHEME "SMTP:"
dc_lot_t* dc_check_qr(dc_context_t* context, const char* qr)
{
char* payload = NULL;
char* addr = NULL;
char* fingerprint = NULL;
char* name = NULL;
char* invitenumber = NULL;
char* auth = NULL;
dc_apeerstate_t* peerstate = dc_apeerstate_new(context);
dc_lot_t* qr_parsed = dc_lot_new();
uint32_t chat_id = 0;
char* device_msg = NULL;
char* grpid = NULL;
char* grpname = NULL;
qr_parsed->state = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || qr==NULL) {
goto cleanup;
}
dc_log_info(context, 0, "Scanned QR code: %s", qr);
if (strncasecmp(qr, DC_OPENPGP4FPR_SCHEME, strlen(DC_OPENPGP4FPR_SCHEME))==0)
{
payload = dc_strdup(&qr[strlen(DC_OPENPGP4FPR_SCHEME)]);
char* fragment = strchr(payload, '#');
if (fragment)
{
*fragment = 0;
fragment++;
dc_param_t* param = dc_param_new();
dc_param_set_urlencoded(param, fragment);
addr = dc_param_get(param, 'a', NULL);
if (addr) {
char* urlencoded = dc_param_get(param, 'n', NULL);
if(urlencoded) {
name = dc_urldecode(urlencoded);
dc_normalize_name(name);
free(urlencoded);
}
invitenumber = dc_param_get(param, 'i', NULL);
auth = dc_param_get(param, 's', NULL);
grpid = dc_param_get(param, 'x', NULL);
if (grpid) {
urlencoded = dc_param_get(param, 'g', NULL);
if (urlencoded) {
grpname = dc_urldecode(urlencoded);
free(urlencoded);
}
}
}
dc_param_unref(param);
}
fingerprint = dc_normalize_fingerprint(payload);
}
else if (strncasecmp(qr, MAILTO_SCHEME, strlen(MAILTO_SCHEME))==0)
{
payload = dc_strdup(&qr[strlen(MAILTO_SCHEME)]);
char* query = strchr(payload, '?');
if (query) {
*query = 0;
}
addr = dc_strdup(payload);
}
else if (strncasecmp(qr, SMTP_SCHEME, strlen(SMTP_SCHEME))==0)
{
payload = dc_strdup(&qr[strlen(SMTP_SCHEME)]);
char* colon = strchr(payload, ':');
if (colon) {
*colon = 0;
}
addr = dc_strdup(payload);
}
else if (strncasecmp(qr, MATMSG_SCHEME, strlen(MATMSG_SCHEME))==0)
{
char* to = strstr(qr, "TO:");
if (to) {
addr = dc_strdup(&to[3]);
char* semicolon = strchr(addr, ';');
if (semicolon) { *semicolon = 0; }
}
else {
qr_parsed->state = DC_QR_ERROR;
qr_parsed->text1 = dc_strdup("Bad e-mail address.");
goto cleanup;
}
}
else if (strncasecmp(qr, VCARD_BEGIN, strlen(VCARD_BEGIN))==0)
{
carray* lines = dc_split_into_lines(qr);
for (int i = 0; i < carray_count(lines); i++) {
char* key = (char*)carray_get(lines, i); dc_trim(key);
char* value = strchr(key, ':');
if (value) {
*value = 0;
value++;
char* semicolon = strchr(key, ';'); if (semicolon) { *semicolon = 0; }
if (strcasecmp(key, "EMAIL")==0) {
semicolon = strchr(value, ';'); if (semicolon) { *semicolon = 0; }
addr = dc_strdup(value);
}
else if (strcasecmp(key, "N")==0) {
semicolon = strchr(value, ';'); if (semicolon) { semicolon = strchr(semicolon+1, ';'); if (semicolon) { *semicolon = 0; } }
name = dc_strdup(value);
dc_str_replace(&name, ";", ",");
dc_normalize_name(name);
}
}
}
dc_free_splitted_lines(lines);
}
if (addr) {
char* temp = dc_urldecode(addr); free(addr); addr = temp;
temp = dc_addr_normalize(addr); free(addr); addr = temp;
if (!dc_may_be_valid_addr(addr)) {
qr_parsed->state = DC_QR_ERROR;
qr_parsed->text1 = dc_strdup("Bad e-mail address.");
goto cleanup;
}
}
if (fingerprint) {
if (strlen(fingerprint) != 40) {
qr_parsed->state = DC_QR_ERROR;
qr_parsed->text1 = dc_strdup("Bad fingerprint length in QR code.");
goto cleanup;
}
}
if (fingerprint)
{
if (addr==NULL || invitenumber==NULL || auth==NULL)
{
if (dc_apeerstate_load_by_fingerprint(peerstate, context->sql, fingerprint)) {
qr_parsed->state = DC_QR_FPR_OK;
qr_parsed->id = dc_add_or_lookup_contact(context, NULL, peerstate->addr, DC_ORIGIN_UNHANDLED_QR_SCAN, NULL);
dc_create_or_lookup_nchat_by_contact_id(context, qr_parsed->id, DC_CHAT_DEADDROP_BLOCKED, &chat_id, NULL);
device_msg = dc_mprintf("%s verified.", peerstate->addr);
}
else {
qr_parsed->text1 = dc_format_fingerprint(fingerprint);
qr_parsed->state = DC_QR_FPR_WITHOUT_ADDR;
}
}
else
{
if (grpid && grpname) {
qr_parsed->state = DC_QR_ASK_VERIFYGROUP;
qr_parsed->text1 = dc_strdup(grpname);
qr_parsed->text2 = dc_strdup(grpid);
}
else {
qr_parsed->state = DC_QR_ASK_VERIFYCONTACT;
}
qr_parsed->id = dc_add_or_lookup_contact(context, name, addr, DC_ORIGIN_UNHANDLED_QR_SCAN, NULL);
qr_parsed->fingerprint = dc_strdup(fingerprint);
qr_parsed->invitenumber = dc_strdup(invitenumber);
qr_parsed->auth = dc_strdup(auth);
}
}
else if (addr)
{
qr_parsed->state = DC_QR_ADDR;
qr_parsed->id = dc_add_or_lookup_contact(context, name, addr, DC_ORIGIN_UNHANDLED_QR_SCAN, NULL);
}
else if (strstr(qr, "http:
{
qr_parsed->state = DC_QR_URL;
qr_parsed->text1 = dc_strdup(qr);
}
else
{
qr_parsed->state = DC_QR_TEXT;
qr_parsed->text1 = dc_strdup(qr);
}
if (device_msg) {
dc_add_device_msg(context, chat_id, device_msg);
}
cleanup:
free(addr);
free(fingerprint);
dc_apeerstate_unref(peerstate);
free(payload);
free(name);
free(invitenumber);
free(auth);
free(device_msg);
free(grpname);
free(grpid);
return qr_parsed;
}