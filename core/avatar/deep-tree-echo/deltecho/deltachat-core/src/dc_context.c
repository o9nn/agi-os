#include <sys/stat.h>
#include <sys/types.h>
#include <locale.h>
#include <unistd.h>
#include <openssl/opensslv.h>
#include <assert.h>
#include "dc_context.h"
#include "dc_imap.h"
#include "dc_smtp.h"
#include "dc_openssl.h"
#include "dc_mimefactory.h"
#include "dc_tools.h"
#include "dc_job.h"
#include "dc_key.h"
#include "dc_pgp.h"
#include "dc_apeerstate.h"
static const char* config_keys[] = {
"addr"
,"mail_server"
,"mail_user"
,"mail_pw"
,"mail_port"
,"send_server"
,"send_user"
,"send_pw"
,"send_port"
,"server_flags"
,"imap_folder"
,"displayname"
,"selfstatus"
,"selfavatar"
,"e2ee_enabled"
,"mdns_enabled"
,"inbox_watch"
,"sentbox_watch"
,"mvbox_watch"
,"mvbox_move"
,"show_emails"
,"save_mime_headers"
,"configured_addr"
,"configured_mail_server"
,"configured_mail_user"
,"configured_mail_pw"
,"configured_mail_port"
,"configured_send_server"
,"configured_send_user"
,"configured_send_pw"
,"configured_send_port"
,"configured_server_flags"
,"configured"
};
static const char* sys_config_keys[] = {
"sys.version"
,"sys.msgsize_max_recommended"
,"sys.config_keys"
};
#define str_array_len(a) (sizeof(a)/sizeof(char*))
static uintptr_t cb_dummy(dc_context_t* context, int event, uintptr_t data1, uintptr_t data2)
{
return 0;
}
static char* cb_get_config(dc_imap_t* imap, const char* key, const char* def)
{
dc_context_t* context = (dc_context_t*)imap->userData;
return dc_sqlite3_get_config(context->sql, key, def);
}
static void cb_set_config(dc_imap_t* imap, const char* key, const char* value)
{
dc_context_t* context = (dc_context_t*)imap->userData;
dc_sqlite3_set_config(context->sql, key, value);
}
static int cb_precheck_imf(dc_imap_t* imap, const char* rfc724_mid,
const char* server_folder, uint32_t server_uid)
{
int rfc724_mid_exists = 0;
uint32_t msg_id = 0;
char* old_server_folder = NULL;
uint32_t old_server_uid = 0;
int mark_seen = 0;
msg_id = dc_rfc724_mid_exists(imap->context, rfc724_mid,
&old_server_folder, &old_server_uid);
if (msg_id!=0)
{
rfc724_mid_exists = 1;
if (old_server_folder[0]==0 && old_server_uid==0) {
dc_log_info(imap->context, 0, "[move] detected bbc-self %s", rfc724_mid);
mark_seen = 1;
}
else if (strcmp(old_server_folder, server_folder)!=0) {
dc_log_info(imap->context, 0, "[move] detected moved message %s", rfc724_mid);
dc_update_msg_move_state(imap->context, rfc724_mid, DC_MOVE_STATE_STAY);
}
if (strcmp(old_server_folder, server_folder)!=0
|| old_server_uid!=server_uid) {
dc_update_server_uid(imap->context, rfc724_mid, server_folder, server_uid);
}
dc_do_heuristics_moves(imap->context, server_folder, msg_id);
if (mark_seen) {
dc_job_add(imap->context, DC_JOB_MARKSEEN_MSG_ON_IMAP, msg_id, NULL, 0);
}
}
free(old_server_folder);
return rfc724_mid_exists;
}
static void cb_receive_imf(dc_imap_t* imap, const char* imf_raw_not_terminated, size_t imf_raw_bytes, const char* server_folder, uint32_t server_uid, uint32_t flags)
{
dc_context_t* context = (dc_context_t*)imap->userData;
dc_receive_imf(context, imf_raw_not_terminated, imf_raw_bytes, server_folder, server_uid, flags);
}
dc_context_t* dc_context_new(dc_callback_t cb, void* userdata, const char* os_name)
{
dc_context_t* context = NULL;
if ((context=calloc(1, sizeof(dc_context_t)))==NULL) {
exit(23);
}
pthread_mutex_init(&context->smear_critical, NULL);
pthread_mutex_init(&context->bobs_qr_critical, NULL);
pthread_mutex_init(&context->inboxidle_condmutex, NULL);
dc_jobthread_init(&context->sentbox_thread, context, "SENTBOX", "configured_sentbox_folder");
dc_jobthread_init(&context->mvbox_thread, context, "MVBOX", "configured_mvbox_folder");
pthread_mutex_init(&context->smtpidle_condmutex, NULL);
pthread_cond_init(&context->smtpidle_cond, NULL);
pthread_mutex_init(&context->oauth2_critical, NULL);
context->magic = DC_CONTEXT_MAGIC;
context->userdata = userdata;
context->cb = cb? cb : cb_dummy;
context->os_name = dc_strdup_keep_null(os_name);
context->shall_stop_ongoing = 1;
dc_openssl_init();
dc_pgp_init();
context->sql = dc_sqlite3_new(context);
dc_job_kill_action(context, DC_JOB_EMPTY_SERVER);
context->inbox = dc_imap_new(cb_get_config, cb_set_config, cb_precheck_imf, cb_receive_imf, (void*)context, context);
context->sentbox_thread.imap = dc_imap_new(cb_get_config, cb_set_config, cb_precheck_imf, cb_receive_imf, (void*)context, context);
context->mvbox_thread.imap = dc_imap_new(cb_get_config, cb_set_config, cb_precheck_imf, cb_receive_imf, (void*)context, context);
context->smtp = dc_smtp_new(context);
uintptr_t seed[5];
seed[0] = (uintptr_t)time(NULL);
seed[1] = (uintptr_t)seed;
seed[2] = (uintptr_t)context;
seed[3] = (uintptr_t)pthread_self();
seed[4] = (uintptr_t)getpid();
dc_pgp_rand_seed(context, seed, sizeof(seed));
return context;
}
void dc_context_unref(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
dc_pgp_exit();
if (dc_is_open(context)) {
dc_close(context);
}
dc_imap_unref(context->inbox);
dc_imap_unref(context->sentbox_thread.imap);
dc_imap_unref(context->mvbox_thread.imap);
dc_smtp_unref(context->smtp);
dc_sqlite3_unref(context->sql);
dc_openssl_exit();
pthread_mutex_destroy(&context->smear_critical);
pthread_mutex_destroy(&context->bobs_qr_critical);
pthread_mutex_destroy(&context->inboxidle_condmutex);
dc_jobthread_exit(&context->sentbox_thread);
dc_jobthread_exit(&context->mvbox_thread);
pthread_cond_destroy(&context->smtpidle_cond);
pthread_mutex_destroy(&context->smtpidle_condmutex);
pthread_mutex_destroy(&context->oauth2_critical);
free(context->os_name);
context->magic = 0;
free(context);
}
void* dc_get_userdata(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return NULL;
}
return context->userdata;
}
int dc_open(dc_context_t* context, const char* dbfile, const char* blobdir)
{
int success = 0;
if (dc_is_open(context)) {
return 0;
}
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || dbfile==NULL) {
goto cleanup;
}
context->dbfile = dc_strdup(dbfile);
if (blobdir && blobdir[0]) {
context->blobdir = dc_strdup(blobdir);
dc_ensure_no_slash(context->blobdir);
}
else {
context->blobdir = dc_mprintf("%s-blobs", dbfile);
dc_create_folder(context, context->blobdir);
}
if (!dc_sqlite3_open(context->sql, dbfile, 0)) {
goto cleanup;
}
success = 1;
cleanup:
if (!success) {
dc_close(context);
}
return success;
}
void dc_close(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
dc_imap_disconnect(context->inbox);
dc_imap_disconnect(context->sentbox_thread.imap);
dc_imap_disconnect(context->mvbox_thread.imap);
dc_smtp_disconnect(context->smtp);
if (dc_sqlite3_is_open(context->sql)) {
dc_sqlite3_close(context->sql);
}
free(context->dbfile);
context->dbfile = NULL;
free(context->blobdir);
context->blobdir = NULL;
}
int dc_is_open(const dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return 0;
}
return dc_sqlite3_is_open(context->sql);
}
char* dc_get_blobdir(const dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return dc_strdup(NULL);
}
return dc_strdup(context->blobdir);
}
static int is_settable_config_key(const char* key)
{
for (int i = 0; i < str_array_len(config_keys); i++) {
if (strcmp(key, config_keys[i]) == 0) {
return 1;
}
}
return 0;
}
static int is_gettable_config_key(const char* key)
{
for (int i = 0; i < str_array_len(sys_config_keys); i++) {
if (strcmp(key, sys_config_keys[i]) == 0) {
return 1;
}
}
return is_settable_config_key(key);
}
static char* get_config_keys_str()
{
dc_strbuilder_t ret;
dc_strbuilder_init(&ret, 0);
for (int i = 0; i < str_array_len(config_keys); i++) {
if (strlen(ret.buf) > 0) {
dc_strbuilder_cat(&ret, " ");
}
dc_strbuilder_cat(&ret, config_keys[i]);
}
for (int i = 0; i < str_array_len(sys_config_keys); i++) {
if (strlen(ret.buf) > 0) {
dc_strbuilder_cat(&ret, " ");
}
dc_strbuilder_cat(&ret, sys_config_keys[i]);
}
return ret.buf;
}
static char* get_sys_config_str(const char* key)
{
if (strcmp(key, "sys.version")==0)
{
return dc_strdup(DC_VERSION_STR);
}
else if (strcmp(key, "sys.msgsize_max_recommended")==0)
{
return dc_mprintf("%i", DC_MSGSIZE_MAX_RECOMMENDED);
}
else if (strcmp(key, "sys.config_keys")==0)
{
return get_config_keys_str();
}
else {
return dc_strdup(NULL);
}
}
int dc_set_config(dc_context_t* context, const char* key, const char* value)
{
int ret = 0;
char* rel_path = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || key==NULL || !is_settable_config_key(key)) {
return 0;
}
if (strcmp(key, "selfavatar")==0 && value)
{
rel_path = dc_strdup(value);
if (!dc_make_rel_and_copy(context, &rel_path)) {
goto cleanup;
}
ret = dc_sqlite3_set_config(context->sql, key, rel_path);
}
else if(strcmp(key, "inbox_watch")==0)
{
ret = dc_sqlite3_set_config(context->sql, key, value);
dc_interrupt_imap_idle(context);
}
else if(strcmp(key, "sentbox_watch")==0)
{
ret = dc_sqlite3_set_config(context->sql, key, value);
dc_interrupt_sentbox_idle(context);
}
else if(strcmp(key, "mvbox_watch")==0)
{
ret = dc_sqlite3_set_config(context->sql, key, value);
dc_interrupt_mvbox_idle(context);
}
else if (strcmp(key, "selfstatus")==0) {
char* def = dc_stock_str(context, DC_STR_STATUSLINE);
ret = dc_sqlite3_set_config(context->sql, key,
(value==NULL || strcmp(value, def)==0)? NULL : value);
free(def);
}
else
{
ret = dc_sqlite3_set_config(context->sql, key, value);
}
cleanup:
free(rel_path);
return ret;
}
char* dc_get_config(dc_context_t* context, const char* key)
{
char* value = NULL;
if (key && key[0]=='s' && key[1]=='y' && key[2]=='s' && key[3]=='.') {
return get_sys_config_str(key);
}
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || key==NULL || !is_gettable_config_key(key)) {
return dc_strdup("");
}
if (strcmp(key, "selfavatar")==0) {
char* rel_path = dc_sqlite3_get_config(context->sql, key, NULL);
if (rel_path) {
value = dc_get_abs_path(context, rel_path);
free(rel_path);
}
}
else {
value = dc_sqlite3_get_config(context->sql, key, NULL);
}
if (value==NULL)
{
if (strcmp(key, "e2ee_enabled")==0) {
value = dc_mprintf("%i", DC_E2EE_DEFAULT_ENABLED);
}
else if (strcmp(key, "mdns_enabled")==0) {
value = dc_mprintf("%i", DC_MDNS_DEFAULT_ENABLED);
}
else if (strcmp(key, "imap_folder")==0) {
value = dc_strdup("INBOX");
}
else if (strcmp(key, "inbox_watch")==0) {
value = dc_mprintf("%i", DC_INBOX_WATCH_DEFAULT);
}
else if (strcmp(key, "sentbox_watch")==0) {
value = dc_mprintf("%i", DC_SENTBOX_WATCH_DEFAULT);
}
else if (strcmp(key, "mvbox_watch")==0) {
value = dc_mprintf("%i", DC_MVBOX_WATCH_DEFAULT);
}
else if (strcmp(key, "mvbox_move")==0) {
value = dc_mprintf("%i", DC_MVBOX_MOVE_DEFAULT);
}
else if (strcmp(key, "show_emails")==0) {
value = dc_mprintf("%i", DC_SHOW_EMAILS_DEFAULT);
}
else if (strcmp(key, "selfstatus")==0) {
value = dc_stock_str(context, DC_STR_STATUSLINE);
}
else {
value = dc_mprintf("");
}
}
return value;
}
int dc_is_inbox(dc_context_t* context, const char* folder_name)
{
int is_inbox = 0;
if (folder_name) {
is_inbox = strcasecmp("INBOX", folder_name)==0? 1 : 0;
}
return is_inbox;
}
int dc_is_sentbox(dc_context_t* context, const char* folder_name)
{
char* sentbox_name = dc_sqlite3_get_config(context->sql, "configured_sentbox_folder", NULL);
int is_sentbox = 0;
if (sentbox_name && folder_name) {
is_sentbox = strcasecmp(sentbox_name, folder_name)==0? 1 : 0;
}
free(sentbox_name);
return is_sentbox;
}
int dc_is_mvbox(dc_context_t* context, const char* folder_name)
{
char* mvbox_name = dc_sqlite3_get_config(context->sql, "configured_mvbox_folder", NULL);
int is_mvbox = 0;
if (mvbox_name && folder_name) {
is_mvbox = strcasecmp(mvbox_name, folder_name)==0? 1 : 0;
}
free(mvbox_name);
return is_mvbox;
}
char* dc_get_version_str(void)
{
return dc_strdup(DC_VERSION_STR);
}
char* dc_get_info(dc_context_t* context)
{
const char* unset = "0";
char* displayname = NULL;
char* temp = NULL;
char* l_readable_str = NULL;
char* l2_readable_str = NULL;
char* fingerprint_str = NULL;
dc_loginparam_t* l = NULL;
dc_loginparam_t* l2 = NULL;
int inbox_watch = 0;
int sentbox_watch = 0;
int mvbox_watch = 0;
int mvbox_move = 0;
int folders_configured = 0;
char* configured_sentbox_folder = NULL;
char* configured_mvbox_folder = NULL;
int contacts = 0;
int chats = 0;
int real_msgs = 0;
int deaddrop_msgs = 0;
int is_configured = 0;
int dbversion = 0;
int show_emails = 0;
int mdns_enabled = 0;
int e2ee_enabled = 0;
int prv_key_cnt = 0;
int pub_key_cnt = 0;
dc_key_t* self_public = dc_key_new();
int rpgp_enabled = 0;
#ifdef DC_USE_RPGP
rpgp_enabled = 1;
#endif
dc_strbuilder_t ret;
dc_strbuilder_init(&ret, 0);
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return dc_strdup("ErrBadPtr");
}
l = dc_loginparam_new();
l2 = dc_loginparam_new();
dc_loginparam_read(l, context->sql, "");
dc_loginparam_read(l2, context->sql, "configured_" );
displayname = dc_sqlite3_get_config(context->sql, "displayname", NULL);
chats = dc_get_chat_cnt(context);
real_msgs = dc_get_real_msg_cnt(context);
deaddrop_msgs = dc_get_deaddrop_msg_cnt(context);
contacts = dc_get_real_contact_cnt(context);
is_configured = dc_sqlite3_get_config_int(context->sql, "configured", 0);
dbversion = dc_sqlite3_get_config_int(context->sql, "dbversion", 0);
e2ee_enabled = dc_sqlite3_get_config_int(context->sql, "e2ee_enabled", DC_E2EE_DEFAULT_ENABLED);
show_emails = dc_sqlite3_get_config_int(context->sql, "show_emails", DC_SHOW_EMAILS_DEFAULT);
mdns_enabled = dc_sqlite3_get_config_int(context->sql, "mdns_enabled", DC_MDNS_DEFAULT_ENABLED);
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql, "SELECT COUNT(*) FROM keypairs;");
sqlite3_step(stmt);
prv_key_cnt = sqlite3_column_int(stmt, 0);
sqlite3_finalize(stmt);
stmt = dc_sqlite3_prepare(context->sql, "SELECT COUNT(*) FROM acpeerstates;");
sqlite3_step(stmt);
pub_key_cnt = sqlite3_column_int(stmt, 0);
sqlite3_finalize(stmt);
if (dc_key_load_self_public(self_public, l2->addr, context->sql)) {
fingerprint_str = dc_key_get_fingerprint(self_public);
}
else {
fingerprint_str = dc_strdup("<Not yet calculated>");
}
l_readable_str = dc_loginparam_get_readable(l);
l2_readable_str = dc_loginparam_get_readable(l2);
inbox_watch = dc_sqlite3_get_config_int(context->sql, "inbox_watch", DC_INBOX_WATCH_DEFAULT);
sentbox_watch = dc_sqlite3_get_config_int(context->sql, "sentbox_watch", DC_SENTBOX_WATCH_DEFAULT);
mvbox_watch = dc_sqlite3_get_config_int(context->sql, "mvbox_watch", DC_MVBOX_WATCH_DEFAULT);
mvbox_move = dc_sqlite3_get_config_int(context->sql, "mvbox_move", DC_MVBOX_MOVE_DEFAULT);
folders_configured = dc_sqlite3_get_config_int(context->sql, "folders_configured", 0);
configured_sentbox_folder = dc_sqlite3_get_config(context->sql, "configured_sentbox_folder", "<unset>");
configured_mvbox_folder = dc_sqlite3_get_config(context->sql, "configured_mvbox_folder", "<unset>");
temp = dc_mprintf(
"deltachat_core_version=v%s\n"
"sqlite_version=%s\n"
"sqlite_thread_safe=%i\n"
"libetpan_version=%i.%i\n"
"openssl_version=%i.%i.%i%c\n"
"rpgp_enabled=%i\n"
"compile_date=" __DATE__ ", " __TIME__ "\n"
"arch=%i\n"
"number_of_chats=%i\n"
"number_of_chat_messages=%i\n"
"messages_in_contact_requests=%i\n"
"number_of_contacts=%i\n"
"database_dir=%s\n"
"database_version=%i\n"
"blobdir=%s\n"
"display_name=%s\n"
"is_configured=%i\n"
"entered_account_settings=%s\n"
"used_account_settings=%s\n"
"inbox_watch=%i\n"
"sentbox_watch=%i\n"
"mvbox_watch=%i\n"
"mvbox_move=%i\n"
"folders_configured=%i\n"
"configured_sentbox_folder=%s\n"
"configured_mvbox_folder=%s\n"
"show_emails=%i\n"
"mdns_enabled=%i\n"
"e2ee_enabled=%i\n"
"private_key_count=%i\n"
"public_key_count=%i\n"
"fingerprint=%s\n"
, DC_VERSION_STR
, SQLITE_VERSION
, sqlite3_threadsafe()
, libetpan_get_version_major(), libetpan_get_version_minor()
, (int)(OPENSSL_VERSION_NUMBER>>28), (int)(OPENSSL_VERSION_NUMBER>>20)&0xFF, (int)(OPENSSL_VERSION_NUMBER>>12)&0xFF, (char)('a'-1+((OPENSSL_VERSION_NUMBER>>4)&0xFF))
, rpgp_enabled
, sizeof(void*)*8
, chats
, real_msgs
, deaddrop_msgs
, contacts
, context->dbfile? context->dbfile : unset
, dbversion
, context->blobdir? context->blobdir : unset
, displayname? displayname : unset
, is_configured
, l_readable_str
, l2_readable_str
, inbox_watch
, sentbox_watch
, mvbox_watch
, mvbox_move
, folders_configured
, configured_sentbox_folder
, configured_mvbox_folder
, show_emails
, mdns_enabled
, e2ee_enabled
, prv_key_cnt
, pub_key_cnt
, fingerprint_str
);
dc_strbuilder_cat(&ret, temp);
free(temp);
dc_loginparam_unref(l);
dc_loginparam_unref(l2);
free(displayname);
free(l_readable_str);
free(l2_readable_str);
free(configured_sentbox_folder);
free(configured_mvbox_folder);
free(fingerprint_str);
dc_key_unref(self_public);
return ret.buf;
}
dc_array_t* dc_get_fresh_msgs(dc_context_t* context)
{
int show_deaddrop = 0;
dc_array_t* ret = dc_array_new(context, 128);
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || ret==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT m.id"
" FROM msgs m"
" LEFT JOIN contacts ct ON m.from_id=ct.id"
" LEFT JOIN chats c ON m.chat_id=c.id"
" WHERE m.state=?"
"   AND m.hidden=0"
"   AND m.chat_id>?"
"   AND ct.blocked=0"
"   AND (c.blocked=0 OR c.blocked=?)"
" ORDER BY m.timestamp DESC,m.id DESC;");
sqlite3_bind_int(stmt, 1, DC_STATE_IN_FRESH);
sqlite3_bind_int(stmt, 2, DC_CHAT_ID_LAST_SPECIAL);
sqlite3_bind_int(stmt, 3, show_deaddrop? DC_CHAT_DEADDROP_BLOCKED : 0);
while (sqlite3_step(stmt)==SQLITE_ROW) {
dc_array_add_id(ret, sqlite3_column_int(stmt, 0));
}
cleanup:
sqlite3_finalize(stmt);
return ret;
}
dc_array_t* dc_search_msgs(dc_context_t* context, uint32_t chat_id, const char* query)
{
int success = 0;
dc_array_t* ret = dc_array_new(context, 100);
char* strLikeInText = NULL;
char* strLikeBeg = NULL;
char* real_query = NULL;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || ret==NULL || query==NULL) {
goto cleanup;
}
real_query = dc_strdup(query);
dc_trim(real_query);
if (real_query[0]==0) {
success = 1;
goto cleanup;
}
strLikeInText = dc_mprintf("%%%s%%", real_query);
strLikeBeg = dc_mprintf("%s%%", real_query);
if (chat_id) {
stmt = dc_sqlite3_prepare(context->sql,
"SELECT m.id, m.timestamp FROM msgs m"
" LEFT JOIN contacts ct ON m.from_id=ct.id"
" WHERE m.chat_id=? "
" AND m.hidden=0 "
" AND ct.blocked=0 AND (txt LIKE ? OR ct.name LIKE ?)"
" ORDER BY m.timestamp,m.id;");
sqlite3_bind_int (stmt, 1, chat_id);
sqlite3_bind_text(stmt, 2, strLikeInText, -1, SQLITE_STATIC);
sqlite3_bind_text(stmt, 3, strLikeBeg, -1, SQLITE_STATIC);
}
else {
int show_deaddrop = 0;
stmt = dc_sqlite3_prepare(context->sql,
"SELECT m.id, m.timestamp FROM msgs m"
" LEFT JOIN contacts ct ON m.from_id=ct.id"
" LEFT JOIN chats c ON m.chat_id=c.id"
" WHERE m.chat_id>" DC_STRINGIFY(DC_CHAT_ID_LAST_SPECIAL)
" AND m.hidden=0 "
" AND (c.blocked=0 OR c.blocked=?)"
" AND ct.blocked=0 AND (m.txt LIKE ? OR ct.name LIKE ?)"
" ORDER BY m.timestamp DESC,m.id DESC;");
sqlite3_bind_int (stmt, 1, show_deaddrop? DC_CHAT_DEADDROP_BLOCKED : 0);
sqlite3_bind_text(stmt, 2, strLikeInText, -1, SQLITE_STATIC);
sqlite3_bind_text(stmt, 3, strLikeBeg, -1, SQLITE_STATIC);
}
while (sqlite3_step(stmt)==SQLITE_ROW) {
dc_array_add_id(ret, sqlite3_column_int(stmt, 0));
}
success = 1;
cleanup:
free(strLikeInText);
free(strLikeBeg);
free(real_query);
sqlite3_finalize(stmt);
if (success) {
return ret;
}
else {
if (ret) {
dc_array_unref(ret);
}
return NULL;
}
}