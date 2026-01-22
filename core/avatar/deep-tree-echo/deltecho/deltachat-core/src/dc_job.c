#include <stdarg.h>
#include <unistd.h>
#include <math.h>
#include "dc_context.h"
#include "dc_loginparam.h"
#include "dc_job.h"
#include "dc_imap.h"
#include "dc_smtp.h"
#include "dc_mimefactory.h"
static void dc_send_mdn(dc_context_t* context, uint32_t msg_id);
static int connect_to_inbox(dc_context_t* context)
{
int ret_connected = DC_NOT_CONNECTED;
ret_connected = dc_connect_to_configured_imap(context, context->inbox);
if (!ret_connected) {
goto cleanup;
}
dc_imap_set_watch_folder(context->inbox, "INBOX");
cleanup:
return ret_connected;
}
static void dc_job_do_DC_JOB_DELETE_MSG_ON_IMAP(dc_context_t* context, dc_job_t* job)
{
int delete_from_server = 1;
dc_msg_t* msg = dc_msg_new_untyped(context);
if (!dc_msg_load_from_db(msg, context, job->foreign_id)
|| msg->rfc724_mid==NULL || msg->rfc724_mid[0]==0 ) {
goto cleanup;
}
if (dc_rfc724_mid_cnt(context, msg->rfc724_mid)!=1) {
dc_log_info(context, 0, "The message is deleted from the server when all parts are deleted.");
delete_from_server = 0;
}
if (delete_from_server)
{
if (!dc_imap_is_connected(context->inbox)) {
connect_to_inbox(context);
if (!dc_imap_is_connected(context->inbox)) {
dc_job_try_again_later(job, DC_STANDARD_DELAY, NULL);
goto cleanup;
}
}
if (!dc_imap_delete_msg(context->inbox, msg->rfc724_mid, msg->server_folder, msg->server_uid))
{
dc_job_try_again_later(job, DC_AT_ONCE, NULL);
goto cleanup;
}
}
dc_delete_msg_from_db(context, msg->id);
cleanup:
dc_msg_unref(msg);
}
static void dc_job_do_DC_JOB_EMPTY_SERVER(dc_context_t* context, dc_job_t* job)
{
char* mvbox_name = NULL;
if (!dc_imap_is_connected(context->inbox)) {
connect_to_inbox(context);
if (!dc_imap_is_connected(context->inbox)) {
goto cleanup;
}
}
if (job->foreign_id&DC_EMPTY_MVBOX) {
char* mvbox_name = dc_sqlite3_get_config(context->sql, "configured_mvbox_folder", NULL);
if (mvbox_name && mvbox_name[0]) {
dc_imap_empty_folder(context->inbox, mvbox_name);
}
}
if (job->foreign_id&DC_EMPTY_INBOX) {
dc_imap_empty_folder(context->inbox, "INBOX");
}
cleanup:
free(mvbox_name);
}
static void dc_job_do_DC_JOB_MOVE_MSG(dc_context_t* context, dc_job_t* job)
{
dc_msg_t* msg = dc_msg_new_untyped(context);
char* dest_folder = NULL;
uint32_t dest_uid = 0;
if (!dc_imap_is_connected(context->inbox)) {
connect_to_inbox(context);
if (!dc_imap_is_connected(context->inbox)) {
dc_job_try_again_later(job, DC_STANDARD_DELAY, NULL);
goto cleanup;
}
}
if (!dc_msg_load_from_db(msg, context, job->foreign_id)) {
goto cleanup;
}
if (dc_sqlite3_get_config_int(context->sql, "folders_configured", 0)<DC_FOLDERS_CONFIGURED_VERSION) {
dc_configure_folders(context, context->inbox, DC_CREATE_MVBOX);
}
dest_folder = dc_sqlite3_get_config(context->sql, "configured_mvbox_folder", NULL);
switch (dc_imap_move(context->inbox, msg->server_folder, msg->server_uid, dest_folder, &dest_uid)) {
case DC_FAILED: goto cleanup;
case DC_RETRY_LATER: dc_job_try_again_later(job, DC_STANDARD_DELAY, NULL); break;
case DC_ALREADY_DONE: break;
case DC_SUCCESS: dc_update_server_uid(context, msg->rfc724_mid, dest_folder, dest_uid); break;
}
cleanup:
free(dest_folder);
dc_msg_unref(msg);
}
static void dc_job_do_DC_JOB_MARKSEEN_MSG_ON_IMAP(dc_context_t* context, dc_job_t* job)
{
dc_msg_t* msg = dc_msg_new_untyped(context);
if (!dc_imap_is_connected(context->inbox)) {
connect_to_inbox(context);
if (!dc_imap_is_connected(context->inbox)) {
dc_job_try_again_later(job, DC_STANDARD_DELAY, NULL);
goto cleanup;
}
}
if (!dc_msg_load_from_db(msg, context, job->foreign_id)) {
goto cleanup;
}
switch (dc_imap_set_seen(context->inbox, msg->server_folder, msg->server_uid)) {
case DC_FAILED: goto cleanup;
case DC_RETRY_LATER: dc_job_try_again_later(job, DC_STANDARD_DELAY, NULL); goto cleanup;
default: break;
}
if (dc_param_get_int(msg->param, DC_PARAM_WANTS_MDN, 0)
&& dc_sqlite3_get_config_int(context->sql, "mdns_enabled", DC_MDNS_DEFAULT_ENABLED))
{
switch (dc_imap_set_mdnsent(context->inbox, msg->server_folder, msg->server_uid)) {
case DC_FAILED: goto cleanup;
case DC_RETRY_LATER: dc_job_try_again_later(job, DC_STANDARD_DELAY, NULL); goto cleanup;
case DC_ALREADY_DONE: break;
case DC_SUCCESS: dc_send_mdn(context, msg->id); break;
}
}
cleanup:
dc_msg_unref(msg);
}
static void dc_job_do_DC_JOB_MARKSEEN_MDN_ON_IMAP(dc_context_t* context, dc_job_t* job)
{
char* folder = dc_param_get(job->param, DC_PARAM_SERVER_FOLDER, NULL);
uint32_t uid = dc_param_get_int(job->param, DC_PARAM_SERVER_UID, 0);
char* dest_folder = NULL;
uint32_t dest_uid = 0;
if (!dc_imap_is_connected(context->inbox)) {
connect_to_inbox(context);
if (!dc_imap_is_connected(context->inbox)) {
dc_job_try_again_later(job, DC_STANDARD_DELAY, NULL);
goto cleanup;
}
}
if (dc_imap_set_seen(context->inbox, folder, uid)==0) {
dc_job_try_again_later(job, DC_STANDARD_DELAY, NULL);
}
if (dc_param_get_int(job->param, DC_PARAM_ALSO_MOVE, 0))
{
if (dc_sqlite3_get_config_int(context->sql, "folders_configured", 0)<DC_FOLDERS_CONFIGURED_VERSION) {
dc_configure_folders(context, context->inbox, DC_CREATE_MVBOX);
}
dest_folder = dc_sqlite3_get_config(context->sql, "configured_mvbox_folder", NULL);
switch (dc_imap_move(context->inbox, folder, uid, dest_folder, &dest_uid)) {
case DC_FAILED: goto cleanup;
case DC_RETRY_LATER: dc_job_try_again_later(job, DC_STANDARD_DELAY, NULL); break;
default: break;
}
}
cleanup:
free(folder);
free(dest_folder);
}
static int dc_add_smtp_job(dc_context_t* context, int action, dc_mimefactory_t* mimefactory)
{
char* pathNfilename = NULL;
int success = 0;
char* recipients = NULL;
dc_param_t* param = dc_param_new();
pathNfilename = dc_get_fine_pathNfilename(context, "$BLOBDIR", mimefactory->rfc724_mid);
if (!pathNfilename) {
dc_log_error(context, 0, "Could not find free file name for message with ID <%s>.", mimefactory->rfc724_mid);
goto cleanup;
}
if (!dc_write_file(context, pathNfilename, mimefactory->out->str, mimefactory->out->len)) {
dc_log_error(context, 0, "Could not write message <%s> to \"%s\".", mimefactory->rfc724_mid, pathNfilename);
goto cleanup;
}
recipients = dc_str_from_clist(mimefactory->recipients_addr, "\x1e");
dc_param_set(param, DC_PARAM_FILE, pathNfilename);
dc_param_set(param, DC_PARAM_RECIPIENTS, recipients);
dc_job_add(context, action, mimefactory->loaded==DC_MF_MSG_LOADED ? mimefactory->msg->id : 0, param->packed, 0);
success = 1;
cleanup:
dc_param_unref(param);
free(recipients);
free(pathNfilename);
return success;
}
int dc_job_send_msg(dc_context_t* context, uint32_t msg_id)
{
int success = 0;
dc_mimefactory_t mimefactory;
dc_mimefactory_init(&mimefactory, context);
if (!dc_mimefactory_load_msg(&mimefactory, msg_id)
|| mimefactory.from_addr==NULL) {
dc_log_warning(context, 0, "Cannot load data to send, maybe the message is deleted in between.");
goto cleanup;
}
if (DC_MSG_NEEDS_ATTACHMENT(mimefactory.msg->type)) {
char* pathNfilename = dc_param_get(mimefactory.msg->param, DC_PARAM_FILE, NULL);
if (pathNfilename) {
if ((mimefactory.msg->type==DC_MSG_IMAGE || mimefactory.msg->type==DC_MSG_GIF)
&& !dc_param_exists(mimefactory.msg->param, DC_PARAM_WIDTH)) {
unsigned char* buf = NULL; size_t buf_bytes; uint32_t w, h;
dc_param_set_int(mimefactory.msg->param, DC_PARAM_WIDTH, 0);
dc_param_set_int(mimefactory.msg->param, DC_PARAM_HEIGHT, 0);
if (dc_read_file(context, pathNfilename, (void**)&buf, &buf_bytes)) {
if (dc_get_filemeta(buf, buf_bytes, &w, &h)) {
dc_param_set_int(mimefactory.msg->param, DC_PARAM_WIDTH, w);
dc_param_set_int(mimefactory.msg->param, DC_PARAM_HEIGHT, h);
}
}
free(buf);
dc_msg_save_param_to_disk(mimefactory.msg);
}
}
free(pathNfilename);
}
{
if (!dc_mimefactory_render(&mimefactory)) {
dc_set_msg_failed(context, msg_id, mimefactory.error);
goto cleanup;
}
if (dc_param_get_int(mimefactory.msg->param, DC_PARAM_GUARANTEE_E2EE, 0) && !mimefactory.out_encrypted) {
dc_set_msg_failed(context, msg_id, "End-to-end-encryption unavailable unexpectedly.");
goto cleanup;
}
if (clist_search_string_nocase(mimefactory.recipients_addr, mimefactory.from_addr)==0) {
clist_append(mimefactory.recipients_names, NULL);
clist_append(mimefactory.recipients_addr, (void*)dc_strdup(mimefactory.from_addr));
}
}
dc_sqlite3_begin_transaction(context->sql);
if (mimefactory.out_gossiped) {
dc_set_gossiped_timestamp(context, mimefactory.msg->chat_id, time(NULL));
}
if (mimefactory.out_last_added_location_id) {
dc_set_kml_sent_timestamp(context, mimefactory.msg->chat_id, time(NULL));
if (!mimefactory.msg->hidden) {
dc_set_msg_location_id(context, mimefactory.msg->id, mimefactory.out_last_added_location_id);
}
}
if (mimefactory.out_encrypted && dc_param_get_int(mimefactory.msg->param, DC_PARAM_GUARANTEE_E2EE, 0)==0) {
dc_param_set_int(mimefactory.msg->param, DC_PARAM_GUARANTEE_E2EE, 1);
dc_msg_save_param_to_disk(mimefactory.msg);
}
dc_add_to_keyhistory(context, NULL, 0, NULL, NULL);
dc_sqlite3_commit(context->sql);
success = dc_add_smtp_job(context, DC_JOB_SEND_MSG_TO_SMTP, &mimefactory);
cleanup:
dc_mimefactory_empty(&mimefactory);
return success;
}
static void dc_job_do_DC_JOB_SEND(dc_context_t* context, dc_job_t* job)
{
char* filename = NULL;
void* buf = NULL;
size_t buf_bytes = 0;
char* recipients = NULL;
clist* recipients_list = NULL;
sqlite3_stmt* stmt = NULL;
if (!dc_smtp_is_connected(context->smtp)) {
dc_loginparam_t* loginparam = dc_loginparam_new();
dc_loginparam_read(loginparam, context->sql, "configured_");
int connected = dc_smtp_connect(context->smtp, loginparam);
dc_loginparam_unref(loginparam);
if (!connected) {
dc_job_try_again_later(job, DC_STANDARD_DELAY, NULL);
goto cleanup;
}
}
filename = dc_param_get(job->param, DC_PARAM_FILE, NULL);
if (!filename) {
dc_log_warning(context, 0, "Missing file name for job %d", job->job_id);
goto cleanup;
}
if (!dc_read_file(context, filename, &buf, &buf_bytes)) {
goto cleanup;
}
recipients = dc_param_get(job->param, DC_PARAM_RECIPIENTS, NULL);
if (!recipients) {
dc_log_warning(context, 0, "Missing recipients for job %d", job->job_id);
goto cleanup;
}
recipients_list = dc_str_to_clist(recipients, "\x1e");
if (job->foreign_id) {
if(!dc_msg_exists(context, job->foreign_id)) {
dc_log_warning(context, 0, "Message %i for job %i does not exist",
job->foreign_id, job->job_id);
goto cleanup;
}
}
{
if (!dc_smtp_send_msg(context->smtp, recipients_list, buf, buf_bytes)) {
if (job->foreign_id && (
MAILSMTP_ERROR_EXCEED_STORAGE_ALLOCATION==context->smtp->error_etpan
|| MAILSMTP_ERROR_INSUFFICIENT_SYSTEM_STORAGE==context->smtp->error_etpan)) {
dc_set_msg_failed(context, job->foreign_id, context->smtp->error);
}
else {
dc_smtp_disconnect(context->smtp);
dc_job_try_again_later(job, DC_AT_ONCE, context->smtp->error);
}
goto cleanup;
}
}
dc_delete_file(context, filename);
if (job->foreign_id) {
dc_update_msg_state(context, job->foreign_id, DC_STATE_OUT_DELIVERED);
stmt = dc_sqlite3_prepare(context->sql, "SELECT chat_id FROM msgs WHERE id=?");
sqlite3_bind_int(stmt, 1, job->foreign_id);
int chat_id = sqlite3_step(stmt)==SQLITE_ROW ? sqlite3_column_int(stmt, 0) : 0;
context->cb(context, DC_EVENT_MSG_DELIVERED, chat_id, job->foreign_id);
}
cleanup:
sqlite3_finalize(stmt);
if (recipients_list) {
clist_free_content(recipients_list);
clist_free(recipients_list);
}
free(recipients);
free(buf);
free(filename);
}
static void dc_send_mdn(dc_context_t* context, uint32_t msg_id)
{
dc_mimefactory_t mimefactory;
dc_mimefactory_init(&mimefactory, context);
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
if (!dc_mimefactory_load_mdn(&mimefactory, msg_id)
|| !dc_mimefactory_render(&mimefactory)) {
goto cleanup;
}
dc_add_smtp_job(context, DC_JOB_SEND_MDN, &mimefactory);
cleanup:
dc_mimefactory_empty(&mimefactory);
}
static void dc_suspend_smtp_thread(dc_context_t* context, int suspend)
{
pthread_mutex_lock(&context->smtpidle_condmutex);
context->smtp_suspended = suspend;
pthread_mutex_unlock(&context->smtpidle_condmutex);
if (suspend)
{
while (1) {
pthread_mutex_lock(&context->smtpidle_condmutex);
if (context->smtp_doing_jobs==0) {
pthread_mutex_unlock(&context->smtpidle_condmutex);
return;
}
pthread_mutex_unlock(&context->smtpidle_condmutex);
usleep(300*1000);
}
}
}
static time_t get_backoff_time_offset(int c_tries)
{
#define MULTIPLY 60
#define JOB_RETRIES 17
time_t N = (time_t)pow((double)2, c_tries - 1);
N = N * MULTIPLY;
time_t seconds = rand() % (N+1);
if (seconds<1) {
seconds = 1;
}
return seconds;
}
static time_t get_next_wakeup_time(dc_context_t* context, int thread)
{
time_t wakeup_time = 0;
sqlite3_stmt* stmt = NULL;
stmt = dc_sqlite3_prepare(context->sql,
"SELECT MIN(desired_timestamp)"
" FROM jobs"
" WHERE thread=?;");
sqlite3_bind_int(stmt, 1, thread);
if (sqlite3_step(stmt)==SQLITE_ROW) {
wakeup_time = sqlite3_column_int(stmt, 0);
}
if (wakeup_time==0) {
wakeup_time = time(NULL) + 10*60;
}
sqlite3_finalize(stmt);
return wakeup_time;
}
int dc_job_action_exists(dc_context_t* context, int action)
{
int job_exists = 0;
sqlite3_stmt* stmt = NULL;
stmt = dc_sqlite3_prepare(context->sql,
"SELECT id FROM jobs WHERE action=?;");
sqlite3_bind_int (stmt, 1, action);
job_exists = (sqlite3_step(stmt)==SQLITE_ROW);
sqlite3_finalize(stmt);
return job_exists;
}
void dc_job_add(dc_context_t* context, int action, int foreign_id, const char* param, int delay_seconds)
{
time_t timestamp = time(NULL);
sqlite3_stmt* stmt = NULL;
int thread = 0;
if (action >= DC_IMAP_THREAD && action < DC_IMAP_THREAD+1000) {
thread = DC_IMAP_THREAD;
}
else if (action >= DC_SMTP_THREAD && action < DC_SMTP_THREAD+1000) {
thread = DC_SMTP_THREAD;
}
else {
return;
}
stmt = dc_sqlite3_prepare(context->sql,
"INSERT INTO jobs (added_timestamp, thread, action, foreign_id, param, desired_timestamp) VALUES (?,?,?,?,?,?);");
sqlite3_bind_int64(stmt, 1, timestamp);
sqlite3_bind_int (stmt, 2, thread);
sqlite3_bind_int (stmt, 3, action);
sqlite3_bind_int (stmt, 4, foreign_id);
sqlite3_bind_text (stmt, 5, param? param : "", -1, SQLITE_STATIC);
sqlite3_bind_int64(stmt, 6, timestamp+delay_seconds);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
if (thread==DC_IMAP_THREAD) {
dc_interrupt_imap_idle(context);
}
else {
dc_interrupt_smtp_idle(context);
}
}
static void dc_job_update(dc_context_t* context, const dc_job_t* job)
{
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE jobs"
" SET desired_timestamp=?, tries=?, param=?"
" WHERE id=?;");
sqlite3_bind_int64(stmt, 1, job->desired_timestamp);
sqlite3_bind_int64(stmt, 2, job->tries);
sqlite3_bind_text (stmt, 3, job->param->packed, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 4, job->job_id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
static void dc_job_delete(dc_context_t* context, const dc_job_t* job)
{
sqlite3_stmt* delete_stmt = dc_sqlite3_prepare(context->sql,
"DELETE FROM jobs WHERE id=?;");
sqlite3_bind_int(delete_stmt, 1, job->job_id);
sqlite3_step(delete_stmt);
sqlite3_finalize(delete_stmt);
}
void dc_job_try_again_later(dc_job_t* job, int try_again, const char* pending_error)
{
if (job==NULL) {
return;
}
job->try_again = try_again;
free(job->pending_error);
job->pending_error = dc_strdup_keep_null(pending_error);
}
void dc_job_kill_action(dc_context_t* context, int action)
{
if (context==NULL) {
return;
}
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"DELETE FROM jobs WHERE action=?;");
sqlite3_bind_int(stmt, 1, action);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
static void dc_job_perform(dc_context_t* context, int thread, int probe_network)
{
sqlite3_stmt* select_stmt = NULL;
dc_job_t job;
#define THREAD_STR (thread==DC_IMAP_THREAD? "INBOX" : "SMTP")
#define IS_EXCLUSIVE_JOB (DC_JOB_CONFIGURE_IMAP==job.action || DC_JOB_IMEX_IMAP==job.action || DC_JOB_EMPTY_SERVER==job.action)
memset(&job, 0, sizeof(dc_job_t));
job.param = dc_param_new();
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
if (probe_network==0) {
#define FIELDS "id, action, foreign_id, param, added_timestamp, desired_timestamp, tries"
select_stmt = dc_sqlite3_prepare(context->sql,
"SELECT " FIELDS " FROM jobs"
" WHERE thread=? AND desired_timestamp<=?"
" ORDER BY action DESC, added_timestamp;");
sqlite3_bind_int64(select_stmt, 1, thread);
sqlite3_bind_int64(select_stmt, 2, time(NULL));
}
else {
select_stmt = dc_sqlite3_prepare(context->sql,
"SELECT " FIELDS " FROM jobs"
" WHERE thread=? AND tries>0"
" ORDER BY desired_timestamp, action DESC;");
sqlite3_bind_int64(select_stmt, 1, thread);
}
while (sqlite3_step(select_stmt)==SQLITE_ROW)
{
job.job_id = sqlite3_column_int (select_stmt, 0);
job.action = sqlite3_column_int (select_stmt, 1);
job.foreign_id = sqlite3_column_int (select_stmt, 2);
dc_param_set_packed(job.param, (char*)sqlite3_column_text (select_stmt, 3));
job.added_timestamp = sqlite3_column_int64(select_stmt, 4);
job.desired_timestamp = sqlite3_column_int64(select_stmt, 5);
job.tries = sqlite3_column_int (select_stmt, 6);
dc_log_info(context, 0, "%s-job #%i, action %i started...", THREAD_STR, (int)job.job_id, (int)job.action);
if (IS_EXCLUSIVE_JOB) {
dc_job_kill_action(context, job.action);
sqlite3_finalize(select_stmt);
select_stmt = NULL;
dc_jobthread_suspend(&context->sentbox_thread, 1);
dc_jobthread_suspend(&context->mvbox_thread, 1);
dc_suspend_smtp_thread(context, 1);
}
for (int tries = 0; tries <= 1; tries++)
{
job.try_again = DC_DONT_TRY_AGAIN;
switch (job.action) {
case DC_JOB_SEND_MSG_TO_SMTP: dc_job_do_DC_JOB_SEND (context, &job); break;
case DC_JOB_DELETE_MSG_ON_IMAP: dc_job_do_DC_JOB_DELETE_MSG_ON_IMAP (context, &job); break;
case DC_JOB_MARKSEEN_MSG_ON_IMAP: dc_job_do_DC_JOB_MARKSEEN_MSG_ON_IMAP (context, &job); break;
case DC_JOB_MARKSEEN_MDN_ON_IMAP: dc_job_do_DC_JOB_MARKSEEN_MDN_ON_IMAP (context, &job); break;
case DC_JOB_MOVE_MSG: dc_job_do_DC_JOB_MOVE_MSG (context, &job); break;
case DC_JOB_SEND_MDN: dc_job_do_DC_JOB_SEND (context, &job); break;
case DC_JOB_CONFIGURE_IMAP: dc_job_do_DC_JOB_CONFIGURE_IMAP (context, &job); break;
case DC_JOB_IMEX_IMAP: dc_job_do_DC_JOB_IMEX_IMAP (context, &job); break;
case DC_JOB_MAYBE_SEND_LOCATIONS: dc_job_do_DC_JOB_MAYBE_SEND_LOCATIONS (context, &job); break;
case DC_JOB_MAYBE_SEND_LOC_ENDED: dc_job_do_DC_JOB_MAYBE_SEND_LOC_ENDED (context, &job); break;
case DC_JOB_EMPTY_SERVER: dc_job_do_DC_JOB_EMPTY_SERVER (context, &job); break;
case DC_JOB_HOUSEKEEPING: dc_housekeeping (context); break;
}
if (job.try_again!=DC_AT_ONCE) {
break;
}
}
if (IS_EXCLUSIVE_JOB) {
dc_jobthread_suspend(&context->sentbox_thread, 0);
dc_jobthread_suspend(&context->mvbox_thread, 0);
dc_suspend_smtp_thread(context, 0);
goto cleanup;
}
else if (job.try_again==DC_INCREATION_POLL)
{
dc_log_info(context, 0, "%s-job #%i not yet ready and will be delayed.", THREAD_STR, (int)job.job_id);
}
else if (job.try_again==DC_AT_ONCE || job.try_again==DC_STANDARD_DELAY)
{
int tries = job.tries + 1;
if( tries < JOB_RETRIES ) {
job.tries = tries;
time_t time_offset = get_backoff_time_offset(tries);
job.desired_timestamp = job.added_timestamp + time_offset;
dc_job_update(context, &job);
dc_log_info(context, 0, "%s-job #%i not succeeded on try #%i, retry in ADD_TIME+%i (in %i seconds).", THREAD_STR, (int)job.job_id,
tries, time_offset, (job.added_timestamp+time_offset)-time(NULL));
if (thread==DC_SMTP_THREAD && tries<(JOB_RETRIES-1)) {
pthread_mutex_lock(&context->smtpidle_condmutex);
context->perform_smtp_jobs_needed = DC_JOBS_NEEDED_AVOID_DOS;
pthread_mutex_unlock(&context->smtpidle_condmutex);
}
}
else {
if (job.action==DC_JOB_SEND_MSG_TO_SMTP) {
dc_set_msg_failed(context, job.foreign_id, job.pending_error);
}
dc_job_delete(context, &job);
}
if (probe_network) {
goto cleanup;
}
}
else
{
dc_job_delete(context, &job);
}
}
cleanup:
dc_param_unref(job.param);
free(job.pending_error);
sqlite3_finalize(select_stmt);
}
void dc_perform_imap_jobs(dc_context_t* context)
{
dc_log_info(context, 0, "INBOX-jobs started...");
pthread_mutex_lock(&context->inboxidle_condmutex);
int probe_imap_network = context->probe_imap_network;
context->probe_imap_network = 0;
context->perform_inbox_jobs_needed = 0;
pthread_mutex_unlock(&context->inboxidle_condmutex);
dc_job_perform(context, DC_IMAP_THREAD, probe_imap_network);
dc_log_info(context, 0, "INBOX-jobs ended.");
}
void dc_perform_imap_fetch(dc_context_t* context)
{
clock_t start = clock();
if (!connect_to_inbox(context)) {
return;
}
if (dc_sqlite3_get_config_int(context->sql, "inbox_watch", DC_INBOX_WATCH_DEFAULT)==0) {
dc_log_info(context, 0, "INBOX-watch disabled.");
return;
}
dc_log_info(context, 0, "INBOX-fetch started...");
dc_imap_fetch(context->inbox);
if (context->inbox->should_reconnect)
{
dc_log_info(context, 0, "INBOX-fetch aborted, starting over...");
dc_imap_fetch(context->inbox);
}
dc_log_info(context, 0, "INBOX-fetch done in %.0f ms.", (double)(clock()-start)*1000.0/CLOCKS_PER_SEC);
}
void dc_perform_imap_idle(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
connect_to_inbox(context);
pthread_mutex_lock(&context->inboxidle_condmutex);
if (context->perform_inbox_jobs_needed) {
dc_log_info(context, 0, "INBOX-IDLE will not be started because of waiting jobs.");
pthread_mutex_unlock(&context->inboxidle_condmutex);
return;
}
pthread_mutex_unlock(&context->inboxidle_condmutex);
dc_log_info(context, 0, "INBOX-IDLE started...");
dc_imap_idle(context->inbox);
dc_log_info(context, 0, "INBOX-IDLE ended.");
}
void dc_interrupt_imap_idle(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || context->inbox==NULL) {
dc_log_warning(context, 0, "Interrupt IMAP-IDLE: Bad parameters.");
return;
}
dc_log_info(context, 0, "Interrupting IMAP-IDLE...");
pthread_mutex_lock(&context->inboxidle_condmutex);
context->perform_inbox_jobs_needed = 1;
pthread_mutex_unlock(&context->inboxidle_condmutex);
dc_imap_interrupt_idle(context->inbox);
}
void dc_perform_mvbox_fetch(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
int use_network = dc_sqlite3_get_config_int(context->sql, "mvbox_watch", DC_MVBOX_WATCH_DEFAULT);
dc_jobthread_fetch(&context->mvbox_thread, use_network);
}
void dc_perform_mvbox_idle(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
int use_network = dc_sqlite3_get_config_int(context->sql, "mvbox_watch", DC_MVBOX_WATCH_DEFAULT);
dc_jobthread_idle(&context->mvbox_thread, use_network);
}
void dc_interrupt_mvbox_idle(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
dc_log_warning(context, 0, "Interrupt MVBOX-IDLE: Bad parameters.");
return;
}
dc_jobthread_interrupt_idle(&context->mvbox_thread);
}
void dc_perform_sentbox_fetch(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
int use_network = dc_sqlite3_get_config_int(context->sql, "sentbox_watch", DC_SENTBOX_WATCH_DEFAULT);
dc_jobthread_fetch(&context->sentbox_thread, use_network);
}
void dc_perform_sentbox_idle(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
int use_network = dc_sqlite3_get_config_int(context->sql, "sentbox_watch", DC_SENTBOX_WATCH_DEFAULT);
dc_jobthread_idle(&context->sentbox_thread, use_network);
}
void dc_interrupt_sentbox_idle(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
dc_log_warning(context, 0, "Interrupt SENT-IDLE: Bad parameters.");
return;
}
dc_jobthread_interrupt_idle(&context->sentbox_thread);
}
void dc_perform_smtp_jobs(dc_context_t* context)
{
pthread_mutex_lock(&context->smtpidle_condmutex);
int probe_smtp_network = context->probe_smtp_network;
context->probe_smtp_network = 0;
context->perform_smtp_jobs_needed = 0;
if (context->smtp_suspended) {
dc_log_info(context, 0, "SMTP-jobs suspended.");
pthread_mutex_unlock(&context->smtpidle_condmutex);
return;
}
context->smtp_doing_jobs = 1;
pthread_mutex_unlock(&context->smtpidle_condmutex);
dc_log_info(context, 0, "SMTP-jobs started...");
dc_job_perform(context, DC_SMTP_THREAD, probe_smtp_network);
dc_log_info(context, 0, "SMTP-jobs ended.");
pthread_mutex_lock(&context->smtpidle_condmutex);
context->smtp_doing_jobs = 0;
pthread_mutex_unlock(&context->smtpidle_condmutex);
}
void dc_perform_smtp_idle(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
dc_log_warning(context, 0, "Cannot perform SMTP-idle: Bad parameters.");
return;
}
dc_log_info(context, 0, "SMTP-idle started...");
pthread_mutex_lock(&context->smtpidle_condmutex);
if (context->perform_smtp_jobs_needed==DC_JOBS_NEEDED_AT_ONCE)
{
dc_log_info(context, 0, "SMTP-idle will not be started because of waiting jobs.");
}
else
{
int r = 0;
struct timespec wakeup_at;
memset(&wakeup_at, 0, sizeof(wakeup_at));
wakeup_at.tv_sec = get_next_wakeup_time(context, DC_SMTP_THREAD)+1;
while (context->smtpidle_condflag==0 && r==0) {
r = pthread_cond_timedwait(&context->smtpidle_cond, &context->smtpidle_condmutex, &wakeup_at);
}
context->smtpidle_condflag = 0;
}
pthread_mutex_unlock(&context->smtpidle_condmutex);
dc_log_info(context, 0, "SMTP-idle ended.");
}
void dc_interrupt_smtp_idle(dc_context_t* context)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
dc_log_warning(context, 0, "Interrupt SMTP-idle: Bad parameters.");
return;
}
dc_log_info(context, 0, "Interrupting SMTP-idle...");
pthread_mutex_lock(&context->smtpidle_condmutex);
context->perform_smtp_jobs_needed = DC_JOBS_NEEDED_AT_ONCE;
context->smtpidle_condflag = 1;
pthread_cond_signal(&context->smtpidle_cond);
pthread_mutex_unlock(&context->smtpidle_condmutex);
}
void dc_maybe_network(dc_context_t* context)
{
pthread_mutex_lock(&context->smtpidle_condmutex);
context->probe_smtp_network = 1;
pthread_mutex_unlock(&context->smtpidle_condmutex);
pthread_mutex_lock(&context->inboxidle_condmutex);
context->probe_imap_network = 1;
pthread_mutex_unlock(&context->inboxidle_condmutex);
dc_interrupt_smtp_idle(context);
dc_interrupt_imap_idle(context);
dc_interrupt_mvbox_idle(context);
dc_interrupt_sentbox_idle(context);
}
void dc_empty_server(dc_context_t* context, int flags)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC
|| (flags&(DC_EMPTY_INBOX|DC_EMPTY_MVBOX))==0) {
return;
}
dc_job_kill_action(context, DC_JOB_EMPTY_SERVER);
dc_job_add(context, DC_JOB_EMPTY_SERVER, flags, NULL, 0);
}