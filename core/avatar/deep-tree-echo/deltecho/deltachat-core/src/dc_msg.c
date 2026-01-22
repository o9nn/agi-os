#include "dc_context.h"
#include "dc_imap.h"
#include "dc_smtp.h"
#include "dc_job.h"
#include "dc_pgp.h"
#include "dc_mimefactory.h"
#define DC_MSG_MAGIC 0x11561156
dc_msg_t* dc_msg_new(dc_context_t* context, int viewtype)
{
dc_msg_t* msg = NULL;
if ((msg=calloc(1, sizeof(dc_msg_t)))==NULL) {
exit(15);
}
msg->context   = context;
msg->magic     = DC_MSG_MAGIC;
msg->type      = viewtype;
msg->state     = DC_STATE_UNDEFINED;
msg->param     = dc_param_new();
return msg;
}
dc_msg_t* dc_msg_new_untyped(dc_context_t* context)
{
return dc_msg_new(context, 0);
}
dc_msg_t* dc_msg_new_load(dc_context_t* context, uint32_t msg_id)
{
dc_msg_t* msg = dc_msg_new_untyped(context);
dc_msg_load_from_db(msg, context, msg_id);
return msg;
}
void dc_msg_unref(dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return;
}
dc_msg_empty(msg);
dc_param_unref(msg->param);
msg->magic = 0;
free(msg);
}
void dc_msg_empty(dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return;
}
free(msg->text);
msg->text = NULL;
free(msg->rfc724_mid);
msg->rfc724_mid = NULL;
free(msg->in_reply_to);
msg->in_reply_to = NULL;
free(msg->server_folder);
msg->server_folder = NULL;
dc_param_set_packed(msg->param, NULL);
msg->hidden = 0;
}
uint32_t dc_msg_get_id(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return msg->id;
}
uint32_t dc_msg_get_from_id(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return msg->from_id;
}
uint32_t dc_msg_get_chat_id(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return msg->chat_blocked? DC_CHAT_ID_DEADDROP : msg->chat_id;
}
int dc_msg_get_viewtype(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return msg->type;
}
int dc_msg_get_state(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return DC_STATE_UNDEFINED;
}
return msg->state;
}
time_t dc_msg_get_timestamp(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return msg->timestamp_sent? msg->timestamp_sent : msg->timestamp_sort;
}
time_t dc_msg_get_received_timestamp(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return msg->timestamp_rcvd;
}
time_t dc_msg_get_sort_timestamp(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return msg->timestamp_sort;
}
int dc_msg_has_deviating_timestamp(const dc_msg_t* msg)
{
long   cnv_to_local = dc_gm2local_offset();
time_t sort_timestamp = dc_msg_get_sort_timestamp(msg) + cnv_to_local;
time_t send_timestamp = dc_msg_get_timestamp(msg) + cnv_to_local;
return (sort_timestamp/DC_SECONDS_PER_DAY != send_timestamp/DC_SECONDS_PER_DAY);
}
int dc_msg_has_location(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return (msg->location_id!=0);
}
char* dc_msg_get_text(const dc_msg_t* msg)
{
char* ret = NULL;
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return dc_strdup(NULL);
}
ret = dc_strdup(msg->text);
dc_truncate_str(ret, DC_MAX_GET_TEXT_LEN);
return ret;
}
char* dc_msg_get_file(const dc_msg_t* msg)
{
char* file_rel = NULL;
char* file_abs = NULL;
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
goto cleanup;
}
if ((file_rel = dc_param_get(msg->param, DC_PARAM_FILE, NULL))!=NULL) {
file_abs = dc_get_abs_path(msg->context, file_rel);
}
cleanup:
free(file_rel);
return file_abs? file_abs : dc_strdup(NULL);
}
char* dc_msg_get_filename(const dc_msg_t* msg)
{
char* ret = NULL;
char* pathNfilename = NULL;
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
goto cleanup;
}
pathNfilename = dc_param_get(msg->param, DC_PARAM_FILE, NULL);
if (pathNfilename==NULL) {
goto cleanup;
}
ret = dc_get_filename(pathNfilename);
cleanup:
free(pathNfilename);
return ret? ret : dc_strdup(NULL);
}
char* dc_msg_get_filemime(const dc_msg_t* msg)
{
char* ret = NULL;
char* file = NULL;
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
goto cleanup;
}
ret = dc_param_get(msg->param, DC_PARAM_MIMETYPE, NULL);
if (ret==NULL) {
file = dc_param_get(msg->param, DC_PARAM_FILE, NULL);
if (file==NULL) {
goto cleanup;
}
dc_msg_guess_msgtype_from_suffix(file, NULL, &ret);
if (ret==NULL) {
ret = dc_strdup("application/octet-stream");
}
}
cleanup:
free(file);
return ret? ret : dc_strdup(NULL);
}
uint64_t dc_msg_get_filebytes(const dc_msg_t* msg)
{
uint64_t ret = 0;
char*    file = NULL;
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
goto cleanup;
}
file = dc_param_get(msg->param, DC_PARAM_FILE, NULL);
if (file==NULL) {
goto cleanup;
}
ret = dc_get_filebytes(msg->context, file);
cleanup:
free(file);
return ret;
}
int dc_msg_get_width(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return dc_param_get_int(msg->param, DC_PARAM_WIDTH, 0);
}
int dc_msg_get_height(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return dc_param_get_int(msg->param, DC_PARAM_HEIGHT, 0);
}
int dc_msg_get_duration(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return dc_param_get_int(msg->param, DC_PARAM_DURATION, 0);
}
int dc_msg_get_showpadlock(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC || msg->context==NULL) {
return 0;
}
if (dc_param_get_int(msg->param, DC_PARAM_GUARANTEE_E2EE, 0)!=0) {
return 1;
}
return 0;
}
dc_lot_t* dc_msg_get_summary(const dc_msg_t* msg, const dc_chat_t* chat)
{
dc_lot_t*      ret = dc_lot_new();
dc_contact_t*  contact = NULL;
dc_chat_t*     chat_to_delete = NULL;
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
goto cleanup;
}
if (chat==NULL) {
if ((chat_to_delete=dc_get_chat(msg->context, msg->chat_id))==NULL) {
goto cleanup;
}
chat = chat_to_delete;
}
if (msg->from_id!=DC_CONTACT_ID_SELF && DC_CHAT_TYPE_IS_MULTI(chat->type)) {
contact = dc_get_contact(chat->context, msg->from_id);
}
dc_lot_fill(ret, msg, chat, contact, msg->context);
cleanup:
dc_contact_unref(contact);
dc_chat_unref(chat_to_delete);
return ret;
}
char* dc_msg_get_summarytext(const dc_msg_t* msg, int approx_characters)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return dc_strdup(NULL);
}
return dc_msg_get_summarytext_by_raw(msg->type, msg->text, msg->param, approx_characters, msg->context);
}
int dc_msg_is_sent(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return (msg->state >= DC_STATE_OUT_DELIVERED)? 1 : 0;
}
int dc_msg_is_starred(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return msg->starred? 1 : 0;
}
int dc_msg_is_forwarded(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
return dc_param_get_int(msg->param, DC_PARAM_FORWARDED, 0)? 1 : 0;
}
int dc_msg_is_info(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return 0;
}
int cmd = dc_param_get_int(msg->param, DC_PARAM_CMD, 0);
if (msg->from_id==DC_CONTACT_ID_DEVICE
|| msg->to_id==DC_CONTACT_ID_DEVICE
|| (cmd && cmd!=DC_CMD_AUTOCRYPT_SETUP_MESSAGE)) {
return 1;
}
return 0;
}
int dc_msg_is_setupmessage(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC || msg->type!=DC_MSG_FILE) {
return 0;
}
return dc_param_get_int(msg->param, DC_PARAM_CMD, 0)==DC_CMD_AUTOCRYPT_SETUP_MESSAGE? 1 : 0;
}
char* dc_msg_get_setupcodebegin(const dc_msg_t* msg)
{
char*        filename = NULL;
char*        buf = NULL;
size_t       buf_bytes = 0;
const char*  buf_headerline = NULL;
const char*  buf_setupcodebegin = NULL;
char*        ret = NULL;
if (!dc_msg_is_setupmessage(msg)) {
goto cleanup;
}
if ((filename=dc_msg_get_file(msg))==NULL || filename[0]==0) {
goto cleanup;
}
if (!dc_read_file(msg->context, filename, (void**)&buf, &buf_bytes) || buf==NULL || buf_bytes <= 0) {
goto cleanup;
}
if (!dc_split_armored_data(buf, &buf_headerline, &buf_setupcodebegin, NULL, NULL)
|| strcmp(buf_headerline, "-----BEGIN PGP MESSAGE-----")!=0 || buf_setupcodebegin==NULL) {
goto cleanup;
}
ret = dc_strdup(buf_setupcodebegin);
cleanup:
free(filename);
free(buf);
return ret? ret : dc_strdup(NULL);
}
#define DC_MSG_FIELDS " m.id,rfc724_mid,m.mime_in_reply_to,m.server_folder,m.server_uid,m.move_state,m.chat_id, " \
" m.from_id,m.to_id,m.timestamp,m.timestamp_sent,m.timestamp_rcvd, m.type,m.state,m.msgrmsg,m.txt, " \
" m.param,m.starred,m.hidden,m.location_id, c.blocked "
static int dc_msg_set_from_stmt(dc_msg_t* msg, sqlite3_stmt* row, int row_offset)
{
dc_msg_empty(msg);
msg->id           =           (uint32_t)sqlite3_column_int  (row, row_offset++);
msg->rfc724_mid   =    dc_strdup((char*)sqlite3_column_text (row, row_offset++));
msg->in_reply_to  =    dc_strdup((char*)sqlite3_column_text (row, row_offset++));
msg->server_folder=    dc_strdup((char*)sqlite3_column_text (row, row_offset++));
msg->server_uid   =           (uint32_t)sqlite3_column_int  (row, row_offset++);
msg->move_state   =    (dc_move_state_t)sqlite3_column_int  (row, row_offset++);
msg->chat_id      =           (uint32_t)sqlite3_column_int  (row, row_offset++);
msg->from_id      =           (uint32_t)sqlite3_column_int  (row, row_offset++);
msg->to_id        =           (uint32_t)sqlite3_column_int  (row, row_offset++);
msg->timestamp_sort =           (time_t)sqlite3_column_int64(row, row_offset++);
msg->timestamp_sent =           (time_t)sqlite3_column_int64(row, row_offset++);
msg->timestamp_rcvd =           (time_t)sqlite3_column_int64(row, row_offset++);
msg->type         =                     sqlite3_column_int  (row, row_offset++);
msg->state        =                     sqlite3_column_int  (row, row_offset++);
msg->is_dc_message=                     sqlite3_column_int  (row, row_offset++);
msg->text         =    dc_strdup((char*)sqlite3_column_text (row, row_offset++));
dc_param_set_packed( msg->param, (char*)sqlite3_column_text (row, row_offset++));
msg->starred      =                     sqlite3_column_int  (row, row_offset++);
msg->hidden       =                     sqlite3_column_int  (row, row_offset++);
msg->location_id  =                     sqlite3_column_int  (row, row_offset++);
msg->chat_blocked =                     sqlite3_column_int  (row, row_offset++);
if (msg->chat_blocked==2) {
dc_truncate_n_unwrap_str(msg->text, 256 ,
0);
}
return 1;
}
int dc_msg_load_from_db(dc_msg_t* msg, dc_context_t* context, uint32_t id)
{
int           success = 0;
sqlite3_stmt* stmt = NULL;
if (msg==NULL || msg->magic!=DC_MSG_MAGIC || context==NULL || context->sql==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT " DC_MSG_FIELDS
" FROM msgs m LEFT JOIN chats c ON c.id=m.chat_id"
" WHERE m.id=?;");
sqlite3_bind_int(stmt, 1, id);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
if (!dc_msg_set_from_stmt(msg, stmt, 0)) {
goto cleanup;
}
msg->context = context;
success = 1;
cleanup:
sqlite3_finalize(stmt);
return success;
}
void dc_msg_guess_msgtype_from_suffix(const char* pathNfilename, int* ret_msgtype, char** ret_mime)
{
char* suffix = NULL;
int   dummy_msgtype = 0;
char* dummy_buf = NULL;
if (pathNfilename==NULL) {
goto cleanup;
}
if (ret_msgtype==NULL) { ret_msgtype = &dummy_msgtype; }
if (ret_mime==NULL)    { ret_mime = &dummy_buf; }
*ret_msgtype = 0;
*ret_mime = NULL;
suffix = dc_get_filesuffix_lc(pathNfilename);
if (suffix==NULL) {
goto cleanup;
}
if (strcmp(suffix, "mp3")==0) {
*ret_msgtype = DC_MSG_AUDIO;
*ret_mime = dc_strdup("audio/mpeg");
}
else if (strcmp(suffix, "aac")==0) {
*ret_msgtype = DC_MSG_AUDIO;
*ret_mime = dc_strdup("audio/aac");
}
else if (strcmp(suffix, "mp4")==0) {
*ret_msgtype = DC_MSG_VIDEO;
*ret_mime = dc_strdup("video/mp4");
}
else if (strcmp(suffix, "jpg")==0 || strcmp(suffix, "jpeg")==0) {
*ret_msgtype = DC_MSG_IMAGE;
*ret_mime = dc_strdup("image/jpeg");
}
else if (strcmp(suffix, "png")==0) {
*ret_msgtype = DC_MSG_IMAGE;
*ret_mime = dc_strdup("image/png");
}
else if (strcmp(suffix, "webp")==0) {
*ret_msgtype = DC_MSG_IMAGE;
*ret_mime = dc_strdup("image/webp");
}
else if (strcmp(suffix, "gif")==0) {
*ret_msgtype = DC_MSG_GIF;
*ret_mime = dc_strdup("image/gif");
}
else if (strcmp(suffix, "vcf")==0 || strcmp(suffix, "vcard")==0) {
*ret_msgtype = DC_MSG_FILE;
*ret_mime = dc_strdup("text/vcard");
}
cleanup:
free(suffix);
free(dummy_buf);
}
char* dc_msg_get_summarytext_by_raw(int type, const char* text, dc_param_t* param, int approx_characters, dc_context_t* context)
{
char* ret = NULL;
char* prefix = NULL;
char* pathNfilename = NULL;
char* label = NULL;
char* value = NULL;
int   append_text = 1;
switch (type) {
case DC_MSG_IMAGE:
prefix = dc_stock_str(context, DC_STR_IMAGE);
break;
case DC_MSG_GIF:
prefix = dc_stock_str(context, DC_STR_GIF);
break;
case DC_MSG_VIDEO:
prefix = dc_stock_str(context, DC_STR_VIDEO);
break;
case DC_MSG_VOICE:
prefix = dc_stock_str(context, DC_STR_VOICEMESSAGE);
break;
case DC_MSG_AUDIO:
case DC_MSG_FILE:
if (dc_param_get_int(param, DC_PARAM_CMD, 0)==DC_CMD_AUTOCRYPT_SETUP_MESSAGE) {
prefix = dc_stock_str(context, DC_STR_AC_SETUP_MSG_SUBJECT);
append_text = 0;
}
else {
pathNfilename = dc_param_get(param, DC_PARAM_FILE, "ErrFilename");
value = dc_get_filename(pathNfilename);
label = dc_stock_str(context, type==DC_MSG_AUDIO? DC_STR_AUDIO : DC_STR_FILE);
prefix = dc_mprintf("%s " DC_NDASH " %s", label, value);
}
break;
default:
if (dc_param_get_int(param, DC_PARAM_CMD, 0)==DC_CMD_LOCATION_ONLY) {
prefix = dc_stock_str(context, DC_STR_LOCATION);
append_text = 0;
}
break;
}
if (append_text && prefix && text && text[0]) {
ret = dc_mprintf("%s " DC_NDASH " %s", prefix, text);
dc_truncate_n_unwrap_str(ret, approx_characters, 1);
}
else if (append_text && text && text[0]) {
ret = dc_strdup(text);
dc_truncate_n_unwrap_str(ret, approx_characters, 1);
}
else {
ret = prefix;
prefix = NULL;
}
free(prefix);
free(pathNfilename);
free(label);
free(value);
if (ret==NULL) {
ret = dc_strdup(NULL);
}
return ret;
}
int dc_msg_is_increation(const dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC || msg->context==NULL) {
return 0;
}
return DC_MSG_NEEDS_ATTACHMENT(msg->type) && msg->state==DC_STATE_OUT_PREPARING;
}
void dc_msg_save_param_to_disk(dc_msg_t* msg)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC || msg->context==NULL || msg->context->sql==NULL) {
return;
}
sqlite3_stmt* stmt = dc_sqlite3_prepare(msg->context->sql,
"UPDATE msgs SET param=? WHERE id=?;");
sqlite3_bind_text(stmt, 1, msg->param->packed, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 2, msg->id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
void dc_msg_set_text(dc_msg_t* msg, const char* text)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return;
}
free(msg->text);
msg->text = dc_strdup(text);
}
void dc_msg_set_file(dc_msg_t* msg, const char* file, const char* filemime)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return;
}
dc_param_set(msg->param, DC_PARAM_FILE, file);
dc_param_set(msg->param, DC_PARAM_MIMETYPE, filemime);
}
void dc_msg_set_dimension(dc_msg_t* msg, int width, int height)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return;
}
dc_param_set_int(msg->param, DC_PARAM_WIDTH, width);
dc_param_set_int(msg->param, DC_PARAM_HEIGHT, height);
}
void dc_msg_set_duration(dc_msg_t* msg, int duration)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
return;
}
dc_param_set_int(msg->param, DC_PARAM_DURATION, duration);
}
void dc_msg_set_location(dc_msg_t* msg, double latitude, double longitude)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC || (latitude==0.0 && longitude==0.0)) {
return;
}
dc_param_set_float(msg->param, DC_PARAM_SET_LATITUDE,  latitude);
dc_param_set_float(msg->param, DC_PARAM_SET_LONGITUDE, longitude);
}
void dc_msg_latefiling_mediasize(dc_msg_t* msg, int width, int height, int duration)
{
if (msg==NULL || msg->magic!=DC_MSG_MAGIC) {
goto cleanup;
}
if (width>0 && height>0) {
dc_param_set_int(msg->param, DC_PARAM_WIDTH, width);
dc_param_set_int(msg->param, DC_PARAM_HEIGHT, height);
}
if (duration>0) {
dc_param_set_int(msg->param, DC_PARAM_DURATION, duration);
}
dc_msg_save_param_to_disk(msg);
cleanup:
;
}
int dc_msg_exists(dc_context_t* context, uint32_t msg_id)
{
int           msg_exists = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC
|| msg_id<=DC_MSG_ID_LAST_SPECIAL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT chat_id FROM msgs WHERE id=?;");
sqlite3_bind_int(stmt, 1, msg_id);
if (sqlite3_step(stmt)==SQLITE_ROW)
{
uint32_t chat_id = sqlite3_column_int(stmt, 0);
if (chat_id!=DC_CHAT_ID_TRASH)
{
msg_exists = 1;
}
}
cleanup:
sqlite3_finalize(stmt);
return msg_exists;
}
void dc_update_msg_chat_id(dc_context_t* context, uint32_t msg_id, uint32_t chat_id)
{
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE msgs SET chat_id=? WHERE id=?;");
sqlite3_bind_int(stmt, 1, chat_id);
sqlite3_bind_int(stmt, 2, msg_id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
void dc_update_msg_state(dc_context_t* context, uint32_t msg_id, int state)
{
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE msgs SET state=? WHERE id=?;");
sqlite3_bind_int(stmt, 1, state);
sqlite3_bind_int(stmt, 2, msg_id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
void dc_update_msg_move_state(dc_context_t* context, const char* rfc724_mid, dc_move_state_t state)
{
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE msgs SET move_state=? WHERE rfc724_mid=?;");
sqlite3_bind_int (stmt, 1, state);
sqlite3_bind_text(stmt, 2, rfc724_mid, -1, SQLITE_STATIC);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
void dc_set_msg_failed(dc_context_t* context, uint32_t msg_id, const char* error)
{
dc_msg_t*     msg = dc_msg_new_untyped(context);
sqlite3_stmt* stmt = NULL;
if (!dc_msg_load_from_db(msg, context, msg_id)) {
goto cleanup;
}
if (DC_STATE_OUT_PREPARING==msg->state ||
DC_STATE_OUT_PENDING  ==msg->state ||
DC_STATE_OUT_DELIVERED==msg->state)
{
msg->state = DC_STATE_OUT_FAILED;
}
if (error) {
dc_param_set(msg->param, DC_PARAM_ERROR, error);
dc_log_error(context, 0, "%s", error);
}
stmt = dc_sqlite3_prepare(context->sql,
"UPDATE msgs SET state=?, param=? WHERE id=?;");
sqlite3_bind_int (stmt, 1, msg->state);
sqlite3_bind_text(stmt, 2, msg->param->packed, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 3, msg_id);
sqlite3_step(stmt);
context->cb(context, DC_EVENT_MSG_FAILED, msg->chat_id, msg_id);
cleanup:
sqlite3_finalize(stmt);
dc_msg_unref(msg);
}
size_t dc_get_real_msg_cnt(dc_context_t* context)
{
sqlite3_stmt* stmt = NULL;
size_t        ret = 0;
if (context->sql->cobj==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) "
" FROM msgs m "
" LEFT JOIN chats c ON c.id=m.chat_id "
" WHERE m.id>" DC_STRINGIFY(DC_MSG_ID_LAST_SPECIAL)
" AND m.chat_id>" DC_STRINGIFY(DC_CHAT_ID_LAST_SPECIAL)
" AND c.blocked=0;");
if (sqlite3_step(stmt)!=SQLITE_ROW) {
dc_sqlite3_log_error(context->sql, "dc_get_real_msg_cnt() failed.");
goto cleanup;
}
ret = sqlite3_column_int(stmt, 0);
cleanup:
sqlite3_finalize(stmt);
return ret;
}
size_t dc_get_deaddrop_msg_cnt(dc_context_t* context)
{
sqlite3_stmt* stmt = NULL;
size_t        ret = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || context->sql->cobj==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM msgs m LEFT JOIN chats c ON c.id=m.chat_id WHERE c.blocked=2;");
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
ret = sqlite3_column_int(stmt, 0);
cleanup:
sqlite3_finalize(stmt);
return ret;
}
int dc_rfc724_mid_cnt(dc_context_t* context, const char* rfc724_mid)
{
int           ret = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || context->sql->cobj==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM msgs WHERE rfc724_mid=?;");
sqlite3_bind_text(stmt, 1, rfc724_mid, -1, SQLITE_STATIC);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
ret = sqlite3_column_int(stmt, 0);
cleanup:
sqlite3_finalize(stmt);
return ret;
}
uint32_t dc_rfc724_mid_exists(dc_context_t* context, const char* rfc724_mid, char** ret_server_folder, uint32_t* ret_server_uid)
{
uint32_t      ret = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || rfc724_mid==NULL || rfc724_mid[0]==0) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT server_folder, server_uid, id FROM msgs WHERE rfc724_mid=?;");
sqlite3_bind_text(stmt, 1, rfc724_mid, -1, SQLITE_STATIC);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
if (ret_server_folder) { *ret_server_folder = NULL; }
if (ret_server_uid)    { *ret_server_uid    = 0; }
goto cleanup;
}
if (ret_server_folder) { *ret_server_folder = dc_strdup((char*)sqlite3_column_text(stmt, 0)); }
if (ret_server_uid)    { *ret_server_uid = sqlite3_column_int(stmt, 1);  }
ret = sqlite3_column_int(stmt, 2);
cleanup:
sqlite3_finalize(stmt);
return ret;
}
void dc_update_server_uid(dc_context_t* context, const char* rfc724_mid, const char* server_folder, uint32_t server_uid)
{
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE msgs SET server_folder=?, server_uid=? WHERE rfc724_mid=?;");
sqlite3_bind_text(stmt, 1, server_folder, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 2, server_uid);
sqlite3_bind_text(stmt, 3, rfc724_mid, -1, SQLITE_STATIC);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
dc_msg_t* dc_get_msg(dc_context_t* context, uint32_t msg_id)
{
int success = 0;
dc_msg_t* obj = dc_msg_new_untyped(context);
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
if (!dc_msg_load_from_db(obj, context, msg_id)) {
goto cleanup;
}
success = 1;
cleanup:
if (success) {
return obj;
}
else {
dc_msg_unref(obj);
return NULL;
}
}
char* dc_get_msg_info(dc_context_t* context, uint32_t msg_id)
{
sqlite3_stmt*   stmt = NULL;
dc_msg_t*       msg = dc_msg_new_untyped(context);
dc_contact_t*   contact_from = dc_contact_new(context);
char*           rawtxt = NULL;
char*           p = NULL;
dc_strbuilder_t ret;
dc_strbuilder_init(&ret, 0);
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
dc_msg_load_from_db(msg, context, msg_id);
dc_contact_load_from_db(contact_from, context->sql, msg->from_id);
stmt = dc_sqlite3_prepare(context->sql,
"SELECT txt_raw FROM msgs WHERE id=?;");
sqlite3_bind_int(stmt, 1, msg_id);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
p = dc_mprintf("Cannot load message #%i.", (int)msg_id); dc_strbuilder_cat(&ret, p); free(p);
goto cleanup;
}
rawtxt = dc_strdup((char*)sqlite3_column_text(stmt, 0));
sqlite3_finalize(stmt);
stmt = NULL;
dc_trim(rawtxt);
dc_truncate_str(rawtxt, DC_MAX_GET_INFO_LEN);
dc_strbuilder_cat(&ret, "Sent: ");
p = dc_timestamp_to_str(dc_msg_get_timestamp(msg)); dc_strbuilder_cat(&ret, p); free(p);
p = dc_contact_get_name_n_addr(contact_from); dc_strbuilder_catf(&ret, " by %s", p); free(p);
dc_strbuilder_cat(&ret, "\n");
if (msg->from_id!=DC_CONTACT_ID_SELF) {
dc_strbuilder_cat(&ret, "Received: ");
p = dc_timestamp_to_str(msg->timestamp_rcvd? msg->timestamp_rcvd : msg->timestamp_sort); dc_strbuilder_cat(&ret, p); free(p);
dc_strbuilder_cat(&ret, "\n");
}
if (msg->from_id==DC_CONTACT_ID_DEVICE || msg->to_id==DC_CONTACT_ID_DEVICE) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT contact_id, timestamp_sent FROM msgs_mdns WHERE msg_id=?;");
sqlite3_bind_int (stmt, 1, msg_id);
while (sqlite3_step(stmt)==SQLITE_ROW) {
dc_strbuilder_cat(&ret, "Read: ");
p = dc_timestamp_to_str(sqlite3_column_int64(stmt, 1)); dc_strbuilder_cat(&ret, p); free(p);
dc_strbuilder_cat(&ret, " by ");
dc_contact_t* contact = dc_contact_new(context);
dc_contact_load_from_db(contact, context->sql, sqlite3_column_int64(stmt, 0));
p = dc_contact_get_name_n_addr(contact); dc_strbuilder_cat(&ret, p); free(p);
dc_contact_unref(contact);
dc_strbuilder_cat(&ret, "\n");
}
sqlite3_finalize(stmt);
stmt = NULL;
p = NULL;
switch (msg->state) {
case DC_STATE_IN_FRESH:      p = dc_strdup("Fresh");           break;
case DC_STATE_IN_NOTICED:    p = dc_strdup("Noticed");         break;
case DC_STATE_IN_SEEN:       p = dc_strdup("Seen");            break;
case DC_STATE_OUT_DELIVERED: p = dc_strdup("Delivered");       break;
case DC_STATE_OUT_FAILED:    p = dc_strdup("Failed");          break;
case DC_STATE_OUT_MDN_RCVD:  p = dc_strdup("Read");            break;
case DC_STATE_OUT_PENDING:   p = dc_strdup("Pending");         break;
case DC_STATE_OUT_PREPARING: p = dc_strdup("Preparing");       break;
default:                     p = dc_mprintf("%i", msg->state); break;
}
dc_strbuilder_catf(&ret, "State: %s", p);
free(p);
if (dc_msg_has_location(msg)) {
dc_strbuilder_cat(&ret, ", Location sent");
}
p = NULL;
int e2ee_errors;
if ((e2ee_errors=dc_param_get_int(msg->param, DC_PARAM_ERRONEOUS_E2EE, 0))) {
if (e2ee_errors&DC_E2EE_NO_VALID_SIGNATURE) {
p = dc_strdup("Encrypted, no valid signature");
}
}
else if (dc_param_get_int(msg->param, DC_PARAM_GUARANTEE_E2EE, 0)) {
p = dc_strdup("Encrypted");
}
if (p) {
dc_strbuilder_catf(&ret, ", %s", p);
free(p);
}
dc_strbuilder_cat(&ret, "\n");
if ((p=dc_param_get(msg->param, DC_PARAM_ERROR, NULL))!=NULL) {
dc_strbuilder_catf(&ret, "Error: %s\n", p);
free(p);
}
if ((p=dc_msg_get_file(msg))!=NULL && p[0]) {
dc_strbuilder_catf(&ret, "\nFile: %s, %i bytes\n", p, (int)dc_get_filebytes(context, p));
}
free(p);
if (msg->type!=DC_MSG_TEXT) {
p = NULL;
switch (msg->type)  {
case DC_MSG_AUDIO: p = dc_strdup("Audio");          break;
case DC_MSG_FILE:  p = dc_strdup("File");           break;
case DC_MSG_GIF:   p = dc_strdup("GIF");            break;
case DC_MSG_IMAGE: p = dc_strdup("Image");          break;
case DC_MSG_VIDEO: p = dc_strdup("Video");          break;
case DC_MSG_VOICE: p = dc_strdup("Voice");          break;
default:           p = dc_mprintf("%i", msg->type); break;
}
dc_strbuilder_catf(&ret, "Type: %s\n", p);
free(p);
p = dc_msg_get_filemime(msg);
dc_strbuilder_catf(&ret, "Mimetype: %s\n", p);
free(p);
}
int w = dc_param_get_int(msg->param, DC_PARAM_WIDTH, 0);
int h = dc_param_get_int(msg->param, DC_PARAM_HEIGHT, 0);
if (w!=0 || h!=0) {
p = dc_mprintf("Dimension: %i x %i\n", w, h); dc_strbuilder_cat(&ret, p); free(p);
}
int duration = dc_param_get_int(msg->param, DC_PARAM_DURATION, 0);
if (duration!=0) {
p = dc_mprintf("Duration: %i ms\n", duration); dc_strbuilder_cat(&ret, p); free(p);
}
if (rawtxt && rawtxt[0]) {
dc_strbuilder_cat(&ret, "\n");
dc_strbuilder_cat(&ret, rawtxt);
dc_strbuilder_cat(&ret, "\n");
}
if (msg->rfc724_mid && msg->rfc724_mid[0]) {
dc_strbuilder_catf(&ret, "\nMessage-ID: %s", msg->rfc724_mid);
}
if (msg->server_folder && msg->server_folder[0]) {
dc_strbuilder_catf(&ret, "\nLast seen as: %s/%i", msg->server_folder, (int)msg->server_uid);
}
cleanup:
sqlite3_finalize(stmt);
dc_msg_unref(msg);
dc_contact_unref(contact_from);
free(rawtxt);
return ret.buf;
}
char* dc_get_mime_headers(dc_context_t* context, uint32_t msg_id)
{
char*         eml = NULL;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT mime_headers FROM msgs WHERE id=?;");
sqlite3_bind_int(stmt, 1, msg_id);
if (sqlite3_step(stmt)==SQLITE_ROW) {
eml = dc_strdup_keep_null((const char*)sqlite3_column_text(stmt, 0));
}
cleanup:
sqlite3_finalize(stmt);
return eml;
}
void dc_star_msgs(dc_context_t* context, const uint32_t* msg_ids, int msg_cnt, int star)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || msg_ids==NULL || msg_cnt<=0 || (star!=0 && star!=1)) {
return;
}
dc_sqlite3_begin_transaction(context->sql);
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE msgs SET starred=? WHERE id=?;");
for (int i = 0; i < msg_cnt; i++)
{
sqlite3_reset(stmt);
sqlite3_bind_int(stmt, 1, star);
sqlite3_bind_int(stmt, 2, msg_ids[i]);
sqlite3_step(stmt);
}
sqlite3_finalize(stmt);
dc_sqlite3_commit(context->sql);
}
void dc_delete_msg_from_db(dc_context_t* context, uint32_t msg_id)
{
dc_msg_t*     msg = dc_msg_new_untyped(context);
sqlite3_stmt* stmt = NULL;
if (!dc_msg_load_from_db(msg, context, msg_id)) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"DELETE FROM msgs WHERE id=?;");
sqlite3_bind_int(stmt, 1, msg->id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
stmt = NULL;
stmt = dc_sqlite3_prepare(context->sql,
"DELETE FROM msgs_mdns WHERE msg_id=?;");
sqlite3_bind_int(stmt, 1, msg->id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
stmt = NULL;
cleanup:
sqlite3_finalize(stmt);
dc_msg_unref(msg);
}
void dc_delete_msgs(dc_context_t* context, const uint32_t* msg_ids, int msg_cnt)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || msg_ids==NULL || msg_cnt<=0) {
return;
}
dc_sqlite3_begin_transaction(context->sql);
for (int i = 0; i < msg_cnt; i++)
{
dc_update_msg_chat_id(context, msg_ids[i], DC_CHAT_ID_TRASH);
dc_job_add(context, DC_JOB_DELETE_MSG_ON_IMAP, msg_ids[i], NULL, 0);
}
dc_sqlite3_commit(context->sql);
if (msg_cnt) {
context->cb(context, DC_EVENT_MSGS_CHANGED, 0, 0);
dc_job_kill_action(context, DC_JOB_HOUSEKEEPING);
dc_job_add(context, DC_JOB_HOUSEKEEPING, 0, NULL, DC_HOUSEKEEPING_DELAY_SEC);
}
}
void dc_markseen_msgs(dc_context_t* context, const uint32_t* msg_ids, int msg_cnt)
{
int transaction_pending = 0;
int i = 0;
int send_event = 0;
int curr_state = 0;
int curr_blocked = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || msg_ids==NULL || msg_cnt<=0) {
goto cleanup;
}
dc_sqlite3_begin_transaction(context->sql);
transaction_pending = 1;
stmt = dc_sqlite3_prepare(context->sql,
"SELECT m.state, c.blocked "
" FROM msgs m "
" LEFT JOIN chats c ON c.id=m.chat_id "
" WHERE m.id=? AND m.chat_id>" DC_STRINGIFY(DC_CHAT_ID_LAST_SPECIAL));
for (i = 0; i < msg_cnt; i++)
{
sqlite3_reset(stmt);
sqlite3_bind_int(stmt, 1, msg_ids[i]);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
continue;
}
curr_state   = sqlite3_column_int(stmt, 0);
curr_blocked = sqlite3_column_int(stmt, 1);
if (curr_blocked==0)
{
if (curr_state==DC_STATE_IN_FRESH || curr_state==DC_STATE_IN_NOTICED) {
dc_update_msg_state(context, msg_ids[i], DC_STATE_IN_SEEN);
dc_log_info(context, 0, "Seen message #%i.", msg_ids[i]);
dc_job_add(context, DC_JOB_MARKSEEN_MSG_ON_IMAP, msg_ids[i], NULL, 0);
send_event = 1;
}
}
else
{
if (curr_state==DC_STATE_IN_FRESH) {
dc_update_msg_state(context, msg_ids[i], DC_STATE_IN_NOTICED);
send_event = 1;
}
}
}
dc_sqlite3_commit(context->sql);
transaction_pending = 0;
if (send_event) {
context->cb(context, DC_EVENT_MSGS_CHANGED, 0, 0);
}
cleanup:
if (transaction_pending) { dc_sqlite3_rollback(context->sql); }
sqlite3_finalize(stmt);
}
int dc_mdn_from_ext(dc_context_t* context, uint32_t from_id, const char* rfc724_mid, time_t timestamp_sent,
uint32_t* ret_chat_id, uint32_t* ret_msg_id)
{
int           read_by_all = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || from_id<=DC_CONTACT_ID_LAST_SPECIAL || rfc724_mid==NULL || ret_chat_id==NULL || ret_msg_id==NULL
|| *ret_chat_id!=0 || *ret_msg_id!=0) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT m.id, c.id, c.type, m.state FROM msgs m "
" LEFT JOIN chats c ON m.chat_id=c.id "
" WHERE rfc724_mid=? AND from_id=1 "
" ORDER BY m.id;");
sqlite3_bind_text(stmt, 1, rfc724_mid, -1, SQLITE_STATIC);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
*ret_msg_id    = sqlite3_column_int(stmt, 0);
*ret_chat_id   = sqlite3_column_int(stmt, 1);
int chat_type  = sqlite3_column_int(stmt, 2);
int msg_state  = sqlite3_column_int(stmt, 3);
sqlite3_finalize(stmt);
stmt = NULL;
if (msg_state!=DC_STATE_OUT_PREPARING &&
msg_state!=DC_STATE_OUT_PENDING &&
msg_state!=DC_STATE_OUT_DELIVERED)
{
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT contact_id FROM msgs_mdns WHERE msg_id=? AND contact_id=?;");
sqlite3_bind_int(stmt, 1, *ret_msg_id);
sqlite3_bind_int(stmt, 2, from_id);
int mdn_already_in_table = (sqlite3_step(stmt)==SQLITE_ROW)? 1 : 0;
sqlite3_finalize(stmt);
stmt = NULL;
if (!mdn_already_in_table) {
stmt = dc_sqlite3_prepare(context->sql,
"INSERT INTO msgs_mdns (msg_id, contact_id, timestamp_sent) VALUES (?, ?, ?);");
sqlite3_bind_int  (stmt, 1, *ret_msg_id);
sqlite3_bind_int  (stmt, 2, from_id);
sqlite3_bind_int64(stmt, 3, timestamp_sent);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
stmt = NULL;
}
if (chat_type==DC_CHAT_TYPE_SINGLE) {
dc_update_msg_state(context, *ret_msg_id, DC_STATE_OUT_MDN_RCVD);
read_by_all = 1;
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM msgs_mdns WHERE msg_id=?;");
sqlite3_bind_int(stmt, 1, *ret_msg_id);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
int ist_cnt  = sqlite3_column_int(stmt, 0);
sqlite3_finalize(stmt);
stmt = NULL;
int soll_cnt = (dc_get_chat_contact_cnt(context, *ret_chat_id)+1) / 2;
if (ist_cnt < soll_cnt) {
goto cleanup;
}
dc_update_msg_state(context, *ret_msg_id, DC_STATE_OUT_MDN_RCVD);
read_by_all = 1;
cleanup:
sqlite3_finalize(stmt);
return read_by_all;
}