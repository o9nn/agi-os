#include <assert.h>
#include "dc_context.h"
#include "dc_job.h"
#include "dc_smtp.h"
#include "dc_imap.h"
#include "dc_mimefactory.h"
#include "dc_apeerstate.h"
#define DC_CHAT_MAGIC 0xc4a7c4a7
dc_chat_t* dc_chat_new(dc_context_t* context)
{
dc_chat_t* chat = NULL;
if (context==NULL || (chat=calloc(1, sizeof(dc_chat_t)))==NULL) {
exit(14);
}
chat->magic = DC_CHAT_MAGIC;
chat->context = context;
chat->type = DC_CHAT_TYPE_UNDEFINED;
chat->param = dc_param_new();
return chat;
}
void dc_chat_unref(dc_chat_t* chat)
{
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return;
}
dc_chat_empty(chat);
dc_param_unref(chat->param);
chat->magic = 0;
free(chat);
}
void dc_chat_empty(dc_chat_t* chat)
{
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return;
}
free(chat->name);
chat->name = NULL;
chat->type = DC_CHAT_TYPE_UNDEFINED;
chat->id = 0;
free(chat->grpid);
chat->grpid = NULL;
chat->blocked = 0;
chat->gossiped_timestamp = 0;
dc_param_set_packed(chat->param, NULL);
}
uint32_t dc_chat_get_id(const dc_chat_t* chat)
{
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return 0;
}
return chat->id;
}
int dc_chat_get_type(const dc_chat_t* chat)
{
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return DC_CHAT_TYPE_UNDEFINED;
}
return chat->type;
}
char* dc_chat_get_name(const dc_chat_t* chat)
{
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return dc_strdup("Err");
}
return dc_strdup(chat->name);
}
char* dc_chat_get_subtitle(const dc_chat_t* chat)
{
char* ret = NULL;
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return dc_strdup("Err");
}
if (chat->type==DC_CHAT_TYPE_SINGLE && dc_param_exists(chat->param, DC_PARAM_SELFTALK))
{
ret = dc_stock_str(chat->context, DC_STR_SELFTALK_SUBTITLE);
}
else if (chat->type==DC_CHAT_TYPE_SINGLE)
{
int r;
sqlite3_stmt* stmt = dc_sqlite3_prepare(chat->context->sql,
"SELECT c.addr FROM chats_contacts cc "
" LEFT JOIN contacts c ON c.id=cc.contact_id "
" WHERE cc.chat_id=?;");
sqlite3_bind_int(stmt, 1, chat->id);
r = sqlite3_step(stmt);
if (r==SQLITE_ROW) {
ret = dc_strdup((const char*)sqlite3_column_text(stmt, 0));
}
sqlite3_finalize(stmt);
}
else if (DC_CHAT_TYPE_IS_MULTI(chat->type))
{
int cnt = 0;
if (chat->id==DC_CHAT_ID_DEADDROP)
{
ret = dc_stock_str(chat->context, DC_STR_DEADDROP);
}
else
{
cnt = dc_get_chat_contact_cnt(chat->context, chat->id);
ret = dc_stock_str_repl_int(chat->context, DC_STR_MEMBER, cnt );
}
}
return ret? ret : dc_strdup("Err");
}
char* dc_chat_get_profile_image(const dc_chat_t* chat)
{
char* image_rel = NULL;
char* image_abs = NULL;
dc_array_t* contacts = NULL;
dc_contact_t* contact = NULL;
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
goto cleanup;
}
image_rel = dc_param_get(chat->param, DC_PARAM_PROFILE_IMAGE, NULL);
if (image_rel && image_rel[0]) {
image_abs = dc_get_abs_path(chat->context, image_rel);
}
else if(chat->type==DC_CHAT_TYPE_SINGLE) {
contacts = dc_get_chat_contacts(chat->context, chat->id);
if (contacts->count >= 1) {
contact = dc_get_contact(chat->context, contacts->array[0]);
image_abs = dc_contact_get_profile_image(contact);
}
}
cleanup:
free(image_rel);
dc_array_unref(contacts);
dc_contact_unref(contact);
return image_abs;
}
uint32_t dc_chat_get_color(const dc_chat_t* chat)
{
uint32_t color = 0;
dc_array_t* contacts = NULL;
dc_contact_t* contact = NULL;
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
goto cleanup;
}
if(chat->type==DC_CHAT_TYPE_SINGLE) {
contacts = dc_get_chat_contacts(chat->context, chat->id);
if (contacts->count >= 1) {
contact = dc_get_contact(chat->context, contacts->array[0]);
color = dc_str_to_color(contact->addr);
}
}
else {
color = dc_str_to_color(chat->name);
}
cleanup:
dc_array_unref(contacts);
dc_contact_unref(contact);
return color;
}
int dc_chat_get_archived(const dc_chat_t* chat)
{
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return 0;
}
return chat->archived;
}
int dc_chat_is_unpromoted(const dc_chat_t* chat)
{
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return 0;
}
return dc_param_get_int(chat->param, DC_PARAM_UNPROMOTED, 0);
}
int dc_chat_is_verified(const dc_chat_t* chat)
{
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return 0;
}
return (chat->type==DC_CHAT_TYPE_VERIFIED_GROUP);
}
int dc_chat_is_self_talk(const dc_chat_t* chat)
{
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return 0;
}
return dc_param_exists(chat->param, DC_PARAM_SELFTALK);
}
int dc_chat_is_sending_locations(const dc_chat_t* chat)
{
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
return 0;
}
return chat->is_sending_locations;
}
int dc_chat_update_param(dc_chat_t* chat)
{
int success = 0;
sqlite3_stmt* stmt = dc_sqlite3_prepare(chat->context->sql,
"UPDATE chats SET param=? WHERE id=?");
sqlite3_bind_text(stmt, 1, chat->param->packed, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 2, chat->id);
success = (sqlite3_step(stmt)==SQLITE_DONE)? 1 : 0;
sqlite3_finalize(stmt);
return success;
}
static int set_from_stmt(dc_chat_t* chat, sqlite3_stmt* row)
{
int row_offset = 0;
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC || row==NULL) {
return 0;
}
dc_chat_empty(chat);
#define CHAT_FIELDS " c.id,c.type,c.name, c.grpid,c.param,c.archived, c.blocked, c.gossiped_timestamp, c.locations_send_until "
chat->id = sqlite3_column_int (row, row_offset++);
chat->type = sqlite3_column_int (row, row_offset++);
chat->name = dc_strdup((char*)sqlite3_column_text (row, row_offset++));
chat->grpid = dc_strdup((char*)sqlite3_column_text (row, row_offset++));
dc_param_set_packed(chat->param, (char*)sqlite3_column_text (row, row_offset++));
chat->archived = sqlite3_column_int (row, row_offset++);
chat->blocked = sqlite3_column_int (row, row_offset++);
chat->gossiped_timestamp = sqlite3_column_int64(row, row_offset++);
chat->is_sending_locations = (sqlite3_column_int64(row, row_offset++)>time(NULL));
if (chat->id==DC_CHAT_ID_DEADDROP) {
free(chat->name);
chat->name = dc_stock_str(chat->context, DC_STR_DEADDROP);
}
else if (chat->id==DC_CHAT_ID_ARCHIVED_LINK) {
free(chat->name);
char* tempname = dc_stock_str(chat->context, DC_STR_ARCHIVEDCHATS);
chat->name = dc_mprintf("%s (%i)", tempname, dc_get_archived_cnt(chat->context));
free(tempname);
}
else if (chat->id==DC_CHAT_ID_STARRED) {
free(chat->name);
chat->name = dc_stock_str(chat->context, DC_STR_STARREDMSGS);
}
else if (dc_param_exists(chat->param, DC_PARAM_SELFTALK)) {
free(chat->name);
chat->name = dc_stock_str(chat->context, DC_STR_SELF);
}
return row_offset;
}
int dc_chat_load_from_db(dc_chat_t* chat, uint32_t chat_id)
{
int success = 0;
sqlite3_stmt* stmt = NULL;
if (chat==NULL || chat->magic!=DC_CHAT_MAGIC) {
goto cleanup;
}
dc_chat_empty(chat);
stmt = dc_sqlite3_prepare(chat->context->sql,
"SELECT " CHAT_FIELDS " FROM chats c WHERE c.id=?;");
sqlite3_bind_int(stmt, 1, chat_id);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
if (!set_from_stmt(chat, stmt)) {
goto cleanup;
}
success = 1;
cleanup:
sqlite3_finalize(stmt);
return success;
}
void dc_set_gossiped_timestamp(dc_context_t* context,
uint32_t chat_id, time_t timestamp)
{
sqlite3_stmt* stmt = NULL;
if (chat_id) {
dc_log_info(context, 0, "set gossiped_timestamp for chat #%i to %i.",
(int)chat_id, (int)timestamp);
stmt = dc_sqlite3_prepare(context->sql,
"UPDATE chats SET gossiped_timestamp=? WHERE id=?;");
sqlite3_bind_int64(stmt, 1, timestamp);
sqlite3_bind_int (stmt, 2, chat_id);
}
else {
dc_log_info(context, 0, "set gossiped_timestamp for all chats to %i.",
(int)timestamp);
stmt = dc_sqlite3_prepare(context->sql,
"UPDATE chats SET gossiped_timestamp=?;");
sqlite3_bind_int64(stmt, 1, timestamp);
}
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
void dc_reset_gossiped_timestamp(dc_context_t* context, uint32_t chat_id)
{
dc_set_gossiped_timestamp(context, chat_id, 0);
}
size_t dc_get_chat_cnt(dc_context_t* context)
{
size_t ret = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || context->sql->cobj==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM chats WHERE id>" DC_STRINGIFY(DC_CHAT_ID_LAST_SPECIAL) " AND blocked=0;");
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
ret = sqlite3_column_int(stmt, 0);
cleanup:
sqlite3_finalize(stmt);
return ret;
}
int dc_add_to_chat_contacts_table(dc_context_t* context, uint32_t chat_id, uint32_t contact_id)
{
int ret = 0;
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"INSERT INTO chats_contacts (chat_id, contact_id) VALUES(?, ?)");
sqlite3_bind_int(stmt, 1, chat_id);
sqlite3_bind_int(stmt, 2, contact_id);
ret = (sqlite3_step(stmt)==SQLITE_DONE)? 1 : 0;
sqlite3_finalize(stmt);
return ret;
}
dc_chat_t* dc_get_chat(dc_context_t* context, uint32_t chat_id)
{
int success = 0;
dc_chat_t* obj = dc_chat_new(context);
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
if (!dc_chat_load_from_db(obj, chat_id)) {
goto cleanup;
}
success = 1;
cleanup:
if (success) {
return obj;
}
else {
dc_chat_unref(obj);
return NULL;
}
}
void dc_marknoticed_chat(dc_context_t* context, uint32_t chat_id)
{
sqlite3_stmt* check = NULL;
sqlite3_stmt* update = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
check = dc_sqlite3_prepare(context->sql,
"SELECT id FROM msgs "
" WHERE chat_id=? AND state=" DC_STRINGIFY(DC_STATE_IN_FRESH) ";");
sqlite3_bind_int(check, 1, chat_id);
if (sqlite3_step(check)!=SQLITE_ROW) {
goto cleanup;
}
update = dc_sqlite3_prepare(context->sql,
"UPDATE msgs "
"   SET state=" DC_STRINGIFY(DC_STATE_IN_NOTICED)
" WHERE chat_id=? AND state=" DC_STRINGIFY(DC_STATE_IN_FRESH) ";");
sqlite3_bind_int(update, 1, chat_id);
sqlite3_step(update);
context->cb(context, DC_EVENT_MSGS_CHANGED, 0, 0);
cleanup:
sqlite3_finalize(check);
sqlite3_finalize(update);
}
void dc_marknoticed_all_chats(dc_context_t* context)
{
sqlite3_stmt* check = NULL;
sqlite3_stmt* update = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
check = dc_sqlite3_prepare(context->sql,
"SELECT id FROM msgs "
" WHERE state=" DC_STRINGIFY(DC_STATE_IN_FRESH) ";");
if (sqlite3_step(check)!=SQLITE_ROW) {
goto cleanup;
}
update = dc_sqlite3_prepare(context->sql,
"UPDATE msgs "
"   SET state=" DC_STRINGIFY(DC_STATE_IN_NOTICED)
" WHERE state=" DC_STRINGIFY(DC_STATE_IN_FRESH) ";");
sqlite3_step(update);
context->cb(context, DC_EVENT_MSGS_CHANGED, 0, 0);
cleanup:
sqlite3_finalize(check);
sqlite3_finalize(update);
}
uint32_t dc_get_chat_id_by_contact_id(dc_context_t* context, uint32_t contact_id)
{
uint32_t chat_id = 0;
int chat_id_blocked = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return 0;
}
dc_lookup_real_nchat_by_contact_id(context, contact_id, &chat_id, &chat_id_blocked);
return chat_id_blocked? 0 : chat_id;
}
uint32_t dc_get_chat_id_by_grpid(dc_context_t* context, const char* grpid, int* ret_blocked, int* ret_verified)
{
uint32_t chat_id = 0;
sqlite3_stmt* stmt = NULL;
if(ret_blocked) { *ret_blocked = 0; }
if(ret_verified) { *ret_verified = 0; }
if (context==NULL || grpid==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT id, blocked, type FROM chats WHERE grpid=?;");
sqlite3_bind_text (stmt, 1, grpid, -1, SQLITE_STATIC);
if (sqlite3_step(stmt)==SQLITE_ROW) {
chat_id = sqlite3_column_int(stmt, 0);
if(ret_blocked) { *ret_blocked = sqlite3_column_int(stmt, 1); }
if(ret_verified) { *ret_verified = (sqlite3_column_int(stmt, 2)==DC_CHAT_TYPE_VERIFIED_GROUP); }
}
cleanup:
sqlite3_finalize(stmt);
return chat_id;
}
uint32_t dc_create_chat_by_contact_id(dc_context_t* context, uint32_t contact_id)
{
uint32_t chat_id = 0;
int chat_blocked = 0;
int send_event = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return 0;
}
dc_lookup_real_nchat_by_contact_id(context, contact_id, &chat_id, &chat_blocked);
if (chat_id) {
if (chat_blocked) {
dc_unblock_chat(context, chat_id);
send_event = 1;
}
goto cleanup;
}
if (0==dc_real_contact_exists(context, contact_id) && contact_id!=DC_CONTACT_ID_SELF) {
dc_log_warning(context, 0, "Cannot create chat, contact %i does not exist.", (int)contact_id);
goto cleanup;
}
dc_create_or_lookup_nchat_by_contact_id(context, contact_id, DC_CHAT_NOT_BLOCKED, &chat_id, NULL);
if (chat_id) {
send_event = 1;
}
dc_scaleup_contact_origin(context, contact_id, DC_ORIGIN_CREATE_CHAT);
cleanup:
if (send_event) {
context->cb(context, DC_EVENT_MSGS_CHANGED, 0, 0);
}
return chat_id;
}
uint32_t dc_create_chat_by_msg_id(dc_context_t* context, uint32_t msg_id)
{
uint32_t chat_id = 0;
int send_event = 0;
dc_msg_t* msg = dc_msg_new_untyped(context);
dc_chat_t* chat = dc_chat_new(context);
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
if (!dc_msg_load_from_db(msg, context, msg_id)
|| !dc_chat_load_from_db(chat, msg->chat_id)
|| chat->id<=DC_CHAT_ID_LAST_SPECIAL) {
goto cleanup;
}
chat_id = chat->id;
if (chat->blocked) {
dc_unblock_chat(context, chat->id);
send_event = 1;
}
dc_scaleup_contact_origin(context, msg->from_id, DC_ORIGIN_CREATE_CHAT);
cleanup:
dc_msg_unref(msg);
dc_chat_unref(chat);
if (send_event) {
context->cb(context, DC_EVENT_MSGS_CHANGED, 0, 0);
}
return chat_id;
}
dc_array_t* dc_get_chat_media(dc_context_t* context, uint32_t chat_id,
int msg_type, int msg_type2, int msg_type3)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return NULL;
}
dc_array_t* ret = dc_array_new(context, 100);
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"SELECT id FROM msgs WHERE chat_id=? AND (type=? OR type=? OR type=?) ORDER BY timestamp, id;");
sqlite3_bind_int(stmt, 1, chat_id);
sqlite3_bind_int(stmt, 2, msg_type);
sqlite3_bind_int(stmt, 3, msg_type2>0? msg_type2 : msg_type);
sqlite3_bind_int(stmt, 4, msg_type3>0? msg_type3 : msg_type);
while (sqlite3_step(stmt)==SQLITE_ROW) {
dc_array_add_id(ret, sqlite3_column_int(stmt, 0));
}
sqlite3_finalize(stmt);
return ret;
}
uint32_t dc_get_next_media(dc_context_t* context, uint32_t curr_msg_id, int dir,
int msg_type, int msg_type2, int msg_type3)
{
uint32_t ret_msg_id = 0;
dc_msg_t* msg = dc_msg_new_untyped(context);
dc_array_t* list = NULL;
int i = 0;
int cnt = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
if (!dc_msg_load_from_db(msg, context, curr_msg_id)) {
goto cleanup;
}
if ((list=dc_get_chat_media(context, msg->chat_id,
msg_type>0? msg_type : msg->type,
msg_type2, msg_type3))==NULL) {
goto cleanup;
}
cnt = dc_array_get_cnt(list);
for (i = 0; i < cnt; i++) {
if (curr_msg_id==dc_array_get_id(list, i))
{
if (dir > 0) {
if (i+1 < cnt) {
ret_msg_id = dc_array_get_id(list, i+1);
}
}
else if (dir < 0) {
if (i-1 >= 0) {
ret_msg_id = dc_array_get_id(list, i-1);
}
}
break;
}
}
cleanup:
dc_array_unref(list);
dc_msg_unref(msg);
return ret_msg_id;
}
dc_array_t* dc_get_chat_contacts(dc_context_t* context, uint32_t chat_id)
{
dc_array_t* ret = dc_array_new(context, 100);
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
if (chat_id==DC_CHAT_ID_DEADDROP) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT cc.contact_id FROM chats_contacts cc"
" LEFT JOIN contacts c ON c.id=cc.contact_id"
" WHERE cc.chat_id=?"
" ORDER BY c.id=1, LOWER(c.name||c.addr), c.id;");
sqlite3_bind_int(stmt, 1, chat_id);
while (sqlite3_step(stmt)==SQLITE_ROW) {
dc_array_add_id(ret, sqlite3_column_int(stmt, 0));
}
cleanup:
sqlite3_finalize(stmt);
return ret;
}
dc_array_t* dc_get_chat_msgs(dc_context_t* context, uint32_t chat_id, uint32_t flags, uint32_t marker1before)
{
int success = 0;
dc_array_t* ret = dc_array_new(context, 512);
sqlite3_stmt* stmt = NULL;
uint32_t curr_id;
time_t curr_local_timestamp;
int curr_day, last_day = 0;
long cnv_to_local = dc_gm2local_offset();
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || ret==NULL) {
goto cleanup;
}
if (chat_id==DC_CHAT_ID_DEADDROP)
{
int show_emails = dc_sqlite3_get_config_int(context->sql,
"show_emails", DC_SHOW_EMAILS_DEFAULT);
stmt = dc_sqlite3_prepare(context->sql,
"SELECT m.id, m.timestamp"
" FROM msgs m"
" LEFT JOIN chats ON m.chat_id=chats.id"
" LEFT JOIN contacts ON m.from_id=contacts.id"
" WHERE m.from_id!=" DC_STRINGIFY(DC_CONTACT_ID_SELF)
"   AND m.from_id!=" DC_STRINGIFY(DC_CONTACT_ID_DEVICE)
"   AND m.hidden=0 "
"   AND chats.blocked=" DC_STRINGIFY(DC_CHAT_DEADDROP_BLOCKED)
"   AND contacts.blocked=0"
"   AND m.msgrmsg>=? "
" ORDER BY m.timestamp,m.id;");
sqlite3_bind_int(stmt, 1, show_emails==DC_SHOW_EMAILS_ALL? 0 : 1);
}
else if (chat_id==DC_CHAT_ID_STARRED)
{
stmt = dc_sqlite3_prepare(context->sql,
"SELECT m.id, m.timestamp"
" FROM msgs m"
" LEFT JOIN contacts ct ON m.from_id=ct.id"
" WHERE m.starred=1 "
"   AND m.hidden=0 "
"   AND ct.blocked=0"
" ORDER BY m.timestamp,m.id;");
}
else
{
stmt = dc_sqlite3_prepare(context->sql,
"SELECT m.id, m.timestamp"
" FROM msgs m"
" WHERE m.chat_id=? "
"   AND m.hidden=0 "
" ORDER BY m.timestamp,m.id;");
sqlite3_bind_int(stmt, 1, chat_id);
}
while (sqlite3_step(stmt)==SQLITE_ROW)
{
curr_id = sqlite3_column_int(stmt, 0);
if (curr_id==marker1before) {
dc_array_add_id(ret, DC_MSG_ID_MARKER1);
}
if (flags&DC_GCM_ADDDAYMARKER) {
curr_local_timestamp = (time_t)sqlite3_column_int64(stmt, 1) + cnv_to_local;
curr_day = curr_local_timestamp/DC_SECONDS_PER_DAY;
if (curr_day!=last_day) {
dc_array_add_id(ret, DC_MSG_ID_DAYMARKER);
last_day = curr_day;
}
}
dc_array_add_id(ret, curr_id);
}
success = 1;
cleanup:
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
static uint32_t get_draft_msg_id(dc_context_t* context, uint32_t chat_id)
{
uint32_t draft_msg_id = 0;
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"SELECT id FROM msgs WHERE chat_id=? AND state=?;");
sqlite3_bind_int(stmt, 1, chat_id);
sqlite3_bind_int(stmt, 2, DC_STATE_OUT_DRAFT);
if (sqlite3_step(stmt)==SQLITE_ROW) {
draft_msg_id = sqlite3_column_int(stmt, 0);
}
sqlite3_finalize(stmt);
return draft_msg_id;
}
static int set_draft_raw(dc_context_t* context, uint32_t chat_id, dc_msg_t* msg)
{
sqlite3_stmt* stmt = NULL;
char* pathNfilename = NULL;
uint32_t prev_draft_msg_id = 0;
int sth_changed = 0;
prev_draft_msg_id = get_draft_msg_id(context, chat_id);
if (prev_draft_msg_id) {
dc_delete_msg_from_db(context, prev_draft_msg_id);
sth_changed = 1;
}
if (msg==NULL)
{
goto cleanup;
}
else if (msg->type==DC_MSG_TEXT)
{
if (msg->text==NULL || msg->text[0]==0) {
goto cleanup;
}
}
else if (DC_MSG_NEEDS_ATTACHMENT(msg->type))
{
pathNfilename = dc_param_get(msg->param, DC_PARAM_FILE, NULL);
if (pathNfilename==NULL) {
goto cleanup;
}
if (dc_msg_is_increation(msg) && !dc_is_blobdir_path(context, pathNfilename)) {
goto cleanup;
}
if (!dc_make_rel_and_copy(context, &pathNfilename)) {
goto cleanup;
}
dc_param_set(msg->param, DC_PARAM_FILE, pathNfilename);
}
else
{
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"INSERT INTO msgs (chat_id, from_id, timestamp,"
" type, state, txt, param, hidden)"
" VALUES (?,?,?, ?,?,?,?,?);");
sqlite3_bind_int (stmt, 1, chat_id);
sqlite3_bind_int (stmt, 2, DC_CONTACT_ID_SELF);
sqlite3_bind_int64(stmt, 3, time(NULL));
sqlite3_bind_int (stmt, 4, msg->type);
sqlite3_bind_int (stmt, 5, DC_STATE_OUT_DRAFT);
sqlite3_bind_text (stmt, 6, msg->text? msg->text : "", -1, SQLITE_STATIC);
sqlite3_bind_text (stmt, 7, msg->param->packed, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 8, 1);
if (sqlite3_step(stmt)!=SQLITE_DONE) {
goto cleanup;
}
sth_changed = 1;
cleanup:
sqlite3_finalize(stmt);
free(pathNfilename);
return sth_changed;
}
void dc_set_draft(dc_context_t* context, uint32_t chat_id, dc_msg_t* msg)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || chat_id<=DC_CHAT_ID_LAST_SPECIAL) {
return;
}
if (set_draft_raw(context, chat_id, msg)) {
context->cb(context, DC_EVENT_MSGS_CHANGED, chat_id, 0);
}
}
dc_msg_t* dc_get_draft(dc_context_t* context, uint32_t chat_id)
{
uint32_t draft_msg_id = 0;
dc_msg_t* draft_msg = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC
|| chat_id<=DC_CHAT_ID_LAST_SPECIAL) {
return NULL;
}
draft_msg_id = get_draft_msg_id(context, chat_id);
if (draft_msg_id==0) {
return NULL;
}
draft_msg = dc_msg_new_untyped(context);
if (!dc_msg_load_from_db(draft_msg, context, draft_msg_id)) {
dc_msg_unref(draft_msg);
return NULL;
}
return draft_msg;
}
void dc_lookup_real_nchat_by_contact_id(dc_context_t* context, uint32_t contact_id, uint32_t* ret_chat_id, int* ret_chat_blocked)
{
sqlite3_stmt* stmt = NULL;
if (ret_chat_id) { *ret_chat_id = 0; }
if (ret_chat_blocked) { *ret_chat_blocked = 0; }
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || context->sql->cobj==NULL) {
return;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT c.id, c.blocked"
" FROM chats c"
" INNER JOIN chats_contacts j ON c.id=j.chat_id"
" WHERE c.type=" DC_STRINGIFY(DC_CHAT_TYPE_SINGLE) " AND c.id>" DC_STRINGIFY(DC_CHAT_ID_LAST_SPECIAL) " AND j.contact_id=?;");
sqlite3_bind_int(stmt, 1, contact_id);
if (sqlite3_step(stmt)==SQLITE_ROW) {
if (ret_chat_id) { *ret_chat_id = sqlite3_column_int(stmt, 0); }
if (ret_chat_blocked) { *ret_chat_blocked = sqlite3_column_int(stmt, 1); }
}
sqlite3_finalize(stmt);
}
void dc_create_or_lookup_nchat_by_contact_id(dc_context_t* context, uint32_t contact_id, int create_blocked, uint32_t* ret_chat_id, int* ret_chat_blocked)
{
uint32_t chat_id = 0;
int chat_blocked = 0;
dc_contact_t* contact = NULL;
char* chat_name = NULL;
char* q = NULL;
sqlite3_stmt* stmt = NULL;
if (ret_chat_id) { *ret_chat_id = 0; }
if (ret_chat_blocked) { *ret_chat_blocked = 0; }
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || context->sql->cobj==NULL) {
return;
}
if (contact_id==0) {
return;
}
dc_lookup_real_nchat_by_contact_id(context, contact_id, &chat_id, &chat_blocked);
if (chat_id!=0) {
if (ret_chat_id) { *ret_chat_id = chat_id; }
if (ret_chat_blocked) { *ret_chat_blocked = chat_blocked; }
return;
}
contact = dc_contact_new(context);
if (!dc_contact_load_from_db(contact, context->sql, contact_id)) {
goto cleanup;
}
chat_name = (contact->name&&contact->name[0])? contact->name : contact->addr;
q = sqlite3_mprintf("INSERT INTO chats (type, name, param, blocked, grpid) VALUES(%i, %Q, %Q, %i, %Q)", DC_CHAT_TYPE_SINGLE, chat_name,
contact_id==DC_CONTACT_ID_SELF? "K=1" : "", create_blocked, contact->addr);
assert( DC_PARAM_SELFTALK=='K');
stmt = dc_sqlite3_prepare(context->sql, q);
if (stmt==NULL) {
goto cleanup;
}
if (sqlite3_step(stmt)!=SQLITE_DONE) {
goto cleanup;
}
chat_id = dc_sqlite3_get_rowid(context->sql, "chats", "grpid", contact->addr);
sqlite3_free(q);
q = NULL;
sqlite3_finalize(stmt);
stmt = NULL;
q = sqlite3_mprintf("INSERT INTO chats_contacts (chat_id, contact_id) VALUES(%i, %i)", chat_id, contact_id);
stmt = dc_sqlite3_prepare(context->sql, q);
if (sqlite3_step(stmt)!=SQLITE_DONE) {
goto cleanup;
}
sqlite3_free(q);
q = NULL;
sqlite3_finalize(stmt);
stmt = NULL;
cleanup:
sqlite3_free(q);
sqlite3_finalize(stmt);
dc_contact_unref(contact);
if (ret_chat_id) { *ret_chat_id = chat_id; }
if (ret_chat_blocked) { *ret_chat_blocked = create_blocked; }
}
int dc_get_msg_cnt(dc_context_t* context, uint32_t chat_id)
{
int ret = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM msgs WHERE chat_id=?;");
sqlite3_bind_int(stmt, 1, chat_id);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
ret = sqlite3_column_int(stmt, 0);
cleanup:
sqlite3_finalize(stmt);
return ret;
}
void dc_unarchive_chat(dc_context_t* context, uint32_t chat_id)
{
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE chats SET archived=0 WHERE id=?");
sqlite3_bind_int (stmt, 1, chat_id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
int dc_get_fresh_msg_cnt(dc_context_t* context, uint32_t chat_id)
{
int ret = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM msgs "
" WHERE state=" DC_STRINGIFY(DC_STATE_IN_FRESH)
"   AND hidden=0 "
"   AND chat_id=?;");
sqlite3_bind_int(stmt, 1, chat_id);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
ret = sqlite3_column_int(stmt, 0);
cleanup:
sqlite3_finalize(stmt);
return ret;
}
void dc_archive_chat(dc_context_t* context, uint32_t chat_id, int archive)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || chat_id<=DC_CHAT_ID_LAST_SPECIAL || (archive!=0 && archive!=1)) {
return;
}
if (archive) {
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE msgs SET state=" DC_STRINGIFY(DC_STATE_IN_NOTICED)
" WHERE chat_id=? AND state=" DC_STRINGIFY(DC_STATE_IN_FRESH) ";");
sqlite3_bind_int(stmt, 1, chat_id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE chats SET archived=? WHERE id=?;");
sqlite3_bind_int (stmt, 1, archive);
sqlite3_bind_int (stmt, 2, chat_id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
context->cb(context, DC_EVENT_MSGS_CHANGED, 0, 0);
}
void dc_block_chat(dc_context_t* context, uint32_t chat_id, int new_blocking)
{
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
stmt = dc_sqlite3_prepare(context->sql,
"UPDATE chats SET blocked=? WHERE id=?;");
sqlite3_bind_int(stmt, 1, new_blocking);
sqlite3_bind_int(stmt, 2, chat_id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
void dc_unblock_chat(dc_context_t* context, uint32_t chat_id)
{
dc_block_chat(context, chat_id, DC_CHAT_NOT_BLOCKED);
}
void dc_delete_chat(dc_context_t* context, uint32_t chat_id)
{
int pending_transaction = 0;
dc_chat_t* obj = dc_chat_new(context);
char* q3 = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || chat_id<=DC_CHAT_ID_LAST_SPECIAL) {
goto cleanup;
}
if (!dc_chat_load_from_db(obj, chat_id)) {
goto cleanup;
}
dc_sqlite3_begin_transaction(context->sql);
pending_transaction = 1;
q3 = sqlite3_mprintf("DELETE FROM msgs_mdns WHERE msg_id IN (SELECT id FROM msgs WHERE chat_id=%i);", chat_id);
if (!dc_sqlite3_execute(context->sql, q3)) {
goto cleanup;
}
sqlite3_free(q3);
q3 = NULL;
q3 = sqlite3_mprintf("DELETE FROM msgs WHERE chat_id=%i;", chat_id);
if (!dc_sqlite3_execute(context->sql, q3)) {
goto cleanup;
}
sqlite3_free(q3);
q3 = NULL;
q3 = sqlite3_mprintf("DELETE FROM chats_contacts WHERE chat_id=%i;", chat_id);
if (!dc_sqlite3_execute(context->sql, q3)) {
goto cleanup;
}
sqlite3_free(q3);
q3 = NULL;
q3 = sqlite3_mprintf("DELETE FROM chats WHERE id=%i;", chat_id);
if (!dc_sqlite3_execute(context->sql, q3)) {
goto cleanup;
}
sqlite3_free(q3);
q3 = NULL;
dc_sqlite3_commit(context->sql);
pending_transaction = 0;
context->cb(context, DC_EVENT_MSGS_CHANGED, 0, 0);
dc_job_kill_action(context, DC_JOB_HOUSEKEEPING);
dc_job_add(context, DC_JOB_HOUSEKEEPING, 0, NULL, DC_HOUSEKEEPING_DELAY_SEC);
cleanup:
if (pending_transaction) { dc_sqlite3_rollback(context->sql); }
dc_chat_unref(obj);
sqlite3_free(q3);
}
#define IS_SELF_IN_GROUP (dc_is_contact_in_chat(context, chat_id, DC_CONTACT_ID_SELF)==1)
#define DO_SEND_STATUS_MAILS (dc_param_get_int(chat->param, DC_PARAM_UNPROMOTED, 0)==0)
int dc_is_group_explicitly_left(dc_context_t* context, const char* grpid)
{
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql, "SELECT id FROM leftgrps WHERE grpid=?;");
sqlite3_bind_text (stmt, 1, grpid, -1, SQLITE_STATIC);
int ret = (sqlite3_step(stmt)==SQLITE_ROW);
sqlite3_finalize(stmt);
return ret;
}
void dc_set_group_explicitly_left(dc_context_t* context, const char* grpid)
{
if (!dc_is_group_explicitly_left(context, grpid))
{
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql, "INSERT INTO leftgrps (grpid) VALUES(?);");
sqlite3_bind_text (stmt, 1, grpid, -1, SQLITE_STATIC);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
}
static int real_group_exists(dc_context_t* context, uint32_t chat_id)
{
sqlite3_stmt* stmt = NULL;
int ret = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || context->sql->cobj==NULL
|| chat_id<=DC_CHAT_ID_LAST_SPECIAL) {
return 0;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT id FROM chats "
" WHERE id=? "
"   AND (type=" DC_STRINGIFY(DC_CHAT_TYPE_GROUP) " OR type=" DC_STRINGIFY(DC_CHAT_TYPE_VERIFIED_GROUP) ");");
sqlite3_bind_int(stmt, 1, chat_id);
if (sqlite3_step(stmt)==SQLITE_ROW) {
ret = 1;
}
sqlite3_finalize(stmt);
return ret;
}
uint32_t dc_create_group_chat(dc_context_t* context, int verified, const char* chat_name)
{
uint32_t chat_id = 0;
char* draft_txt = NULL;
dc_msg_t* draft_msg = NULL;
char* grpid = NULL;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || chat_name==NULL || chat_name[0]==0) {
return 0;
}
draft_txt = dc_stock_str_repl_string(context, DC_STR_NEWGROUPDRAFT, chat_name);
grpid = dc_create_id();
stmt = dc_sqlite3_prepare(context->sql,
"INSERT INTO chats (type, name, grpid, param) VALUES(?, ?, ?, 'U=1');" );
sqlite3_bind_int (stmt, 1, verified? DC_CHAT_TYPE_VERIFIED_GROUP : DC_CHAT_TYPE_GROUP);
sqlite3_bind_text (stmt, 2, chat_name, -1, SQLITE_STATIC);
sqlite3_bind_text (stmt, 3, grpid, -1, SQLITE_STATIC);
if ( sqlite3_step(stmt)!=SQLITE_DONE) {
goto cleanup;
}
if ((chat_id=dc_sqlite3_get_rowid(context->sql, "chats", "grpid", grpid))==0) {
goto cleanup;
}
if (!dc_add_to_chat_contacts_table(context, chat_id, DC_CONTACT_ID_SELF)) {
goto cleanup;
}
draft_msg = dc_msg_new(context, DC_MSG_TEXT);
dc_msg_set_text(draft_msg, draft_txt);
set_draft_raw(context, chat_id, draft_msg);
cleanup:
sqlite3_finalize(stmt);
free(draft_txt);
dc_msg_unref(draft_msg);
free(grpid);
if (chat_id) {
context->cb(context, DC_EVENT_MSGS_CHANGED, 0, 0);
}
return chat_id;
}
int dc_set_chat_name(dc_context_t* context, uint32_t chat_id, const char* new_name)
{
int success = 0;
dc_chat_t* chat = dc_chat_new(context);
dc_msg_t* msg = dc_msg_new_untyped(context);
char* q3 = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || new_name==NULL || new_name[0]==0 || chat_id<=DC_CHAT_ID_LAST_SPECIAL) {
goto cleanup;
}
if (0==real_group_exists(context, chat_id)
|| 0==dc_chat_load_from_db(chat, chat_id)) {
goto cleanup;
}
if (strcmp(chat->name, new_name)==0) {
success = 1;
goto cleanup;
}
if (!IS_SELF_IN_GROUP) {
dc_log_event(context, DC_EVENT_ERROR_SELF_NOT_IN_GROUP, 0,
"Cannot set chat name; self not in group");
goto cleanup;
}
q3 = sqlite3_mprintf("UPDATE chats SET name=%Q WHERE id=%i;", new_name, chat_id);
if (!dc_sqlite3_execute(context->sql, q3)) {
goto cleanup;
}
if (DO_SEND_STATUS_MAILS)
{
msg->type = DC_MSG_TEXT;
msg->text = dc_stock_system_msg(context, DC_STR_MSGGRPNAME, chat->name, new_name, DC_CONTACT_ID_SELF);
dc_param_set_int(msg->param, DC_PARAM_CMD, DC_CMD_GROUPNAME_CHANGED);
dc_param_set (msg->param, DC_PARAM_CMD_ARG, chat->name);
msg->id = dc_send_msg(context, chat_id, msg);
context->cb(context, DC_EVENT_MSGS_CHANGED, chat_id, msg->id);
}
context->cb(context, DC_EVENT_CHAT_MODIFIED, chat_id, 0);
success = 1;
cleanup:
sqlite3_free(q3);
dc_chat_unref(chat);
dc_msg_unref(msg);
return success;
}
int dc_set_chat_profile_image(dc_context_t* context, uint32_t chat_id, const char* new_image )
{
int success = 0;
dc_chat_t* chat = dc_chat_new(context);
dc_msg_t* msg = dc_msg_new_untyped(context);
char* new_image_rel = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || chat_id<=DC_CHAT_ID_LAST_SPECIAL) {
goto cleanup;
}
if (0==real_group_exists(context, chat_id)
|| 0==dc_chat_load_from_db(chat, chat_id)) {
goto cleanup;
}
if (!IS_SELF_IN_GROUP) {
dc_log_event(context, DC_EVENT_ERROR_SELF_NOT_IN_GROUP, 0,
"Cannot set chat profile image; self not in group.");
goto cleanup;
}
if (new_image) {
new_image_rel = dc_strdup(new_image);
if (!dc_make_rel_and_copy(context, &new_image_rel)) {
goto cleanup;
}
}
dc_param_set(chat->param, DC_PARAM_PROFILE_IMAGE, new_image_rel);
if (!dc_chat_update_param(chat)) {
goto cleanup;
}
if (DO_SEND_STATUS_MAILS)
{
dc_param_set_int(msg->param, DC_PARAM_CMD, DC_CMD_GROUPIMAGE_CHANGED);
dc_param_set (msg->param, DC_PARAM_CMD_ARG, new_image_rel);
msg->type = DC_MSG_TEXT;
msg->text = dc_stock_system_msg(context, new_image_rel? DC_STR_MSGGRPIMGCHANGED : DC_STR_MSGGRPIMGDELETED, NULL, NULL, DC_CONTACT_ID_SELF);
msg->id = dc_send_msg(context, chat_id, msg);
context->cb(context, DC_EVENT_MSGS_CHANGED, chat_id, msg->id);
}
context->cb(context, DC_EVENT_CHAT_MODIFIED, chat_id, 0);
success = 1;
cleanup:
dc_chat_unref(chat);
dc_msg_unref(msg);
free(new_image_rel);
return success;
}
int dc_get_chat_contact_cnt(dc_context_t* context, uint32_t chat_id)
{
int ret = 0;
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM chats_contacts WHERE chat_id=?;");
sqlite3_bind_int(stmt, 1, chat_id);
if (sqlite3_step(stmt)==SQLITE_ROW) {
ret = sqlite3_column_int(stmt, 0);
}
sqlite3_finalize(stmt);
return ret;
}
int dc_is_contact_in_chat(dc_context_t* context, uint32_t chat_id, uint32_t contact_id)
{
int ret = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT contact_id FROM chats_contacts WHERE chat_id=? AND contact_id=?;");
sqlite3_bind_int(stmt, 1, chat_id);
sqlite3_bind_int(stmt, 2, contact_id);
ret = (sqlite3_step(stmt)==SQLITE_ROW)? 1 : 0;
cleanup:
sqlite3_finalize(stmt);
return ret;
}
int dc_add_contact_to_chat_ex(dc_context_t* context, uint32_t chat_id, uint32_t contact_id, int flags)
{
int success = 0;
dc_contact_t* contact = dc_get_contact(context, contact_id);
dc_chat_t* chat = dc_chat_new(context);
dc_msg_t* msg = dc_msg_new_untyped(context);
char* self_addr = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || contact==NULL || chat_id<=DC_CHAT_ID_LAST_SPECIAL) {
goto cleanup;
}
dc_reset_gossiped_timestamp(context, chat_id);
if (0==real_group_exists(context, chat_id)
|| (0==dc_real_contact_exists(context, contact_id) && contact_id!=DC_CONTACT_ID_SELF)
|| 0==dc_chat_load_from_db(chat, chat_id)) {
goto cleanup;
}
if (!IS_SELF_IN_GROUP) {
dc_log_event(context, DC_EVENT_ERROR_SELF_NOT_IN_GROUP, 0,
"Cannot add contact to group; self not in group.");
goto cleanup;
}
if ((flags&DC_FROM_HANDSHAKE) && dc_param_get_int(chat->param, DC_PARAM_UNPROMOTED, 0)==1) {
dc_param_set(chat->param, DC_PARAM_UNPROMOTED, NULL);
dc_chat_update_param(chat);
}
self_addr = dc_sqlite3_get_config(context->sql, "configured_addr", "");
if (strcasecmp(contact->addr, self_addr)==0) {
goto cleanup;
}
if (dc_is_contact_in_chat(context, chat_id, contact_id))
{
if (!(flags&DC_FROM_HANDSHAKE)) {
success = 1;
goto cleanup;
}
}
else
{
if (chat->type==DC_CHAT_TYPE_VERIFIED_GROUP)
{
if (dc_contact_is_verified(contact)!=DC_BIDIRECT_VERIFIED) {
dc_log_error(context, 0, "Only bidirectional verified contacts can be added to verified groups.");
goto cleanup;
}
}
if (0==dc_add_to_chat_contacts_table(context, chat_id, contact_id)) {
goto cleanup;
}
}
if (DO_SEND_STATUS_MAILS)
{
msg->type = DC_MSG_TEXT;
msg->text = dc_stock_system_msg(context, DC_STR_MSGADDMEMBER, contact->addr, NULL, DC_CONTACT_ID_SELF);
dc_param_set_int(msg->param, DC_PARAM_CMD, DC_CMD_MEMBER_ADDED_TO_GROUP);
dc_param_set (msg->param, DC_PARAM_CMD_ARG, contact->addr);
dc_param_set_int(msg->param, DC_PARAM_CMD_ARG2, flags);
msg->id = dc_send_msg(context, chat_id, msg);
context->cb(context, DC_EVENT_MSGS_CHANGED, chat_id, msg->id);
}
context->cb(context, DC_EVENT_CHAT_MODIFIED, chat_id, 0);
success = 1;
cleanup:
dc_chat_unref(chat);
dc_contact_unref(contact);
dc_msg_unref(msg);
free(self_addr);
return success;
}
int dc_add_contact_to_chat(dc_context_t* context, uint32_t chat_id, uint32_t contact_id )
{
return dc_add_contact_to_chat_ex(context, chat_id, contact_id, 0);
}
int dc_remove_contact_from_chat(dc_context_t* context, uint32_t chat_id, uint32_t contact_id )
{
int success = 0;
dc_contact_t* contact = dc_get_contact(context, contact_id);
dc_chat_t* chat = dc_chat_new(context);
dc_msg_t* msg = dc_msg_new_untyped(context);
char* q3 = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || chat_id<=DC_CHAT_ID_LAST_SPECIAL || (contact_id<=DC_CONTACT_ID_LAST_SPECIAL && contact_id!=DC_CONTACT_ID_SELF)) {
goto cleanup;
}
if (0==real_group_exists(context, chat_id)
|| 0==dc_chat_load_from_db(chat, chat_id)) {
goto cleanup;
}
if (!IS_SELF_IN_GROUP) {
dc_log_event(context, DC_EVENT_ERROR_SELF_NOT_IN_GROUP, 0,
"Cannot remove contact from chat; self not in group.");
goto cleanup;
}
if (contact)
{
if (DO_SEND_STATUS_MAILS)
{
msg->type = DC_MSG_TEXT;
if (contact->id==DC_CONTACT_ID_SELF) {
dc_set_group_explicitly_left(context, chat->grpid);
msg->text = dc_stock_system_msg(context, DC_STR_MSGGROUPLEFT, NULL, NULL, DC_CONTACT_ID_SELF);
}
else {
msg->text = dc_stock_system_msg(context, DC_STR_MSGDELMEMBER, contact->addr, NULL, DC_CONTACT_ID_SELF);
}
dc_param_set_int(msg->param, DC_PARAM_CMD, DC_CMD_MEMBER_REMOVED_FROM_GROUP);
dc_param_set (msg->param, DC_PARAM_CMD_ARG, contact->addr);
msg->id = dc_send_msg(context, chat_id, msg);
context->cb(context, DC_EVENT_MSGS_CHANGED, chat_id, msg->id);
}
}
q3 = sqlite3_mprintf("DELETE FROM chats_contacts WHERE chat_id=%i AND contact_id=%i;", chat_id, contact_id);
if (!dc_sqlite3_execute(context->sql, q3)) {
goto cleanup;
}
context->cb(context, DC_EVENT_CHAT_MODIFIED, chat_id, 0);
success = 1;
cleanup:
sqlite3_free(q3);
dc_chat_unref(chat);
dc_contact_unref(contact);
dc_msg_unref(msg);
return success;
}
static int last_msg_in_chat_encrypted(dc_sqlite3_t* sql, uint32_t chat_id)
{
int last_is_encrypted = 0;
sqlite3_stmt* stmt = dc_sqlite3_prepare(sql,
"SELECT param "
" FROM msgs "
" WHERE timestamp=(SELECT MAX(timestamp) FROM msgs WHERE chat_id=?) "
" ORDER BY id DESC;");
sqlite3_bind_int(stmt, 1, chat_id);
if (sqlite3_step(stmt)==SQLITE_ROW) {
dc_param_t* msg_param = dc_param_new();
dc_param_set_packed(msg_param, (char*)sqlite3_column_text(stmt, 0));
if (dc_param_exists(msg_param, DC_PARAM_GUARANTEE_E2EE)) {
last_is_encrypted = 1;
}
dc_param_unref(msg_param);
}
sqlite3_finalize(stmt);
return last_is_encrypted;
}
static int get_parent_mime_headers(const dc_chat_t* chat,
char** parent_rfc724_mid,
char** parent_in_reply_to,
char** parent_references)
{
int success = 0;
sqlite3_stmt* stmt = NULL;
if (chat==NULL
|| parent_rfc724_mid==NULL || parent_in_reply_to==NULL || parent_references==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(chat->context->sql,
"SELECT rfc724_mid, mime_in_reply_to, mime_references"
" FROM msgs"
" WHERE chat_id=? AND timestamp=(SELECT max(timestamp) FROM msgs WHERE chat_id=? AND from_id!=?);");
sqlite3_bind_int (stmt, 1, chat->id);
sqlite3_bind_int (stmt, 2, chat->id);
sqlite3_bind_int (stmt, 3, DC_CONTACT_ID_SELF);
if (sqlite3_step(stmt)==SQLITE_ROW) {
*parent_rfc724_mid = dc_strdup((const char*)sqlite3_column_text(stmt, 0));
*parent_in_reply_to = dc_strdup((const char*)sqlite3_column_text(stmt, 1));
*parent_references = dc_strdup((const char*)sqlite3_column_text(stmt, 2));
success = 1;
}
sqlite3_finalize(stmt);
stmt = NULL;
if (!success) {
stmt = dc_sqlite3_prepare(chat->context->sql,
"SELECT rfc724_mid, mime_in_reply_to, mime_references"
" FROM msgs"
" WHERE chat_id=? AND timestamp=(SELECT min(timestamp) FROM msgs WHERE chat_id=? AND from_id==?);");
sqlite3_bind_int (stmt, 1, chat->id);
sqlite3_bind_int (stmt, 2, chat->id);
sqlite3_bind_int (stmt, 3, DC_CONTACT_ID_SELF);
if (sqlite3_step(stmt)==SQLITE_ROW) {
*parent_rfc724_mid = dc_strdup((const char*)sqlite3_column_text(stmt, 0));
*parent_in_reply_to = dc_strdup((const char*)sqlite3_column_text(stmt, 1));
*parent_references = dc_strdup((const char*)sqlite3_column_text(stmt, 2));
success = 1;
}
}
cleanup:
sqlite3_finalize(stmt);
return success;
}
static uint32_t prepare_msg_raw(dc_context_t* context, dc_chat_t* chat, const dc_msg_t* msg, time_t timestamp)
{
char* parent_rfc724_mid = NULL;
char* parent_references = NULL;
char* parent_in_reply_to = NULL;
char* new_rfc724_mid = NULL;
char* new_references = NULL;
char* new_in_reply_to = NULL;
sqlite3_stmt* stmt = NULL;
uint32_t msg_id = 0;
uint32_t to_id = 0;
uint32_t location_id = 0;
if (!DC_CHAT_TYPE_CAN_SEND(chat->type)) {
dc_log_error(context, 0, "Cannot send to chat type #%i.", chat->type);
goto cleanup;
}
if (DC_CHAT_TYPE_IS_MULTI(chat->type) && !dc_is_contact_in_chat(context, chat->id, DC_CONTACT_ID_SELF)) {
dc_log_event(context, DC_EVENT_ERROR_SELF_NOT_IN_GROUP, 0,
"Cannot send message; self not in group.");
goto cleanup;
}
{
char* from = dc_sqlite3_get_config(context->sql, "configured_addr", NULL);
if (from==NULL) {
dc_log_error(context, 0, "Cannot send message, not configured.");
goto cleanup;
}
new_rfc724_mid = dc_create_outgoing_rfc724_mid(DC_CHAT_TYPE_IS_MULTI(chat->type)? chat->grpid : NULL, from);
free(from);
}
if (chat->type==DC_CHAT_TYPE_SINGLE)
{
stmt = dc_sqlite3_prepare(context->sql,
"SELECT contact_id FROM chats_contacts WHERE chat_id=?;");
sqlite3_bind_int(stmt, 1, chat->id);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
dc_log_error(context, 0, "Cannot send message, contact for chat #%i not found.", chat->id);
goto cleanup;
}
to_id = sqlite3_column_int(stmt, 0);
sqlite3_finalize(stmt);
stmt = NULL;
}
else if (DC_CHAT_TYPE_IS_MULTI(chat->type))
{
if (dc_param_get_int(chat->param, DC_PARAM_UNPROMOTED, 0)==1) {
dc_param_set(chat->param, DC_PARAM_UNPROMOTED, NULL);
dc_chat_update_param(chat);
}
}
int do_guarantee_e2ee = 0;
int e2ee_enabled = dc_sqlite3_get_config_int(context->sql, "e2ee_enabled", DC_E2EE_DEFAULT_ENABLED);
if (e2ee_enabled && dc_param_get_int(msg->param, DC_PARAM_FORCE_PLAINTEXT, 0)==0)
{
int can_encrypt = 1, all_mutual = 1;
stmt = dc_sqlite3_prepare(context->sql,
"SELECT ps.prefer_encrypted, c.addr"
" FROM chats_contacts cc "
" LEFT JOIN contacts c ON cc.contact_id=c.id "
" LEFT JOIN acpeerstates ps ON c.addr=ps.addr "
" WHERE cc.chat_id=? "
" AND cc.contact_id>" DC_STRINGIFY(DC_CONTACT_ID_LAST_SPECIAL) ";");
sqlite3_bind_int(stmt, 1, chat->id);
while (sqlite3_step(stmt)==SQLITE_ROW)
{
if (sqlite3_column_type(stmt, 0)==SQLITE_NULL) {
dc_log_info(context, 0, "[autocrypt] no peerstate for %s",
sqlite3_column_text(stmt, 1));
can_encrypt = 0;
all_mutual = 0;
}
else {
int prefer_encrypted = sqlite3_column_int(stmt, 0);
if (prefer_encrypted!=DC_PE_MUTUAL) {
dc_log_info(context, 0, "[autocrypt] peerstate for %s is %s",
sqlite3_column_text(stmt, 1),
prefer_encrypted==DC_PE_NOPREFERENCE? "NOPREFERENCE" : "RESET");
all_mutual = 0;
}
}
}
sqlite3_finalize(stmt);
stmt = NULL;
if (can_encrypt)
{
if (all_mutual) {
do_guarantee_e2ee = 1;
}
else {
if (last_msg_in_chat_encrypted(context->sql, chat->id)) {
do_guarantee_e2ee = 1;
}
}
}
}
if (do_guarantee_e2ee) {
dc_param_set_int(msg->param, DC_PARAM_GUARANTEE_E2EE, 1);
}
dc_param_set(msg->param, DC_PARAM_ERRONEOUS_E2EE, NULL);
if (!dc_chat_is_self_talk(chat)
&& get_parent_mime_headers(chat, &parent_rfc724_mid, &parent_in_reply_to, &parent_references))
{
if (parent_rfc724_mid && parent_rfc724_mid[0]) {
new_in_reply_to = dc_strdup(parent_rfc724_mid);
}
if (parent_references) {
char* space = NULL;
if ((space=strchr(parent_references, ' '))!=NULL) {
*space = 0;
}
}
if (parent_references && parent_references[0]
&& parent_rfc724_mid && parent_rfc724_mid[0]) {
new_references = dc_mprintf("%s %s", parent_references, parent_rfc724_mid);
}
else if (parent_references && parent_references[0]) {
new_references = dc_strdup(parent_references);
}
else if (parent_in_reply_to && parent_in_reply_to[0]
&& parent_rfc724_mid && parent_rfc724_mid[0]) {
new_references = dc_mprintf("%s %s", parent_in_reply_to, parent_rfc724_mid);
}
else if (parent_in_reply_to && parent_in_reply_to[0]) {
new_references = dc_strdup(parent_in_reply_to);
}
}
if (dc_param_exists(msg->param, DC_PARAM_SET_LATITUDE)) {
stmt = dc_sqlite3_prepare(context->sql,
"INSERT INTO locations "
" (timestamp,from_id,chat_id, latitude,longitude,independent)"
" VALUES (?,?,?, ?,?,1);");
sqlite3_bind_int64 (stmt, 1, timestamp);
sqlite3_bind_int (stmt, 2, DC_CONTACT_ID_SELF);
sqlite3_bind_int (stmt, 3, chat->id);
sqlite3_bind_double(stmt, 4, dc_param_get_float(msg->param, DC_PARAM_SET_LATITUDE, 0.0));
sqlite3_bind_double(stmt, 5, dc_param_get_float(msg->param, DC_PARAM_SET_LONGITUDE, 0.0));
sqlite3_step(stmt);
sqlite3_finalize(stmt);
stmt = NULL;
location_id = dc_sqlite3_get_rowid2(context->sql, "locations",
"timestamp", timestamp,
"from_id", DC_CONTACT_ID_SELF);
}
stmt = dc_sqlite3_prepare(context->sql,
"INSERT INTO msgs (rfc724_mid, chat_id, from_id, to_id, timestamp,"
" type, state, txt, param, hidden,"
" mime_in_reply_to, mime_references, location_id)"
" VALUES (?,?,?,?,?, ?,?,?,?,?, ?,?,?);");
sqlite3_bind_text (stmt, 1, new_rfc724_mid, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 2, chat->id);
sqlite3_bind_int (stmt, 3, DC_CONTACT_ID_SELF);
sqlite3_bind_int (stmt, 4, to_id);
sqlite3_bind_int64(stmt, 5, timestamp);
sqlite3_bind_int (stmt, 6, msg->type);
sqlite3_bind_int (stmt, 7, msg->state);
sqlite3_bind_text (stmt, 8, msg->text? msg->text : "", -1, SQLITE_STATIC);
sqlite3_bind_text (stmt, 9, msg->param->packed, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 10, msg->hidden);
sqlite3_bind_text (stmt, 11, new_in_reply_to, -1, SQLITE_STATIC);
sqlite3_bind_text (stmt, 12, new_references, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 13, location_id);
if (sqlite3_step(stmt)!=SQLITE_DONE) {
dc_log_error(context, 0, "Cannot send message, cannot insert to database.", chat->id);
goto cleanup;
}
msg_id = dc_sqlite3_get_rowid(context->sql, "msgs", "rfc724_mid", new_rfc724_mid);
cleanup:
free(parent_rfc724_mid);
free(parent_in_reply_to);
free(parent_references);
free(new_rfc724_mid);
free(new_in_reply_to);
free(new_references);
sqlite3_finalize(stmt);
return msg_id;
}
static uint32_t prepare_msg_common(dc_context_t* context, uint32_t chat_id, dc_msg_t* msg)
{
char* pathNfilename = NULL;
dc_chat_t* chat = NULL;
msg->id = 0;
msg->context = context;
if (msg->type==DC_MSG_TEXT)
{
;
}
else if (DC_MSG_NEEDS_ATTACHMENT(msg->type))
{
pathNfilename = dc_param_get(msg->param, DC_PARAM_FILE, NULL);
if (pathNfilename==NULL) {
dc_log_error(context, 0, "Attachment missing for message of type #%i.", (int)msg->type);
goto cleanup;
}
if (msg->state==DC_STATE_OUT_PREPARING && !dc_is_blobdir_path(context, pathNfilename)) {
dc_log_error(context, 0, "Files must be created in the blob-directory.");
goto cleanup;
}
if (!dc_make_rel_and_copy(context, &pathNfilename)) {
goto cleanup;
}
dc_param_set(msg->param, DC_PARAM_FILE, pathNfilename);
if (msg->type==DC_MSG_FILE || msg->type==DC_MSG_IMAGE)
{
int better_type = 0;
char* better_mime = NULL;
dc_msg_guess_msgtype_from_suffix(pathNfilename, &better_type, &better_mime);
if (better_type) {
msg->type = better_type;
dc_param_set(msg->param, DC_PARAM_MIMETYPE, better_mime);
}
free(better_mime);
}
else if (!dc_param_exists(msg->param, DC_PARAM_MIMETYPE))
{
char* better_mime = NULL;
dc_msg_guess_msgtype_from_suffix(pathNfilename, NULL, &better_mime);
dc_param_set(msg->param, DC_PARAM_MIMETYPE, better_mime);
free(better_mime);
}
dc_log_info(context, 0, "Attaching \"%s\" for message type #%i.", pathNfilename, (int)msg->type);
}
else
{
dc_log_error(context, 0, "Cannot send messages of type #%i.", (int)msg->type);
goto cleanup;
}
dc_unarchive_chat(context, chat_id);
context->smtp->log_connect_errors = 1;
chat = dc_chat_new(context);
if (dc_chat_load_from_db(chat, chat_id)) {
if (msg->state!=DC_STATE_OUT_PREPARING) msg->state = DC_STATE_OUT_PENDING;
msg->id = prepare_msg_raw(context, chat, msg, dc_create_smeared_timestamp(context));
msg->chat_id = chat_id;
}
cleanup:
dc_chat_unref(chat);
free(pathNfilename);
return msg->id;
}
uint32_t dc_prepare_msg(dc_context_t* context, uint32_t chat_id, dc_msg_t* msg)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || msg==NULL || chat_id<=DC_CHAT_ID_LAST_SPECIAL) {
return 0;
}
msg->state = DC_STATE_OUT_PREPARING;
uint32_t msg_id = prepare_msg_common(context, chat_id, msg);
context->cb(context, DC_EVENT_MSGS_CHANGED, msg->chat_id, msg->id);
if (dc_param_exists(msg->param, DC_PARAM_SET_LATITUDE)) {
context->cb(context, DC_EVENT_LOCATION_CHANGED, DC_CONTACT_ID_SELF, 0);
}
return msg_id;
}
uint32_t dc_send_msg(dc_context_t* context, uint32_t chat_id, dc_msg_t* msg)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || msg==NULL) {
return 0;
}
if (msg->state!=DC_STATE_OUT_PREPARING) {
if (!prepare_msg_common(context, chat_id, msg)) {
return 0;
};
}
else {
if (chat_id!=0 && chat_id!=msg->chat_id) {
return 0;
}
dc_update_msg_state(context, msg->id, DC_STATE_OUT_PENDING);
}
if (!dc_job_send_msg(context, msg->id)) {
return 0;
}
context->cb(context, DC_EVENT_MSGS_CHANGED, msg->chat_id, msg->id);
if (dc_param_exists(msg->param, DC_PARAM_SET_LATITUDE)) {
context->cb(context, DC_EVENT_LOCATION_CHANGED, DC_CONTACT_ID_SELF, 0);
}
if (!chat_id) {
char* forwards = dc_param_get(msg->param, DC_PARAM_PREP_FORWARDS, NULL);
if (forwards) {
char* p = forwards;
while (*p) {
int32_t id = strtol(p, &p, 10);
if (!id) break;
dc_msg_t* copy = dc_get_msg(context, id);
if (copy) {
dc_send_msg(context, 0, copy);
}
dc_msg_unref(copy);
}
dc_param_set(msg->param, DC_PARAM_PREP_FORWARDS, NULL);
dc_msg_save_param_to_disk(msg);
}
free(forwards);
}
return msg->id;
}
uint32_t dc_send_text_msg(dc_context_t* context, uint32_t chat_id, const char* text_to_send)
{
dc_msg_t* msg = dc_msg_new(context, DC_MSG_TEXT);
uint32_t ret = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || chat_id<=DC_CHAT_ID_LAST_SPECIAL || text_to_send==NULL) {
goto cleanup;
}
msg->text = dc_strdup(text_to_send);
ret = dc_send_msg(context, chat_id, msg);
cleanup:
dc_msg_unref(msg);
return ret;
}
void dc_add_device_msg(dc_context_t* context, uint32_t chat_id, const char* text)
{
uint32_t msg_id = 0;
sqlite3_stmt* stmt = NULL;
char* rfc724_mid = dc_create_outgoing_rfc724_mid(NULL, "@device");
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || text==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"INSERT INTO msgs (chat_id,from_id,to_id, timestamp,type,state, txt,rfc724_mid) VALUES (?,?,?, ?,?,?, ?,?);");
sqlite3_bind_int (stmt, 1, chat_id);
sqlite3_bind_int (stmt, 2, DC_CONTACT_ID_DEVICE);
sqlite3_bind_int (stmt, 3, DC_CONTACT_ID_DEVICE);
sqlite3_bind_int64(stmt, 4, dc_create_smeared_timestamp(context));
sqlite3_bind_int (stmt, 5, DC_MSG_TEXT);
sqlite3_bind_int (stmt, 6, DC_STATE_IN_NOTICED);
sqlite3_bind_text (stmt, 7, text, -1, SQLITE_STATIC);
sqlite3_bind_text (stmt, 8, rfc724_mid, -1, SQLITE_STATIC);
if (sqlite3_step(stmt)!=SQLITE_DONE) {
goto cleanup;
}
msg_id = dc_sqlite3_get_rowid(context->sql, "msgs", "rfc724_mid", rfc724_mid);
context->cb(context, DC_EVENT_MSGS_CHANGED, chat_id, msg_id);
cleanup:
free(rfc724_mid);
sqlite3_finalize(stmt);
}
void dc_forward_msgs(dc_context_t* context, const uint32_t* msg_ids, int msg_cnt, uint32_t chat_id)
{
dc_msg_t* msg = dc_msg_new_untyped(context);
dc_chat_t* chat = dc_chat_new(context);
dc_contact_t* contact = dc_contact_new(context);
int transaction_pending = 0;
carray* created_db_entries = carray_new(16);
char* idsstr = NULL;
char* q3 = NULL;
sqlite3_stmt* stmt = NULL;
time_t curr_timestamp = 0;
dc_param_t* original_param = dc_param_new();
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || msg_ids==NULL || msg_cnt<=0 || chat_id<=DC_CHAT_ID_LAST_SPECIAL) {
goto cleanup;
}
dc_sqlite3_begin_transaction(context->sql);
transaction_pending = 1;
dc_unarchive_chat(context, chat_id);
context->smtp->log_connect_errors = 1;
if (!dc_chat_load_from_db(chat, chat_id)) {
goto cleanup;
}
curr_timestamp = dc_create_smeared_timestamps(context, msg_cnt);
idsstr = dc_arr_to_string(msg_ids, msg_cnt);
q3 = sqlite3_mprintf("SELECT id FROM msgs WHERE id IN(%s) ORDER BY timestamp,id", idsstr);
stmt = dc_sqlite3_prepare(context->sql, q3);
while (sqlite3_step(stmt)==SQLITE_ROW)
{
int src_msg_id = sqlite3_column_int(stmt, 0);
if (!dc_msg_load_from_db(msg, context, src_msg_id)) {
goto cleanup;
}
dc_param_set_packed(original_param, msg->param->packed);
if (msg->from_id!=DC_CONTACT_ID_SELF) {
dc_param_set_int(msg->param, DC_PARAM_FORWARDED, 1);
}
dc_param_set(msg->param, DC_PARAM_GUARANTEE_E2EE, NULL);
dc_param_set(msg->param, DC_PARAM_FORCE_PLAINTEXT, NULL);
dc_param_set(msg->param, DC_PARAM_CMD, NULL);
uint32_t new_msg_id;
if (msg->state==DC_STATE_OUT_PREPARING) {
new_msg_id = prepare_msg_raw(context, chat, msg, curr_timestamp++);
dc_param_t* save_param = msg->param;
msg->param = original_param;
msg->id = src_msg_id;
{
char* old_fwd = dc_param_get(msg->param, DC_PARAM_PREP_FORWARDS, "");
char* new_fwd = dc_mprintf("%s %d", old_fwd, new_msg_id);
dc_param_set(msg->param, DC_PARAM_PREP_FORWARDS, new_fwd);
dc_msg_save_param_to_disk(msg);
free(new_fwd);
free(old_fwd);
}
msg->param = save_param;
}
else {
msg->state = DC_STATE_OUT_PENDING;
new_msg_id = prepare_msg_raw(context, chat, msg, curr_timestamp++);
dc_job_send_msg(context, new_msg_id);
}
carray_add(created_db_entries, (void*)(uintptr_t)chat_id, NULL);
carray_add(created_db_entries, (void*)(uintptr_t)new_msg_id, NULL);
}
dc_sqlite3_commit(context->sql);
transaction_pending = 0;
cleanup:
if (transaction_pending) { dc_sqlite3_rollback(context->sql); }
if (created_db_entries) {
size_t i, icnt = carray_count(created_db_entries);
for (i = 0; i < icnt; i += 2) {
context->cb(context, DC_EVENT_MSGS_CHANGED, (uintptr_t)carray_get(created_db_entries, i), (uintptr_t)carray_get(created_db_entries, i+1));
}
carray_free(created_db_entries);
}
dc_contact_unref(contact);
dc_msg_unref(msg);
dc_chat_unref(chat);
sqlite3_finalize(stmt);
free(idsstr);
sqlite3_free(q3);
dc_param_unref(original_param);
}