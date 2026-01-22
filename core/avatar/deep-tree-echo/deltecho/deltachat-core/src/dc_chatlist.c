#include "dc_context.h"
#define DC_CHATLIST_MAGIC 0xc4a71157
dc_chatlist_t* dc_chatlist_new(dc_context_t* context)
{
dc_chatlist_t* chatlist = NULL;
if ((chatlist=calloc(1, sizeof(dc_chatlist_t)))==NULL) {
exit(20);
}
chatlist->magic   = DC_CHATLIST_MAGIC;
chatlist->context = context;
if ((chatlist->chatNlastmsg_ids=dc_array_new(context, 128))==NULL) {
exit(32);
}
return chatlist;
}
void dc_chatlist_unref(dc_chatlist_t* chatlist)
{
if (chatlist==NULL || chatlist->magic!=DC_CHATLIST_MAGIC) {
return;
}
dc_chatlist_empty(chatlist);
dc_array_unref(chatlist->chatNlastmsg_ids);
chatlist->magic = 0;
free(chatlist);
}
void dc_chatlist_empty(dc_chatlist_t* chatlist)
{
if (chatlist==NULL || chatlist->magic!=DC_CHATLIST_MAGIC) {
return;
}
chatlist->cnt = 0;
dc_array_empty(chatlist->chatNlastmsg_ids);
}
size_t dc_chatlist_get_cnt(const dc_chatlist_t* chatlist)
{
if (chatlist==NULL || chatlist->magic!=DC_CHATLIST_MAGIC) {
return 0;
}
return chatlist->cnt;
}
uint32_t dc_chatlist_get_chat_id(const dc_chatlist_t* chatlist, size_t index)
{
if (chatlist==NULL || chatlist->magic!=DC_CHATLIST_MAGIC || chatlist->chatNlastmsg_ids==NULL || index>=chatlist->cnt) {
return 0;
}
return dc_array_get_id(chatlist->chatNlastmsg_ids, index*DC_CHATLIST_IDS_PER_RESULT);
}
uint32_t dc_chatlist_get_msg_id(const dc_chatlist_t* chatlist, size_t index)
{
if (chatlist==NULL || chatlist->magic!=DC_CHATLIST_MAGIC || chatlist->chatNlastmsg_ids==NULL || index>=chatlist->cnt) {
return 0;
}
return dc_array_get_id(chatlist->chatNlastmsg_ids, index*DC_CHATLIST_IDS_PER_RESULT+1);
}
dc_lot_t* dc_chatlist_get_summary(const dc_chatlist_t* chatlist, size_t index, dc_chat_t* chat )
{
dc_lot_t*      ret = dc_lot_new();
uint32_t       lastmsg_id = 0;
dc_msg_t*      lastmsg = NULL;
dc_contact_t*  lastcontact = NULL;
dc_chat_t*     chat_to_delete = NULL;
if (chatlist==NULL || chatlist->magic!=DC_CHATLIST_MAGIC || index>=chatlist->cnt) {
ret->text2 = dc_strdup("ErrBadChatlistIndex");
goto cleanup;
}
lastmsg_id = dc_array_get_id(chatlist->chatNlastmsg_ids, index*DC_CHATLIST_IDS_PER_RESULT+1);
if (chat==NULL) {
chat = dc_chat_new(chatlist->context);
chat_to_delete = chat;
if (!dc_chat_load_from_db(chat, dc_array_get_id(chatlist->chatNlastmsg_ids, index*DC_CHATLIST_IDS_PER_RESULT))) {
ret->text2 = dc_strdup("ErrCannotReadChat");
goto cleanup;
}
}
if (lastmsg_id)
{
lastmsg = dc_msg_new_untyped(chatlist->context);
dc_msg_load_from_db(lastmsg, chatlist->context, lastmsg_id);
if (lastmsg->from_id!=DC_CONTACT_ID_SELF  &&  DC_CHAT_TYPE_IS_MULTI(chat->type))
{
lastcontact = dc_contact_new(chatlist->context);
dc_contact_load_from_db(lastcontact, chatlist->context->sql, lastmsg->from_id);
}
}
if (chat->id==DC_CHAT_ID_ARCHIVED_LINK)
{
ret->text2 = dc_strdup(NULL);
}
else if (lastmsg==NULL || lastmsg->from_id==0)
{
ret->text2 = dc_stock_str(chatlist->context, DC_STR_NOMESSAGES);
}
else
{
dc_lot_fill(ret, lastmsg, chat, lastcontact, chatlist->context);
}
cleanup:
dc_msg_unref(lastmsg);
dc_contact_unref(lastcontact);
dc_chat_unref(chat_to_delete);
return ret;
}
dc_context_t* dc_chatlist_get_context(dc_chatlist_t* chatlist)
{
if (chatlist==NULL || chatlist->magic!=DC_CHATLIST_MAGIC) {
return NULL;
}
return chatlist->context;
}
static uint32_t get_last_deaddrop_fresh_msg(dc_context_t* context)
{
uint32_t      ret = 0;
sqlite3_stmt* stmt = NULL;
stmt = dc_sqlite3_prepare(context->sql,
"SELECT m.id "
" FROM msgs m "
" LEFT JOIN chats c ON c.id=m.chat_id "
" WHERE m.state=" DC_STRINGIFY(DC_STATE_IN_FRESH)
"   AND m.hidden=0 "
"   AND c.blocked=" DC_STRINGIFY(DC_CHAT_DEADDROP_BLOCKED)
" ORDER BY m.timestamp DESC, m.id DESC;");
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
ret = sqlite3_column_int(stmt, 0);
cleanup:
sqlite3_finalize(stmt);
return ret;
}
static int dc_chatlist_load_from_db(dc_chatlist_t* chatlist, int listflags, const char* query__, uint32_t query_contact_id)
{
int           success = 0;
int           add_archived_link_item = 0;
sqlite3_stmt* stmt = NULL;
char*         strLikeCmd = NULL;
char*         query = NULL;
if (chatlist==NULL || chatlist->magic!=DC_CHATLIST_MAGIC || chatlist->context==NULL) {
goto cleanup;
}
dc_chatlist_empty(chatlist);
#define QUR1 "SELECT c.id, m.id FROM chats c " \
" LEFT JOIN msgs m " \
"        ON c.id=m.chat_id " \
"       AND m.timestamp=( " \
"SELECT MAX(timestamp) " \
"  FROM msgs " \
" WHERE chat_id=c.id " \
"   AND (hidden=0 OR (hidden=1 AND state=" DC_STRINGIFY(DC_STATE_OUT_DRAFT) ")))" \
" WHERE c.id>" DC_STRINGIFY(DC_CHAT_ID_LAST_SPECIAL) \
"   AND c.blocked=0"
#define QUR2 " GROUP BY c.id " \
" ORDER BY IFNULL(m.timestamp,0) DESC, m.id DESC;"
if (query_contact_id)
{
stmt = dc_sqlite3_prepare(chatlist->context->sql,
QUR1 " AND c.id IN(SELECT chat_id FROM chats_contacts WHERE contact_id=?) " QUR2);
sqlite3_bind_int(stmt, 1, query_contact_id);
}
else if (listflags & DC_GCL_ARCHIVED_ONLY)
{
stmt = dc_sqlite3_prepare(chatlist->context->sql,
QUR1 " AND c.archived=1 " QUR2);
}
else if (query__==NULL)
{
if (!(listflags & DC_GCL_NO_SPECIALS)) {
uint32_t last_deaddrop_fresh_msg_id = get_last_deaddrop_fresh_msg(chatlist->context);
if (last_deaddrop_fresh_msg_id > 0) {
dc_array_add_id(chatlist->chatNlastmsg_ids, DC_CHAT_ID_DEADDROP);
dc_array_add_id(chatlist->chatNlastmsg_ids, last_deaddrop_fresh_msg_id);
}
add_archived_link_item = 1;
}
stmt = dc_sqlite3_prepare(chatlist->context->sql,
QUR1 " AND c.archived=0 " QUR2);
}
else
{
query = dc_strdup(query__);
dc_trim(query);
if (query[0]==0) {
success = 1;
goto cleanup;
}
strLikeCmd = dc_mprintf("%%%s%%", query);
stmt = dc_sqlite3_prepare(chatlist->context->sql,
QUR1 " AND c.name LIKE ? " QUR2);
sqlite3_bind_text(stmt, 1, strLikeCmd, -1, SQLITE_STATIC);
}
while (sqlite3_step(stmt)==SQLITE_ROW)
{
dc_array_add_id(chatlist->chatNlastmsg_ids, sqlite3_column_int(stmt, 0));
dc_array_add_id(chatlist->chatNlastmsg_ids, sqlite3_column_int(stmt, 1));
}
if (add_archived_link_item && dc_get_archived_cnt(chatlist->context)>0)
{
if (dc_array_get_cnt(chatlist->chatNlastmsg_ids)==0 && (listflags & DC_GCL_ADD_ALLDONE_HINT)) {
dc_array_add_id(chatlist->chatNlastmsg_ids, DC_CHAT_ID_ALLDONE_HINT);
dc_array_add_id(chatlist->chatNlastmsg_ids, 0);
}
dc_array_add_id(chatlist->chatNlastmsg_ids, DC_CHAT_ID_ARCHIVED_LINK);
dc_array_add_id(chatlist->chatNlastmsg_ids, 0);
}
chatlist->cnt = dc_array_get_cnt(chatlist->chatNlastmsg_ids)/DC_CHATLIST_IDS_PER_RESULT;
success = 1;
cleanup:
sqlite3_finalize(stmt);
free(query);
free(strLikeCmd);
return success;
}
int dc_get_archived_cnt(dc_context_t* context)
{
int ret = 0;
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM chats WHERE blocked=0 AND archived=1;");
if (sqlite3_step(stmt)==SQLITE_ROW) {
ret = sqlite3_column_int(stmt, 0);
}
sqlite3_finalize(stmt);
return ret;
}
dc_chatlist_t* dc_get_chatlist(dc_context_t* context, int listflags, const char* query_str, uint32_t query_id)
{
int            success = 0;
dc_chatlist_t* obj = dc_chatlist_new(context);
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
if (!dc_chatlist_load_from_db(obj, listflags, query_str, query_id)) {
goto cleanup;
}
success = 1;
cleanup:
if (success) {
return obj;
}
else {
dc_chatlist_unref(obj);
return NULL;
}
}