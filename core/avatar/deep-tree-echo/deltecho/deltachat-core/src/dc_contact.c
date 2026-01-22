#include "dc_context.h"
#include "dc_contact.h"
#include "dc_apeerstate.h"
#include "dc_loginparam.h"
#include "dc_pgp.h"
#define DC_CONTACT_MAGIC 0x0c047ac7
dc_contact_t* dc_contact_new(dc_context_t* context)
{
dc_contact_t* contact = NULL;
if ((contact=calloc(1, sizeof(dc_contact_t)))==NULL) {
exit(19);
}
contact->magic   = DC_CONTACT_MAGIC;
contact->context = context;
return contact;
}
void dc_contact_unref(dc_contact_t* contact)
{
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
return;
}
dc_contact_empty(contact);
contact->magic = 0;
free(contact);
}
void dc_contact_empty(dc_contact_t* contact)
{
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
return;
}
contact->id = 0;
free(contact->name);
contact->name = NULL;
free(contact->authname);
contact->authname = NULL;
free(contact->addr);
contact->addr = NULL;
contact->origin = 0;
contact->blocked = 0;
}
uint32_t dc_contact_get_id(const dc_contact_t* contact)
{
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
return 0;
}
return contact->id;
}
char* dc_contact_get_addr(const dc_contact_t* contact)
{
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
return dc_strdup(NULL);
}
return dc_strdup(contact->addr);
}
char* dc_contact_get_name(const dc_contact_t* contact)
{
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
return dc_strdup(NULL);
}
return dc_strdup(contact->name);
}
char* dc_contact_get_display_name(const dc_contact_t* contact)
{
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
return dc_strdup(NULL);
}
if (contact->name && contact->name[0]) {
return dc_strdup(contact->name);
}
return dc_strdup(contact->addr);
}
char* dc_contact_get_name_n_addr(const dc_contact_t* contact)
{
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
return dc_strdup(NULL);
}
if (contact->name && contact->name[0]) {
return dc_mprintf("%s (%s)", contact->name, contact->addr);
}
return dc_strdup(contact->addr);
}
char* dc_contact_get_first_name(const dc_contact_t* contact)
{
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
return dc_strdup(NULL);
}
if (contact->name && contact->name[0]) {
return dc_get_first_name(contact->name);
}
return dc_strdup(contact->addr);
}
char* dc_contact_get_profile_image(const dc_contact_t* contact)
{
char* selfavatar = NULL;
char* image_abs = NULL;
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
goto cleanup;
}
if (contact->id==DC_CONTACT_ID_SELF) {
selfavatar = dc_get_config(contact->context, "selfavatar");
if (selfavatar && selfavatar[0]) {
image_abs = dc_strdup(selfavatar);
}
}
cleanup:
free(selfavatar);
return image_abs;
}
uint32_t dc_contact_get_color(const dc_contact_t* contact)
{
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
return 0x000000;
}
return dc_str_to_color(contact->addr);
}
int dc_contact_is_blocked(const dc_contact_t* contact)
{
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
return 0;
}
return contact->blocked;
}
int dc_contact_is_verified_ex(dc_contact_t* contact, const dc_apeerstate_t* peerstate)
{
int              contact_verified = DC_NOT_VERIFIED;
dc_apeerstate_t* peerstate_to_delete = NULL;
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC) {
goto cleanup;
}
if (contact->id==DC_CONTACT_ID_SELF) {
contact_verified = DC_BIDIRECT_VERIFIED;
goto cleanup;
}
if (peerstate==NULL) {
peerstate_to_delete = dc_apeerstate_new(contact->context);
if (!dc_apeerstate_load_by_addr(peerstate_to_delete, contact->context->sql, contact->addr)) {
goto cleanup;
}
peerstate = peerstate_to_delete;
}
contact_verified = peerstate->verified_key? DC_BIDIRECT_VERIFIED : 0;
cleanup:
dc_apeerstate_unref(peerstate_to_delete);
return contact_verified;
}
int dc_contact_is_verified(dc_contact_t* contact)
{
return dc_contact_is_verified_ex(contact, NULL);
}
int dc_contact_load_from_db(dc_contact_t* contact, dc_sqlite3_t* sql, uint32_t contact_id)
{
int           success = 0;
sqlite3_stmt* stmt = NULL;
if (contact==NULL || contact->magic!=DC_CONTACT_MAGIC || sql==NULL) {
goto cleanup;
}
dc_contact_empty(contact);
if (contact_id==DC_CONTACT_ID_SELF)
{
contact->id   = contact_id;
contact->name = dc_stock_str(contact->context, DC_STR_SELF);
contact->addr = dc_sqlite3_get_config(sql, "configured_addr", "");
}
else
{
stmt = dc_sqlite3_prepare(sql,
"SELECT c.name, c.addr, c.origin, c.blocked, c.authname "
" FROM contacts c "
" WHERE c.id=?;");
sqlite3_bind_int(stmt, 1, contact_id);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
contact->id               = contact_id;
contact->name             = dc_strdup((char*)sqlite3_column_text (stmt, 0));
contact->addr             = dc_strdup((char*)sqlite3_column_text (stmt, 1));
contact->origin           =                  sqlite3_column_int  (stmt, 2);
contact->blocked          =                  sqlite3_column_int  (stmt, 3);
contact->authname         = dc_strdup((char*)sqlite3_column_text (stmt, 4));
}
success = 1;
cleanup:
sqlite3_finalize(stmt);
return success;
}
char* dc_get_first_name(const char* full_name)
{
char* first_name = dc_strdup(full_name);
char* p1 = strchr(first_name, ' ');
if (p1) {
*p1 = 0;
dc_rtrim(first_name);
if (first_name[0]==0) {
free(first_name);
first_name = dc_strdup(full_name);
}
}
return first_name;
}
void dc_normalize_name(char* full_name)
{
if (full_name==NULL) {
return;
}
dc_trim(full_name);
int len = strlen(full_name);
if (len > 0) {
char firstchar = full_name[0], lastchar = full_name[len-1];
if ((firstchar=='\'' && lastchar=='\'')
|| (firstchar=='"'  && lastchar=='"')
|| (firstchar=='<'  && lastchar=='>')) {
full_name[0]     = ' ';
full_name[len-1] = ' ';
}
}
char* p1 = strchr(full_name, ',');
if (p1) {
*p1 = 0;
char* last_name  = dc_strdup(full_name);
char* first_name = dc_strdup(p1+1);
dc_trim(last_name);
dc_trim(first_name);
strcpy(full_name, first_name);
strcat(full_name, " ");
strcat(full_name, last_name);
free(last_name);
free(first_name);
}
else {
dc_trim(full_name);
}
}
char* dc_addr_normalize(const char* addr)
{
char* addr_normalized = dc_strdup(addr);
dc_trim(addr_normalized);
if (strncmp(addr_normalized, "mailto:", 7)==0) {
char* old = addr_normalized;
addr_normalized = dc_strdup(&old[7]);
free(old);
dc_trim(addr_normalized);
}
return addr_normalized;
}
int dc_addr_cmp(const char* addr1, const char* addr2)
{
char* norm1 = dc_addr_normalize(addr1);
char* norm2 = dc_addr_normalize(addr2);
int ret = strcasecmp(addr1, addr2);
free(norm1);
free(norm2);
return ret;
}
int dc_addr_equals_self(dc_context_t* context, const char* addr)
{
int   ret             = 0;
char* normalized_addr = NULL;
char* self_addr       = NULL;
if (context==NULL || addr==NULL) {
goto cleanup;
}
normalized_addr = dc_addr_normalize(addr);
if (NULL==(self_addr=dc_sqlite3_get_config(context->sql, "configured_addr", NULL))) {
goto cleanup;
}
ret = strcasecmp(normalized_addr, self_addr)==0? 1 : 0;
cleanup:
free(self_addr);
free(normalized_addr);
return ret;
}
int dc_addr_equals_contact(dc_context_t* context, const char* addr, uint32_t contact_id)
{
int addr_are_equal = 0;
if (addr) {
dc_contact_t* contact = dc_contact_new(context);
if (dc_contact_load_from_db(contact, context->sql, contact_id)) {
if (contact->addr) {
char* normalized_addr = dc_addr_normalize(addr);
if (strcasecmp(contact->addr, normalized_addr)==0) {
addr_are_equal = 1;
}
free(normalized_addr);
}
}
dc_contact_unref(contact);
}
return addr_are_equal;
}
int dc_real_contact_exists(dc_context_t* context, uint32_t contact_id)
{
sqlite3_stmt* stmt = NULL;
int           ret = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || context->sql->cobj==NULL
|| contact_id<=DC_CONTACT_ID_LAST_SPECIAL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT id FROM contacts WHERE id=?;");
sqlite3_bind_int(stmt, 1, contact_id);
if (sqlite3_step(stmt)==SQLITE_ROW) {
ret = 1;
}
cleanup:
sqlite3_finalize(stmt);
return ret;
}
size_t dc_get_real_contact_cnt(dc_context_t* context)
{
size_t        ret = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || context->sql->cobj==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql, "SELECT COUNT(*) FROM contacts WHERE id>?;");
sqlite3_bind_int(stmt, 1, DC_CONTACT_ID_LAST_SPECIAL);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
ret = sqlite3_column_int(stmt, 0);
cleanup:
sqlite3_finalize(stmt);
return ret;
}
uint32_t dc_add_or_lookup_contact( dc_context_t* context,
const char*   name ,
const char*   addr__,
int           origin,
int*          sth_modified )
{
#define       CONTACT_MODIFIED 1
#define       CONTACT_CREATED  2
sqlite3_stmt* stmt = NULL;
uint32_t      row_id = 0;
int           dummy = 0;
char*         addr = NULL;
char*         addr_self = NULL;
char*         row_name = NULL;
char*         row_addr = NULL;
char*         row_authname = NULL;
if (sth_modified==NULL) {
sth_modified = &dummy;
}
*sth_modified = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || addr__==NULL || origin<=0) {
goto cleanup;
}
addr = dc_addr_normalize(addr__);
addr_self = dc_sqlite3_get_config(context->sql, "configured_addr", "");
if (strcasecmp(addr, addr_self)==0) {
row_id = DC_CONTACT_ID_SELF;
goto cleanup;
}
if (!dc_may_be_valid_addr(addr)) {
dc_log_warning(context, 0, "Bad address \"%s\" for contact \"%s\".", addr, name?name:"<unset>");
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT id, name, addr, origin, authname FROM contacts WHERE addr=? COLLATE NOCASE;");
sqlite3_bind_text(stmt, 1, (const char*)addr, -1, SQLITE_STATIC);
if (sqlite3_step(stmt)==SQLITE_ROW)
{
int         row_origin, update_addr = 0, update_name = 0, update_authname = 0;
row_id       = sqlite3_column_int(stmt, 0);
row_name     = dc_strdup((char*)sqlite3_column_text(stmt, 1));
row_addr     = dc_strdup((char*)sqlite3_column_text(stmt, 2));
row_origin   = sqlite3_column_int(stmt, 3);
row_authname = dc_strdup((char*)sqlite3_column_text(stmt, 4));
sqlite3_finalize (stmt);
stmt = NULL;
if (name && name[0]) {
if (row_name[0]) {
if (origin>=row_origin && strcmp(name, row_name)!=0) {
update_name = 1;
}
}
else {
update_name = 1;
}
if (origin==DC_ORIGIN_INCOMING_UNKNOWN_FROM && strcmp(name, row_authname)!=0) {
update_authname = 1;
}
}
if (origin>=row_origin && strcmp(addr, row_addr)!=0 ) {
update_addr = 1;
}
if (update_name || update_authname || update_addr || origin>row_origin)
{
stmt = dc_sqlite3_prepare(context->sql,
"UPDATE contacts SET name=?, addr=?, origin=?, authname=? WHERE id=?;");
sqlite3_bind_text(stmt, 1, update_name?       name   : row_name, -1, SQLITE_STATIC);
sqlite3_bind_text(stmt, 2, update_addr?       addr   : row_addr, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 3, origin>row_origin? origin : row_origin);
sqlite3_bind_text(stmt, 4, update_authname?   name   : row_authname, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 5, row_id);
sqlite3_step     (stmt);
sqlite3_finalize (stmt);
stmt = NULL;
if (update_name)
{
stmt = dc_sqlite3_prepare(context->sql,
"UPDATE chats SET name=? WHERE type=? AND id IN(SELECT chat_id FROM chats_contacts WHERE contact_id=?);");
sqlite3_bind_text(stmt, 1, name, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 2, DC_CHAT_TYPE_SINGLE);
sqlite3_bind_int (stmt, 3, row_id);
sqlite3_step     (stmt);
}
*sth_modified = CONTACT_MODIFIED;
}
}
else
{
sqlite3_finalize (stmt);
stmt = NULL;
stmt = dc_sqlite3_prepare(context->sql,
"INSERT INTO contacts (name, addr, origin) VALUES(?, ?, ?);");
sqlite3_bind_text(stmt, 1, name? name : "", -1, SQLITE_STATIC);
sqlite3_bind_text(stmt, 2, addr,    -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 3, origin);
if (sqlite3_step(stmt)==SQLITE_DONE)
{
row_id = dc_sqlite3_get_rowid(context->sql, "contacts", "addr", addr);
*sth_modified = CONTACT_CREATED;
}
else
{
dc_log_error(context, 0, "Cannot add contact.");
}
}
cleanup:
free(addr);
free(addr_self);
free(row_addr);
free(row_name);
free(row_authname);
sqlite3_finalize(stmt);
return row_id;
}
void dc_scaleup_contact_origin(dc_context_t* context, uint32_t contact_id, int origin)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE contacts SET origin=? WHERE id=? AND origin<?;");
sqlite3_bind_int(stmt, 1, origin);
sqlite3_bind_int(stmt, 2, contact_id);
sqlite3_bind_int(stmt, 3, origin);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
}
int dc_is_contact_blocked(dc_context_t* context, uint32_t contact_id)
{
int           is_blocked = 0;
dc_contact_t* contact = dc_contact_new(context);
if (dc_contact_load_from_db(contact, context->sql, contact_id)) {
if (contact->blocked) {
is_blocked = 1;
}
}
dc_contact_unref(contact);
return is_blocked;
}
int dc_get_contact_origin(dc_context_t* context, uint32_t contact_id, int* ret_blocked)
{
int           ret = 0;
int           dummy = 0; if (ret_blocked==NULL) { ret_blocked = &dummy; }
dc_contact_t* contact = dc_contact_new(context);
*ret_blocked = 0;
if (!dc_contact_load_from_db(contact, context->sql, contact_id)) {
goto cleanup;
}
if (contact->blocked) {
*ret_blocked = 1;
goto cleanup;
}
ret = contact->origin;
cleanup:
dc_contact_unref(contact);
return ret;
}
uint32_t dc_create_contact(dc_context_t* context, const char* name, const char* addr)
{
uint32_t contact_id = 0;
int      sth_modified = 0;
int      blocked = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || addr==NULL || addr[0]==0) {
goto cleanup;
}
contact_id = dc_add_or_lookup_contact(context, name, addr, DC_ORIGIN_MANUALLY_CREATED, &sth_modified);
blocked = dc_is_contact_blocked(context, contact_id);
context->cb(context, DC_EVENT_CONTACTS_CHANGED, sth_modified==CONTACT_CREATED? contact_id : 0, 0);
if (blocked) {
dc_block_contact(context, contact_id, 0);
}
cleanup:
return contact_id;
}
int dc_add_address_book(dc_context_t* context, const char* adr_book)
{
carray* lines = NULL;
size_t  i = 0;
size_t  iCnt = 0;
int     sth_modified = 0;
int     modify_cnt = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || adr_book==NULL) {
goto cleanup;
}
if ((lines=dc_split_into_lines(adr_book))==NULL) {
goto cleanup;
}
dc_sqlite3_begin_transaction(context->sql);
iCnt = carray_count(lines);
for (i = 0; i+1 < iCnt; i += 2) {
char* name = (char*)carray_get(lines, i);
char* addr = (char*)carray_get(lines, i+1);
dc_normalize_name(name);
dc_add_or_lookup_contact(context, name, addr, DC_ORIGIN_ADRESS_BOOK, &sth_modified);
if (sth_modified) {
modify_cnt++;
}
}
dc_sqlite3_commit(context->sql);
if (modify_cnt) {
context->cb(context, DC_EVENT_CONTACTS_CHANGED, 0, 0);
}
cleanup:
dc_free_splitted_lines(lines);
return modify_cnt;
}
int dc_may_be_valid_addr(const char* addr)
{
if (addr==NULL) {
return 0;
}
const char* at = strchr(addr, '@');
if (at==NULL || (at-addr)<1) {
return 0;
}
const char* dot = strchr(at, '.');
if (dot==NULL || (dot-at)<2 || dot[1]==0 || dot[2]==0) {
return 0;
}
return 1;
}
uint32_t dc_lookup_contact_id_by_addr(dc_context_t* context, const char* addr)
{
int           contact_id = 0;
char*         addr_normalized = NULL;
char*         addr_self = NULL;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || addr==NULL || addr[0]==0) {
goto cleanup;
}
addr_normalized = dc_addr_normalize(addr);
addr_self = dc_sqlite3_get_config(context->sql, "configured_addr", "");
if (strcasecmp(addr_normalized, addr_self)==0) {
contact_id = DC_CONTACT_ID_SELF;
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT id FROM contacts"
" WHERE addr=?1 COLLATE NOCASE"
" AND id>?2 AND origin>=?3 AND blocked=0;");
sqlite3_bind_text(stmt, 1, (const char*)addr_normalized, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 2, DC_CONTACT_ID_LAST_SPECIAL);
sqlite3_bind_int (stmt, 3, DC_ORIGIN_MIN_CONTACT_LIST);
if (sqlite3_step(stmt)==SQLITE_ROW) {
contact_id = sqlite3_column_int(stmt, 0);
}
cleanup:
sqlite3_finalize(stmt);
free(addr_normalized);
free(addr_self);
return contact_id;
}
dc_array_t* dc_get_contacts(dc_context_t* context, uint32_t listflags, const char* query)
{
char*         self_addr = NULL;
char*         self_name = NULL;
char*         self_name2 = NULL;
int           add_self = 0;
dc_array_t*   ret = dc_array_new(context, 100);
char*         s3strLikeCmd = NULL;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
self_addr = dc_sqlite3_get_config(context->sql, "configured_addr", "");
if ((listflags&DC_GCL_VERIFIED_ONLY) || query)
{
if ((s3strLikeCmd=sqlite3_mprintf("%%%s%%", query? query : ""))==NULL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT c.id FROM contacts c"
" LEFT JOIN acpeerstates ps ON c.addr=ps.addr "
" WHERE c.addr!=?1 AND c.id>?2 AND c.origin>=?3"
" AND c.blocked=0 AND (c.name LIKE ?4 OR c.addr LIKE ?5)"
" AND (1=?6 OR LENGTH(ps.verified_key_fingerprint)!=0) "
" ORDER BY LOWER(c.name||c.addr),c.id;");
sqlite3_bind_text(stmt, 1, self_addr, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 2, DC_CONTACT_ID_LAST_SPECIAL);
sqlite3_bind_int (stmt, 3, DC_ORIGIN_MIN_CONTACT_LIST);
sqlite3_bind_text(stmt, 4, s3strLikeCmd, -1, SQLITE_STATIC);
sqlite3_bind_text(stmt, 5, s3strLikeCmd, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 6, (listflags&DC_GCL_VERIFIED_ONLY)? 0 : 1);
self_name  = dc_sqlite3_get_config(context->sql, "displayname", "");
self_name2 = dc_stock_str(context, DC_STR_SELF);
if (query==NULL || dc_str_contains(self_addr, query) || dc_str_contains(self_name, query) || dc_str_contains(self_name2, query)) {
add_self = 1;
}
}
else
{
stmt = dc_sqlite3_prepare(context->sql,
"SELECT id FROM contacts"
" WHERE addr!=?1 AND id>?2 AND origin>=?3 AND blocked=0"
" ORDER BY LOWER(name||addr),id;");
sqlite3_bind_text(stmt, 1, self_addr, -1, SQLITE_STATIC);
sqlite3_bind_int (stmt, 2, DC_CONTACT_ID_LAST_SPECIAL);
sqlite3_bind_int (stmt, 3, DC_ORIGIN_MIN_CONTACT_LIST);
add_self = 1;
}
while (sqlite3_step(stmt)==SQLITE_ROW) {
dc_array_add_id(ret, sqlite3_column_int(stmt, 0));
}
if ((listflags&DC_GCL_ADD_SELF) && add_self) {
dc_array_add_id(ret, DC_CONTACT_ID_SELF);
}
cleanup:
sqlite3_finalize(stmt);
sqlite3_free(s3strLikeCmd);
free(self_addr);
free(self_name);
free(self_name2);
return ret;
}
dc_array_t* dc_get_blocked_contacts(dc_context_t* context)
{
dc_array_t*   ret = dc_array_new(context, 100);
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT id FROM contacts"
" WHERE id>? AND blocked!=0"
" ORDER BY LOWER(name||addr),id;");
sqlite3_bind_int(stmt, 1, DC_CONTACT_ID_LAST_SPECIAL);
while (sqlite3_step(stmt)==SQLITE_ROW) {
dc_array_add_id(ret, sqlite3_column_int(stmt, 0));
}
cleanup:
sqlite3_finalize(stmt);
return ret;
}
dc_contact_t* dc_get_contact(dc_context_t* context, uint32_t contact_id)
{
dc_contact_t* ret = dc_contact_new(context);
if (!dc_contact_load_from_db(ret, context->sql, contact_id)) {
dc_contact_unref(ret);
ret = NULL;
}
return ret;
}
void dc_marknoticed_contact(dc_context_t* context, uint32_t contact_id)
{
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
return;
}
sqlite3_stmt* stmt = dc_sqlite3_prepare(context->sql,
"UPDATE msgs SET state=" DC_STRINGIFY(DC_STATE_IN_NOTICED) " WHERE from_id=? AND state=" DC_STRINGIFY(DC_STATE_IN_FRESH) ";");
sqlite3_bind_int(stmt, 1, contact_id);
sqlite3_step(stmt);
sqlite3_finalize(stmt);
context->cb(context, DC_EVENT_MSGS_CHANGED, 0, 0);
}
int dc_get_blocked_cnt(dc_context_t* context)
{
int           ret = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM contacts"
" WHERE id>? AND blocked!=0");
sqlite3_bind_int(stmt, 1, DC_CONTACT_ID_LAST_SPECIAL);
if (sqlite3_step(stmt)!=SQLITE_ROW) {
goto cleanup;
}
ret = sqlite3_column_int(stmt, 0);
cleanup:
sqlite3_finalize(stmt);
return ret;
}
void dc_block_contact(dc_context_t* context, uint32_t contact_id, int new_blocking)
{
int           send_event = 0;
dc_contact_t* contact = dc_contact_new(context);
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || contact_id<=DC_CONTACT_ID_LAST_SPECIAL) {
goto cleanup;
}
if (dc_contact_load_from_db(contact, context->sql, contact_id)
&& contact->blocked!=new_blocking)
{
stmt = dc_sqlite3_prepare(context->sql,
"UPDATE contacts SET blocked=? WHERE id=?;");
sqlite3_bind_int(stmt, 1, new_blocking);
sqlite3_bind_int(stmt, 2, contact_id);
if (sqlite3_step(stmt)!=SQLITE_DONE) {
goto cleanup;
}
sqlite3_finalize(stmt);
stmt = NULL;
stmt = dc_sqlite3_prepare(context->sql,
"UPDATE chats SET blocked=? WHERE type=? AND id IN (SELECT chat_id FROM chats_contacts WHERE contact_id=?);");
sqlite3_bind_int(stmt, 1, new_blocking);
sqlite3_bind_int(stmt, 2, DC_CHAT_TYPE_SINGLE);
sqlite3_bind_int(stmt, 3, contact_id);
if (sqlite3_step(stmt)!=SQLITE_DONE) {
goto cleanup;
}
dc_marknoticed_contact(context, contact_id);
send_event = 1;
}
if (send_event) {
context->cb(context, DC_EVENT_CONTACTS_CHANGED, 0, 0);
}
cleanup:
sqlite3_finalize(stmt);
dc_contact_unref(contact);
}
static void cat_fingerprint(dc_strbuilder_t* ret, const char* addr, const char* fingerprint_verified, const char* fingerprint_unverified)
{
dc_strbuilder_cat(ret, "\n\n");
dc_strbuilder_cat(ret, addr);
dc_strbuilder_cat(ret, ":\n");
dc_strbuilder_cat(ret, (fingerprint_verified&&fingerprint_verified[0])? fingerprint_verified : fingerprint_unverified);
if (fingerprint_verified && fingerprint_verified[0]
&& fingerprint_unverified && fingerprint_unverified[0]
&& strcmp(fingerprint_verified, fingerprint_unverified)!=0) {
dc_strbuilder_cat(ret, "\n\n");
dc_strbuilder_cat(ret, addr);
dc_strbuilder_cat(ret, " (alternative):\n");
dc_strbuilder_cat(ret, fingerprint_unverified);
}
}
char* dc_get_contact_encrinfo(dc_context_t* context, uint32_t contact_id)
{
dc_loginparam_t* loginparam = dc_loginparam_new();
dc_contact_t*    contact = dc_contact_new(context);
dc_apeerstate_t* peerstate = dc_apeerstate_new(context);
dc_key_t*        self_key = dc_key_new();
char*            fingerprint_self = NULL;
char*            fingerprint_other_verified = NULL;
char*            fingerprint_other_unverified = NULL;
char*            p = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC) {
goto cleanup;
}
dc_strbuilder_t  ret;
dc_strbuilder_init(&ret, 0);
if (!dc_contact_load_from_db(contact, context->sql, contact_id)) {
goto cleanup;
}
dc_apeerstate_load_by_addr(peerstate, context->sql, contact->addr);
dc_loginparam_read(loginparam, context->sql, "configured_");
dc_key_load_self_public(self_key, loginparam->addr, context->sql);
if (dc_apeerstate_peek_key(peerstate, DC_NOT_VERIFIED))
{
p = dc_stock_str(context, peerstate->prefer_encrypt==DC_PE_MUTUAL? DC_STR_E2E_PREFERRED : DC_STR_E2E_AVAILABLE); dc_strbuilder_cat(&ret, p); free(p);
if (self_key->binary==NULL) {
dc_pgp_rand_seed(context, peerstate->addr, strlen(peerstate->addr) );
dc_ensure_secret_key_exists(context);
dc_key_load_self_public(self_key, loginparam->addr, context->sql);
}
dc_strbuilder_cat(&ret, " ");
p = dc_stock_str(context, DC_STR_FINGERPRINTS); dc_strbuilder_cat(&ret, p); free(p);
dc_strbuilder_cat(&ret, ":");
fingerprint_self = dc_key_get_formatted_fingerprint(self_key);
fingerprint_other_verified = dc_key_get_formatted_fingerprint(dc_apeerstate_peek_key(peerstate, DC_BIDIRECT_VERIFIED));
fingerprint_other_unverified = dc_key_get_formatted_fingerprint(dc_apeerstate_peek_key(peerstate, DC_NOT_VERIFIED));
if (strcmp(loginparam->addr, peerstate->addr)<0) {
cat_fingerprint(&ret, loginparam->addr, fingerprint_self, NULL);
cat_fingerprint(&ret, peerstate->addr, fingerprint_other_verified, fingerprint_other_unverified);
}
else {
cat_fingerprint(&ret, peerstate->addr, fingerprint_other_verified, fingerprint_other_unverified);
cat_fingerprint(&ret, loginparam->addr, fingerprint_self, NULL);
}
}
else
{
if (!(loginparam->server_flags&DC_LP_IMAP_SOCKET_PLAIN)
&& !(loginparam->server_flags&DC_LP_SMTP_SOCKET_PLAIN))
{
p = dc_stock_str(context, DC_STR_ENCR_TRANSP); dc_strbuilder_cat(&ret, p); free(p);
}
else
{
p = dc_stock_str(context, DC_STR_ENCR_NONE); dc_strbuilder_cat(&ret, p); free(p);
}
}
cleanup:
dc_apeerstate_unref(peerstate);
dc_contact_unref(contact);
dc_loginparam_unref(loginparam);
dc_key_unref(self_key);
free(fingerprint_self);
free(fingerprint_other_verified);
free(fingerprint_other_unverified);
return ret.buf;
}
int dc_delete_contact(dc_context_t* context, uint32_t contact_id)
{
int           success = 0;
sqlite3_stmt* stmt = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || contact_id<=DC_CONTACT_ID_LAST_SPECIAL) {
goto cleanup;
}
stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM chats_contacts WHERE contact_id=?;");
sqlite3_bind_int(stmt, 1, contact_id);
if (sqlite3_step(stmt)!=SQLITE_ROW || sqlite3_column_int(stmt, 0) >= 1) {
goto cleanup;
}
sqlite3_finalize(stmt);
stmt = NULL;
stmt = dc_sqlite3_prepare(context->sql,
"SELECT COUNT(*) FROM msgs WHERE from_id=? OR to_id=?;");
sqlite3_bind_int(stmt, 1, contact_id);
sqlite3_bind_int(stmt, 2, contact_id);
if (sqlite3_step(stmt)!=SQLITE_ROW || sqlite3_column_int(stmt, 0) >= 1) {
goto cleanup;
}
sqlite3_finalize(stmt);
stmt = NULL;
stmt = dc_sqlite3_prepare(context->sql,
"DELETE FROM contacts WHERE id=?;");
sqlite3_bind_int(stmt, 1, contact_id);
if (sqlite3_step(stmt)!=SQLITE_DONE) {
goto cleanup;
}
context->cb(context, DC_EVENT_CONTACTS_CHANGED, 0, 0);
success = 1;
cleanup:
sqlite3_finalize(stmt);
return success;
}