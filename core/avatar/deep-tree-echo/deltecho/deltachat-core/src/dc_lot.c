#include "dc_context.h"
#define DC_LOT_MAGIC 0x00107107
dc_lot_t* dc_lot_new()
{
dc_lot_t* lot = NULL;
if ((lot=calloc(1, sizeof(dc_lot_t)))==NULL) {
exit(27);
}
lot->magic = DC_LOT_MAGIC;
lot->text1_meaning  = 0;
return lot;
}
void dc_lot_unref(dc_lot_t* set)
{
if (set==NULL || set->magic!=DC_LOT_MAGIC) {
return;
}
dc_lot_empty(set);
set->magic = 0;
free(set);
}
void dc_lot_empty(dc_lot_t* lot)
{
if (lot==NULL || lot->magic!=DC_LOT_MAGIC) {
return;
}
free(lot->text1);
lot->text1 = NULL;
lot->text1_meaning = 0;
free(lot->text2);
lot->text2 = NULL;
free(lot->fingerprint);
lot->fingerprint = NULL;
free(lot->invitenumber);
lot->invitenumber = NULL;
free(lot->auth);
lot->auth = NULL;
lot->timestamp = 0;
lot->state = 0;
lot->id = 0;
}
char* dc_lot_get_text1(const dc_lot_t* lot)
{
if (lot==NULL || lot->magic!=DC_LOT_MAGIC) {
return NULL;
}
return dc_strdup_keep_null(lot->text1);
}
char* dc_lot_get_text2(const dc_lot_t* lot)
{
if (lot==NULL || lot->magic!=DC_LOT_MAGIC) {
return NULL;
}
return dc_strdup_keep_null(lot->text2);
}
int dc_lot_get_text1_meaning(const dc_lot_t* lot)
{
if (lot==NULL || lot->magic!=DC_LOT_MAGIC) {
return 0;
}
return lot->text1_meaning;
}
int dc_lot_get_state(const dc_lot_t* lot)
{
if (lot==NULL || lot->magic!=DC_LOT_MAGIC) {
return 0;
}
return lot->state;
}
uint32_t dc_lot_get_id(const dc_lot_t* lot)
{
if (lot==NULL || lot->magic!=DC_LOT_MAGIC) {
return 0;
}
return lot->id;
}
time_t dc_lot_get_timestamp(const dc_lot_t* lot)
{
if (lot==NULL || lot->magic!=DC_LOT_MAGIC) {
return 0;
}
return lot->timestamp;
}
void dc_lot_fill(dc_lot_t* lot, const dc_msg_t* msg, const dc_chat_t* chat, const dc_contact_t* contact, dc_context_t* context)
{
if (lot==NULL || lot->magic!=DC_LOT_MAGIC || msg==NULL) {
return;
}
if (msg->state==DC_STATE_OUT_DRAFT)
{
lot->text1 = dc_stock_str(context, DC_STR_DRAFT);
lot->text1_meaning = DC_TEXT1_DRAFT;
}
else if (msg->from_id==DC_CONTACT_ID_SELF)
{
if (dc_msg_is_info(msg) || dc_chat_is_self_talk(chat)) {
lot->text1 = NULL;
lot->text1_meaning = 0;
}
else {
lot->text1 = dc_stock_str(context, DC_STR_SELF);
lot->text1_meaning = DC_TEXT1_SELF;
}
}
else if (chat==NULL)
{
lot->text1 = NULL;
lot->text1_meaning = 0;
}
else if (DC_CHAT_TYPE_IS_MULTI(chat->type))
{
if (dc_msg_is_info(msg) || contact==NULL) {
lot->text1 = NULL;
lot->text1_meaning = 0;
}
else {
if (chat!=NULL && chat->id==DC_CHAT_ID_DEADDROP) {
lot->text1 = dc_contact_get_display_name(contact);
}
else {
lot->text1 = dc_contact_get_first_name(contact);
}
lot->text1_meaning = DC_TEXT1_USERNAME;
}
}
lot->text2     = dc_msg_get_summarytext_by_raw(msg->type, msg->text, msg->param, DC_SUMMARY_CHARACTERS, context);
lot->timestamp = dc_msg_get_timestamp(msg);
lot->state     = msg->state;
}