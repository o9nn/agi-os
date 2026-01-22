#include "dc_context.h"
#include "dc_pgp.h"
#include "dc_aheader.h"
#include "dc_keyring.h"
#include "dc_mimeparser.h"
#include "dc_apeerstate.h"
static struct mailmime* new_data_part(void* data, size_t data_bytes, char* default_content_type, int default_encoding)
{
struct mailmime_mechanism * encoding;
struct mailmime_content * content;
struct mailmime * mime;
struct mailmime_fields * mime_fields;
int encoding_type;
char * content_type_str;
int do_encoding;
encoding = NULL;
if (default_content_type==NULL)
content_type_str = "application/octet-stream";
else
content_type_str = default_content_type;
content = mailmime_content_new_with_str(content_type_str);
if (content==NULL) {
goto free_content;
}
do_encoding = 1;
if (content->ct_type->tp_type==MAILMIME_TYPE_COMPOSITE_TYPE) {
struct mailmime_composite_type * composite;
composite = content->ct_type->tp_data.tp_composite_type;
switch (composite->ct_type) {
case MAILMIME_COMPOSITE_TYPE_MESSAGE:
if (strcasecmp(content->ct_subtype, "rfc822")==0)
do_encoding = 0;
break;
case MAILMIME_COMPOSITE_TYPE_MULTIPART:
do_encoding = 0;
break;
}
}
if (do_encoding) {
if (default_encoding==-1)
encoding_type = MAILMIME_MECHANISM_BASE64;
else
encoding_type = default_encoding;
encoding = mailmime_mechanism_new(encoding_type, NULL);
if (encoding==NULL) {
goto free_content;
}
}
mime_fields = mailmime_fields_new_with_data(encoding,
NULL, NULL, NULL, NULL);
if (mime_fields==NULL) {
goto free_content;
}
mime = mailmime_new_empty(content, mime_fields);
if (mime==NULL) {
goto free_mime_fields;
}
if (data!=NULL && data_bytes>0 && mime->mm_type==MAILMIME_SINGLE) {
mailmime_set_body_text(mime, data, data_bytes);
}
return mime;
goto err;
free_mime_fields:
mailmime_fields_free(mime_fields);
mailmime_content_free(content);
goto err;
free_content:
if (encoding!=NULL)
mailmime_mechanism_free(encoding);
if (content!=NULL)
mailmime_content_free(content);
err:
return NULL;
}
static int contains_report(struct mailmime* mime)
{
if (mime->mm_type==MAILMIME_MULTIPLE)
{
if (mime->mm_content_type->ct_type->tp_type==MAILMIME_TYPE_COMPOSITE_TYPE
&& mime->mm_content_type->ct_type->tp_data.tp_composite_type->ct_type==MAILMIME_COMPOSITE_TYPE_MULTIPART
&& strcmp(mime->mm_content_type->ct_subtype, "report")==0) {
return 1;
}
clistiter* cur;
for (cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list); cur!=NULL; cur=clist_next(cur)) {
if (contains_report((struct mailmime*)clist_content(cur))) {
return 1;
}
}
}
else if (mime->mm_type==MAILMIME_MESSAGE)
{
if (contains_report(mime->mm_data.mm_message.mm_msg_mime)) {
return 1;
}
}
return 0;
}
static int load_or_generate_self_public_key(dc_context_t* context, dc_key_t* public_key, const char* self_addr,
struct mailmime* random_data_mime )
{
static int s_in_key_creation = 0;
int key_created = 0;
int success = 0, key_creation_here = 0;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || public_key==NULL) {
goto cleanup;
}
if (!dc_key_load_self_public(public_key, self_addr, context->sql))
{
if (s_in_key_creation) { goto cleanup; }
key_creation_here = 1;
s_in_key_creation = 1;
{
uintptr_t seed[4];
seed[0] = (uintptr_t)time(NULL);
seed[1] = (uintptr_t)seed;
seed[2] = (uintptr_t)public_key;
seed[3] = (uintptr_t)pthread_self();
dc_pgp_rand_seed(context, seed, sizeof(seed));
if (random_data_mime) {
MMAPString* random_data_mmap = NULL;
int col = 0;
if ((random_data_mmap=mmap_string_new(""))==NULL) {
goto cleanup;
}
mailmime_write_mem(random_data_mmap, &col, random_data_mime);
dc_pgp_rand_seed(context, random_data_mmap->str, random_data_mmap->len);
mmap_string_free(random_data_mmap);
}
}
{
dc_key_t* private_key = dc_key_new();
clock_t start = clock();
dc_log_info(context, 0, "Generating keypair with %i bits, e=%i ...", DC_KEYGEN_BITS, DC_KEYGEN_E);
key_created = dc_pgp_create_keypair(context, self_addr, public_key, private_key);
if (!key_created) {
dc_log_warning(context, 0, "Cannot create keypair.");
goto cleanup;
}
if (!dc_pgp_is_valid_key(context, public_key)
|| !dc_pgp_is_valid_key(context, private_key)) {
dc_log_warning(context, 0, "Generated keys are not valid.");
goto cleanup;
}
if (!dc_key_save_self_keypair(public_key, private_key, self_addr, 1, context->sql)) {
dc_log_warning(context, 0, "Cannot save keypair.");
goto cleanup;
}
dc_log_info(context, 0, "Keypair generated in %.3f s.", (double)(clock()-start)/CLOCKS_PER_SEC);
dc_key_unref(private_key);
}
}
success = 1;
cleanup:
if (key_creation_here) { s_in_key_creation = 0; }
return success;
}
int dc_ensure_secret_key_exists(dc_context_t* context)
{
int success = 0;
dc_key_t* public_key = dc_key_new();
char* self_addr = NULL;
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || public_key==NULL) {
goto cleanup;
}
if ((self_addr=dc_sqlite3_get_config(context->sql, "configured_addr", NULL))==NULL) {
dc_log_warning(context, 0, "Cannot ensure secret key if context is not configured.");
goto cleanup;
}
if (!load_or_generate_self_public_key(context, public_key, self_addr, NULL)) {
goto cleanup;
}
success = 1;
cleanup:
dc_key_unref(public_key);
free(self_addr);
return success;
}
void dc_e2ee_encrypt(dc_context_t* context, const clist* recipients_addr,
int force_unencrypted,
int e2ee_guaranteed,
int min_verified,
int do_gossip,
struct mailmime* in_out_message, dc_e2ee_helper_t* helper)
{
int col = 0;
int do_encrypt = 0;
dc_aheader_t* autocryptheader = dc_aheader_new();
struct mailimf_fields* imffields_unprotected = NULL;
dc_keyring_t* keyring = dc_keyring_new();
dc_key_t* sign_key = dc_key_new();
MMAPString* plain = mmap_string_new("");
char* ctext = NULL;
size_t ctext_bytes = 0;
dc_array_t* peerstates = dc_array_new(NULL, 10);
if (helper) { memset(helper, 0, sizeof(dc_e2ee_helper_t)); }
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || recipients_addr==NULL || in_out_message==NULL
|| in_out_message->mm_parent
|| autocryptheader==NULL || keyring==NULL || sign_key==NULL || plain==NULL || helper==NULL) {
goto cleanup;
}
autocryptheader->prefer_encrypt = DC_PE_NOPREFERENCE;
if (dc_sqlite3_get_config_int(context->sql, "e2ee_enabled", DC_E2EE_DEFAULT_ENABLED)) {
autocryptheader->prefer_encrypt = DC_PE_MUTUAL;
}
autocryptheader->addr = dc_sqlite3_get_config(context->sql, "configured_addr", NULL);
if (autocryptheader->addr==NULL) {
goto cleanup;
}
if (!load_or_generate_self_public_key(context, autocryptheader->public_key, autocryptheader->addr, in_out_message)) {
goto cleanup;
}
if (autocryptheader->prefer_encrypt==DC_PE_MUTUAL || e2ee_guaranteed)
{
do_encrypt = 1;
clistiter* iter1;
for (iter1 = clist_begin(recipients_addr); iter1!=NULL ; iter1=clist_next(iter1)) {
const char* recipient_addr = clist_content(iter1);
dc_apeerstate_t* peerstate = dc_apeerstate_new(context);
dc_key_t* key_to_use = NULL;
if (strcasecmp(recipient_addr, autocryptheader->addr)==0)
{
;
}
else if (dc_apeerstate_load_by_addr(peerstate, context->sql, recipient_addr)
&& (key_to_use=dc_apeerstate_peek_key(peerstate, min_verified))!=NULL
&& (peerstate->prefer_encrypt==DC_PE_MUTUAL || e2ee_guaranteed))
{
dc_keyring_add(keyring, key_to_use);
dc_array_add_ptr(peerstates, peerstate);
}
else
{
dc_apeerstate_unref(peerstate);
do_encrypt = 0;
break;
}
}
}
if (do_encrypt) {
dc_keyring_add(keyring, autocryptheader->public_key);
if (!dc_key_load_self_private(sign_key, autocryptheader->addr, context->sql)) {
do_encrypt = 0;
}
}
if (force_unencrypted) {
do_encrypt = 0;
}
if ((imffields_unprotected=mailmime_find_mailimf_fields(in_out_message))==NULL) {
goto cleanup;
}
if (do_encrypt)
{
mailprivacy_prepare_mime(in_out_message);
struct mailmime* part_to_encrypt = in_out_message->mm_data.mm_message.mm_msg_mime;
part_to_encrypt->mm_parent = NULL;
struct mailimf_fields* imffields_encrypted = mailimf_fields_new_empty();
struct mailmime* message_to_encrypt = mailmime_new(MAILMIME_MESSAGE, NULL, 0, mailmime_fields_new_empty(),
mailmime_get_content_message(), NULL, NULL, NULL, NULL, imffields_encrypted, part_to_encrypt);
if (do_gossip) {
int iCnt = dc_array_get_cnt(peerstates);
if (iCnt > 1) {
for (int i = 0; i < iCnt; i++) {
char* p = dc_apeerstate_render_gossip_header((dc_apeerstate_t*)dc_array_get_ptr(peerstates, i), min_verified);
if (p) {
mailimf_fields_add(imffields_encrypted, mailimf_field_new_custom(strdup("Autocrypt-Gossip"), p));
}
}
}
}
clistiter* cur = clist_begin(imffields_unprotected->fld_list);
while (cur!=NULL) {
int move_to_encrypted = 0;
struct mailimf_field* field = (struct mailimf_field*)clist_content(cur);
if (field) {
if (field->fld_type==MAILIMF_FIELD_SUBJECT) {
move_to_encrypted = 1;
}
else if (field->fld_type==MAILIMF_FIELD_OPTIONAL_FIELD) {
struct mailimf_optional_field* opt_field = field->fld_data.fld_optional_field;
if (opt_field && opt_field->fld_name) {
if ( strncmp(opt_field->fld_name, "Secure-Join", 11)==0
|| (strncmp(opt_field->fld_name, "Chat-", 5)==0 && strcmp(opt_field->fld_name, "Chat-Version")!=0)) {
move_to_encrypted = 1;
}
}
}
}
if (move_to_encrypted) {
mailimf_fields_add(imffields_encrypted, field);
cur = clist_delete(imffields_unprotected->fld_list, cur);
}
else {
cur = clist_next(cur);
}
}
struct mailimf_subject* subject = mailimf_subject_new(dc_strdup("..."));
mailimf_fields_add(imffields_unprotected, mailimf_field_new(MAILIMF_FIELD_SUBJECT, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, subject, NULL, NULL, NULL));
clist_append(part_to_encrypt->mm_content_type->ct_parameters, mailmime_param_new_with_data("protected-headers", "v1"));
mailmime_write_mem(plain, &col, message_to_encrypt);
if (plain->str==NULL || plain->len<=0) {
goto cleanup;
}
if (!dc_pgp_pk_encrypt(context, plain->str, plain->len, keyring, sign_key, 1, (void**)&ctext, &ctext_bytes)) {
goto cleanup;
}
helper->cdata_to_free = ctext;
struct mailmime* encrypted_part = new_data_part(NULL, 0, "multipart/encrypted", -1);
struct mailmime_content* content = encrypted_part->mm_content_type;
clist_append(content->ct_parameters, mailmime_param_new_with_data("protocol", "application/pgp-encrypted"));
static char version_content[] = "Version: 1\r\n";
struct mailmime* version_mime = new_data_part(version_content, strlen(version_content), "application/pgp-encrypted", MAILMIME_MECHANISM_7BIT);
mailmime_smart_add_part(encrypted_part, version_mime);
struct mailmime* ctext_part = new_data_part(ctext, ctext_bytes, "application/octet-stream", MAILMIME_MECHANISM_7BIT);
mailmime_smart_add_part(encrypted_part, ctext_part);
in_out_message->mm_data.mm_message.mm_msg_mime = encrypted_part;
encrypted_part->mm_parent = in_out_message;
mailmime_free(message_to_encrypt);
helper->encryption_successfull = 1;
}
char* p = dc_aheader_render(autocryptheader);
if (p==NULL) {
goto cleanup;
}
mailimf_fields_add(imffields_unprotected, mailimf_field_new_custom(strdup("Autocrypt"), p));
cleanup:
dc_aheader_unref(autocryptheader);
dc_keyring_unref(keyring);
dc_key_unref(sign_key);
if (plain) { mmap_string_free(plain); }
for (int i=dc_array_get_cnt(peerstates)-1; i>=0; i--) { dc_apeerstate_unref((dc_apeerstate_t*)dc_array_get_ptr(peerstates, i)); }
dc_array_unref(peerstates);
}
void dc_e2ee_thanks(dc_e2ee_helper_t* helper)
{
if (helper==NULL) {
return;
}
free(helper->cdata_to_free);
helper->cdata_to_free = NULL;
if (helper->gossipped_addr)
{
dc_hash_clear(helper->gossipped_addr);
free(helper->gossipped_addr);
helper->gossipped_addr = NULL;
}
if (helper->signatures)
{
dc_hash_clear(helper->signatures);
free(helper->signatures);
helper->signatures = NULL;
}
}
static int has_decrypted_pgp_armor(const char* str__, int str_bytes)
{
const unsigned char* str_end = (const unsigned char*)str__+str_bytes;
const unsigned char* p=(const unsigned char*)str__;
while (p < str_end) {
if (*p > ' ') {
break;
}
p++;
str_bytes--;
}
if (str_bytes>27 && strncmp((const char*)p, "-----BEGIN PGP MESSAGE-----", 27)==0) {
return 1;
}
return 0;
}
static int decrypt_part(dc_context_t* context,
struct mailmime* mime,
const dc_keyring_t* private_keyring,
const dc_keyring_t* public_keyring_for_validate,
dc_hash_t* ret_valid_signatures,
struct mailmime** ret_decrypted_mime)
{
struct mailmime_data* mime_data = NULL;
int mime_transfer_encoding = MAILMIME_MECHANISM_BINARY;
char* transfer_decoding_buffer = NULL;
const char* decoded_data = NULL;
size_t decoded_data_bytes = 0;
void* plain_buf = NULL;
size_t plain_bytes = 0;
int sth_decrypted = 0;
*ret_decrypted_mime = NULL;
mime_data = mime->mm_data.mm_single;
if (mime_data->dt_type!=MAILMIME_DATA_TEXT
|| mime_data->dt_data.dt_text.dt_data==NULL
|| mime_data->dt_data.dt_text.dt_length <= 0) {
goto cleanup;
}
if (mime->mm_mime_fields!=NULL) {
clistiter* cur;
for (cur = clist_begin(mime->mm_mime_fields->fld_list); cur!=NULL; cur = clist_next(cur)) {
struct mailmime_field* field = (struct mailmime_field*)clist_content(cur);
if (field) {
if (field->fld_type==MAILMIME_FIELD_TRANSFER_ENCODING && field->fld_data.fld_encoding) {
mime_transfer_encoding = field->fld_data.fld_encoding->enc_type;
}
}
}
}
if (mime_transfer_encoding==MAILMIME_MECHANISM_7BIT
|| mime_transfer_encoding==MAILMIME_MECHANISM_8BIT
|| mime_transfer_encoding==MAILMIME_MECHANISM_BINARY)
{
decoded_data = mime_data->dt_data.dt_text.dt_data;
decoded_data_bytes = mime_data->dt_data.dt_text.dt_length;
if (decoded_data==NULL || decoded_data_bytes <= 0) {
goto cleanup;
}
}
else
{
int r;
size_t current_index = 0;
r = mailmime_part_parse(mime_data->dt_data.dt_text.dt_data, mime_data->dt_data.dt_text.dt_length,
&current_index, mime_transfer_encoding,
&transfer_decoding_buffer, &decoded_data_bytes);
if (r!=MAILIMF_NO_ERROR || transfer_decoding_buffer==NULL || decoded_data_bytes <= 0) {
goto cleanup;
}
decoded_data = transfer_decoding_buffer;
}
if (!has_decrypted_pgp_armor(decoded_data, decoded_data_bytes)) {
goto cleanup;
}
dc_hash_t* add_signatures = dc_hash_cnt(ret_valid_signatures)<=0?
ret_valid_signatures : NULL;
if (!dc_pgp_pk_decrypt(context, decoded_data, decoded_data_bytes, private_keyring, public_keyring_for_validate, 1, &plain_buf, &plain_bytes, add_signatures)
|| plain_buf==NULL || plain_bytes<=0) {
goto cleanup;
}
{
size_t index = 0;
struct mailmime* decrypted_mime = NULL;
if (mailmime_parse(plain_buf, plain_bytes, &index, &decrypted_mime)!=MAIL_NO_ERROR
|| decrypted_mime==NULL) {
if(decrypted_mime) {mailmime_free(decrypted_mime);}
goto cleanup;
}
*ret_decrypted_mime = decrypted_mime;
sth_decrypted = 1;
}
cleanup:
if (transfer_decoding_buffer) {
mmap_string_unref(transfer_decoding_buffer);
}
return sth_decrypted;
}
static int decrypt_recursive(dc_context_t* context,
struct mailmime* mime,
const dc_keyring_t* private_keyring,
const dc_keyring_t* public_keyring_for_validate,
dc_hash_t* ret_valid_signatures,
struct mailimf_fields** ret_gossip_headers,
int* ret_has_unencrypted_parts)
{
struct mailmime_content* ct = NULL;
clistiter* cur = NULL;
if (context==NULL || mime==NULL) {
return 0;
}
if (mime->mm_type==MAILMIME_MULTIPLE)
{
ct = mime->mm_content_type;
if (ct && ct->ct_subtype && strcmp(ct->ct_subtype, "encrypted")==0) {
for (cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list); cur!=NULL; cur=clist_next(cur)) {
struct mailmime* decrypted_mime = NULL;
if (decrypt_part(context, (struct mailmime*)clist_content(cur), private_keyring, public_keyring_for_validate, ret_valid_signatures, &decrypted_mime))
{
if (*ret_gossip_headers==NULL
&& dc_hash_cnt(ret_valid_signatures) > 0 )
{
size_t dummy = 0;
struct mailimf_fields* test = NULL;
if (mailimf_envelope_and_optional_fields_parse(decrypted_mime->mm_mime_start, decrypted_mime->mm_length, &dummy, &test)==MAILIMF_NO_ERROR
&& test) {
*ret_gossip_headers = test;
}
}
mailmime_substitute(mime, decrypted_mime);
mailmime_free(mime);
return 1;
}
}
*ret_has_unencrypted_parts = 1;
}
else {
for (cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list); cur!=NULL; cur=clist_next(cur)) {
if (decrypt_recursive(context, (struct mailmime*)clist_content(cur), private_keyring, public_keyring_for_validate, ret_valid_signatures, ret_gossip_headers, ret_has_unencrypted_parts)) {
return 1;
}
}
}
}
else if (mime->mm_type==MAILMIME_MESSAGE)
{
if (decrypt_recursive(context, mime->mm_data.mm_message.mm_msg_mime, private_keyring, public_keyring_for_validate, ret_valid_signatures, ret_gossip_headers, ret_has_unencrypted_parts)) {
return 1;
}
}
else
{
*ret_has_unencrypted_parts = 1;
}
return 0;
}
static dc_hash_t* update_gossip_peerstates(dc_context_t* context, time_t message_time, struct mailimf_fields* imffields, const struct mailimf_fields* gossip_headers)
{
clistiter* cur1 = NULL;
dc_hash_t* recipients = NULL;
dc_hash_t* gossipped_addr = NULL;
for (cur1 = clist_begin(gossip_headers->fld_list); cur1!=NULL ; cur1=clist_next(cur1))
{
struct mailimf_field* field = (struct mailimf_field*)clist_content(cur1);
if (field->fld_type==MAILIMF_FIELD_OPTIONAL_FIELD)
{
const struct mailimf_optional_field* optional_field = field->fld_data.fld_optional_field;
if (optional_field && optional_field->fld_name && strcasecmp(optional_field->fld_name, "Autocrypt-Gossip")==0)
{
dc_aheader_t* gossip_header = dc_aheader_new();
if (dc_aheader_set_from_string(gossip_header, optional_field->fld_value)
&& dc_pgp_is_valid_key(context, gossip_header->public_key))
{
if (recipients==NULL) {
recipients = mailimf_get_recipients(imffields);
}
if (dc_hash_find(recipients, gossip_header->addr, strlen(gossip_header->addr)))
{
dc_apeerstate_t* peerstate = dc_apeerstate_new(context);
if (!dc_apeerstate_load_by_addr(peerstate, context->sql, gossip_header->addr)) {
dc_apeerstate_init_from_gossip(peerstate, gossip_header, message_time);
dc_apeerstate_save_to_db(peerstate, context->sql, 1);
}
else {
dc_apeerstate_apply_gossip(peerstate, gossip_header, message_time);
dc_apeerstate_save_to_db(peerstate, context->sql, 0);
}
if (peerstate->degrade_event) {
dc_handle_degrade_event(context, peerstate);
}
dc_apeerstate_unref(peerstate);
if (gossipped_addr==NULL) {
gossipped_addr = malloc(sizeof(dc_hash_t));
dc_hash_init(gossipped_addr, DC_HASH_STRING, 1);
}
dc_hash_insert(gossipped_addr, gossip_header->addr, strlen(gossip_header->addr), (void*)1);
}
else
{
dc_log_info(context, 0, "Ignoring gossipped \"%s\" as the address is not in To/Cc list.", gossip_header->addr);
}
}
dc_aheader_unref(gossip_header);
}
}
}
if (recipients) {
dc_hash_clear(recipients);
free(recipients);
}
return gossipped_addr;
}
void dc_e2ee_decrypt(dc_context_t* context, struct mailmime* in_out_message,
dc_e2ee_helper_t* helper)
{
struct mailimf_fields* imffields = mailmime_find_mailimf_fields(in_out_message);
dc_aheader_t* autocryptheader = NULL;
time_t message_time = 0;
dc_apeerstate_t* peerstate = dc_apeerstate_new(context);
char* from = NULL;
char* self_addr = NULL;
dc_keyring_t* private_keyring = dc_keyring_new();
dc_keyring_t* public_keyring_for_validate = dc_keyring_new();
struct mailimf_fields* gossip_headers = NULL;
if (helper) { memset(helper, 0, sizeof(dc_e2ee_helper_t)); }
if (context==NULL || context->magic!=DC_CONTEXT_MAGIC || in_out_message==NULL
|| helper==NULL || imffields==NULL) {
goto cleanup;
}
if (imffields)
{
struct mailimf_field* field = mailimf_find_field(imffields, MAILIMF_FIELD_FROM);
if (field && field->fld_data.fld_from) {
from = mailimf_find_first_addr(field->fld_data.fld_from->frm_mb_list);
}
field = mailimf_find_field(imffields, MAILIMF_FIELD_ORIG_DATE);
if (field && field->fld_data.fld_orig_date) {
struct mailimf_orig_date* orig_date = field->fld_data.fld_orig_date;
if (orig_date) {
message_time = dc_timestamp_from_date(orig_date->dt_date_time);
if (message_time!=DC_INVALID_TIMESTAMP && message_time > time(NULL)) {
message_time = time(NULL);
}
}
}
}
autocryptheader = dc_aheader_new_from_imffields(from, imffields);
if (autocryptheader) {
if (!dc_pgp_is_valid_key(context, autocryptheader->public_key)) {
dc_aheader_unref(autocryptheader);
autocryptheader = NULL;
}
}
if (message_time > 0
&& from)
{
if (dc_apeerstate_load_by_addr(peerstate, context->sql, from)) {
if (autocryptheader) {
dc_apeerstate_apply_header(peerstate, autocryptheader, message_time);
dc_apeerstate_save_to_db(peerstate, context->sql, 0);
}
else {
if (message_time > peerstate->last_seen_autocrypt
&& !contains_report(in_out_message) ){
dc_apeerstate_degrade_encryption(peerstate, message_time);
dc_apeerstate_save_to_db(peerstate, context->sql, 0);
}
}
}
else if (autocryptheader) {
dc_apeerstate_init_from_header(peerstate, autocryptheader, message_time);
dc_apeerstate_save_to_db(peerstate, context->sql, 1);
}
}
if ((self_addr=dc_sqlite3_get_config(context->sql, "configured_addr", NULL))==NULL) {
goto cleanup;
}
if (!dc_keyring_load_self_private_for_decrypting(private_keyring, self_addr, context->sql)) {
goto cleanup;
}
if (peerstate->last_seen==0) {
dc_apeerstate_load_by_addr(peerstate, context->sql, from);
}
if (peerstate->degrade_event) {
dc_handle_degrade_event(context, peerstate);
}
dc_keyring_add(public_keyring_for_validate, peerstate->gossip_key);
dc_keyring_add(public_keyring_for_validate, peerstate->public_key);
helper->signatures = malloc(sizeof(dc_hash_t));
dc_hash_init(helper->signatures, DC_HASH_STRING, 1);
int iterations = 0;
while (iterations < 10) {
int has_unencrypted_parts = 0;
if (!decrypt_recursive(context, in_out_message, private_keyring,
public_keyring_for_validate,
helper->signatures, &gossip_headers, &has_unencrypted_parts)) {
break;
}
if (iterations==0
&& !has_unencrypted_parts) {
helper->encrypted = 1;
}
iterations++;
}
if (gossip_headers) {
helper->gossipped_addr = update_gossip_peerstates(context, message_time, imffields, gossip_headers);
}
cleanup:
if (gossip_headers) { mailimf_fields_free(gossip_headers); }
dc_aheader_unref(autocryptheader);
dc_apeerstate_unref(peerstate);
dc_keyring_unref(private_keyring);
dc_keyring_unref(public_keyring_for_validate);
free(from);
free(self_addr);
}