#include "dc_context.h"
#include "dc_mimeparser.h"
#include "dc_mimefactory.h"
#include "dc_pgp.h"
#include "dc_simplify.h"
static void hash_header(dc_hash_t* out, const struct mailimf_fields* in, dc_context_t* context);
static int s_generate_compound_msgs = 1;
void dc_no_compound_msgs(void)
{
s_generate_compound_msgs = 0;
}
#ifdef DC_USE_MIME_DEBUG
static void display_mime_content(struct mailmime_content * content_type);
static void display_mime_data(struct mailmime_data * data)
{
switch (data->dt_type) {
case MAILMIME_DATA_TEXT:
printf("data : %i bytes\n", (int) data->dt_data.dt_text.dt_length);
break;
case MAILMIME_DATA_FILE:
printf("data (file) : %s\n", data->dt_data.dt_filename);
break;
}
}
static void display_mime_dsp_parm(struct mailmime_disposition_parm * param)
{
switch (param->pa_type) {
case MAILMIME_DISPOSITION_PARM_FILENAME:
printf("filename: %s\n", param->pa_data.pa_filename);
break;
}
}
static void display_mime_disposition(struct mailmime_disposition * disposition)
{
clistiter * cur;
for(cur = clist_begin(disposition->dsp_parms) ;
cur!=NULL ; cur = clist_next(cur)) {
struct mailmime_disposition_parm * param;
param = (struct mailmime_disposition_parm*)clist_content(cur);
display_mime_dsp_parm(param);
}
}
static void display_mime_field(struct mailmime_field * field)
{
switch (field->fld_type) {
case MAILMIME_FIELD_VERSION:
printf("MIME-Version: ...\n");
break;
case MAILMIME_FIELD_TYPE:
printf("content-type: ");
display_mime_content(field->fld_data.fld_content);
printf("\n");
break;
case MAILMIME_FIELD_DISPOSITION:
display_mime_disposition(field->fld_data.fld_disposition);
break;
}
}
static void display_mime_fields(struct mailmime_fields * fields)
{
clistiter * cur;
for(cur = clist_begin(fields->fld_list) ; cur!=NULL ; cur = clist_next(cur)) {
struct mailmime_field * field;
field = (struct mailmime_field*)clist_content(cur);
display_mime_field(field);
}
}
static void display_date_time(struct mailimf_date_time * d)
{
printf("%02i/%02i/%i %02i:%02i:%02i %+04i",
d->dt_day, d->dt_month, d->dt_year,
d->dt_hour, d->dt_min, d->dt_sec, d->dt_zone);
}
static void display_orig_date(struct mailimf_orig_date * orig_date)
{
display_date_time(orig_date->dt_date_time);
}
static void display_mailbox(struct mailimf_mailbox * mb)
{
if (mb->mb_display_name!=NULL)
printf("%s ", mb->mb_display_name);
printf("<%s>", mb->mb_addr_spec);
}
static void display_mailbox_list(struct mailimf_mailbox_list * mb_list)
{
clistiter * cur;
for(cur = clist_begin(mb_list->mb_list) ; cur!=NULL ;
cur = clist_next(cur)) {
struct mailimf_mailbox * mb;
mb = (struct mailimf_mailbox*)clist_content(cur);
display_mailbox(mb);
if (clist_next(cur)!=NULL) {
printf(", ");
}
}
}
static void display_group(struct mailimf_group * group)
{
clistiter * cur;
printf("%s: ", group->grp_display_name);
for(cur = clist_begin(group->grp_mb_list->mb_list) ; cur!=NULL ; cur = clist_next(cur)) {
struct mailimf_mailbox * mb;
mb = (struct mailimf_mailbox*)clist_content(cur);
display_mailbox(mb);
}
printf("; ");
}
static void display_address(struct mailimf_address * a)
{
switch (a->ad_type) {
case MAILIMF_ADDRESS_GROUP:
display_group(a->ad_data.ad_group);
break;
case MAILIMF_ADDRESS_MAILBOX:
display_mailbox(a->ad_data.ad_mailbox);
break;
}
}
static void display_address_list(struct mailimf_address_list * addr_list)
{
clistiter * cur;
for(cur = clist_begin(addr_list->ad_list) ; cur!=NULL ;
cur = clist_next(cur)) {
struct mailimf_address * addr;
addr = (struct mailimf_address*)clist_content(cur);
display_address(addr);
if (clist_next(cur)!=NULL) {
printf(", ");
}
}
}
static void display_from(struct mailimf_from * from)
{
display_mailbox_list(from->frm_mb_list);
}
static void display_to(struct mailimf_to * to)
{
display_address_list(to->to_addr_list);
}
static void display_cc(struct mailimf_cc * cc)
{
display_address_list(cc->cc_addr_list);
}
static void display_subject(struct mailimf_subject * subject)
{
printf("%s", subject->sbj_value);
}
static void display_field(struct mailimf_field * field)
{
switch (field->fld_type)
{
case MAILIMF_FIELD_ORIG_DATE:
printf("Date: ");
display_orig_date(field->fld_data.fld_orig_date);
printf("\n");
break;
case MAILIMF_FIELD_FROM:
printf("From: ");
display_from(field->fld_data.fld_from);
printf("\n");
break;
case MAILIMF_FIELD_TO:
printf("To: ");
display_to(field->fld_data.fld_to);
printf("\n");
break;
case MAILIMF_FIELD_CC:
printf("Cc: ");
display_cc(field->fld_data.fld_cc);
printf("\n");
break;
case MAILIMF_FIELD_SUBJECT:
printf("Subject: ");
display_subject(field->fld_data.fld_subject);
printf("\n");
break;
case MAILIMF_FIELD_MESSAGE_ID:
printf("Message-ID: %s\n", field->fld_data.fld_message_id->mid_value);
break;
case MAILIMF_FIELD_OPTIONAL_FIELD:
{
struct mailimf_optional_field* of = field->fld_data.fld_optional_field;
if (of) {
printf("%s: %s\n", of->fld_name? of->fld_name : "?", of->fld_value? of->fld_value : "?");
}
}
break;
default:
printf("MAILIMF_FIELD_%i\n", (int)field->fld_type);
break;
}
}
static void display_fields(struct mailimf_fields * fields)
{
clistiter * cur;
for(cur = clist_begin(fields->fld_list) ; cur!=NULL ;
cur = clist_next(cur)) {
struct mailimf_field * f;
f = (struct mailimf_field*)clist_content(cur);
display_field(f);
}
}
static void display_mime_discrete_type(struct mailmime_discrete_type * discrete_type)
{
switch (discrete_type->dt_type) {
case MAILMIME_DISCRETE_TYPE_TEXT:
printf("text");
break;
case MAILMIME_DISCRETE_TYPE_IMAGE:
printf("image");
break;
case MAILMIME_DISCRETE_TYPE_AUDIO:
printf("audio");
break;
case MAILMIME_DISCRETE_TYPE_VIDEO:
printf("video");
break;
case MAILMIME_DISCRETE_TYPE_APPLICATION:
printf("application");
break;
case MAILMIME_DISCRETE_TYPE_EXTENSION:
printf("%s", discrete_type->dt_extension);
break;
}
}
static void display_mime_composite_type(struct mailmime_composite_type * ct)
{
switch (ct->ct_type) {
case MAILMIME_COMPOSITE_TYPE_MESSAGE:
printf("message");
break;
case MAILMIME_COMPOSITE_TYPE_MULTIPART:
printf("multipart");
break;
case MAILMIME_COMPOSITE_TYPE_EXTENSION:
printf("%s", ct->ct_token);
break;
}
}
static void display_mime_type(struct mailmime_type * type)
{
switch (type->tp_type) {
case MAILMIME_TYPE_DISCRETE_TYPE:
display_mime_discrete_type(type->tp_data.tp_discrete_type);
break;
case MAILMIME_TYPE_COMPOSITE_TYPE:
display_mime_composite_type(type->tp_data.tp_composite_type);
break;
}
}
static void display_mime_content(struct mailmime_content * content_type)
{
printf("type: ");
display_mime_type(content_type->ct_type);
printf("/%s\n", content_type->ct_subtype);
}
static void print_mime(struct mailmime * mime)
{
clistiter * cur;
if (mime==NULL) {
printf("ERROR: NULL given to print_mime()\n");
return;
}
switch (mime->mm_type) {
case MAILMIME_SINGLE:
printf("single part\n");
break;
case MAILMIME_MULTIPLE:
printf("multipart\n");
break;
case MAILMIME_MESSAGE:
printf("message\n");
break;
}
if (mime->mm_mime_fields!=NULL) {
if (clist_begin(mime->mm_mime_fields->fld_list)!=NULL) {
printf("--------------------------------<mime-headers>--------------------------------\n");
display_mime_fields(mime->mm_mime_fields);
printf("--------------------------------</mime-headers>-------------------------------\n");
}
}
display_mime_content(mime->mm_content_type);
switch (mime->mm_type) {
case MAILMIME_SINGLE:
display_mime_data(mime->mm_data.mm_single);
break;
case MAILMIME_MULTIPLE:
for(cur = clist_begin(mime->mm_data.mm_multipart.mm_mp_list) ; cur!=NULL ; cur = clist_next(cur)) {
printf("---------------------------<mime-part-of-multiple>----------------------------\n");
print_mime((struct mailmime*)clist_content(cur));
printf("---------------------------</mime-part-of-multiple>---------------------------\n");
}
break;
case MAILMIME_MESSAGE:
if (mime->mm_data.mm_message.mm_fields) {
if (clist_begin(mime->mm_data.mm_message.mm_fields->fld_list)!=NULL) {
printf("-------------------------------<email-headers>--------------------------------\n");
display_fields(mime->mm_data.mm_message.mm_fields);
printf("-------------------------------</email-headers>-------------------------------\n");
}
if (mime->mm_data.mm_message.mm_msg_mime!=NULL) {
printf("----------------------------<mime-part-of-message>----------------------------\n");
print_mime(mime->mm_data.mm_message.mm_msg_mime);
printf("----------------------------</mime-part-of-message>---------------------------\n");
}
}
break;
}
}
void mailmime_print(struct mailmime* mime)
{
printf("====================================<mime>====================================\n");
print_mime(mime);
printf("====================================</mime>===================================\n\n");
}
#endif
static void mailimf_get_recipients__add_addr(dc_hash_t* recipients, struct mailimf_mailbox* mb)
{
if (mb)  {
char* addr_norm = dc_addr_normalize(mb->mb_addr_spec);
dc_hash_insert(recipients, addr_norm, strlen(addr_norm), (void*)1);
free(addr_norm);
}
}
dc_hash_t* mailimf_get_recipients(struct mailimf_fields* imffields)
{
dc_hash_t* recipients = malloc(sizeof(dc_hash_t));
dc_hash_init(recipients, DC_HASH_STRING, 1);
clistiter* cur1;
for (cur1 = clist_begin(imffields->fld_list); cur1!=NULL ; cur1=clist_next(cur1))
{
struct mailimf_field*        fld = (struct mailimf_field*)clist_content(cur1);
struct mailimf_to*           fld_to = NULL;
struct mailimf_cc*           fld_cc = NULL;
struct mailimf_address_list* addr_list = NULL;
switch (fld->fld_type)
{
case MAILIMF_FIELD_TO: fld_to = fld->fld_data.fld_to; if (fld_to) { addr_list = fld_to->to_addr_list; } break;
case MAILIMF_FIELD_CC: fld_cc = fld->fld_data.fld_cc; if (fld_cc) { addr_list = fld_cc->cc_addr_list; } break;
}
if (addr_list) {
clistiter* cur2;
for (cur2 = clist_begin(addr_list->ad_list); cur2!=NULL ; cur2=clist_next(cur2)) {
struct mailimf_address* adr = (struct mailimf_address*)clist_content(cur2);
if (adr) {
if (adr->ad_type==MAILIMF_ADDRESS_MAILBOX) {
mailimf_get_recipients__add_addr(recipients, adr->ad_data.ad_mailbox);
}
else if (adr->ad_type==MAILIMF_ADDRESS_GROUP) {
struct mailimf_group* group = adr->ad_data.ad_group;
if (group && group->grp_mb_list) {
clistiter* cur3;
for (cur3 = clist_begin(group->grp_mb_list->mb_list); cur3!=NULL ; cur3=clist_next(cur3)) {
mailimf_get_recipients__add_addr(recipients, (struct mailimf_mailbox*)clist_content(cur3));
}
}
}
}
}
}
}
return recipients;
}
struct mailmime_parameter* mailmime_find_ct_parameter(struct mailmime* mime, const char* name)
{
if (mime==NULL || name==NULL
|| mime->mm_content_type==NULL || mime->mm_content_type->ct_parameters==NULL)
{
return NULL;
}
clistiter* cur;
for (cur = clist_begin(mime->mm_content_type->ct_parameters); cur!=NULL; cur = clist_next(cur)) {
struct mailmime_parameter* param = (struct mailmime_parameter*)clist_content(cur);
if (param && param->pa_name) {
if (strcmp(param->pa_name, name)==0) {
return param;
}
}
}
return NULL;
}
int mailmime_transfer_decode(struct mailmime* mime, const char** ret_decoded_data, size_t* ret_decoded_data_bytes, char** ret_to_mmap_string_unref)
{
int                   mime_transfer_encoding = MAILMIME_MECHANISM_BINARY;
struct mailmime_data* mime_data = NULL;
const char*           decoded_data = NULL;
size_t                decoded_data_bytes = 0;
char*                 transfer_decoding_buffer = NULL;
if (mime==NULL || ret_decoded_data==NULL || ret_decoded_data_bytes==NULL || ret_to_mmap_string_unref==NULL
|| *ret_decoded_data!=NULL || *ret_decoded_data_bytes!=0 || *ret_to_mmap_string_unref!=NULL) {
return 0;
}
mime_data = mime->mm_data.mm_single;
if (mime->mm_mime_fields!=NULL) {
clistiter* cur;
for (cur = clist_begin(mime->mm_mime_fields->fld_list); cur!=NULL; cur = clist_next(cur)) {
struct mailmime_field* field = (struct mailmime_field*)clist_content(cur);
if (field && field->fld_type==MAILMIME_FIELD_TRANSFER_ENCODING && field->fld_data.fld_encoding) {
mime_transfer_encoding = field->fld_data.fld_encoding->enc_type;
break;
}
}
}
if (mime_transfer_encoding==MAILMIME_MECHANISM_7BIT
|| mime_transfer_encoding==MAILMIME_MECHANISM_8BIT
|| mime_transfer_encoding==MAILMIME_MECHANISM_BINARY)
{
decoded_data       = mime_data->dt_data.dt_text.dt_data;
decoded_data_bytes = mime_data->dt_data.dt_text.dt_length;
if (decoded_data==NULL || decoded_data_bytes <= 0) {
return 0;
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
return 0;
}
decoded_data = transfer_decoding_buffer;
}
*ret_decoded_data         = decoded_data;
*ret_decoded_data_bytes   = decoded_data_bytes;
*ret_to_mmap_string_unref = transfer_decoding_buffer;
return 1;
}
struct mailimf_fields* mailmime_find_mailimf_fields(struct mailmime* mime)
{
if (mime==NULL) {
return NULL;
}
switch (mime->mm_type) {
case MAILMIME_MULTIPLE:
for (clistiter* cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list); cur!=NULL ; cur=clist_next(cur)) {
struct mailimf_fields* header = mailmime_find_mailimf_fields(clist_content(cur));
if (header) {
return header;
}
}
break;
case MAILMIME_MESSAGE:
return mime->mm_data.mm_message.mm_fields;
}
return NULL;
}
char* mailimf_find_first_addr(const struct mailimf_mailbox_list* mb_list)
{
if (mb_list==NULL) {
return NULL;
}
for (clistiter* cur = clist_begin(mb_list->mb_list); cur!=NULL ; cur=clist_next(cur)) {
struct mailimf_mailbox* mb = (struct mailimf_mailbox*)clist_content(cur);
if (mb && mb->mb_addr_spec) {
return dc_addr_normalize(mb->mb_addr_spec);
}
}
return NULL;
}
struct mailimf_field* mailimf_find_field(struct mailimf_fields* header, int wanted_fld_type)
{
if (header==NULL || header->fld_list==NULL) {
return NULL;
}
for (clistiter* cur1 = clist_begin(header->fld_list); cur1!=NULL ; cur1=clist_next(cur1))
{
struct mailimf_field* field = (struct mailimf_field*)clist_content(cur1);
if (field)
{
if (field->fld_type==wanted_fld_type) {
return field;
}
}
}
return NULL;
}
struct mailimf_optional_field* mailimf_find_optional_field(struct mailimf_fields* header, const char* wanted_fld_name)
{
if (header==NULL || header->fld_list==NULL) {
return NULL;
}
for (clistiter* cur1 = clist_begin(header->fld_list); cur1!=NULL ; cur1=clist_next(cur1))
{
struct mailimf_field* field = (struct mailimf_field*)clist_content(cur1);
if (field && field->fld_type==MAILIMF_FIELD_OPTIONAL_FIELD)
{
struct mailimf_optional_field* optional_field = field->fld_data.fld_optional_field;
if (optional_field && optional_field->fld_name && optional_field->fld_value && strcasecmp(optional_field->fld_name, wanted_fld_name)==0) {
return optional_field;
}
}
}
return NULL;
}
static int mailmime_is_attachment_disposition(struct mailmime* mime)
{
if (mime->mm_mime_fields!=NULL) {
for (clistiter* cur = clist_begin(mime->mm_mime_fields->fld_list); cur!=NULL; cur = clist_next(cur)) {
struct mailmime_field* field = (struct mailmime_field*)clist_content(cur);
if (field && field->fld_type==MAILMIME_FIELD_DISPOSITION && field->fld_data.fld_disposition) {
if (field->fld_data.fld_disposition->dsp_type
&& field->fld_data.fld_disposition->dsp_type->dsp_type==MAILMIME_DISPOSITION_TYPE_ATTACHMENT)
{
return 1;
}
}
}
}
return 0;
}
static void reconcat_mime(char** raw_mime, const char* type, const char* subtype)
{
if (raw_mime) {
*raw_mime = dc_mprintf("%s/%s",
type? type : "application",
subtype? subtype : "octet-stream");
}
}
static int mailmime_get_mime_type(struct mailmime* mime, int* msg_type,
char** raw_mime )
{
#define DC_MIMETYPE_MP_ALTERNATIVE      10
#define DC_MIMETYPE_MP_RELATED          20
#define DC_MIMETYPE_MP_MIXED            30
#define DC_MIMETYPE_MP_NOT_DECRYPTABLE  40
#define DC_MIMETYPE_MP_REPORT           45
#define DC_MIMETYPE_MP_SIGNED           46
#define DC_MIMETYPE_MP_OTHER            50
#define DC_MIMETYPE_TEXT_PLAIN          60
#define DC_MIMETYPE_TEXT_HTML           70
#define DC_MIMETYPE_IMAGE               80
#define DC_MIMETYPE_AUDIO               90
#define DC_MIMETYPE_VIDEO              100
#define DC_MIMETYPE_FILE               110
#define DC_MIMETYPE_AC_SETUP_FILE      111
struct mailmime_content* c = mime->mm_content_type;
int dummy = 0; if (msg_type==NULL) { msg_type = &dummy; }
*msg_type = 0;
if (c==NULL || c->ct_type==NULL) {
return 0;
}
switch (c->ct_type->tp_type)
{
case MAILMIME_TYPE_DISCRETE_TYPE:
switch (c->ct_type->tp_data.tp_discrete_type->dt_type)
{
case MAILMIME_DISCRETE_TYPE_TEXT:
if (mailmime_is_attachment_disposition(mime)) {
;
}
else if (strcmp(c->ct_subtype, "plain")==0) {
*msg_type = DC_MSG_TEXT;
return DC_MIMETYPE_TEXT_PLAIN;
}
else if (strcmp(c->ct_subtype, "html")==0) {
*msg_type = DC_MSG_TEXT;
return DC_MIMETYPE_TEXT_HTML;
}
*msg_type = DC_MSG_FILE;
reconcat_mime(raw_mime, "text", c->ct_subtype);
return DC_MIMETYPE_FILE;
case MAILMIME_DISCRETE_TYPE_IMAGE:
if (strcmp(c->ct_subtype, "gif")==0) {
*msg_type = DC_MSG_GIF;
}
else if (strcmp(c->ct_subtype, "svg+xml")==0) {
*msg_type = DC_MSG_FILE;
reconcat_mime(raw_mime, "image", c->ct_subtype);
return DC_MIMETYPE_FILE;
}
else {
*msg_type = DC_MSG_IMAGE;
}
reconcat_mime(raw_mime, "image", c->ct_subtype);
return DC_MIMETYPE_IMAGE;
case MAILMIME_DISCRETE_TYPE_AUDIO:
*msg_type = DC_MSG_AUDIO;
reconcat_mime(raw_mime, "audio", c->ct_subtype);
return DC_MIMETYPE_AUDIO;
case MAILMIME_DISCRETE_TYPE_VIDEO:
*msg_type = DC_MSG_VIDEO;
reconcat_mime(raw_mime, "video", c->ct_subtype);
return DC_MIMETYPE_VIDEO;
default:
*msg_type = DC_MSG_FILE;
if (c->ct_type->tp_data.tp_discrete_type->dt_type==MAILMIME_DISCRETE_TYPE_APPLICATION
&& strcmp(c->ct_subtype, "autocrypt-setup")==0) {
reconcat_mime(raw_mime, "application", c->ct_subtype);
return DC_MIMETYPE_AC_SETUP_FILE;
}
reconcat_mime(raw_mime, c->ct_type->tp_data.tp_discrete_type->dt_extension, c->ct_subtype);
return DC_MIMETYPE_FILE;
}
break;
case MAILMIME_TYPE_COMPOSITE_TYPE:
if (c->ct_type->tp_data.tp_composite_type->ct_type==MAILMIME_COMPOSITE_TYPE_MULTIPART)
{
if (strcmp(c->ct_subtype, "alternative")==0) {
return DC_MIMETYPE_MP_ALTERNATIVE;
}
else if (strcmp(c->ct_subtype, "related")==0) {
return DC_MIMETYPE_MP_RELATED;
}
else if (strcmp(c->ct_subtype, "encrypted")==0) {
return DC_MIMETYPE_MP_NOT_DECRYPTABLE;
}
else if (strcmp(c->ct_subtype, "signed")==0) {
return DC_MIMETYPE_MP_SIGNED;
}
else if (strcmp(c->ct_subtype, "mixed")==0) {
return DC_MIMETYPE_MP_MIXED;
}
else if (strcmp(c->ct_subtype, "report")==0) {
return DC_MIMETYPE_MP_REPORT;
}
else {
return DC_MIMETYPE_MP_OTHER;
}
}
else if (c->ct_type->tp_data.tp_composite_type->ct_type==MAILMIME_COMPOSITE_TYPE_MESSAGE)
{
return 0;
}
break;
default:
break;
}
return 0;
}
static dc_mimepart_t* dc_mimepart_new(void)
{
dc_mimepart_t* mimepart = NULL;
if ((mimepart=calloc(1, sizeof(dc_mimepart_t)))==NULL) {
exit(33);
}
mimepart->type    = 0;
mimepart->param   = dc_param_new();
return mimepart;
}
static void dc_mimepart_unref(dc_mimepart_t* mimepart)
{
if (mimepart==NULL) {
return;
}
free(mimepart->msg);
mimepart->msg = NULL;
free(mimepart->msg_raw);
mimepart->msg_raw = NULL;
dc_param_unref(mimepart->param);
free(mimepart);
}
dc_mimeparser_t* dc_mimeparser_new(const char* blobdir, dc_context_t* context)
{
dc_mimeparser_t* mimeparser = NULL;
if ((mimeparser=calloc(1, sizeof(dc_mimeparser_t)))==NULL) {
exit(30);
}
mimeparser->context = context;
mimeparser->parts   = carray_new(16);
mimeparser->blobdir = blobdir;
mimeparser->reports = carray_new(16);
mimeparser->e2ee_helper = calloc(1, sizeof(dc_e2ee_helper_t));
dc_hash_init(&mimeparser->header, DC_HASH_STRING, 0);
return mimeparser;
}
void dc_mimeparser_unref(dc_mimeparser_t* mimeparser)
{
if (mimeparser==NULL) {
return;
}
dc_mimeparser_empty(mimeparser);
if (mimeparser->parts) {
carray_free(mimeparser->parts);
}
if (mimeparser->reports) {
carray_free(mimeparser->reports);
}
free(mimeparser->e2ee_helper);
free(mimeparser);
}
void dc_mimeparser_empty(dc_mimeparser_t* mimeparser)
{
if (mimeparser==NULL) {
return;
}
if (mimeparser->parts)
{
int i, cnt = carray_count(mimeparser->parts);
for (i = 0; i < cnt; i++) {
dc_mimepart_t* part = (dc_mimepart_t*)carray_get(mimeparser->parts, i);
if (part) {
dc_mimepart_unref(part);
}
}
carray_set_size(mimeparser->parts, 0);
}
mimeparser->header_root  = NULL;
dc_hash_clear(&mimeparser->header);
if (mimeparser->header_protected) {
mailimf_fields_free(mimeparser->header_protected);
mimeparser->header_protected = NULL;
}
mimeparser->is_send_by_messenger  = 0;
mimeparser->is_system_message = 0;
free(mimeparser->subject);
mimeparser->subject = NULL;
if (mimeparser->mimeroot)
{
mailmime_free(mimeparser->mimeroot);
mimeparser->mimeroot = NULL;
}
mimeparser->is_forwarded = 0;
if (mimeparser->reports) {
carray_set_size(mimeparser->reports, 0);
}
mimeparser->decrypting_failed = 0;
dc_e2ee_thanks(mimeparser->e2ee_helper);
dc_kml_unref(mimeparser->location_kml);
mimeparser->location_kml = NULL;
dc_kml_unref(mimeparser->message_kml);
mimeparser->message_kml = NULL;
}
void dc_mimeparser_repl_msg_by_error(dc_mimeparser_t* mimeparser,
const char* error_msg)
{
dc_mimepart_t* part = NULL;
int            i = 0;
if (mimeparser==NULL || mimeparser->parts==NULL
|| carray_count(mimeparser->parts)<=0) {
return;
}
part = (dc_mimepart_t*)carray_get(mimeparser->parts, 0);
part->type = DC_MSG_TEXT;
free(part->msg);
part->msg = dc_mprintf(DC_EDITORIAL_OPEN "%s" DC_EDITORIAL_CLOSE, error_msg);
for (i = 1; i < carray_count(mimeparser->parts); i++) {
part = (dc_mimepart_t*)carray_get(mimeparser->parts, i);
if (part) {
dc_mimepart_unref(part);
}
}
carray_set_size(mimeparser->parts, 1);
}
static void do_add_single_part(dc_mimeparser_t* parser, dc_mimepart_t* part)
{
if (parser->e2ee_helper->encrypted && dc_hash_cnt(parser->e2ee_helper->signatures)>0) {
dc_param_set_int(part->param, DC_PARAM_GUARANTEE_E2EE, 1);
}
else if (parser->e2ee_helper->encrypted) {
dc_param_set_int(part->param, DC_PARAM_ERRONEOUS_E2EE, DC_E2EE_NO_VALID_SIGNATURE);
}
carray_add(parser->parts, (void*)part, NULL);
}
static void do_add_single_file_part(dc_mimeparser_t* parser, int msg_type, int mime_type,
const char* raw_mime,
const char* decoded_data, size_t decoded_data_bytes,
const char* desired_filename)
{
dc_mimepart_t* part = NULL;
char*          pathNfilename = NULL;
if ((pathNfilename=dc_get_fine_pathNfilename(parser->context, "$BLOBDIR", desired_filename))==NULL) {
goto cleanup;
}
if (dc_write_file(parser->context, pathNfilename, decoded_data, decoded_data_bytes)==0) {
goto cleanup;
}
part = dc_mimepart_new();
part->type  = msg_type;
part->int_mimetype = mime_type;
part->bytes = decoded_data_bytes;
dc_param_set(part->param, DC_PARAM_FILE, pathNfilename);
dc_param_set(part->param, DC_PARAM_MIMETYPE, raw_mime);
if (mime_type==DC_MIMETYPE_IMAGE) {
uint32_t w = 0, h = 0;
if (dc_get_filemeta(decoded_data, decoded_data_bytes, &w, &h)) {
dc_param_set_int(part->param, DC_PARAM_WIDTH, w);
dc_param_set_int(part->param, DC_PARAM_HEIGHT, h);
}
}
do_add_single_part(parser, part);
part = NULL;
cleanup:
free(pathNfilename);
dc_mimepart_unref(part);
}
static int dc_mimeparser_add_single_part_if_known(dc_mimeparser_t* mimeparser, struct mailmime* mime)
{
dc_mimepart_t*               part = NULL;
int                          old_part_count = carray_count(mimeparser->parts);
int                          mime_type;
struct mailmime_data*        mime_data;
char*                        file_suffix = NULL;
char*                        desired_filename = NULL;
int                          msg_type = 0;
char*                        raw_mime = NULL;
char*                        transfer_decoding_buffer = NULL;
char*                        charset_buffer = NULL;
const char*                  decoded_data = NULL;
size_t                       decoded_data_bytes = 0;
dc_simplify_t*               simplifier = NULL;
if (mime==NULL || mime->mm_data.mm_single==NULL) {
goto cleanup;
}
mime_type = mailmime_get_mime_type(mime, &msg_type, &raw_mime);
mime_data = mime->mm_data.mm_single;
if (mime_data->dt_type!=MAILMIME_DATA_TEXT
|| mime_data->dt_data.dt_text.dt_data==NULL
|| mime_data->dt_data.dt_text.dt_length <= 0) {
goto cleanup;
}
if (!mailmime_transfer_decode(mime, &decoded_data, &decoded_data_bytes, &transfer_decoding_buffer)) {
goto cleanup;
}
switch (mime_type)
{
case DC_MIMETYPE_TEXT_PLAIN:
case DC_MIMETYPE_TEXT_HTML:
{
if (simplifier==NULL) {
simplifier = dc_simplify_new();
if (simplifier==NULL) {
goto cleanup;
}
}
const char* charset = mailmime_content_charset_get(mime->mm_content_type);
if (charset!=NULL && strcmp(charset, "utf-8")!=0 && strcmp(charset, "UTF-8")!=0) {
size_t ret_bytes = 0;
int r = charconv_buffer("utf-8", charset, decoded_data, decoded_data_bytes, &charset_buffer, &ret_bytes);
if (r!=MAIL_CHARCONV_NO_ERROR) {
dc_log_warning(mimeparser->context, 0, "Cannot convert %i bytes from \"%s\" to \"utf-8\"; errorcode is %i.",
(int)decoded_data_bytes, charset, (int)r);
}
else if (charset_buffer==NULL || ret_bytes <= 0) {
goto cleanup;
}
else  {
decoded_data = charset_buffer;
decoded_data_bytes = ret_bytes;
}
}
int is_msgrmsg = dc_mimeparser_lookup_optional_field(mimeparser, "Chat-Version")!=NULL;
char* simplified_txt = dc_simplify_simplify(simplifier,
decoded_data, decoded_data_bytes,
mime_type==DC_MIMETYPE_TEXT_HTML? 1 : 0,
is_msgrmsg);
if (simplified_txt && simplified_txt[0])
{
part = dc_mimepart_new();
part->type = DC_MSG_TEXT;
part->int_mimetype = mime_type;
part->msg = simplified_txt;
part->msg_raw = strndup(decoded_data, decoded_data_bytes);
do_add_single_part(mimeparser, part);
part = NULL;
}
else
{
free(simplified_txt);
}
if (simplifier->is_forwarded) {
mimeparser->is_forwarded = 1;
}
}
break;
case DC_MIMETYPE_IMAGE:
case DC_MIMETYPE_AUDIO:
case DC_MIMETYPE_VIDEO:
case DC_MIMETYPE_FILE:
case DC_MIMETYPE_AC_SETUP_FILE:
{
dc_strbuilder_t filename_parts;
dc_strbuilder_init(&filename_parts, 0);
for (clistiter* cur1 = clist_begin(mime->mm_mime_fields->fld_list); cur1!=NULL; cur1 = clist_next(cur1))
{
struct mailmime_field* field = (struct mailmime_field*)clist_content(cur1);
if (field && field->fld_type==MAILMIME_FIELD_DISPOSITION && field->fld_data.fld_disposition)
{
struct mailmime_disposition* file_disposition = field->fld_data.fld_disposition;
if (file_disposition)
{
for (clistiter* cur2 = clist_begin(file_disposition->dsp_parms); cur2!=NULL; cur2 = clist_next(cur2))
{
struct mailmime_disposition_parm* dsp_param = (struct mailmime_disposition_parm*)clist_content(cur2);
if (dsp_param)
{
if (dsp_param->pa_type==MAILMIME_DISPOSITION_PARM_PARAMETER
&& dsp_param->pa_data.pa_parameter
&& dsp_param->pa_data.pa_parameter->pa_name
&& strncmp(dsp_param->pa_data.pa_parameter->pa_name, "filename*", 9)==0)
{
dc_strbuilder_cat(&filename_parts, dsp_param->pa_data.pa_parameter->pa_value);
}
else if (dsp_param->pa_type==MAILMIME_DISPOSITION_PARM_FILENAME)
{
desired_filename = dc_decode_header_words(dsp_param->pa_data.pa_filename);
}
}
}
}
break;
}
}
if (strlen(filename_parts.buf)) {
free(desired_filename);
desired_filename = dc_decode_ext_header(filename_parts.buf);
}
free(filename_parts.buf);
if (desired_filename==NULL) {
struct mailmime_parameter* param = mailmime_find_ct_parameter(mime, "name");
if (param && param->pa_value && param->pa_value[0]) {
desired_filename = dc_strdup(param->pa_value);
}
}
if (desired_filename==NULL) {
if (mime->mm_content_type && mime->mm_content_type->ct_subtype) {
desired_filename = dc_mprintf("file.%s", mime->mm_content_type->ct_subtype);
}
else {
goto cleanup;
}
}
if (strncmp(desired_filename, "location", 8)==0
&& strncmp(desired_filename+strlen(desired_filename)-4, ".kml", 4)==0) {
mimeparser->location_kml = dc_kml_parse(mimeparser->context,
decoded_data, decoded_data_bytes);
goto cleanup;
}
if (strncmp(desired_filename, "message", 7)==0
&& strncmp(desired_filename+strlen(desired_filename)-4, ".kml", 4)==0) {
mimeparser->message_kml = dc_kml_parse(mimeparser->context,
decoded_data, decoded_data_bytes);
goto cleanup;
}
dc_replace_bad_utf8_chars(desired_filename);
do_add_single_file_part(mimeparser, msg_type, mime_type, raw_mime, decoded_data, decoded_data_bytes, desired_filename);
}
break;
default:
break;
}
cleanup:
dc_simplify_unref(simplifier);
if (charset_buffer) { charconv_buffer_free(charset_buffer); }
if (transfer_decoding_buffer) { mmap_string_unref(transfer_decoding_buffer); }
free(file_suffix);
free(desired_filename);
dc_mimepart_unref(part);
free(raw_mime);
return carray_count(mimeparser->parts)>old_part_count? 1 : 0;
}
static int dc_mimeparser_parse_mime_recursive(dc_mimeparser_t* mimeparser, struct mailmime* mime)
{
int        any_part_added = 0;
clistiter* cur = NULL;
if (mimeparser==NULL || mime==NULL) {
return 0;
}
if (mailmime_find_ct_parameter(mime, "protected-headers"))
{
if (mime->mm_type==MAILMIME_SINGLE
&& mime->mm_content_type->ct_type->tp_type==MAILMIME_TYPE_DISCRETE_TYPE
&& mime->mm_content_type->ct_type->tp_data.tp_discrete_type->dt_type==MAILMIME_DISCRETE_TYPE_TEXT
&& mime->mm_content_type->ct_subtype
&& strcmp(mime->mm_content_type->ct_subtype, "rfc822-headers")==0) {
dc_log_info(mimeparser->context, 0, "Protected headers found in text/rfc822-headers attachment: Will be ignored.");
return 0;
}
if (mimeparser->header_protected==NULL) {
size_t dummy = 0;
if (mailimf_envelope_and_optional_fields_parse(mime->mm_mime_start, mime->mm_length, &dummy, &mimeparser->header_protected)!=MAILIMF_NO_ERROR
|| mimeparser->header_protected==NULL) {
dc_log_warning(mimeparser->context, 0, "Protected headers parsing error.");
}
else {
hash_header(&mimeparser->header, mimeparser->header_protected, mimeparser->context);
}
}
else {
dc_log_info(mimeparser->context, 0, "Protected headers found in MIME header: Will be ignored as we already found an outer one.");
}
}
switch (mime->mm_type)
{
case MAILMIME_SINGLE:
any_part_added = dc_mimeparser_add_single_part_if_known(mimeparser, mime);
break;
case MAILMIME_MULTIPLE:
switch (mailmime_get_mime_type(mime, NULL, NULL))
{
case DC_MIMETYPE_MP_ALTERNATIVE:
for (cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list); cur!=NULL; cur=clist_next(cur)) {
struct mailmime* childmime = (struct mailmime*)clist_content(cur);
if (mailmime_get_mime_type(childmime, NULL, NULL)==DC_MIMETYPE_MP_MIXED) {
any_part_added = dc_mimeparser_parse_mime_recursive(mimeparser, childmime);
break;
}
}
if (!any_part_added) {
for (cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list); cur!=NULL; cur=clist_next(cur)) {
struct mailmime* childmime = (struct mailmime*)clist_content(cur);
if (mailmime_get_mime_type(childmime, NULL, NULL)==DC_MIMETYPE_TEXT_PLAIN) {
any_part_added = dc_mimeparser_parse_mime_recursive(mimeparser, childmime);
break;
}
}
}
if (!any_part_added) {
for (cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list); cur!=NULL; cur=clist_next(cur)) {
if (dc_mimeparser_parse_mime_recursive(mimeparser, (struct mailmime*)clist_content(cur))) {
any_part_added = 1;
break;
}
}
}
break;
case DC_MIMETYPE_MP_RELATED:
cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list);
if (cur) {
any_part_added = dc_mimeparser_parse_mime_recursive(mimeparser, (struct mailmime*)clist_content(cur));
}
break;
case DC_MIMETYPE_MP_NOT_DECRYPTABLE:
{
dc_mimepart_t* part = dc_mimepart_new();
part->type = DC_MSG_TEXT;
char* msg_body = dc_stock_str(mimeparser->context, DC_STR_CANTDECRYPT_MSG_BODY);
part->msg = dc_mprintf(DC_EDITORIAL_OPEN "%s" DC_EDITORIAL_CLOSE, msg_body);
part->msg_raw = dc_strdup(part->msg);
free(msg_body);
carray_add(mimeparser->parts, (void*)part, NULL);
any_part_added = 1;
mimeparser->decrypting_failed = 1;
}
break;
case DC_MIMETYPE_MP_SIGNED:
if ((cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list))!=NULL)
{
any_part_added = dc_mimeparser_parse_mime_recursive(mimeparser, (struct mailmime*)clist_content(cur));
}
break;
case DC_MIMETYPE_MP_REPORT:
if (clist_count(mime->mm_data.mm_multipart.mm_mp_list) >= 2)
{
struct mailmime_parameter* report_type = mailmime_find_ct_parameter(mime, "report-type");
if (report_type && report_type->pa_value
&& strcmp(report_type->pa_value, "disposition-notification")==0)
{
carray_add(mimeparser->reports, (void*)mime, NULL);
}
else
{
any_part_added = dc_mimeparser_parse_mime_recursive(mimeparser, (struct mailmime*)clist_content(clist_begin(mime->mm_data.mm_multipart.mm_mp_list)));
}
}
break;
default:
{
struct mailmime* skip_part = NULL;
{
struct mailmime* html_part = NULL;
int plain_cnt = 0, html_cnt = 0;
for (cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list); cur!=NULL; cur=clist_next(cur)) {
struct mailmime* childmime = (struct mailmime*)clist_content(cur);
if (mailmime_get_mime_type(childmime, NULL, NULL)==DC_MIMETYPE_TEXT_PLAIN) {
plain_cnt++;
}
else if (mailmime_get_mime_type(childmime, NULL, NULL)==DC_MIMETYPE_TEXT_HTML) {
html_part = childmime;
html_cnt++;
}
}
if (plain_cnt==1 && html_cnt==1)  {
dc_log_warning(mimeparser->context, 0, "HACK: multipart/mixed message found with PLAIN and HTML, we'll skip the HTML part as this seems to be unwanted.");
skip_part = html_part;
}
}
for (cur=clist_begin(mime->mm_data.mm_multipart.mm_mp_list); cur!=NULL; cur=clist_next(cur)) {
struct mailmime* childmime = (struct mailmime*)clist_content(cur);
if (childmime!=skip_part) {
if (dc_mimeparser_parse_mime_recursive(mimeparser, childmime)) {
any_part_added = 1;
}
}
}
}
break;
}
break;
case MAILMIME_MESSAGE:
if (mimeparser->header_root==NULL)
{
mimeparser->header_root = mime->mm_data.mm_message.mm_fields;
hash_header(&mimeparser->header, mimeparser->header_root, mimeparser->context);
}
if (mime->mm_data.mm_message.mm_msg_mime)
{
any_part_added = dc_mimeparser_parse_mime_recursive(mimeparser, mime->mm_data.mm_message.mm_msg_mime);
}
break;
}
return any_part_added;
}
static void hash_header(dc_hash_t* out, const struct mailimf_fields* in, dc_context_t* context)
{
if (NULL==in) {
return;
}
for (clistiter* cur1=clist_begin(in->fld_list); cur1!=NULL ; cur1=clist_next(cur1))
{
struct mailimf_field* field = (struct mailimf_field*)clist_content(cur1);
const char *key = NULL;
switch (field->fld_type)
{
case MAILIMF_FIELD_RETURN_PATH: key = "Return-Path"; break;
case MAILIMF_FIELD_ORIG_DATE:   key = "Date";        break;
case MAILIMF_FIELD_FROM:        key = "From";        break;
case MAILIMF_FIELD_SENDER:      key = "Sender";      break;
case MAILIMF_FIELD_REPLY_TO:    key = "Reply-To";    break;
case MAILIMF_FIELD_TO:          key = "To";          break;
case MAILIMF_FIELD_CC:          key = "Cc";          break;
case MAILIMF_FIELD_BCC:         key = "Bcc";         break;
case MAILIMF_FIELD_MESSAGE_ID:  key = "Message-ID";  break;
case MAILIMF_FIELD_IN_REPLY_TO: key = "In-Reply-To"; break;
case MAILIMF_FIELD_REFERENCES:  key = "References";  break;
case MAILIMF_FIELD_SUBJECT:     key = "Subject";     break;
case MAILIMF_FIELD_OPTIONAL_FIELD:
{
const struct mailimf_optional_field* optional_field = field->fld_data.fld_optional_field;
if (optional_field) {
key = optional_field->fld_name;
}
}
break;
}
if (key)
{
int key_len = strlen(key);
if (dc_hash_find(out, key, key_len))
{
if (field->fld_type!=MAILIMF_FIELD_OPTIONAL_FIELD
|| (key_len>5 && strncasecmp(key, "Chat-", 5)==0))
{
dc_hash_insert(out, key, key_len, field);
}
}
else
{
dc_hash_insert(out, key, key_len, field);
}
}
}
}
void dc_mimeparser_parse(dc_mimeparser_t* mimeparser, const char* body_not_terminated, size_t body_bytes)
{
int                            r = 0;
size_t                         index = 0;
struct mailimf_optional_field* optional_field = NULL;
dc_mimeparser_empty(mimeparser);
r = mailmime_parse(body_not_terminated, body_bytes, &index, &mimeparser->mimeroot);
if(r!=MAILIMF_NO_ERROR || mimeparser->mimeroot==NULL) {
goto cleanup;
}
dc_e2ee_decrypt(mimeparser->context, mimeparser->mimeroot, mimeparser->e2ee_helper);
dc_mimeparser_parse_mime_recursive(mimeparser, mimeparser->mimeroot);
{
struct mailimf_field* field = dc_mimeparser_lookup_field(mimeparser, "Subject");
if (field && field->fld_type==MAILIMF_FIELD_SUBJECT) {
mimeparser->subject = dc_decode_header_words(field->fld_data.fld_subject->sbj_value);
}
}
if (dc_mimeparser_lookup_optional_field(mimeparser, "Chat-Version")) {
mimeparser->is_send_by_messenger = 1;
}
if (dc_mimeparser_lookup_field(mimeparser, "Autocrypt-Setup-Message")) {
int i, has_setup_file = 0;
for (i = 0; i < carray_count(mimeparser->parts); i++) {
dc_mimepart_t* part = (dc_mimepart_t*)carray_get(mimeparser->parts, i);
if (part->int_mimetype==DC_MIMETYPE_AC_SETUP_FILE) {
has_setup_file = 1;
}
}
if (has_setup_file) {
mimeparser->is_system_message = DC_CMD_AUTOCRYPT_SETUP_MESSAGE;
for (i = 0; i < carray_count(mimeparser->parts); i++) {
dc_mimepart_t* part = (dc_mimepart_t*)carray_get(mimeparser->parts, i);
if (part->int_mimetype!=DC_MIMETYPE_AC_SETUP_FILE) {
dc_mimepart_unref(part);
carray_delete_slow(mimeparser->parts, i);
i--;
}
}
}
}
else if ((optional_field=dc_mimeparser_lookup_optional_field(mimeparser, "Chat-Content"))!=NULL
&& optional_field->fld_value!=NULL)
{
if (strcmp(optional_field->fld_value, "location-streaming-enabled")==0) {
mimeparser->is_system_message = DC_CMD_LOCATION_STREAMING_ENABLED;
}
}
if (dc_mimeparser_lookup_field(mimeparser, "Chat-Group-Image")
&& carray_count(mimeparser->parts)>=1) {
dc_mimepart_t* textpart = (dc_mimepart_t*)carray_get(mimeparser->parts, 0);
if (textpart->type==DC_MSG_TEXT) {
if (carray_count(mimeparser->parts)>=2) {
dc_mimepart_t* imgpart = (dc_mimepart_t*)carray_get(mimeparser->parts, 1);
if (imgpart->type==DC_MSG_IMAGE) {
imgpart->is_meta = 1;
}
}
}
}
if (mimeparser->is_send_by_messenger
&& s_generate_compound_msgs
&& carray_count(mimeparser->parts)==2)
{
dc_mimepart_t* textpart = (dc_mimepart_t*)carray_get(mimeparser->parts, 0);
dc_mimepart_t* filepart = (dc_mimepart_t*)carray_get(mimeparser->parts, 1);
if (textpart->type==DC_MSG_TEXT
&& DC_MSG_NEEDS_ATTACHMENT(filepart->type)
&& !filepart->is_meta)
{
free(filepart->msg);
filepart->msg = textpart->msg;
textpart->msg = NULL;
dc_mimepart_unref(textpart);
carray_delete_slow(mimeparser->parts, 0);
}
}
if (mimeparser->subject)
{
int prepend_subject = 1;
if (!mimeparser->decrypting_failed )
{
char* p = strchr(mimeparser->subject, ':');
if ((p-mimeparser->subject)==2
|| (p-mimeparser->subject)==3
|| mimeparser->is_send_by_messenger
|| strstr(mimeparser->subject, DC_CHAT_PREFIX)!=NULL) {
prepend_subject = 0;
}
}
if (prepend_subject)
{
char* subj = dc_strdup(mimeparser->subject);
char* p = strchr(subj, '[');
if (p) {
*p = 0;
}
dc_trim(subj);
if (subj[0]) {
int i, icnt = carray_count(mimeparser->parts);
for (i = 0; i < icnt; i++) {
dc_mimepart_t* part = (dc_mimepart_t*)carray_get(mimeparser->parts, i);
if (part->type==DC_MSG_TEXT) {
char* new_txt = dc_mprintf("%s " DC_NDASH " %s", subj, part->msg);
free(part->msg);
part->msg = new_txt;
break;
}
}
}
free(subj);
}
}
if (mimeparser->is_forwarded) {
int i, icnt = carray_count(mimeparser->parts);
for (i = 0; i < icnt; i++) {
dc_mimepart_t* part = (dc_mimepart_t*)carray_get(mimeparser->parts, i);
dc_param_set_int(part->param, DC_PARAM_FORWARDED, 1);
}
}
if (carray_count(mimeparser->parts)==1)
{
dc_mimepart_t* part = (dc_mimepart_t*)carray_get(mimeparser->parts, 0);
if (part->type==DC_MSG_AUDIO) {
if (dc_mimeparser_lookup_optional_field(mimeparser, "Chat-Voice-Message")) {
part->type = DC_MSG_VOICE;
}
}
if (part->type==DC_MSG_AUDIO || part->type==DC_MSG_VOICE || part->type==DC_MSG_VIDEO) {
const struct mailimf_optional_field* field = dc_mimeparser_lookup_optional_field(mimeparser, "Chat-Duration");
if (field) {
int duration_ms = atoi(field->fld_value);
if (duration_ms > 0 && duration_ms < 24*60*60*1000) {
dc_param_set_int(part->param, DC_PARAM_DURATION, duration_ms);
}
}
}
}
if (!mimeparser->decrypting_failed)
{
const struct mailimf_optional_field* dn_field = dc_mimeparser_lookup_optional_field(mimeparser, "Chat-Disposition-Notification-To");
if (dn_field && dc_mimeparser_get_last_nonmeta(mimeparser))
{
struct mailimf_mailbox_list* mb_list = NULL;
size_t index = 0;
if (mailimf_mailbox_list_parse(dn_field->fld_value, strlen(dn_field->fld_value), &index, &mb_list)==MAILIMF_NO_ERROR && mb_list)
{
char* dn_to_addr = mailimf_find_first_addr(mb_list);
if (dn_to_addr)
{
struct mailimf_field* from_field = dc_mimeparser_lookup_field(mimeparser, "From");
if (from_field && from_field->fld_type==MAILIMF_FIELD_FROM && from_field->fld_data.fld_from)
{
char* from_addr = mailimf_find_first_addr(from_field->fld_data.fld_from->frm_mb_list);
if (from_addr)
{
if (strcmp(from_addr, dn_to_addr)==0)
{
dc_mimepart_t* part = dc_mimeparser_get_last_nonmeta(mimeparser);
if (part) {
dc_param_set_int(part->param, DC_PARAM_WANTS_MDN, 1);
}
}
free(from_addr);
}
}
free(dn_to_addr);
}
mailimf_mailbox_list_free(mb_list);
}
}
}
cleanup:
if (!dc_mimeparser_has_nonmeta(mimeparser) && carray_count(mimeparser->reports)==0) {
dc_mimepart_t* part = dc_mimepart_new();
part->type = DC_MSG_TEXT;
if (mimeparser->subject && !mimeparser->is_send_by_messenger) {
part->msg = dc_strdup(mimeparser->subject);
}
else {
part->msg = dc_strdup("");
}
carray_add(mimeparser->parts, (void*)part, NULL);
}
}
struct mailimf_field* dc_mimeparser_lookup_field(dc_mimeparser_t* mimeparser, const char* field_name)
{
return (struct mailimf_field*)dc_hash_find_str(&mimeparser->header, field_name);
}
struct mailimf_optional_field* dc_mimeparser_lookup_optional_field(dc_mimeparser_t* mimeparser, const char* field_name)
{
struct mailimf_field* field = dc_hash_find_str(&mimeparser->header, field_name);
if (field && field->fld_type==MAILIMF_FIELD_OPTIONAL_FIELD) {
return field->fld_data.fld_optional_field;
}
return NULL;
}
dc_mimepart_t* dc_mimeparser_get_last_nonmeta(dc_mimeparser_t* mimeparser)
{
if (mimeparser && mimeparser->parts) {
int i, icnt = carray_count(mimeparser->parts);
for (i = icnt-1; i >= 0; i--) {
dc_mimepart_t* part = (dc_mimepart_t*)carray_get(mimeparser->parts, i);
if (part && !part->is_meta) {
return part;
}
}
}
return NULL;
}
int dc_mimeparser_is_mailinglist_message(dc_mimeparser_t* mimeparser)
{
if (mimeparser==NULL) {
return 0;
}
if (dc_mimeparser_lookup_field(mimeparser, "List-Id")!=NULL) {
return 1;
}
struct mailimf_optional_field* precedence = dc_mimeparser_lookup_optional_field(mimeparser, "Precedence");
if (precedence!=NULL) {
if (strcasecmp(precedence->fld_value, "list")==0
|| strcasecmp(precedence->fld_value, "bulk")==0) {
return 1;
}
}
return 0;
}
int dc_mimeparser_sender_equals_recipient(dc_mimeparser_t* mimeparser)
{
int                         sender_equals_recipient = 0;
const struct mailimf_field* fld = NULL;
const struct mailimf_from*  fld_from = NULL;
struct mailimf_mailbox*     mb = NULL;
char*                       from_addr_norm = NULL;
dc_hash_t*                  recipients = NULL;
if (mimeparser==NULL || mimeparser->header_root==NULL) {
goto cleanup;
}
if ((fld=mailimf_find_field(mimeparser->header_root, MAILIMF_FIELD_FROM))==NULL
|| (fld_from=fld->fld_data.fld_from)==NULL
|| fld_from->frm_mb_list==NULL
|| fld_from->frm_mb_list->mb_list==NULL
|| clist_count(fld_from->frm_mb_list->mb_list)!=1) {
goto cleanup;
}
mb = (struct mailimf_mailbox*)clist_content(clist_begin(fld_from->frm_mb_list->mb_list));
if (mb==NULL) {
goto cleanup;
}
from_addr_norm = dc_addr_normalize(mb->mb_addr_spec);
recipients = mailimf_get_recipients(mimeparser->header_root);
if (dc_hash_cnt(recipients)!=1) {
goto cleanup;
}
if (dc_hash_find_str(recipients, from_addr_norm)) {
sender_equals_recipient = 1;
}
cleanup:
dc_hash_clear(recipients);
free(recipients);
free(from_addr_norm);
return sender_equals_recipient;
}