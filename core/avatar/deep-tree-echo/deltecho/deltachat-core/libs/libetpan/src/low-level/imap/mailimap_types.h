#ifndef MAILIMAP_TYPES_H
#define MAILIMAP_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/mailstream.h>
#include <libetpan/clist.h>
#include <stdbool.h>
struct mailimap_address {
char * ad_personal_name;
char * ad_source_route;
char * ad_mailbox_name;
char * ad_host_name;
};
LIBETPAN_EXPORT
struct mailimap_address *
mailimap_address_new(char * ad_personal_name, char * ad_source_route,
char * ad_mailbox_name, char * ad_host_name);
LIBETPAN_EXPORT
void mailimap_address_free(struct mailimap_address * addr);
enum {
MAILIMAP_BODY_ERROR,
MAILIMAP_BODY_1PART,
MAILIMAP_BODY_MPART
};
struct mailimap_body {
int bd_type;
union {
struct mailimap_body_type_1part * bd_body_1part;
struct mailimap_body_type_mpart * bd_body_mpart;
} bd_data;
};
LIBETPAN_EXPORT
struct mailimap_body *
mailimap_body_new(int bd_type,
struct mailimap_body_type_1part * bd_body_1part,
struct mailimap_body_type_mpart * bd_body_mpart);
LIBETPAN_EXPORT
void mailimap_body_free(struct mailimap_body * body);
enum {
MAILIMAP_BODY_EXTENSION_ERROR,
MAILIMAP_BODY_EXTENSION_NSTRING,
MAILIMAP_BODY_EXTENSION_NUMBER,
MAILIMAP_BODY_EXTENSION_LIST
};
struct mailimap_body_extension {
int ext_type;
union {
char * ext_nstring;
uint32_t ext_number;
clist * ext_body_extension_list;
} ext_data;
};
LIBETPAN_EXPORT
struct mailimap_body_extension *
mailimap_body_extension_new(int ext_type, char * ext_nstring,
uint32_t ext_number,
clist * ext_body_extension_list);
LIBETPAN_EXPORT
void mailimap_body_extension_free(struct mailimap_body_extension * be);
struct mailimap_body_ext_1part {
char * bd_md5;
struct mailimap_body_fld_dsp * bd_disposition;
struct mailimap_body_fld_lang * bd_language;
char * bd_loc;
clist * bd_extension_list;
};
LIBETPAN_EXPORT
struct mailimap_body_ext_1part *
mailimap_body_ext_1part_new(char * bd_md5,
struct mailimap_body_fld_dsp * bd_disposition,
struct mailimap_body_fld_lang * bd_language,
char * bd_loc,
clist * bd_extension_list);
LIBETPAN_EXPORT
void
mailimap_body_ext_1part_free(struct mailimap_body_ext_1part * body_ext_1part);
struct mailimap_body_ext_mpart {
struct mailimap_body_fld_param * bd_parameter;
struct mailimap_body_fld_dsp * bd_disposition;
struct mailimap_body_fld_lang * bd_language;
char * bd_loc;
clist * bd_extension_list;
};
LIBETPAN_EXPORT
struct mailimap_body_ext_mpart *
mailimap_body_ext_mpart_new(struct mailimap_body_fld_param * bd_parameter,
struct mailimap_body_fld_dsp * bd_disposition,
struct mailimap_body_fld_lang * bd_language,
char * bd_loc,
clist * bd_extension_list);
LIBETPAN_EXPORT
void
mailimap_body_ext_mpart_free(struct mailimap_body_ext_mpart * body_ext_mpart);
struct mailimap_body_fields {
struct mailimap_body_fld_param * bd_parameter;
char * bd_id;
char * bd_description;
struct mailimap_body_fld_enc * bd_encoding;
uint32_t bd_size;
};
LIBETPAN_EXPORT
struct mailimap_body_fields *
mailimap_body_fields_new(struct mailimap_body_fld_param * bd_parameter,
char * bd_id,
char * bd_description,
struct mailimap_body_fld_enc * bd_encoding,
uint32_t bd_size);
LIBETPAN_EXPORT
void
mailimap_body_fields_free(struct mailimap_body_fields * body_fields);
struct mailimap_body_fld_dsp {
char * dsp_type;
struct mailimap_body_fld_param * dsp_attributes;
};
LIBETPAN_EXPORT
struct mailimap_body_fld_dsp *
mailimap_body_fld_dsp_new(char * dsp_type,
struct mailimap_body_fld_param * dsp_attributes);
LIBETPAN_EXPORT
void mailimap_body_fld_dsp_free(struct mailimap_body_fld_dsp * bfd);
enum {
MAILIMAP_BODY_FLD_ENC_7BIT,
MAILIMAP_BODY_FLD_ENC_8BIT,
MAILIMAP_BODY_FLD_ENC_BINARY,
MAILIMAP_BODY_FLD_ENC_BASE64,
MAILIMAP_BODY_FLD_ENC_QUOTED_PRINTABLE,
MAILIMAP_BODY_FLD_ENC_OTHER
};
struct mailimap_body_fld_enc {
int enc_type;
char * enc_value;
};
LIBETPAN_EXPORT
struct mailimap_body_fld_enc *
mailimap_body_fld_enc_new(int enc_type, char * enc_value);
LIBETPAN_EXPORT
void mailimap_body_fld_enc_free(struct mailimap_body_fld_enc * bfe);
enum {
MAILIMAP_BODY_FLD_LANG_ERROR,
MAILIMAP_BODY_FLD_LANG_SINGLE,
MAILIMAP_BODY_FLD_LANG_LIST
};
struct mailimap_body_fld_lang {
int lg_type;
union {
char * lg_single;
clist * lg_list;
} lg_data;
};
LIBETPAN_EXPORT
struct mailimap_body_fld_lang *
mailimap_body_fld_lang_new(int lg_type, char * lg_single, clist * lg_list);
LIBETPAN_EXPORT
void
mailimap_body_fld_lang_free(struct mailimap_body_fld_lang * fld_lang);
struct mailimap_single_body_fld_param {
char * pa_name;
char * pa_value;
};
LIBETPAN_EXPORT
struct mailimap_single_body_fld_param *
mailimap_single_body_fld_param_new(char * pa_name, char * pa_value);
LIBETPAN_EXPORT
void
mailimap_single_body_fld_param_free(struct mailimap_single_body_fld_param * p);
struct mailimap_body_fld_param {
clist * pa_list;
};
LIBETPAN_EXPORT
struct mailimap_body_fld_param *
mailimap_body_fld_param_new(clist * pa_list);
LIBETPAN_EXPORT
void
mailimap_body_fld_param_free(struct mailimap_body_fld_param * fld_param);
enum {
MAILIMAP_BODY_TYPE_1PART_ERROR,
MAILIMAP_BODY_TYPE_1PART_BASIC,
MAILIMAP_BODY_TYPE_1PART_MSG,
MAILIMAP_BODY_TYPE_1PART_TEXT
};
struct mailimap_body_type_1part {
int bd_type;
union {
struct mailimap_body_type_basic * bd_type_basic;
struct mailimap_body_type_msg * bd_type_msg;
struct mailimap_body_type_text * bd_type_text;
} bd_data;
struct mailimap_body_ext_1part * bd_ext_1part;
};
LIBETPAN_EXPORT
struct mailimap_body_type_1part *
mailimap_body_type_1part_new(int bd_type,
struct mailimap_body_type_basic * bd_type_basic,
struct mailimap_body_type_msg * bd_type_msg,
struct mailimap_body_type_text * bd_type_text,
struct mailimap_body_ext_1part * bd_ext_1part);
LIBETPAN_EXPORT
void
mailimap_body_type_1part_free(struct mailimap_body_type_1part * bt1p);
struct mailimap_body_type_basic {
struct mailimap_media_basic * bd_media_basic;
struct mailimap_body_fields * bd_fields;
};
LIBETPAN_EXPORT
struct mailimap_body_type_basic *
mailimap_body_type_basic_new(struct mailimap_media_basic * bd_media_basic,
struct mailimap_body_fields * bd_fields);
LIBETPAN_EXPORT
void mailimap_body_type_basic_free(struct mailimap_body_type_basic *
body_type_basic);
struct mailimap_body_type_mpart {
clist * bd_list;
char * bd_media_subtype;
struct mailimap_body_ext_mpart * bd_ext_mpart;
};
LIBETPAN_EXPORT
struct mailimap_body_type_mpart *
mailimap_body_type_mpart_new(clist * bd_list, char * bd_media_subtype,
struct mailimap_body_ext_mpart * bd_ext_mpart);
LIBETPAN_EXPORT
void mailimap_body_type_mpart_free(struct mailimap_body_type_mpart *
body_type_mpart);
struct mailimap_body_type_msg {
struct mailimap_body_fields * bd_fields;
struct mailimap_envelope * bd_envelope;
struct mailimap_body * bd_body;
uint32_t bd_lines;
};
LIBETPAN_EXPORT
struct mailimap_body_type_msg *
mailimap_body_type_msg_new(struct mailimap_body_fields * bd_fields,
struct mailimap_envelope * bd_envelope,
struct mailimap_body * bd_body,
uint32_t bd_lines);
LIBETPAN_EXPORT
void
mailimap_body_type_msg_free(struct mailimap_body_type_msg * body_type_msg);
struct mailimap_body_type_text {
char * bd_media_text;
struct mailimap_body_fields * bd_fields;
uint32_t bd_lines;
};
LIBETPAN_EXPORT
struct mailimap_body_type_text *
mailimap_body_type_text_new(char * bd_media_text,
struct mailimap_body_fields * bd_fields,
uint32_t bd_lines);
LIBETPAN_EXPORT
void
mailimap_body_type_text_free(struct mailimap_body_type_text * body_type_text);
enum {
MAILIMAP_CAPABILITY_AUTH_TYPE,
MAILIMAP_CAPABILITY_NAME
};
struct mailimap_capability {
int cap_type;
union {
char * cap_auth_type;
char * cap_name;
} cap_data;
};
LIBETPAN_EXPORT
struct mailimap_capability *
mailimap_capability_new(int cap_type, char * cap_auth_type, char * cap_name);
LIBETPAN_EXPORT
void mailimap_capability_free(struct mailimap_capability * c);
struct mailimap_capability_data {
clist * cap_list;
};
LIBETPAN_EXPORT
struct mailimap_capability_data *
mailimap_capability_data_new(clist * cap_list);
LIBETPAN_EXPORT
void
mailimap_capability_data_free(struct mailimap_capability_data * cap_data);
enum {
MAILIMAP_CONTINUE_REQ_ERROR,
MAILIMAP_CONTINUE_REQ_TEXT,
MAILIMAP_CONTINUE_REQ_BASE64
};
struct mailimap_continue_req {
int cr_type;
union {
struct mailimap_resp_text * cr_text;
char * cr_base64;
} cr_data;
};
LIBETPAN_EXPORT
struct mailimap_continue_req *
mailimap_continue_req_new(int cr_type, struct mailimap_resp_text * cr_text,
char * cr_base64);
LIBETPAN_EXPORT
void mailimap_continue_req_free(struct mailimap_continue_req * cont_req);
struct mailimap_date_time {
int dt_day;
int dt_month;
int dt_year;
int dt_hour;
int dt_min;
int dt_sec;
int dt_zone;
};
LIBETPAN_EXPORT
struct mailimap_date_time *
mailimap_date_time_new(int dt_day, int dt_month, int dt_year, int dt_hour,
int dt_min, int dt_sec, int dt_zone);
LIBETPAN_EXPORT
void mailimap_date_time_free(struct mailimap_date_time * date_time);
struct mailimap_envelope {
char * env_date;
char * env_subject;
struct mailimap_env_from * env_from;
struct mailimap_env_sender * env_sender;
struct mailimap_env_reply_to * env_reply_to;
struct mailimap_env_to * env_to;
struct mailimap_env_cc * env_cc;
struct mailimap_env_bcc * env_bcc;
char * env_in_reply_to;
char * env_message_id;
};
LIBETPAN_EXPORT
struct mailimap_envelope *
mailimap_envelope_new(char * env_date, char * env_subject,
struct mailimap_env_from * env_from,
struct mailimap_env_sender * env_sender,
struct mailimap_env_reply_to * env_reply_to,
struct mailimap_env_to * env_to,
struct mailimap_env_cc* env_cc,
struct mailimap_env_bcc * env_bcc,
char * env_in_reply_to, char * env_message_id);
LIBETPAN_EXPORT
void mailimap_envelope_free(struct mailimap_envelope * env);
struct mailimap_env_bcc {
clist * bcc_list;
};
LIBETPAN_EXPORT
struct mailimap_env_bcc * mailimap_env_bcc_new(clist * bcc_list);
LIBETPAN_EXPORT
void mailimap_env_bcc_free(struct mailimap_env_bcc * env_bcc);
struct mailimap_env_cc {
clist * cc_list;
};
LIBETPAN_EXPORT
struct mailimap_env_cc * mailimap_env_cc_new(clist * cc_list);
LIBETPAN_EXPORT
void mailimap_env_cc_free(struct mailimap_env_cc * env_cc);
struct mailimap_env_from {
clist * frm_list;
};
LIBETPAN_EXPORT
struct mailimap_env_from * mailimap_env_from_new(clist * frm_list);
LIBETPAN_EXPORT
void mailimap_env_from_free(struct mailimap_env_from * env_from);
struct mailimap_env_reply_to {
clist * rt_list;
};
LIBETPAN_EXPORT
struct mailimap_env_reply_to * mailimap_env_reply_to_new(clist * rt_list);
LIBETPAN_EXPORT
void
mailimap_env_reply_to_free(struct mailimap_env_reply_to * env_reply_to);
struct mailimap_env_sender {
clist * snd_list;
};
LIBETPAN_EXPORT
struct mailimap_env_sender * mailimap_env_sender_new(clist * snd_list);
LIBETPAN_EXPORT
void mailimap_env_sender_free(struct mailimap_env_sender * env_sender);
struct mailimap_env_to {
clist * to_list;
};
LIBETPAN_EXPORT
struct mailimap_env_to * mailimap_env_to_new(clist * to_list);
LIBETPAN_EXPORT
void mailimap_env_to_free(struct mailimap_env_to * env_to);
enum {
MAILIMAP_FLAG_ANSWERED,
MAILIMAP_FLAG_FLAGGED,
MAILIMAP_FLAG_DELETED,
MAILIMAP_FLAG_SEEN,
MAILIMAP_FLAG_DRAFT,
MAILIMAP_FLAG_KEYWORD,
MAILIMAP_FLAG_EXTENSION
};
struct mailimap_flag {
int fl_type;
union {
char * fl_keyword;
char * fl_extension;
} fl_data;
};
LIBETPAN_EXPORT
struct mailimap_flag * mailimap_flag_new(int fl_type,
char * fl_keyword, char * fl_extension);
LIBETPAN_EXPORT
void mailimap_flag_free(struct mailimap_flag * f);
enum {
MAILIMAP_FLAG_FETCH_ERROR,
MAILIMAP_FLAG_FETCH_RECENT,
MAILIMAP_FLAG_FETCH_OTHER
};
struct mailimap_flag_fetch {
int fl_type;
struct mailimap_flag * fl_flag;
};
LIBETPAN_EXPORT
struct mailimap_flag_fetch *
mailimap_flag_fetch_new(int fl_type, struct mailimap_flag * fl_flag);
LIBETPAN_EXPORT
void mailimap_flag_fetch_free(struct mailimap_flag_fetch * flag_fetch);
enum {
MAILIMAP_FLAG_PERM_ERROR,
MAILIMAP_FLAG_PERM_FLAG,
MAILIMAP_FLAG_PERM_ALL
};
struct mailimap_flag_perm {
int fl_type;
struct mailimap_flag * fl_flag;
};
LIBETPAN_EXPORT
struct mailimap_flag_perm *
mailimap_flag_perm_new(int fl_type, struct mailimap_flag * fl_flag);
LIBETPAN_EXPORT
void mailimap_flag_perm_free(struct mailimap_flag_perm * flag_perm);
struct mailimap_flag_list {
clist * fl_list;
};
LIBETPAN_EXPORT
struct mailimap_flag_list *
mailimap_flag_list_new(clist * fl_list);
LIBETPAN_EXPORT
void mailimap_flag_list_free(struct mailimap_flag_list * flag_list);
enum {
MAILIMAP_GREETING_RESP_COND_ERROR,
MAILIMAP_GREETING_RESP_COND_AUTH,
MAILIMAP_GREETING_RESP_COND_BYE
};
struct mailimap_greeting {
int gr_type;
union {
struct mailimap_resp_cond_auth * gr_auth;
struct mailimap_resp_cond_bye * gr_bye;
} gr_data;
};
LIBETPAN_EXPORT
struct mailimap_greeting *
mailimap_greeting_new(int gr_type,
struct mailimap_resp_cond_auth * gr_auth,
struct mailimap_resp_cond_bye * gr_bye);
LIBETPAN_EXPORT
void mailimap_greeting_free(struct mailimap_greeting * greeting);
struct mailimap_header_list {
clist * hdr_list;
};
LIBETPAN_EXPORT
struct mailimap_header_list *
mailimap_header_list_new(clist * hdr_list);
LIBETPAN_EXPORT
void
mailimap_header_list_free(struct mailimap_header_list * header_list);
enum {
MAILIMAP_STATUS_ATT_MESSAGES,
MAILIMAP_STATUS_ATT_RECENT,
MAILIMAP_STATUS_ATT_UIDNEXT,
MAILIMAP_STATUS_ATT_UIDVALIDITY,
MAILIMAP_STATUS_ATT_UNSEEN,
MAILIMAP_STATUS_ATT_HIGHESTMODSEQ,
MAILIMAP_STATUS_ATT_EXTENSION
};
struct mailimap_status_info {
int st_att;
uint32_t st_value;
struct mailimap_extension_data * st_ext_data;
};
LIBETPAN_EXPORT
struct mailimap_status_info *
mailimap_status_info_new(int st_att, uint32_t st_value,
struct mailimap_extension_data * st_ext_data);
LIBETPAN_EXPORT
void mailimap_status_info_free(struct mailimap_status_info * info);
struct mailimap_mailbox_data_status {
char * st_mailbox;
clist * st_info_list;
};
LIBETPAN_EXPORT
struct mailimap_mailbox_data_status *
mailimap_mailbox_data_status_new(char * st_mailbox,
clist * st_info_list);
LIBETPAN_EXPORT
void
mailimap_mailbox_data_status_free(struct mailimap_mailbox_data_status * info);
enum {
MAILIMAP_MAILBOX_DATA_ERROR,
MAILIMAP_MAILBOX_DATA_FLAGS,
MAILIMAP_MAILBOX_DATA_LIST,
MAILIMAP_MAILBOX_DATA_LSUB,
MAILIMAP_MAILBOX_DATA_SEARCH,
MAILIMAP_MAILBOX_DATA_STATUS,
MAILIMAP_MAILBOX_DATA_EXISTS,
MAILIMAP_MAILBOX_DATA_RECENT,
MAILIMAP_MAILBOX_DATA_EXTENSION_DATA
};
struct mailimap_mailbox_data {
int mbd_type;
union {
struct mailimap_flag_list * mbd_flags;
struct mailimap_mailbox_list * mbd_list;
struct mailimap_mailbox_list * mbd_lsub;
clist * mbd_search;
struct mailimap_mailbox_data_status *  mbd_status;
uint32_t mbd_exists;
uint32_t mbd_recent;
struct mailimap_extension_data * mbd_extension;
} mbd_data;
};
LIBETPAN_EXPORT
struct mailimap_mailbox_data *
mailimap_mailbox_data_new(int mbd_type, struct mailimap_flag_list * mbd_flags,
struct mailimap_mailbox_list * mbd_list,
struct mailimap_mailbox_list * mbd_lsub,
clist * mbd_search,
struct mailimap_mailbox_data_status * mbd_status,
uint32_t mbd_exists,
uint32_t mbd_recent,
struct mailimap_extension_data * mbd_extension);
LIBETPAN_EXPORT
void
mailimap_mailbox_data_free(struct mailimap_mailbox_data * mb_data);
enum {
MAILIMAP_MBX_LIST_FLAGS_SFLAG,
MAILIMAP_MBX_LIST_FLAGS_NO_SFLAG
};
enum {
MAILIMAP_MBX_LIST_SFLAG_ERROR,
MAILIMAP_MBX_LIST_SFLAG_MARKED,
MAILIMAP_MBX_LIST_SFLAG_NOSELECT,
MAILIMAP_MBX_LIST_SFLAG_UNMARKED
};
struct mailimap_mbx_list_flags {
int mbf_type;
clist * mbf_oflags;
int mbf_sflag;
};
LIBETPAN_EXPORT
struct mailimap_mbx_list_flags *
mailimap_mbx_list_flags_new(int mbf_type,
clist * mbf_oflags, int mbf_sflag);
LIBETPAN_EXPORT
void
mailimap_mbx_list_flags_free(struct mailimap_mbx_list_flags * mbx_list_flags);
enum {
MAILIMAP_MBX_LIST_OFLAG_ERROR,
MAILIMAP_MBX_LIST_OFLAG_NOINFERIORS,
MAILIMAP_MBX_LIST_OFLAG_FLAG_EXT
};
struct mailimap_mbx_list_oflag {
int of_type;
char * of_flag_ext;
};
LIBETPAN_EXPORT
struct mailimap_mbx_list_oflag *
mailimap_mbx_list_oflag_new(int of_type, char * of_flag_ext);
LIBETPAN_EXPORT
void
mailimap_mbx_list_oflag_free(struct mailimap_mbx_list_oflag * oflag);
struct mailimap_mailbox_list {
struct mailimap_mbx_list_flags * mb_flag;
char mb_delimiter;
char * mb_name;
};
LIBETPAN_EXPORT
struct mailimap_mailbox_list *
mailimap_mailbox_list_new(struct mailimap_mbx_list_flags * mbx_flags,
char mb_delimiter, char * mb_name);
LIBETPAN_EXPORT
void
mailimap_mailbox_list_free(struct mailimap_mailbox_list * mb_list);
enum {
MAILIMAP_MEDIA_BASIC_APPLICATION,
MAILIMAP_MEDIA_BASIC_AUDIO,
MAILIMAP_MEDIA_BASIC_IMAGE,
MAILIMAP_MEDIA_BASIC_MESSAGE,
MAILIMAP_MEDIA_BASIC_VIDEO,
MAILIMAP_MEDIA_BASIC_OTHER
};
struct mailimap_media_basic {
int med_type;
char * med_basic_type;
char * med_subtype;
};
LIBETPAN_EXPORT
struct mailimap_media_basic *
mailimap_media_basic_new(int med_type,
char * med_basic_type, char * med_subtype);
LIBETPAN_EXPORT
void
mailimap_media_basic_free(struct mailimap_media_basic * media_basic);
enum {
MAILIMAP_MESSAGE_DATA_ERROR,
MAILIMAP_MESSAGE_DATA_EXPUNGE,
MAILIMAP_MESSAGE_DATA_FETCH
};
struct mailimap_message_data {
uint32_t mdt_number;
int mdt_type;
struct mailimap_msg_att * mdt_msg_att;
};
LIBETPAN_EXPORT
struct mailimap_message_data *
mailimap_message_data_new(uint32_t mdt_number, int mdt_type,
struct mailimap_msg_att * mdt_msg_att);
LIBETPAN_EXPORT
void
mailimap_message_data_free(struct mailimap_message_data * msg_data);
enum {
MAILIMAP_MSG_ATT_ITEM_ERROR,
MAILIMAP_MSG_ATT_ITEM_DYNAMIC,
MAILIMAP_MSG_ATT_ITEM_STATIC,
MAILIMAP_MSG_ATT_ITEM_EXTENSION
};
struct mailimap_msg_att_item {
int att_type;
union {
struct mailimap_msg_att_dynamic * att_dyn;
struct mailimap_msg_att_static * att_static;
struct mailimap_extension_data * att_extension_data;
} att_data;
};
LIBETPAN_EXPORT
struct mailimap_msg_att_item *
mailimap_msg_att_item_new(int att_type,
struct mailimap_msg_att_dynamic * att_dyn,
struct mailimap_msg_att_static * att_static,
struct mailimap_extension_data * att_extension_data);
LIBETPAN_EXPORT
void
mailimap_msg_att_item_free(struct mailimap_msg_att_item * item);
struct mailimap_msg_att {
clist * att_list;
uint32_t att_number;
};
LIBETPAN_EXPORT
struct mailimap_msg_att * mailimap_msg_att_new(clist * att_list);
LIBETPAN_EXPORT
void mailimap_msg_att_free(struct mailimap_msg_att * msg_att);
struct mailimap_msg_att_dynamic {
clist * att_list;
};
LIBETPAN_EXPORT
struct mailimap_msg_att_dynamic *
mailimap_msg_att_dynamic_new(clist * att_list);
LIBETPAN_EXPORT
void
mailimap_msg_att_dynamic_free(struct mailimap_msg_att_dynamic * msg_att_dyn);
struct mailimap_msg_att_body_section {
struct mailimap_section * sec_section;
uint32_t sec_origin_octet;
char * sec_body_part;
size_t sec_length;
};
LIBETPAN_EXPORT
struct mailimap_msg_att_body_section *
mailimap_msg_att_body_section_new(struct mailimap_section * section,
uint32_t sec_origin_octet,
char * sec_body_part,
size_t sec_length);
LIBETPAN_EXPORT
void
mailimap_msg_att_body_section_free(struct mailimap_msg_att_body_section *
msg_att_body_section);
enum {
MAILIMAP_MSG_ATT_ERROR,
MAILIMAP_MSG_ATT_ENVELOPE,
MAILIMAP_MSG_ATT_INTERNALDATE,
MAILIMAP_MSG_ATT_RFC822,
MAILIMAP_MSG_ATT_RFC822_HEADER,
MAILIMAP_MSG_ATT_RFC822_TEXT,
MAILIMAP_MSG_ATT_RFC822_SIZE,
MAILIMAP_MSG_ATT_BODY,
MAILIMAP_MSG_ATT_BODYSTRUCTURE,
MAILIMAP_MSG_ATT_BODY_SECTION,
MAILIMAP_MSG_ATT_UID
};
struct mailimap_msg_att_static {
int att_type;
union {
struct mailimap_envelope * att_env;
struct mailimap_date_time * att_internal_date;
struct {
char * att_content;
size_t att_length;
} att_rfc822;
struct {
char * att_content;
size_t att_length;
} att_rfc822_header;
struct {
char * att_content;
size_t att_length;
} att_rfc822_text;
uint32_t att_rfc822_size;
struct mailimap_body * att_bodystructure;
struct mailimap_body * att_body;
struct mailimap_msg_att_body_section * att_body_section;
uint32_t att_uid;
} att_data;
};
LIBETPAN_EXPORT
struct mailimap_msg_att_static *
mailimap_msg_att_static_new(int att_type, struct mailimap_envelope * att_env,
struct mailimap_date_time * att_internal_date,
char * att_rfc822,
char * att_rfc822_header,
char * att_rfc822_text,
size_t att_length,
uint32_t att_rfc822_size,
struct mailimap_body * att_bodystructure,
struct mailimap_body * att_body,
struct mailimap_msg_att_body_section * att_body_section,
uint32_t att_uid);
LIBETPAN_EXPORT
void
mailimap_msg_att_static_free(struct mailimap_msg_att_static * item);
enum {
MAILIMAP_RESP_ERROR,
MAILIMAP_RESP_CONT_REQ,
MAILIMAP_RESP_RESP_DATA
};
struct mailimap_cont_req_or_resp_data {
int rsp_type;
union {
struct mailimap_continue_req * rsp_cont_req;
struct mailimap_response_data * rsp_resp_data;
} rsp_data;
};
LIBETPAN_EXPORT
struct mailimap_cont_req_or_resp_data *
mailimap_cont_req_or_resp_data_new(int rsp_type,
struct mailimap_continue_req * rsp_cont_req,
struct mailimap_response_data * rsp_resp_data);
LIBETPAN_EXPORT
void
mailimap_cont_req_or_resp_data_free(struct mailimap_cont_req_or_resp_data *
cont_req_or_resp_data);
struct mailimap_response {
clist * rsp_cont_req_or_resp_data_list;
struct mailimap_response_done * rsp_resp_done;
};
LIBETPAN_EXPORT
struct mailimap_response *
mailimap_response_new(clist * rsp_cont_req_or_resp_data_list,
struct mailimap_response_done * rsp_resp_done);
LIBETPAN_EXPORT
void
mailimap_response_free(struct mailimap_response * resp);
enum {
MAILIMAP_RESP_DATA_TYPE_ERROR,
MAILIMAP_RESP_DATA_TYPE_COND_STATE,
MAILIMAP_RESP_DATA_TYPE_COND_BYE,
MAILIMAP_RESP_DATA_TYPE_MAILBOX_DATA,
MAILIMAP_RESP_DATA_TYPE_MESSAGE_DATA,
MAILIMAP_RESP_DATA_TYPE_CAPABILITY_DATA,
MAILIMAP_RESP_DATA_TYPE_EXTENSION_DATA
};
struct mailimap_response_data {
int rsp_type;
union {
struct mailimap_resp_cond_state * rsp_cond_state;
struct mailimap_resp_cond_bye * rsp_bye;
struct mailimap_mailbox_data * rsp_mailbox_data;
struct mailimap_message_data * rsp_message_data;
struct mailimap_capability_data * rsp_capability_data;
struct mailimap_extension_data * rsp_extension_data;
} rsp_data;
};
LIBETPAN_EXPORT
struct mailimap_response_data *
mailimap_response_data_new(int rsp_type,
struct mailimap_resp_cond_state * rsp_cond_state,
struct mailimap_resp_cond_bye * rsp_bye,
struct mailimap_mailbox_data * rsp_mailbox_data,
struct mailimap_message_data * rsp_message_data,
struct mailimap_capability_data * rsp_capability_data,
struct mailimap_extension_data * rsp_extension_data);
LIBETPAN_EXPORT
void
mailimap_response_data_free(struct mailimap_response_data * resp_data);
enum {
MAILIMAP_RESP_DONE_TYPE_ERROR,
MAILIMAP_RESP_DONE_TYPE_TAGGED,
MAILIMAP_RESP_DONE_TYPE_FATAL
};
struct mailimap_response_done {
int rsp_type;
union {
struct mailimap_response_tagged * rsp_tagged;
struct mailimap_response_fatal * rsp_fatal;
} rsp_data;
};
LIBETPAN_EXPORT
struct mailimap_response_done *
mailimap_response_done_new(int rsp_type,
struct mailimap_response_tagged * rsp_tagged,
struct mailimap_response_fatal * rsp_fatal);
LIBETPAN_EXPORT
void mailimap_response_done_free(struct mailimap_response_done *
resp_done);
struct mailimap_response_fatal {
struct mailimap_resp_cond_bye * rsp_bye;
};
LIBETPAN_EXPORT
struct mailimap_response_fatal *
mailimap_response_fatal_new(struct mailimap_resp_cond_bye * rsp_bye);
LIBETPAN_EXPORT
void mailimap_response_fatal_free(struct mailimap_response_fatal * resp_fatal);
struct mailimap_response_tagged {
char * rsp_tag;
struct mailimap_resp_cond_state * rsp_cond_state;
};
LIBETPAN_EXPORT
struct mailimap_response_tagged *
mailimap_response_tagged_new(char * rsp_tag,
struct mailimap_resp_cond_state * rsp_cond_state);
LIBETPAN_EXPORT
void
mailimap_response_tagged_free(struct mailimap_response_tagged * tagged);
enum {
MAILIMAP_RESP_COND_AUTH_ERROR,
MAILIMAP_RESP_COND_AUTH_OK,
MAILIMAP_RESP_COND_AUTH_PREAUTH
};
struct mailimap_resp_cond_auth {
int rsp_type;
struct mailimap_resp_text * rsp_text;
};
LIBETPAN_EXPORT
struct mailimap_resp_cond_auth *
mailimap_resp_cond_auth_new(int rsp_type,
struct mailimap_resp_text * rsp_text);
LIBETPAN_EXPORT
void
mailimap_resp_cond_auth_free(struct mailimap_resp_cond_auth * cond_auth);
struct mailimap_resp_cond_bye {
struct mailimap_resp_text * rsp_text;
};
LIBETPAN_EXPORT
struct mailimap_resp_cond_bye *
mailimap_resp_cond_bye_new(struct mailimap_resp_text * rsp_text);
LIBETPAN_EXPORT
void
mailimap_resp_cond_bye_free(struct mailimap_resp_cond_bye * cond_bye);
enum {
MAILIMAP_RESP_COND_STATE_OK,
MAILIMAP_RESP_COND_STATE_NO,
MAILIMAP_RESP_COND_STATE_BAD
};
struct mailimap_resp_cond_state {
int rsp_type;
struct mailimap_resp_text * rsp_text;
};
LIBETPAN_EXPORT
struct mailimap_resp_cond_state *
mailimap_resp_cond_state_new(int rsp_type,
struct mailimap_resp_text * rsp_text);
LIBETPAN_EXPORT
void
mailimap_resp_cond_state_free(struct mailimap_resp_cond_state * cond_state);
struct mailimap_resp_text {
struct mailimap_resp_text_code * rsp_code;
char * rsp_text;
};
LIBETPAN_EXPORT
struct mailimap_resp_text *
mailimap_resp_text_new(struct mailimap_resp_text_code * resp_code,
char * rsp_text);
LIBETPAN_EXPORT
void mailimap_resp_text_free(struct mailimap_resp_text * resp_text);
enum {
MAILIMAP_RESP_TEXT_CODE_ALERT,
MAILIMAP_RESP_TEXT_CODE_BADCHARSET,
MAILIMAP_RESP_TEXT_CODE_CAPABILITY_DATA,
MAILIMAP_RESP_TEXT_CODE_PARSE,
MAILIMAP_RESP_TEXT_CODE_PERMANENTFLAGS,
MAILIMAP_RESP_TEXT_CODE_READ_ONLY,
MAILIMAP_RESP_TEXT_CODE_READ_WRITE,
MAILIMAP_RESP_TEXT_CODE_TRY_CREATE,
MAILIMAP_RESP_TEXT_CODE_UIDNEXT,
MAILIMAP_RESP_TEXT_CODE_UIDVALIDITY,
MAILIMAP_RESP_TEXT_CODE_UNSEEN,
MAILIMAP_RESP_TEXT_CODE_OTHER,
MAILIMAP_RESP_TEXT_CODE_EXTENSION
};
struct mailimap_resp_text_code {
int rc_type;
union {
clist * rc_badcharset;
struct mailimap_capability_data * rc_cap_data;
clist * rc_perm_flags;
uint32_t rc_uidnext;
uint32_t rc_uidvalidity;
uint32_t rc_first_unseen;
struct {
char * atom_name;
char * atom_value;
} rc_atom;
struct mailimap_extension_data * rc_ext_data;
} rc_data;
};
LIBETPAN_EXPORT
struct mailimap_resp_text_code *
mailimap_resp_text_code_new(int rc_type, clist * rc_badcharset,
struct mailimap_capability_data * rc_cap_data,
clist * rc_perm_flags,
uint32_t rc_uidnext, uint32_t rc_uidvalidity,
uint32_t rc_first_unseen, char * rc_atom, char * rc_atom_value,
struct mailimap_extension_data * rc_ext_data);
LIBETPAN_EXPORT
void
mailimap_resp_text_code_free(struct mailimap_resp_text_code * resp_text_code);
struct mailimap_section {
struct mailimap_section_spec * sec_spec;
};
LIBETPAN_EXPORT
struct mailimap_section *
mailimap_section_new(struct mailimap_section_spec * sec_spec);
LIBETPAN_EXPORT
void mailimap_section_free(struct mailimap_section * section);
enum {
MAILIMAP_SECTION_MSGTEXT_HEADER,
MAILIMAP_SECTION_MSGTEXT_HEADER_FIELDS,
MAILIMAP_SECTION_MSGTEXT_HEADER_FIELDS_NOT,
MAILIMAP_SECTION_MSGTEXT_TEXT
};
struct mailimap_section_msgtext {
int sec_type;
struct mailimap_header_list * sec_header_list;
};
LIBETPAN_EXPORT
struct mailimap_section_msgtext *
mailimap_section_msgtext_new(int sec_type,
struct mailimap_header_list * sec_header_list);
LIBETPAN_EXPORT
void
mailimap_section_msgtext_free(struct mailimap_section_msgtext * msgtext);
struct mailimap_section_part {
clist * sec_id;
};
LIBETPAN_EXPORT
struct mailimap_section_part *
mailimap_section_part_new(clist * sec_id);
LIBETPAN_EXPORT
void
mailimap_section_part_free(struct mailimap_section_part * section_part);
enum {
MAILIMAP_SECTION_SPEC_SECTION_MSGTEXT,
MAILIMAP_SECTION_SPEC_SECTION_PART
};
struct mailimap_section_spec {
int sec_type;
union {
struct mailimap_section_msgtext * sec_msgtext;
struct mailimap_section_part * sec_part;
} sec_data;
struct mailimap_section_text * sec_text;
};
LIBETPAN_EXPORT
struct mailimap_section_spec *
mailimap_section_spec_new(int sec_type,
struct mailimap_section_msgtext * sec_msgtext,
struct mailimap_section_part * sec_part,
struct mailimap_section_text * sec_text);
LIBETPAN_EXPORT
void
mailimap_section_spec_free(struct mailimap_section_spec * section_spec);
enum {
MAILIMAP_SECTION_TEXT_ERROR,
MAILIMAP_SECTION_TEXT_SECTION_MSGTEXT,
MAILIMAP_SECTION_TEXT_MIME
};
struct mailimap_section_text {
int sec_type;
struct mailimap_section_msgtext * sec_msgtext;
};
LIBETPAN_EXPORT
struct mailimap_section_text *
mailimap_section_text_new(int sec_type,
struct mailimap_section_msgtext * sec_msgtext);
LIBETPAN_EXPORT
void
mailimap_section_text_free(struct mailimap_section_text * section_text);
struct mailimap_set_item {
uint32_t set_first;
uint32_t set_last;
};
LIBETPAN_EXPORT
struct mailimap_set_item *
mailimap_set_item_new(uint32_t set_first, uint32_t set_last);
LIBETPAN_EXPORT
void mailimap_set_item_free(struct mailimap_set_item * set_item);
struct mailimap_set {
clist * set_list;
};
LIBETPAN_EXPORT
struct mailimap_set * mailimap_set_new(clist * list);
LIBETPAN_EXPORT
void mailimap_set_free(struct mailimap_set * set);
struct mailimap_date {
int dt_day;
int dt_month;
int dt_year;
};
LIBETPAN_EXPORT
struct mailimap_date *
mailimap_date_new(int dt_day, int dt_month, int dt_year);
LIBETPAN_EXPORT
void mailimap_date_free(struct mailimap_date * date);
enum {
MAILIMAP_FETCH_ATT_ENVELOPE,
MAILIMAP_FETCH_ATT_FLAGS,
MAILIMAP_FETCH_ATT_INTERNALDATE,
MAILIMAP_FETCH_ATT_RFC822,
MAILIMAP_FETCH_ATT_RFC822_HEADER,
MAILIMAP_FETCH_ATT_RFC822_SIZE,
MAILIMAP_FETCH_ATT_RFC822_TEXT,
MAILIMAP_FETCH_ATT_BODY,
MAILIMAP_FETCH_ATT_BODYSTRUCTURE,
MAILIMAP_FETCH_ATT_UID,
MAILIMAP_FETCH_ATT_BODY_SECTION,
MAILIMAP_FETCH_ATT_BODY_PEEK_SECTION,
MAILIMAP_FETCH_ATT_EXTENSION
};
struct mailimap_fetch_att {
int att_type;
struct mailimap_section * att_section;
uint32_t att_offset;
uint32_t att_size;
char * att_extension;
};
LIBETPAN_EXPORT
struct mailimap_fetch_att *
mailimap_fetch_att_new(int att_type, struct mailimap_section * att_section,
uint32_t att_offset, uint32_t att_size, char * att_extension);
LIBETPAN_EXPORT
void mailimap_fetch_att_free(struct mailimap_fetch_att * fetch_att);
enum {
MAILIMAP_FETCH_TYPE_ALL,
MAILIMAP_FETCH_TYPE_FULL,
MAILIMAP_FETCH_TYPE_FAST,
MAILIMAP_FETCH_TYPE_FETCH_ATT,
MAILIMAP_FETCH_TYPE_FETCH_ATT_LIST
};
struct mailimap_fetch_type {
int ft_type;
union {
struct mailimap_fetch_att * ft_fetch_att;
clist * ft_fetch_att_list;
} ft_data;
};
LIBETPAN_EXPORT
struct mailimap_fetch_type *
mailimap_fetch_type_new(int ft_type,
struct mailimap_fetch_att * ft_fetch_att,
clist * ft_fetch_att_list);
LIBETPAN_EXPORT
void mailimap_fetch_type_free(struct mailimap_fetch_type * fetch_type);
struct mailimap_store_att_flags {
int fl_sign;
int fl_silent;
struct mailimap_flag_list * fl_flag_list;
};
LIBETPAN_EXPORT
struct mailimap_store_att_flags *
mailimap_store_att_flags_new(int fl_sign, int fl_silent,
struct mailimap_flag_list * fl_flag_list);
LIBETPAN_EXPORT
void mailimap_store_att_flags_free(struct mailimap_store_att_flags *
store_att_flags);
enum {
MAILIMAP_SEARCH_KEY_ALL,
MAILIMAP_SEARCH_KEY_ANSWERED,
MAILIMAP_SEARCH_KEY_BCC,
MAILIMAP_SEARCH_KEY_BEFORE,
MAILIMAP_SEARCH_KEY_BODY,
MAILIMAP_SEARCH_KEY_CC,
MAILIMAP_SEARCH_KEY_DELETED,
MAILIMAP_SEARCH_KEY_FLAGGED,
MAILIMAP_SEARCH_KEY_FROM,
MAILIMAP_SEARCH_KEY_KEYWORD,
MAILIMAP_SEARCH_KEY_NEW,
MAILIMAP_SEARCH_KEY_OLD,
MAILIMAP_SEARCH_KEY_ON,
MAILIMAP_SEARCH_KEY_RECENT,
MAILIMAP_SEARCH_KEY_SEEN,
MAILIMAP_SEARCH_KEY_SINCE,
MAILIMAP_SEARCH_KEY_SUBJECT,
MAILIMAP_SEARCH_KEY_TEXT,
MAILIMAP_SEARCH_KEY_TO,
MAILIMAP_SEARCH_KEY_UNANSWERED,
MAILIMAP_SEARCH_KEY_UNDELETED,
MAILIMAP_SEARCH_KEY_UNFLAGGED,
MAILIMAP_SEARCH_KEY_UNKEYWORD,
MAILIMAP_SEARCH_KEY_UNSEEN,
MAILIMAP_SEARCH_KEY_DRAFT,
MAILIMAP_SEARCH_KEY_HEADER,
MAILIMAP_SEARCH_KEY_LARGER,
MAILIMAP_SEARCH_KEY_NOT,
MAILIMAP_SEARCH_KEY_OR,
MAILIMAP_SEARCH_KEY_SENTBEFORE,
MAILIMAP_SEARCH_KEY_SENTON,
MAILIMAP_SEARCH_KEY_SENTSINCE,
MAILIMAP_SEARCH_KEY_SMALLER,
MAILIMAP_SEARCH_KEY_UID,
MAILIMAP_SEARCH_KEY_UNDRAFT,
MAILIMAP_SEARCH_KEY_SET,
MAILIMAP_SEARCH_KEY_MULTIPLE,
MAILIMAP_SEARCH_KEY_MODSEQ,
MAILIMAP_SEARCH_KEY_XGMTHRID,
MAILIMAP_SEARCH_KEY_XGMMSGID,
MAILIMAP_SEARCH_KEY_XGMRAW
};
enum {
MAILIMAP_SEARCH_KEY_MODSEQ_ENTRY_TYPE_REQ_PRIV,
MAILIMAP_SEARCH_KEY_MODSEQ_ENTRY_TYPE_REQ_SHARED,
MAILIMAP_SEARCH_KEY_MODSEQ_ENTRY_TYPE_REQ_ALL,
};
struct mailimap_search_key {
int sk_type;
union {
char * sk_bcc;
struct mailimap_date * sk_before;
char * sk_body;
char * sk_cc;
char * sk_from;
char * sk_keyword;
struct mailimap_date * sk_on;
struct mailimap_date * sk_since;
char * sk_subject;
char * sk_text;
char * sk_to;
char * sk_unkeyword;
struct {
char * sk_header_name;
char * sk_header_value;
} sk_header;
uint32_t sk_larger;
struct mailimap_search_key * sk_not;
struct {
struct mailimap_search_key * sk_or1;
struct mailimap_search_key * sk_or2;
} sk_or;
struct mailimap_date * sk_sentbefore;
struct mailimap_date * sk_senton;
struct mailimap_date * sk_sentsince;
uint32_t sk_smaller;
struct mailimap_set * sk_uid;
struct mailimap_set * sk_set;
uint64_t sk_xgmthrid;
uint64_t sk_xgmmsgid;
char * sk_xgmraw;
clist * sk_multiple;
struct {
struct mailimap_flag * sk_entry_name;
int sk_entry_type_req;
uint64_t sk_modseq_valzer;
} sk_modseq;
} sk_data;
};
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new(int sk_type,
char * sk_bcc, struct mailimap_date * sk_before, char * sk_body,
char * sk_cc, char * sk_from, char * sk_keyword,
struct mailimap_date * sk_on, struct mailimap_date * sk_since,
char * sk_subject, char * sk_text, char * sk_to,
char * sk_unkeyword, char * sk_header_name,
char * sk_header_value, uint32_t sk_larger,
struct mailimap_search_key * sk_not,
struct mailimap_search_key * sk_or1,
struct mailimap_search_key * sk_or2,
struct mailimap_date * sk_sentbefore,
struct mailimap_date * sk_senton,
struct mailimap_date * sk_sentsince,
uint32_t sk_smaller, struct mailimap_set * sk_uid,
struct mailimap_set * sk_set, clist * sk_multiple);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_xgmthrid(uint64_t sk_xgmthrid);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_xgmmsgid(uint64_t sk_xgmmsgid);
LIBETPAN_EXPORT
struct mailimap_search_key *
mailimap_search_key_new_xgmraw(char * sk_xgmraw);
LIBETPAN_EXPORT
void mailimap_search_key_free(struct mailimap_search_key * key);
struct mailimap_status_att_list {
clist * att_list;
};
LIBETPAN_EXPORT
struct mailimap_status_att_list *
mailimap_status_att_list_new(clist * att_list);
LIBETPAN_EXPORT
void mailimap_status_att_list_free(struct mailimap_status_att_list *
status_att_list);
LIBETPAN_EXPORT
uint32_t * mailimap_number_alloc_new(uint32_t number);
LIBETPAN_EXPORT
void mailimap_number_alloc_free(uint32_t * pnumber);
LIBETPAN_EXPORT
void mailimap_addr_host_free(char * addr_host);
LIBETPAN_EXPORT
void mailimap_addr_mailbox_free(char * addr_mailbox);
LIBETPAN_EXPORT
void mailimap_addr_adl_free(char * addr_adl);
LIBETPAN_EXPORT
void mailimap_addr_name_free(char * addr_name);
LIBETPAN_EXPORT
void mailimap_astring_free(char * astring);
LIBETPAN_EXPORT
void mailimap_atom_free(char * atom);
LIBETPAN_EXPORT
void mailimap_auth_type_free(char * auth_type);
LIBETPAN_EXPORT
void mailimap_base64_free(char * base64);
LIBETPAN_EXPORT
void mailimap_body_fld_desc_free(char * body_fld_desc);
LIBETPAN_EXPORT
void mailimap_body_fld_id_free(char * body_fld_id);
LIBETPAN_EXPORT
void mailimap_body_fld_md5_free(char * body_fld_md5);
LIBETPAN_EXPORT
void mailimap_body_fld_loc_free(char * body_fld_loc);
LIBETPAN_EXPORT
void mailimap_env_date_free(char * date);
LIBETPAN_EXPORT
void mailimap_env_in_reply_to_free(char * in_reply_to);
LIBETPAN_EXPORT
void mailimap_env_message_id_free(char * message_id);
LIBETPAN_EXPORT
void mailimap_env_subject_free(char * subject);
LIBETPAN_EXPORT
void mailimap_flag_extension_free(char * flag_extension);
LIBETPAN_EXPORT
void mailimap_flag_keyword_free(char * flag_keyword);
LIBETPAN_EXPORT
void
mailimap_header_fld_name_free(char * header_fld_name);
LIBETPAN_EXPORT
void mailimap_literal_free(char * literal);
LIBETPAN_EXPORT
void mailimap_mailbox_free(char * mailbox);
LIBETPAN_EXPORT
void
mailimap_mailbox_data_search_free(clist * data_search);
LIBETPAN_EXPORT
void mailimap_media_subtype_free(char * media_subtype);
LIBETPAN_EXPORT
void mailimap_media_text_free(char * media_text);
LIBETPAN_EXPORT
void mailimap_msg_att_envelope_free(struct mailimap_envelope * env);
LIBETPAN_EXPORT
void
mailimap_msg_att_internaldate_free(struct mailimap_date_time * date_time);
LIBETPAN_EXPORT
void
mailimap_msg_att_rfc822_free(char * str);
LIBETPAN_EXPORT
void
mailimap_msg_att_rfc822_header_free(char * str);
LIBETPAN_EXPORT
void
mailimap_msg_att_rfc822_text_free(char * str);
LIBETPAN_EXPORT
void
mailimap_msg_att_body_free(struct mailimap_body * body);
LIBETPAN_EXPORT
void
mailimap_msg_att_bodystructure_free(struct mailimap_body * body);
LIBETPAN_EXPORT
void mailimap_nstring_free(char * str);
LIBETPAN_EXPORT
void
mailimap_string_free(char * str);
LIBETPAN_EXPORT
void mailimap_tag_free(char * tag);
LIBETPAN_EXPORT
void mailimap_text_free(char * text);
enum {
MAILIMAP_STATE_DISCONNECTED,
MAILIMAP_STATE_NON_AUTHENTICATED,
MAILIMAP_STATE_AUTHENTICATED,
MAILIMAP_STATE_SELECTED,
MAILIMAP_STATE_LOGOUT
};
typedef void mailimap_msg_att_handler(struct mailimap_msg_att * msg_att, void * context);
typedef bool mailimap_msg_body_handler(int msg_att_type, struct mailimap_msg_att_body_section * section,
const char * bytes, size_t length, void * context);
typedef struct mailimap mailimap;
struct mailimap {
char * imap_response;
mailstream * imap_stream;
size_t imap_progr_rate;
progress_function * imap_progr_fun;
MMAPString * imap_stream_buffer;
MMAPString * imap_response_buffer;
int imap_state;
int imap_tag;
struct mailimap_connection_info * imap_connection_info;
struct mailimap_selection_info * imap_selection_info;
struct mailimap_response_info * imap_response_info;
struct {
void * sasl_conn;
const char * sasl_server_fqdn;
const char * sasl_login;
const char * sasl_auth_name;
const char * sasl_password;
const char * sasl_realm;
void * sasl_secret;
} imap_sasl;
time_t imap_idle_timestamp;
time_t imap_idle_maxdelay;
mailprogress_function * imap_body_progress_fun;
mailprogress_function * imap_items_progress_fun;
void * imap_progress_context;
mailimap_msg_att_handler * imap_msg_att_handler;
void * imap_msg_att_handler_context;
mailimap_msg_body_handler * imap_msg_body_handler;
void * imap_msg_body_handler_context;
time_t imap_timeout;
void (* imap_logger)(mailimap * session, int log_type, const char * str, size_t size, void * context);
void * imap_logger_context;
int is_163_workaround_enabled;
int is_rambler_workaround_enabled;
int is_qip_workaround_enabled;
};
struct mailimap_connection_info {
struct mailimap_capability_data * imap_capability;
};
LIBETPAN_EXPORT
struct mailimap_connection_info *
mailimap_connection_info_new(void);
LIBETPAN_EXPORT
void
mailimap_connection_info_free(struct mailimap_connection_info * conn_info);
enum {
MAILIMAP_MAILBOX_READONLY,
MAILIMAP_MAILBOX_READWRITE
};
struct mailimap_selection_info {
clist * sel_perm_flags;
int sel_perm;
uint32_t sel_uidnext;
uint32_t sel_uidvalidity;
uint32_t sel_first_unseen;
struct mailimap_flag_list * sel_flags;
uint32_t sel_exists;
uint32_t sel_recent;
uint32_t sel_unseen;
uint8_t  sel_has_exists:1;
uint8_t  sel_has_recent:1;
};
LIBETPAN_EXPORT
struct mailimap_selection_info *
mailimap_selection_info_new(void);
LIBETPAN_EXPORT
void
mailimap_selection_info_free(struct mailimap_selection_info * sel_info);
struct mailimap_response_info {
char * rsp_alert;
char * rsp_parse;
clist * rsp_badcharset;
int rsp_trycreate;
clist * rsp_mailbox_list;
clist * rsp_mailbox_lsub;
clist * rsp_search_result;
struct mailimap_mailbox_data_status * rsp_status;
clist * rsp_expunged;
clist * rsp_fetch_list;
clist * rsp_extension_list;
char * rsp_atom;
char * rsp_value;
};
LIBETPAN_EXPORT
struct mailimap_response_info *
mailimap_response_info_new(void);
LIBETPAN_EXPORT
void
mailimap_response_info_free(struct mailimap_response_info * resp_info);
enum {
MAILIMAP_NO_ERROR = 0,
MAILIMAP_NO_ERROR_AUTHENTICATED = 1,
MAILIMAP_NO_ERROR_NON_AUTHENTICATED = 2,
MAILIMAP_ERROR_BAD_STATE,
MAILIMAP_ERROR_STREAM,
MAILIMAP_ERROR_PARSE,
MAILIMAP_ERROR_CONNECTION_REFUSED,
MAILIMAP_ERROR_MEMORY,
MAILIMAP_ERROR_FATAL,
MAILIMAP_ERROR_PROTOCOL,
MAILIMAP_ERROR_DONT_ACCEPT_CONNECTION,
MAILIMAP_ERROR_APPEND,
MAILIMAP_ERROR_NOOP,
MAILIMAP_ERROR_LOGOUT,
MAILIMAP_ERROR_CAPABILITY,
MAILIMAP_ERROR_CHECK,
MAILIMAP_ERROR_CLOSE,
MAILIMAP_ERROR_EXPUNGE,
MAILIMAP_ERROR_COPY,
MAILIMAP_ERROR_UID_COPY,
MAILIMAP_ERROR_MOVE,
MAILIMAP_ERROR_UID_MOVE,
MAILIMAP_ERROR_CREATE,
MAILIMAP_ERROR_DELETE,
MAILIMAP_ERROR_EXAMINE,
MAILIMAP_ERROR_FETCH,
MAILIMAP_ERROR_UID_FETCH,
MAILIMAP_ERROR_LIST,
MAILIMAP_ERROR_LOGIN,
MAILIMAP_ERROR_LSUB,
MAILIMAP_ERROR_RENAME,
MAILIMAP_ERROR_SEARCH,
MAILIMAP_ERROR_UID_SEARCH,
MAILIMAP_ERROR_SELECT,
MAILIMAP_ERROR_STATUS,
MAILIMAP_ERROR_STORE,
MAILIMAP_ERROR_UID_STORE,
MAILIMAP_ERROR_SUBSCRIBE,
MAILIMAP_ERROR_UNSUBSCRIBE,
MAILIMAP_ERROR_STARTTLS,
MAILIMAP_ERROR_INVAL,
MAILIMAP_ERROR_EXTENSION,
MAILIMAP_ERROR_SASL,
MAILIMAP_ERROR_SSL,
MAILIMAP_ERROR_NEEDS_MORE_DATA,
MAILIMAP_ERROR_CUSTOM_COMMAND
};
struct mailimap_parser_context {
int is_rambler_workaround_enabled;
int is_qip_workaround_enabled;
mailimap_msg_body_handler * msg_body_handler;
void * msg_body_handler_context;
struct mailimap_msg_att_body_section * msg_body_section;
int msg_body_att_type;
bool msg_body_parse_in_progress;
};
LIBETPAN_EXPORT
struct mailimap_parser_context *
mailimap_parser_context_new(mailimap * session);
LIBETPAN_EXPORT
void
mailimap_parser_context_free(struct mailimap_parser_context * ctx);
LIBETPAN_EXPORT
int
mailimap_parser_context_is_rambler_workaround_enabled(struct mailimap_parser_context * parser_ctx);
LIBETPAN_EXPORT
int
mailimap_parser_context_is_qip_workaround_enabled(struct mailimap_parser_context * parser_ctx);
#ifdef __cplusplus
}
#endif
#endif