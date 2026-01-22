#ifndef MAILIMF_TYPES_H
#define MAILIMF_TYPES_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/libetpan-config.h>
#include <libetpan/clist.h>
#include <sys/types.h>
struct mailimf_date_time {
int dt_day;
int dt_month;
int dt_year;
int dt_hour;
int dt_min;
int dt_sec;
int dt_zone;
};
LIBETPAN_EXPORT
struct mailimf_date_time *
mailimf_date_time_new(int dt_day, int dt_month, int dt_year,
int dt_hour, int dt_min, int dt_sec, int dt_zone);
LIBETPAN_EXPORT
void mailimf_date_time_free(struct mailimf_date_time * date_time);
enum {
MAILIMF_ADDRESS_ERROR,
MAILIMF_ADDRESS_MAILBOX,
MAILIMF_ADDRESS_GROUP
};
struct mailimf_address {
int ad_type;
union {
struct mailimf_mailbox * ad_mailbox;
struct mailimf_group * ad_group;
} ad_data;
};
LIBETPAN_EXPORT
struct mailimf_address *
mailimf_address_new(int ad_type, struct mailimf_mailbox * ad_mailbox,
struct mailimf_group * ad_group);
LIBETPAN_EXPORT
void mailimf_address_free(struct mailimf_address * address);
struct mailimf_mailbox {
char * mb_display_name;
char * mb_addr_spec;
};
LIBETPAN_EXPORT
struct mailimf_mailbox *
mailimf_mailbox_new(char * mb_display_name, char * mb_addr_spec);
LIBETPAN_EXPORT
void mailimf_mailbox_free(struct mailimf_mailbox * mailbox);
struct mailimf_group {
char * grp_display_name;
struct mailimf_mailbox_list * grp_mb_list;
};
LIBETPAN_EXPORT
struct mailimf_group *
mailimf_group_new(char * grp_display_name,
struct mailimf_mailbox_list * grp_mb_list);
LIBETPAN_EXPORT
void mailimf_group_free(struct mailimf_group * group);
struct mailimf_mailbox_list {
clist * mb_list;
};
LIBETPAN_EXPORT
struct mailimf_mailbox_list *
mailimf_mailbox_list_new(clist * mb_list);
LIBETPAN_EXPORT
void mailimf_mailbox_list_free(struct mailimf_mailbox_list * mb_list);
struct mailimf_address_list {
clist * ad_list;
};
LIBETPAN_EXPORT
struct mailimf_address_list *
mailimf_address_list_new(clist * ad_list);
LIBETPAN_EXPORT
void mailimf_address_list_free(struct mailimf_address_list * addr_list);
struct mailimf_body {
const char * bd_text;
size_t bd_size;
};
LIBETPAN_EXPORT
struct mailimf_body * mailimf_body_new(const char * bd_text, size_t bd_size);
LIBETPAN_EXPORT
void mailimf_body_free(struct mailimf_body * body);
struct mailimf_message {
struct mailimf_fields * msg_fields;
struct mailimf_body * msg_body;
};
LIBETPAN_EXPORT
struct mailimf_message *
mailimf_message_new(struct mailimf_fields * msg_fields,
struct mailimf_body * msg_body);
LIBETPAN_EXPORT
void mailimf_message_free(struct mailimf_message * message);
struct mailimf_fields {
clist * fld_list;
};
LIBETPAN_EXPORT
struct mailimf_fields * mailimf_fields_new(clist * fld_list);
LIBETPAN_EXPORT
void mailimf_fields_free(struct mailimf_fields * fields);
enum {
MAILIMF_FIELD_NONE,
MAILIMF_FIELD_RETURN_PATH,
MAILIMF_FIELD_RESENT_DATE,
MAILIMF_FIELD_RESENT_FROM,
MAILIMF_FIELD_RESENT_SENDER,
MAILIMF_FIELD_RESENT_TO,
MAILIMF_FIELD_RESENT_CC,
MAILIMF_FIELD_RESENT_BCC,
MAILIMF_FIELD_RESENT_MSG_ID,
MAILIMF_FIELD_ORIG_DATE,
MAILIMF_FIELD_FROM,
MAILIMF_FIELD_SENDER,
MAILIMF_FIELD_REPLY_TO,
MAILIMF_FIELD_TO,
MAILIMF_FIELD_CC,
MAILIMF_FIELD_BCC,
MAILIMF_FIELD_MESSAGE_ID,
MAILIMF_FIELD_IN_REPLY_TO,
MAILIMF_FIELD_REFERENCES,
MAILIMF_FIELD_SUBJECT,
MAILIMF_FIELD_COMMENTS,
MAILIMF_FIELD_KEYWORDS,
MAILIMF_FIELD_OPTIONAL_FIELD
};
#define LIBETPAN_MAILIMF_FIELD_UNION
struct mailimf_field {
int fld_type;
union {
struct mailimf_return * fld_return_path;
struct mailimf_orig_date * fld_resent_date;
struct mailimf_from * fld_resent_from;
struct mailimf_sender * fld_resent_sender;
struct mailimf_to * fld_resent_to;
struct mailimf_cc * fld_resent_cc;
struct mailimf_bcc * fld_resent_bcc;
struct mailimf_message_id * fld_resent_msg_id;
struct mailimf_orig_date * fld_orig_date;
struct mailimf_from * fld_from;
struct mailimf_sender * fld_sender;
struct mailimf_reply_to * fld_reply_to;
struct mailimf_to * fld_to;
struct mailimf_cc * fld_cc;
struct mailimf_bcc * fld_bcc;
struct mailimf_message_id * fld_message_id;
struct mailimf_in_reply_to * fld_in_reply_to;
struct mailimf_references * fld_references;
struct mailimf_subject * fld_subject;
struct mailimf_comments * fld_comments;
struct mailimf_keywords * fld_keywords;
struct mailimf_optional_field * fld_optional_field;
} fld_data;
};
LIBETPAN_EXPORT
struct mailimf_field *
mailimf_field_new(int fld_type,
struct mailimf_return * fld_return_path,
struct mailimf_orig_date * fld_resent_date,
struct mailimf_from * fld_resent_from,
struct mailimf_sender * fld_resent_sender,
struct mailimf_to * fld_resent_to,
struct mailimf_cc * fld_resent_cc,
struct mailimf_bcc * fld_resent_bcc,
struct mailimf_message_id * fld_resent_msg_id,
struct mailimf_orig_date * fld_orig_date,
struct mailimf_from * fld_from,
struct mailimf_sender * fld_sender,
struct mailimf_reply_to * fld_reply_to,
struct mailimf_to * fld_to,
struct mailimf_cc * fld_cc,
struct mailimf_bcc * fld_bcc,
struct mailimf_message_id * fld_message_id,
struct mailimf_in_reply_to * fld_in_reply_to,
struct mailimf_references * fld_references,
struct mailimf_subject * fld_subject,
struct mailimf_comments * fld_comments,
struct mailimf_keywords * fld_keywords,
struct mailimf_optional_field * fld_optional_field);
LIBETPAN_EXPORT
void mailimf_field_free(struct mailimf_field * field);
struct mailimf_orig_date {
struct mailimf_date_time * dt_date_time;
};
LIBETPAN_EXPORT
struct mailimf_orig_date * mailimf_orig_date_new(struct mailimf_date_time *
dt_date_time);
LIBETPAN_EXPORT
void mailimf_orig_date_free(struct mailimf_orig_date * orig_date);
struct mailimf_from {
struct mailimf_mailbox_list * frm_mb_list;
};
LIBETPAN_EXPORT
struct mailimf_from *
mailimf_from_new(struct mailimf_mailbox_list * frm_mb_list);
LIBETPAN_EXPORT
void mailimf_from_free(struct mailimf_from * from);
struct mailimf_sender {
struct mailimf_mailbox * snd_mb;
};
LIBETPAN_EXPORT
struct mailimf_sender * mailimf_sender_new(struct mailimf_mailbox * snd_mb);
LIBETPAN_EXPORT
void mailimf_sender_free(struct mailimf_sender * sender);
struct mailimf_reply_to {
struct mailimf_address_list * rt_addr_list;
};
LIBETPAN_EXPORT
struct mailimf_reply_to *
mailimf_reply_to_new(struct mailimf_address_list * rt_addr_list);
LIBETPAN_EXPORT
void mailimf_reply_to_free(struct mailimf_reply_to * reply_to);
struct mailimf_to {
struct mailimf_address_list * to_addr_list;
};
LIBETPAN_EXPORT
struct mailimf_to * mailimf_to_new(struct mailimf_address_list * to_addr_list);
LIBETPAN_EXPORT
void mailimf_to_free(struct mailimf_to * to);
struct mailimf_cc {
struct mailimf_address_list * cc_addr_list;
};
LIBETPAN_EXPORT
struct mailimf_cc * mailimf_cc_new(struct mailimf_address_list * cc_addr_list);
LIBETPAN_EXPORT
void mailimf_cc_free(struct mailimf_cc * cc);
struct mailimf_bcc {
struct mailimf_address_list * bcc_addr_list;
};
LIBETPAN_EXPORT
struct mailimf_bcc *
mailimf_bcc_new(struct mailimf_address_list * bcc_addr_list);
LIBETPAN_EXPORT
void mailimf_bcc_free(struct mailimf_bcc * bcc);
struct mailimf_message_id {
char * mid_value;
};
LIBETPAN_EXPORT
struct mailimf_message_id * mailimf_message_id_new(char * mid_value);
LIBETPAN_EXPORT
void mailimf_message_id_free(struct mailimf_message_id * message_id);
struct mailimf_in_reply_to {
clist * mid_list;
};
LIBETPAN_EXPORT
struct mailimf_in_reply_to * mailimf_in_reply_to_new(clist * mid_list);
LIBETPAN_EXPORT
void mailimf_in_reply_to_free(struct mailimf_in_reply_to * in_reply_to);
struct mailimf_references {
clist * mid_list;
};
LIBETPAN_EXPORT
struct mailimf_references * mailimf_references_new(clist * mid_list);
LIBETPAN_EXPORT
void mailimf_references_free(struct mailimf_references * references);
struct mailimf_subject {
char * sbj_value;
};
LIBETPAN_EXPORT
struct mailimf_subject * mailimf_subject_new(char * sbj_value);
LIBETPAN_EXPORT
void mailimf_subject_free(struct mailimf_subject * subject);
struct mailimf_comments {
char * cm_value;
};
LIBETPAN_EXPORT
struct mailimf_comments * mailimf_comments_new(char * cm_value);
LIBETPAN_EXPORT
void mailimf_comments_free(struct mailimf_comments * comments);
struct mailimf_keywords {
clist * kw_list;
};
LIBETPAN_EXPORT
struct mailimf_keywords * mailimf_keywords_new(clist * kw_list);
LIBETPAN_EXPORT
void mailimf_keywords_free(struct mailimf_keywords * keywords);
struct mailimf_return {
struct mailimf_path * ret_path;
};
LIBETPAN_EXPORT
struct mailimf_return *
mailimf_return_new(struct mailimf_path * ret_path);
LIBETPAN_EXPORT
void mailimf_return_free(struct mailimf_return * return_path);
struct mailimf_path {
char * pt_addr_spec;
};
LIBETPAN_EXPORT
struct mailimf_path * mailimf_path_new(char * pt_addr_spec);
LIBETPAN_EXPORT
void mailimf_path_free(struct mailimf_path * path);
struct mailimf_optional_field {
char * fld_name;
char * fld_value;
};
LIBETPAN_EXPORT
struct mailimf_optional_field *
mailimf_optional_field_new(char * fld_name, char * fld_value);
LIBETPAN_EXPORT
void mailimf_optional_field_free(struct mailimf_optional_field * opt_field);
struct mailimf_single_fields {
struct mailimf_orig_date * fld_orig_date;
struct mailimf_from * fld_from;
struct mailimf_sender * fld_sender;
struct mailimf_reply_to * fld_reply_to;
struct mailimf_to * fld_to;
struct mailimf_cc * fld_cc;
struct mailimf_bcc * fld_bcc;
struct mailimf_message_id * fld_message_id;
struct mailimf_in_reply_to * fld_in_reply_to;
struct mailimf_references * fld_references;
struct mailimf_subject * fld_subject;
struct mailimf_comments * fld_comments;
struct mailimf_keywords * fld_keywords;
};
LIBETPAN_EXPORT
void mailimf_atom_free(char * atom);
LIBETPAN_EXPORT
void mailimf_dot_atom_free(char * dot_atom);
LIBETPAN_EXPORT
void mailimf_dot_atom_text_free(char * dot_atom);
LIBETPAN_EXPORT
void mailimf_quoted_string_free(char * quoted_string);
LIBETPAN_EXPORT
void mailimf_word_free(char * word);
LIBETPAN_EXPORT
void mailimf_phrase_free(char * phrase);
LIBETPAN_EXPORT
void mailimf_unstructured_free(char * unstructured);
LIBETPAN_EXPORT
void mailimf_angle_addr_free(char * angle_addr);
LIBETPAN_EXPORT
void mailimf_display_name_free(char * display_name);
LIBETPAN_EXPORT
void mailimf_addr_spec_free(char * addr_spec);
LIBETPAN_EXPORT
void mailimf_local_part_free(char * local_part);
LIBETPAN_EXPORT
void mailimf_domain_free(char * domain);
LIBETPAN_EXPORT
void mailimf_domain_literal_free(char * domain);
LIBETPAN_EXPORT
void mailimf_msg_id_free(char * msg_id);
LIBETPAN_EXPORT
void mailimf_id_left_free(char * id_left);
LIBETPAN_EXPORT
void mailimf_id_right_free(char * id_right);
LIBETPAN_EXPORT
void mailimf_no_fold_quote_free(char * nfq);
LIBETPAN_EXPORT
void mailimf_no_fold_literal_free(char * nfl);
LIBETPAN_EXPORT
void mailimf_field_name_free(char * field_name);
enum {
MAILIMF_NO_ERROR = 0,
MAILIMF_ERROR_PARSE,
MAILIMF_ERROR_MEMORY,
MAILIMF_ERROR_INVAL,
MAILIMF_ERROR_FILE
};
#ifdef __cplusplus
}
#endif
#endif