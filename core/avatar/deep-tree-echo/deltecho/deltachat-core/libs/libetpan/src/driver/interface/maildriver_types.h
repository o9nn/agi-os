#ifndef MAILDRIVER_TYPES_H
#define MAILDRIVER_TYPES_H
#ifndef _MSC_VER
# ifdef HAVE_INTTYPES_H
# include <inttypes.h>
# endif
# include <sys/types.h>
#endif
#include <libetpan/mailstream.h>
#include <libetpan/mailimf.h>
#include <libetpan/mailmime.h>
#include <libetpan/carray.h>
#include <libetpan/mailthread_types.h>
#include <libetpan/maildriver_errors.h>
#ifdef __cplusplus
extern "C" {
#endif
typedef struct mailsession_driver mailsession_driver;
typedef struct mailsession mailsession;
typedef struct mailmessage_driver mailmessage_driver;
typedef struct mailmessage mailmessage;
struct mailmessage_list {
carray * msg_tab;
};
LIBETPAN_EXPORT
struct mailmessage_list * mailmessage_list_new(carray * msg_tab);
LIBETPAN_EXPORT
void mailmessage_list_free(struct mailmessage_list * env_list);
struct mail_list {
clist * mb_list;
};
LIBETPAN_EXPORT
struct mail_list * mail_list_new(clist * mb_list);
LIBETPAN_EXPORT
void mail_list_free(struct mail_list * resp);
enum {
MAIL_FLAG_NEW = 1 << 0,
MAIL_FLAG_SEEN = 1 << 1,
MAIL_FLAG_FLAGGED = 1 << 2,
MAIL_FLAG_DELETED = 1 << 3,
MAIL_FLAG_ANSWERED = 1 << 4,
MAIL_FLAG_FORWARDED = 1 << 5,
MAIL_FLAG_CANCELLED = 1 << 6
};
struct mail_flags {
uint32_t fl_flags;
clist * fl_extension;
};
LIBETPAN_EXPORT
struct mail_flags * mail_flags_new(uint32_t fl_flags, clist * fl_ext);
LIBETPAN_EXPORT
void mail_flags_free(struct mail_flags * flags);
LIBETPAN_EXPORT
struct mail_flags * mail_flags_new_empty(void);
LIBETPAN_EXPORT
int32_t mailimf_date_time_comp(struct mailimf_date_time * date1,
struct mailimf_date_time * date2);
enum {
MAIL_SEARCH_KEY_ALL,
MAIL_SEARCH_KEY_ANSWERED,
MAIL_SEARCH_KEY_BCC,
MAIL_SEARCH_KEY_BEFORE,
MAIL_SEARCH_KEY_BODY,
MAIL_SEARCH_KEY_CC,
MAIL_SEARCH_KEY_DELETED,
MAIL_SEARCH_KEY_FLAGGED,
MAIL_SEARCH_KEY_FROM,
MAIL_SEARCH_KEY_NEW,
MAIL_SEARCH_KEY_OLD,
MAIL_SEARCH_KEY_ON,
MAIL_SEARCH_KEY_RECENT,
MAIL_SEARCH_KEY_SEEN,
MAIL_SEARCH_KEY_SINCE,
MAIL_SEARCH_KEY_SUBJECT,
MAIL_SEARCH_KEY_TEXT,
MAIL_SEARCH_KEY_TO,
MAIL_SEARCH_KEY_UNANSWERED,
MAIL_SEARCH_KEY_UNDELETED,
MAIL_SEARCH_KEY_UNFLAGGED,
MAIL_SEARCH_KEY_UNSEEN,
MAIL_SEARCH_KEY_HEADER,
MAIL_SEARCH_KEY_LARGER,
MAIL_SEARCH_KEY_NOT,
MAIL_SEARCH_KEY_OR,
MAIL_SEARCH_KEY_SMALLER,
MAIL_SEARCH_KEY_MULTIPLE
};
#if 0
struct mail_search_key {
int sk_type;
union {
char * sk_bcc;
struct mailimf_date_time * sk_before;
char * sk_body;
char * sk_cc;
char * sk_from;
struct mailimf_date_time * sk_on;
struct mailimf_date_time * sk_since;
char * sk_subject;
char * sk_text;
char * sk_to;
char * sk_header_name;
char * sk_header_value;
size_t sk_larger;
struct mail_search_key * sk_not;
struct mail_search_key * sk_or1;
struct mail_search_key * sk_or2;
size_t sk_smaller;
clist * sk_multiple;
} sk_data;
};
struct mail_search_key *
mail_search_key_new(int sk_type,
char * sk_bcc, struct mailimf_date_time * sk_before,
char * sk_body, char * sk_cc, char * sk_from,
struct mailimf_date_time * sk_on, struct mailimf_date_time * sk_since,
char * sk_subject, char * sk_text, char * sk_to,
char * sk_header_name, char * sk_header_value, size_t sk_larger,
struct mail_search_key * sk_not, struct mail_search_key * sk_or1,
struct mail_search_key * sk_or2, size_t sk_smaller,
clist * sk_multiple);
void mail_search_key_free(struct mail_search_key * key);
#endif
#if 0
struct mail_search_result {
clist * sr_list;
};
struct mail_search_result * mail_search_result_new(clist * sr_list);
void mail_search_result_free(struct mail_search_result * search_result);
#endif
struct mailsession_driver {
char * sess_name;
int (* sess_initialize)(mailsession * session);
void (* sess_uninitialize)(mailsession * session);
int (* sess_parameters)(mailsession * session,
int id, void * value);
int (* sess_connect_stream)(mailsession * session, mailstream * s);
int (* sess_connect_path)(mailsession * session, const char * path);
int (* sess_starttls)(mailsession * session);
int (* sess_login)(mailsession * session, const char * userid, const char * password);
int (* sess_logout)(mailsession * session);
int (* sess_noop)(mailsession * session);
int (* sess_build_folder_name)(mailsession * session, const char * mb,
const char * name, char ** result);
int (* sess_create_folder)(mailsession * session, const char * mb);
int (* sess_delete_folder)(mailsession * session, const char * mb);
int (* sess_rename_folder)(mailsession * session, const char * mb,
const char * new_name);
int (* sess_check_folder)(mailsession * session);
int (* sess_examine_folder)(mailsession * session, const char * mb);
int (* sess_select_folder)(mailsession * session, const char * mb);
int (* sess_expunge_folder)(mailsession * session);
int (* sess_status_folder)(mailsession * session, const char * mb,
uint32_t * result_num, uint32_t * result_recent,
uint32_t * result_unseen);
int (* sess_messages_number)(mailsession * session, const char * mb,
uint32_t * result);
int (* sess_recent_number)(mailsession * session, const char * mb,
uint32_t * result);
int (* sess_unseen_number)(mailsession * session, const char * mb,
uint32_t * result);
int (* sess_list_folders)(mailsession * session, const char * mb,
struct mail_list ** result);
int (* sess_lsub_folders)(mailsession * session, const char * mb,
struct mail_list ** result);
int (* sess_subscribe_folder)(mailsession * session, const char * mb);
int (* sess_unsubscribe_folder)(mailsession * session, const char * mb);
int (* sess_append_message)(mailsession * session,
const char * message, size_t size);
int (* sess_append_message_flags)(mailsession * session,
const char * message, size_t size, struct mail_flags * flags);
int (* sess_copy_message)(mailsession * session,
uint32_t num, const char * mb);
int (* sess_move_message)(mailsession * session,
uint32_t num, const char * mb);
int (* sess_get_message)(mailsession * session,
uint32_t num, mailmessage ** result);
int (* sess_get_message_by_uid)(mailsession * session,
const char * uid, mailmessage ** result);
int (* sess_get_messages_list)(mailsession * session,
struct mailmessage_list ** result);
int (* sess_get_envelopes_list)(mailsession * session,
struct mailmessage_list * env_list);
int (* sess_remove_message)(mailsession * session, uint32_t num);
int (* sess_login_sasl)(mailsession * session, const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm);
};
struct mailsession {
void * sess_data;
mailsession_driver * sess_driver;
};
#define LIBETPAN_MAIL_MESSAGE_CHECK
struct mailmessage_driver {
char * msg_name;
int (* msg_initialize)(mailmessage * msg_info);
void (* msg_uninitialize)(mailmessage * msg_info);
void (* msg_flush)(mailmessage * msg_info);
void (* msg_check)(mailmessage * msg_info);
void (* msg_fetch_result_free)(mailmessage * msg_info,
char * msg);
int (* msg_fetch)(mailmessage * msg_info,
char ** result,
size_t * result_len);
int (* msg_fetch_header)(mailmessage * msg_info,
char ** result,
size_t * result_len);
int (* msg_fetch_body)(mailmessage * msg_info,
char ** result, size_t * result_len);
int (* msg_fetch_size)(mailmessage * msg_info,
size_t * result);
int (* msg_get_bodystructure)(mailmessage * msg_info,
struct mailmime ** result);
int (* msg_fetch_section)(mailmessage * msg_info,
struct mailmime * mime,
char ** result, size_t * result_len);
int (* msg_fetch_section_header)(mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
int (* msg_fetch_section_mime)(mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
int (* msg_fetch_section_body)(mailmessage * msg_info,
struct mailmime * mime,
char ** result,
size_t * result_len);
int (* msg_fetch_envelope)(mailmessage * msg_info,
struct mailimf_fields ** result);
int (* msg_get_flags)(mailmessage * msg_info,
struct mail_flags ** result);
};
struct mailmessage {
mailsession * msg_session;
mailmessage_driver * msg_driver;
uint32_t msg_index;
char * msg_uid;
size_t msg_size;
struct mailimf_fields * msg_fields;
struct mail_flags * msg_flags;
int msg_resolved;
struct mailimf_single_fields msg_single_fields;
struct mailmime * msg_mime;
int msg_cached;
void * msg_data;
void * msg_folder;
void * msg_user_data;
};
struct mailmessage_tree {
struct mailmessage_tree * node_parent;
char * node_msgid;
time_t node_date;
mailmessage * node_msg;
carray * node_children;
int node_is_reply;
char * node_base_subject;
};
LIBETPAN_EXPORT
struct mailmessage_tree *
mailmessage_tree_new(char * node_msgid, time_t node_date,
mailmessage * node_msg);
LIBETPAN_EXPORT
void mailmessage_tree_free(struct mailmessage_tree * tree);
LIBETPAN_EXPORT
void mailmessage_tree_free_recursive(struct mailmessage_tree * tree);
struct generic_message_t {
int (* msg_prefetch)(mailmessage * msg_info);
void (* msg_prefetch_free)(struct generic_message_t * msg);
int msg_fetched;
char * msg_message;
size_t msg_length;
void * msg_data;
};
LIBETPAN_EXPORT
const char * maildriver_strerror(int err);
LIBETPAN_EXPORT
void *libetpan_malloc(size_t length);
LIBETPAN_EXPORT
void libetpan_free(void* data);
#ifdef __cplusplus
}
#endif
#endif