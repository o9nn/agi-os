#ifndef MAILDRIVER_H
#define MAILDRIVER_H
#include <libetpan/maildriver_types.h>
#include <libetpan/maildriver_types_helper.h>
#ifdef __cplusplus
extern "C" {
#endif
LIBETPAN_EXPORT
mailsession * mailsession_new(mailsession_driver * sess_driver);
LIBETPAN_EXPORT
void mailsession_free(mailsession * session);
LIBETPAN_EXPORT
int mailsession_parameters(mailsession * session,
int id, void * value);
LIBETPAN_EXPORT
int mailsession_connect_stream(mailsession * session, mailstream * s);
LIBETPAN_EXPORT
int mailsession_connect_path(mailsession * session, const char * path);
LIBETPAN_EXPORT
int mailsession_starttls(mailsession * session);
LIBETPAN_EXPORT
int mailsession_login(mailsession * session,
const char * userid, const char * password);
LIBETPAN_EXPORT
int mailsession_logout(mailsession * session);
LIBETPAN_EXPORT
int mailsession_noop(mailsession * session);
LIBETPAN_EXPORT
int mailsession_build_folder_name(mailsession * session, const char * mb,
const char * name, char ** result);
LIBETPAN_EXPORT
int mailsession_create_folder(mailsession * session, const char * mb);
LIBETPAN_EXPORT
int mailsession_delete_folder(mailsession * session, const char * mb);
LIBETPAN_EXPORT
int mailsession_rename_folder(mailsession * session,
const char * mb, const char * new_name);
LIBETPAN_EXPORT
int mailsession_check_folder(mailsession * session);
LIBETPAN_EXPORT
int mailsession_examine_folder(mailsession * session, const char * mb);
LIBETPAN_EXPORT
int mailsession_select_folder(mailsession * session, const char * mb);
LIBETPAN_EXPORT
int mailsession_expunge_folder(mailsession * session);
LIBETPAN_EXPORT
int mailsession_status_folder(mailsession * session, const char * mb,
uint32_t * result_messages, uint32_t * result_recent,
uint32_t * result_unseen);
LIBETPAN_EXPORT
int mailsession_messages_number(mailsession * session, const char * mb,
uint32_t * result);
LIBETPAN_EXPORT
int mailsession_recent_number(mailsession * session,
const char * mb, uint32_t * result);
LIBETPAN_EXPORT
int mailsession_unseen_number(mailsession * session, const char * mb,
uint32_t * result);
LIBETPAN_EXPORT
int mailsession_list_folders(mailsession * session, const char * mb,
struct mail_list ** result);
LIBETPAN_EXPORT
int mailsession_lsub_folders(mailsession * session, const char * mb,
struct mail_list ** result);
LIBETPAN_EXPORT
int mailsession_subscribe_folder(mailsession * session, const char * mb);
LIBETPAN_EXPORT
int mailsession_unsubscribe_folder(mailsession * session, const char * mb);
LIBETPAN_EXPORT
int mailsession_append_message(mailsession * session,
const char * message, size_t size);
LIBETPAN_EXPORT
int mailsession_append_message_flags(mailsession * session,
const char * message, size_t size, struct mail_flags * flags);
LIBETPAN_EXPORT
int mailsession_copy_message(mailsession * session,
uint32_t num, const char * mb);
LIBETPAN_EXPORT
int mailsession_move_message(mailsession * session,
uint32_t num, const char * mb);
LIBETPAN_EXPORT
int mailsession_get_messages_list(mailsession * session,
struct mailmessage_list ** result);
LIBETPAN_EXPORT
int mailsession_get_envelopes_list(mailsession * session,
struct mailmessage_list * result);
LIBETPAN_EXPORT
int mailsession_remove_message(mailsession * session, uint32_t num);
#if 0
LIBETPAN_EXPORT
int mailsession_search_messages(mailsession * session, const char * charset,
struct mail_search_key * key,
struct mail_search_result ** result);
#endif
LIBETPAN_EXPORT
int mailsession_get_message(mailsession * session,
uint32_t num, mailmessage ** result);
LIBETPAN_EXPORT
int mailsession_get_message_by_uid(mailsession * session,
const char * uid, mailmessage ** result);
LIBETPAN_EXPORT
int mailsession_login_sasl(mailsession * session, const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm);
#ifdef __cplusplus
}
#endif
#endif