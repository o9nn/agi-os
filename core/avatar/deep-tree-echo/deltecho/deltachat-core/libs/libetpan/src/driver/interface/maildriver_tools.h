#ifndef MAILDRIVER_TOOLS_H
#define MAILDRIVER_TOOLS_H
#include "maildriver_types.h"
#include "mail_cache_db_types.h"
#ifdef __cplusplus
extern "C" {
#endif
int
maildriver_generic_get_envelopes_list(mailsession * session,
struct mailmessage_list * env_list);
#if 0
int maildriver_generic_search_messages(mailsession * session, char * charset,
struct mail_search_key * key,
struct mail_search_result ** result);
#endif
int
maildriver_env_list_to_msg_list(struct mailmessage_list * env_list,
clist ** result);
int maildriver_imf_error_to_mail_error(int error);
char * maildriver_quote_mailbox(const char * mb);
int
maildriver_env_list_to_msg_list_no_flags(struct mailmessage_list * env_list,
clist ** result);
int maildriver_cache_clean_up(struct mail_cache_db * cache_db_env,
struct mail_cache_db * cache_db_flags,
struct mailmessage_list * env_list);
int maildriver_message_cache_clean_up(char * cache_dir,
struct mailmessage_list * env_list,
void (* get_uid_from_filename)(char *));
#ifdef __cplusplus
}
#endif
#endif