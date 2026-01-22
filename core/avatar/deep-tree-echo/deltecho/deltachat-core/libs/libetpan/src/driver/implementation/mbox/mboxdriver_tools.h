#ifndef MBOXDRIVER_TOOLS_H
#define MBOXDRIVER_TOOLS_H
#include "mail_cache_db_types.h"
#include "mboxdriver_types.h"
#include "mailmbox.h"
#ifdef __cplusplus
extern "C" {
#endif
int mboxdriver_mbox_error_to_mail_error(int error);
int mboxdriver_fetch_msg(mailsession * session, uint32_t indx,
char ** result, size_t * result_len);
int mboxdriver_fetch_size(mailsession * session, uint32_t indx,
size_t * result);
int
mboxdriver_get_cached_flags(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
mailsession * session,
uint32_t num,
struct mail_flags ** result);
int
mboxdriver_write_cached_flags(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
char * uid, struct mail_flags * flags);
int mbox_get_uid_messages_list(struct mailmbox_folder * folder,
mailsession * session,
mailmessage_driver * driver,
struct mailmessage_list ** result);
int mbox_get_messages_list(struct mailmbox_folder * folder,
mailsession * session,
mailmessage_driver * driver,
struct mailmessage_list ** result);
int mboxdriver_fetch_header(mailsession * session, uint32_t indx,
char ** result, size_t * result_len);
#ifdef __cplusplus
}
#endif
#endif