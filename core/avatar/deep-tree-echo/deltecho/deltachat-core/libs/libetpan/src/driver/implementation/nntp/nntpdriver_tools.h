#ifndef NNTPDRIVER_TOOLS_H
#define NNTPDRIVER_TOOLS_H
#ifdef __cplusplus
extern "C" {
#endif
#include "mail_cache_db_types.h"
#include "nntpdriver_types.h"
int nntpdriver_nntp_error_to_mail_error(int error);
int nntpdriver_authenticate_password(mailsession * session);
int nntpdriver_authenticate_user(mailsession * session);
int nntpdriver_article(mailsession * session, uint32_t indx,
char ** result, size_t * result_len);
int nntpdriver_head(mailsession * session, uint32_t indx,
char ** result,
size_t * result_len);
int nntpdriver_size(mailsession * session, uint32_t indx,
size_t * result);
int
nntpdriver_get_cached_flags(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
uint32_t num,
struct mail_flags ** result);
int
nntpdriver_write_cached_flags(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
uint32_t num,
struct mail_flags * flags);
int nntpdriver_select_folder(mailsession * session, const char * mb);
int nntp_get_messages_list(mailsession * nntp_session,
mailsession * session,
mailmessage_driver * driver,
struct mailmessage_list ** result);
int nntpdriver_mode_reader(mailsession * session);
#ifdef __cplusplus
}
#endif
#endif