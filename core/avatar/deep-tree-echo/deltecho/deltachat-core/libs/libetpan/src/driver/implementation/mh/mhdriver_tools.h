#ifndef MHDRIVER_TOOLS_H
#define MHDRIVER_TOOLS_H
#include "maildriver_types.h"
#include "mail_cache_db_types.h"
#include "mailmh.h"
#ifdef __cplusplus
extern "C" {
#endif
int mhdriver_mh_error_to_mail_error(int error);
int mhdriver_fetch_message(mailsession * session, uint32_t indx,
char ** result, size_t * result_len);
int mhdriver_fetch_header(mailsession * session, uint32_t indx,
char ** result, size_t * result_len);
int mhdriver_fetch_size(mailsession * session, uint32_t indx,
size_t * result);
int
mhdriver_get_cached_flags(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
mailsession * session,
uint32_t num,
struct mail_flags ** result);
int
mhdriver_write_cached_flags(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
char * uid,
struct mail_flags * flags);
int mh_get_messages_list(struct mailmh_folder * folder,
mailsession * session, mailmessage_driver * driver,
struct mailmessage_list ** result);
#ifdef __cplusplus
}
#endif
#endif