#ifndef POP3DRIVER_TOOLS_H
#define POP3DRIVER_TOOLS_H
#include "mail_cache_db_types.h"
#include "pop3driver_types.h"
#include "mailpop3.h"
#ifdef __cplusplus
extern "C" {
#endif
int pop3driver_pop3_error_to_mail_error(int error);
int pop3driver_retr(mailsession * session, uint32_t indx,
char ** result, size_t * result_len);
int pop3driver_header(mailsession * session, uint32_t indx,
char ** result,
size_t * result_len);
int pop3driver_size(mailsession * session, uint32_t indx,
size_t * result);
int
pop3driver_get_cached_flags(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
mailsession * session,
uint32_t num,
struct mail_flags ** result);
int
pop3driver_write_cached_flags(struct mail_cache_db * cache_db,
MMAPString * mmapstr,
char * uid,
struct mail_flags * flags);
int pop3_get_messages_list(mailpop3 * pop3,
mailsession * session,
mailmessage_driver * driver,
struct mailmessage_list ** result);
#ifdef __cplusplus
}
#endif
#endif