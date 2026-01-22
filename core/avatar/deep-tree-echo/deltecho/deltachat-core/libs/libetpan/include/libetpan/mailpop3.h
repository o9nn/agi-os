#ifndef MAILPOP3_H
#define MAILPOP3_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailpop3_types.h>
#include <libetpan/mailpop3_helper.h>
#include <libetpan/mailpop3_socket.h>
#include <libetpan/mailpop3_ssl.h>
#define POP3_STRING_SIZE 513
LIBETPAN_EXPORT
mailpop3 * mailpop3_new(size_t pop3_progr_rate,
progress_function * pop3_progr_fun);
LIBETPAN_EXPORT
void mailpop3_free(mailpop3 * f);
LIBETPAN_EXPORT
void mailpop3_set_timeout(mailpop3 * f, time_t timeout);
LIBETPAN_EXPORT
time_t mailpop3_get_timeout(mailpop3 * f);
LIBETPAN_EXPORT
void mailpop3_set_progress_callback(mailpop3 * f, mailprogress_function * progr_fun, void * context);
LIBETPAN_EXPORT
int mailpop3_connect(mailpop3 * f, mailstream * s);
LIBETPAN_EXPORT
int mailpop3_quit(mailpop3 * f);
LIBETPAN_EXPORT
int mailpop3_apop(mailpop3 * f, const char * user, const char * password);
LIBETPAN_EXPORT
int mailpop3_user(mailpop3 * f, const char * user);
LIBETPAN_EXPORT
int mailpop3_pass(mailpop3 * f, const char * password);
LIBETPAN_EXPORT
int mailpop3_list(mailpop3 * f, carray ** result);
LIBETPAN_EXPORT
int mailpop3_retr(mailpop3 * f, unsigned int indx, char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailpop3_top(mailpop3 * f, unsigned int indx,
unsigned int count, char ** result,
size_t * result_len);
LIBETPAN_EXPORT
int mailpop3_dele(mailpop3 * f, unsigned int indx);
LIBETPAN_EXPORT
int mailpop3_noop(mailpop3 * f);
LIBETPAN_EXPORT
int mailpop3_rset(mailpop3 * f);
LIBETPAN_EXPORT
void mailpop3_top_free(char * str);
LIBETPAN_EXPORT
void mailpop3_retr_free(char * str);
LIBETPAN_EXPORT
int mailpop3_get_msg_info(mailpop3 * f, unsigned int indx,
struct mailpop3_msg_info ** result);
LIBETPAN_EXPORT
int mailpop3_capa(mailpop3 * f, clist ** result);
LIBETPAN_EXPORT
void mailpop3_capa_resp_free(clist * capa_list);
LIBETPAN_EXPORT
int mailpop3_stat(mailpop3 * f, struct mailpop3_stat_response ** result);
LIBETPAN_EXPORT
void mailpop3_stat_resp_free(struct mailpop3_stat_response * stat_result);
LIBETPAN_EXPORT
int mailpop3_stls(mailpop3 * f);
LIBETPAN_EXPORT
int mailpop3_auth(mailpop3 * f, const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm);
LIBETPAN_EXPORT
void mailpop3_set_logger(mailpop3 * session, void (* logger)(mailpop3 * session, int log_type,
const char * str, size_t size, void * context), void * logger_context);
#ifdef __cplusplus
}
#endif
#endif