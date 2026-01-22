#ifndef MAILSMTP_H
#define MAILSMTP_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailsmtp_types.h>
#include <libetpan/mailsmtp_helper.h>
#include <libetpan/mailsmtp_socket.h>
#include <libetpan/mailsmtp_ssl.h>
#include <libetpan/mailsmtp_oauth2.h>
LIBETPAN_EXPORT
mailsmtp * mailsmtp_new(size_t progr_rate,
progress_function * progr_fun);
LIBETPAN_EXPORT
void mailsmtp_free(mailsmtp * session);
LIBETPAN_EXPORT
void mailsmtp_set_timeout(mailsmtp * session, time_t timeout);
LIBETPAN_EXPORT
time_t mailsmtp_get_timeout(mailsmtp * session);
LIBETPAN_EXPORT
int mailsmtp_connect(mailsmtp * session, mailstream * s);
LIBETPAN_EXPORT
int mailsmtp_quit(mailsmtp * session);
LIBETPAN_EXPORT
int mailsmtp_auth(mailsmtp * session, const char * user, const char * pass);
LIBETPAN_EXPORT
int mailsmtp_auth_type(mailsmtp * session,
const char * user, const char * pass, int type);
LIBETPAN_EXPORT
int mailsmtp_helo(mailsmtp * session);
LIBETPAN_EXPORT
int mailsmtp_helo_with_ip(mailsmtp * session, int useip);
LIBETPAN_EXPORT
int mailsmtp_mail(mailsmtp * session, const char * from);
LIBETPAN_EXPORT
int mailsmtp_rcpt(mailsmtp * session, const char * to);
LIBETPAN_EXPORT
int mailsmtp_data(mailsmtp * session);
LIBETPAN_EXPORT
int mailsmtp_data_message(mailsmtp * session,
const char * message,
size_t size);
LIBETPAN_EXPORT
int maillmtp_data_message(mailsmtp * session,
const char * message,
size_t size,
clist * recipient_list,
int * retcodes);
LIBETPAN_EXPORT
int mailsmtp_data_message_quit(mailsmtp * session,
const char * message,
size_t size);
LIBETPAN_EXPORT
int mailsmtp_data_message_quit_no_disconnect(mailsmtp * session,
const char * message,
size_t size);
LIBETPAN_EXPORT
int mailesmtp_lhlo(mailsmtp * session, const char *hostname);
LIBETPAN_EXPORT
int mailesmtp_ehlo(mailsmtp * session);
LIBETPAN_EXPORT
int mailesmtp_ehlo_with_ip(mailsmtp * session, int useip);
LIBETPAN_EXPORT
int mailesmtp_mail(mailsmtp * session,
const char * from,
int return_full,
const char * envid);
LIBETPAN_EXPORT
int mailesmtp_mail_size(mailsmtp * session,
const char * from,
int return_full,
const char * envid, size_t size);
LIBETPAN_EXPORT
int mailesmtp_rcpt(mailsmtp * session,
const char * to,
int notify,
const char * orcpt);
LIBETPAN_EXPORT
int mailesmtp_starttls(mailsmtp * session);
LIBETPAN_EXPORT
const char * mailsmtp_strerror(int errnum);
LIBETPAN_EXPORT
int mailesmtp_auth_sasl(mailsmtp * session, const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm);
LIBETPAN_EXPORT
int mailsmtp_noop(mailsmtp * session);
LIBETPAN_EXPORT
int mailsmtp_reset(mailsmtp * session);
LIBETPAN_EXPORT
void mailsmtp_set_progress_callback(mailsmtp * session,
mailprogress_function * progr_fun,
void * context);
LIBETPAN_EXPORT
void mailsmtp_set_logger(mailsmtp * session, void (* logger)(mailsmtp * session, int log_type,
const char * str, size_t size, void * context), void * logger_context);
#ifdef __cplusplus
}
#endif
#endif