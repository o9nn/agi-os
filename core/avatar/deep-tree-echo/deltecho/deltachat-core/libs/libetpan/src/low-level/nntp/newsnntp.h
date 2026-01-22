#ifndef NEWSNNTP_H
#define NEWSNNTP_H
#ifdef __cplusplus
extern "C" {
#endif
#ifdef HAVE_INTTYPES_H
#	include <inttypes.h>
#endif
#include <sys/types.h>
#include <time.h>
#include <libetpan/clist.h>
#include <libetpan/mailstream.h>
#include <libetpan/newsnntp_socket.h>
#include <libetpan/newsnntp_ssl.h>
#include <libetpan/newsnntp_types.h>
LIBETPAN_EXPORT
newsnntp * newsnntp_new(size_t nntp_progr_rate,
progress_function * nntp_progr_fun);
LIBETPAN_EXPORT
void newsnntp_free(newsnntp * session);
LIBETPAN_EXPORT
void newsnntp_set_logger(newsnntp * session, void (* logger)(newsnntp * session, int log_type,
const char * str, size_t size, void * context), void * logger_context);
LIBETPAN_EXPORT
void newsnntp_set_progress_callback(newsnntp * f, mailprogress_function * progr_fun, void * context);
LIBETPAN_EXPORT
void newsnntp_set_timeout(newsnntp * session, time_t timeout);
LIBETPAN_EXPORT
time_t newsnntp_get_timeout(newsnntp * session);
LIBETPAN_EXPORT
int newsnntp_connect(newsnntp * session, mailstream * s);
LIBETPAN_EXPORT
int newsnntp_quit(newsnntp * session);
LIBETPAN_EXPORT
int newsnntp_head(newsnntp * session, uint32_t indx,
char ** result, size_t * result_len);
LIBETPAN_EXPORT
void newsnntp_head_free(char * str);
LIBETPAN_EXPORT
int newsnntp_article(newsnntp * session, uint32_t indx,
char ** result, size_t * result_len);
LIBETPAN_EXPORT
int newsnntp_article_by_message_id(newsnntp * session, char * msg_id,
char ** result, size_t * result_len);
LIBETPAN_EXPORT
void newsnntp_article_free(char * str);
LIBETPAN_EXPORT
int newsnntp_body(newsnntp * session, uint32_t indx,
char ** result, size_t * result_len);
LIBETPAN_EXPORT
void newsnntp_body_free(char * str);
LIBETPAN_EXPORT
int newsnntp_mode_reader(newsnntp * session);
LIBETPAN_EXPORT
int newsnntp_date(newsnntp * session, struct tm * tm);
LIBETPAN_EXPORT
int newsnntp_authinfo_username(newsnntp * session, const char * username);
LIBETPAN_EXPORT
int newsnntp_authinfo_password(newsnntp * session, const char * password);
LIBETPAN_EXPORT
int newsnntp_post(newsnntp * session, const char * message, size_t size);
LIBETPAN_EXPORT
int newsnntp_group(newsnntp * session, const char * groupname,
struct newsnntp_group_info ** info);
LIBETPAN_EXPORT
void newsnntp_group_free(struct newsnntp_group_info * info);
LIBETPAN_EXPORT
int newsnntp_list(newsnntp * session, clist ** result);
LIBETPAN_EXPORT
void newsnntp_list_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_list_overview_fmt(newsnntp * session, clist ** result);
LIBETPAN_EXPORT
void newsnntp_list_overview_fmt_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_list_active(newsnntp * session, const char * wildmat, clist ** result);
LIBETPAN_EXPORT
void newsnntp_list_active_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_list_active_times(newsnntp * session, clist ** result);
LIBETPAN_EXPORT
void newsnntp_list_active_times_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_list_distribution(newsnntp * session, clist ** result);
LIBETPAN_EXPORT
void newsnntp_list_distribution_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_list_distrib_pats(newsnntp * session, clist ** result);
LIBETPAN_EXPORT
void newsnntp_list_distrib_pats_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_list_newsgroups(newsnntp * session, const char * pattern,
clist ** result);
LIBETPAN_EXPORT
void newsnntp_list_newsgroups_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_list_subscriptions(newsnntp * session, clist ** result);
LIBETPAN_EXPORT
void newsnntp_list_subscriptions_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_listgroup(newsnntp * session, const char * group_name,
clist ** result);
LIBETPAN_EXPORT
void newsnntp_listgroup_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_xhdr_single(newsnntp * session, const char * header, uint32_t article,
clist ** result);
LIBETPAN_EXPORT
int newsnntp_xhdr_range(newsnntp * session, const char * header,
uint32_t rangeinf, uint32_t rangesup,
clist ** result);
LIBETPAN_EXPORT
void newsnntp_xhdr_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_xover_single(newsnntp * session, uint32_t article,
struct newsnntp_xover_resp_item ** result);
LIBETPAN_EXPORT
int newsnntp_xover_range(newsnntp * session, uint32_t rangeinf, uint32_t rangesup,
clist ** result);
void xover_resp_item_free(struct newsnntp_xover_resp_item * n);
LIBETPAN_EXPORT
void newsnntp_xover_resp_list_free(clist * l);
LIBETPAN_EXPORT
int newsnntp_authinfo_generic(newsnntp * session, const char * authentificator,
const char * arguments);
#ifdef __cplusplus
}
#endif
#endif