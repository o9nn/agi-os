#ifndef MAILIMAP_H
#define MAILIMAP_H
#ifdef __cplusplus
extern "C" {
#endif
#include <libetpan/mailimap_types.h>
#include <libetpan/mailimap_types_helper.h>
#include <libetpan/mailimap_helper.h>
#include <libetpan/mailimap_socket.h>
#include <libetpan/mailimap_ssl.h>
#include <libetpan/acl.h>
#include <libetpan/annotatemore.h>
#include <libetpan/uidplus.h>
#include <libetpan/idle.h>
#include <libetpan/quota.h>
#include <libetpan/namespace.h>
#include <libetpan/mailimap_id.h>
#include <libetpan/enable.h>
#include <libetpan/xlist.h>
#include <libetpan/xgmlabels.h>
#include <libetpan/xgmmsgid.h>
#include <libetpan/xgmthrid.h>
#include <libetpan/condstore.h>
#include <libetpan/qresync.h>
#include <libetpan/mailimap_sort.h>
#include <libetpan/mailimap_compress.h>
#include <libetpan/mailimap_oauth2.h>
LIBETPAN_EXPORT
int mailimap_connect(mailimap * session, mailstream * s);
LIBETPAN_EXPORT
int mailimap_append(mailimap * session, const char * mailbox,
struct mailimap_flag_list * flag_list,
struct mailimap_date_time * date_time,
const char * literal, size_t literal_size);
LIBETPAN_EXPORT
int mailimap_noop(mailimap * session);
LIBETPAN_EXPORT
int mailimap_logout(mailimap * session);
LIBETPAN_EXPORT
int mailimap_capability(mailimap * session,
struct mailimap_capability_data ** result);
LIBETPAN_EXPORT
int mailimap_check(mailimap * session);
LIBETPAN_EXPORT
int mailimap_close(mailimap * session);
LIBETPAN_EXPORT
int mailimap_expunge(mailimap * session);
LIBETPAN_EXPORT
int mailimap_copy(mailimap * session, struct mailimap_set * set,
const char * mb);
LIBETPAN_EXPORT
int mailimap_uid_copy(mailimap * session,
struct mailimap_set * set, const char * mb);
LIBETPAN_EXPORT
int mailimap_move(mailimap * session, struct mailimap_set * set,
const char * mb);
LIBETPAN_EXPORT
int mailimap_uid_move(mailimap * session, struct mailimap_set * set,
const char * mb);
LIBETPAN_EXPORT
int mailimap_create(mailimap * session, const char * mb);
LIBETPAN_EXPORT
int mailimap_delete(mailimap * session, const char * mb);
LIBETPAN_EXPORT
int mailimap_examine(mailimap * session, const char * mb);
LIBETPAN_EXPORT
int
mailimap_fetch(mailimap * session, struct mailimap_set * set,
struct mailimap_fetch_type * fetch_type, clist ** result);
LIBETPAN_EXPORT
int
mailimap_uid_fetch(mailimap * session,
struct mailimap_set * set,
struct mailimap_fetch_type * fetch_type, clist ** result);
LIBETPAN_EXPORT
void mailimap_fetch_list_free(clist * fetch_list);
LIBETPAN_EXPORT
int mailimap_list(mailimap * session, const char * mb,
const char * list_mb, clist ** result);
LIBETPAN_EXPORT
int mailimap_login(mailimap * session,
const char * userid, const char * password);
LIBETPAN_EXPORT
int mailimap_authenticate(mailimap * session, const char * auth_type,
const char * server_fqdn,
const char * local_ip_port,
const char * remote_ip_port,
const char * login, const char * auth_name,
const char * password, const char * realm);
LIBETPAN_EXPORT
int mailimap_lsub(mailimap * session, const char * mb,
const char * list_mb, clist ** result);
LIBETPAN_EXPORT
void mailimap_list_result_free(clist * list);
LIBETPAN_EXPORT
int mailimap_rename(mailimap * session,
const char * mb, const char * new_name);
LIBETPAN_EXPORT
int
mailimap_search(mailimap * session, const char * charset,
struct mailimap_search_key * key, clist ** result);
LIBETPAN_EXPORT
int
mailimap_uid_search(mailimap * session, const char * charset,
struct mailimap_search_key * key, clist ** result);
LIBETPAN_EXPORT int mailimap_search_literalplus(mailimap * session, const char * charset,
struct mailimap_search_key * key, clist ** result);
LIBETPAN_EXPORT int mailimap_uid_search_literalplus(mailimap * session, const char * charset,
struct mailimap_search_key * key, clist ** result);
LIBETPAN_EXPORT
void mailimap_search_result_free(clist * search_result);
LIBETPAN_EXPORT
int
mailimap_select(mailimap * session, const char * mb);
LIBETPAN_EXPORT
int mailimap_custom_command(mailimap * session, const char * command);
LIBETPAN_EXPORT
int
mailimap_status(mailimap * session, const char * mb,
struct mailimap_status_att_list * status_att_list,
struct mailimap_mailbox_data_status ** result);
LIBETPAN_EXPORT
int
mailimap_store(mailimap * session,
struct mailimap_set * set,
struct mailimap_store_att_flags * store_att_flags);
LIBETPAN_EXPORT
int
mailimap_uid_store(mailimap * session,
struct mailimap_set * set,
struct mailimap_store_att_flags * store_att_flags);
LIBETPAN_EXPORT
int mailimap_subscribe(mailimap * session, const char * mb);
LIBETPAN_EXPORT
int mailimap_unsubscribe(mailimap * session, const char * mb);
LIBETPAN_EXPORT
int mailimap_starttls(mailimap * session);
LIBETPAN_EXPORT
mailimap * mailimap_new(size_t imap_progr_rate,
progress_function * imap_progr_fun);
LIBETPAN_EXPORT
void mailimap_free(mailimap * session);
int mailimap_send_current_tag(mailimap * session);
char * mailimap_read_line(mailimap * session);
int mailimap_parse_response(mailimap * session,
struct mailimap_response ** result);
LIBETPAN_EXPORT
void mailimap_set_progress_callback(mailimap * session,
mailprogress_function * body_progr_fun,
mailprogress_function * items_progr_fun,
void * context);
LIBETPAN_EXPORT
void mailimap_set_msg_att_handler(mailimap * session,
mailimap_msg_att_handler * handler,
void * context);
LIBETPAN_EXPORT
void mailimap_set_msg_body_handler(mailimap * session,
mailimap_msg_body_handler * handler,
void * context);
LIBETPAN_EXPORT
void mailimap_set_timeout(mailimap * session, time_t timeout);;
LIBETPAN_EXPORT
time_t mailimap_get_timeout(mailimap * session);
LIBETPAN_EXPORT
void mailimap_set_logger(mailimap * session, void (* logger)(mailimap * session, int log_type,
const char * str, size_t size, void * context), void * logger_context);
#ifndef LIBETPAN_HAS_MAILIMAP_163_WORKAROUND
#define LIBETPAN_HAS_MAILIMAP_163_WORKAROUND	1
#endif
LIBETPAN_EXPORT
int mailimap_is_163_workaround_enabled(mailimap * session);
LIBETPAN_EXPORT
void mailimap_set_163_workaround_enabled(mailimap * session, int enabled);
#ifndef LIBETPAN_HAS_MAILIMAP_RAMBLER_WORKAROUND
#define LIBETPAN_HAS_MAILIMAP_RAMBLER_WORKAROUND	1
#endif
LIBETPAN_EXPORT
int mailimap_is_rambler_workaround_enabled(mailimap * session);
LIBETPAN_EXPORT
void mailimap_set_rambler_workaround_enabled(mailimap * session, int enabled);
#ifndef LIBETPAN_HAS_MAILIMAP_QIP_WORKAROUND
#define LIBETPAN_HAS_MAILIMAP_QIP_WORKAROUND	1
#endif
LIBETPAN_EXPORT
int mailimap_is_qip_workaround_enabled(mailimap * session);
LIBETPAN_EXPORT
void mailimap_set_qip_workaround_enabled(mailimap * session, int enabled);
#ifdef __cplusplus
}
#endif
#endif