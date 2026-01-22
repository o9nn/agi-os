#ifndef __FTPCONN_H__
#define __FTPCONN_H__
#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <features.h>
#define __need_error_t
#include <errno.h>
#ifndef __error_t_defined
typedef int error_t;
#define __error_t_defined
#endif
#ifdef FTP_CONN_DEFINE_EI
#define FTP_CONN_EI
#else
#define FTP_CONN_EI __extern_inline
#endif
struct ftp_conn;
struct ftp_conn_params;
struct ftp_conn_stat;
typedef error_t (*ftp_conn_add_stat_fun_t) (const char *name,
# if _FILE_OFFSET_BITS == 64
const struct stat *stat,
# else
const struct stat64 *stat,
# endif
const char *symlink_target,
void *hook);
struct ftp_conn_syshooks
{
error_t (*pasv_addr) (struct ftp_conn *conn, const char *txt,
struct sockaddr **addr);
error_t (*interp_err) (struct ftp_conn *conn, const char *txt,
const error_t *poss_errs);
error_t (*start_get_stats) (struct ftp_conn *conn, const char *name,
int contents, int *fd, void **state);
error_t (*cont_get_stats) (struct ftp_conn *conn, int fd, void *state,
ftp_conn_add_stat_fun_t add_stat, void *hook);
error_t (*append_name) (struct ftp_conn *conn,
const char *dir, const char *name,
char **composite);
error_t (*basename) (struct ftp_conn *conn, char **name);
};
#define FTP_CONN_CNTL_DEBUG_CMD 1
#define FTP_CONN_CNTL_DEBUG_REPLY 2
#define FTP_CONN_GET_LOGIN_PARAM_USER 1
#define FTP_CONN_GET_LOGIN_PARAM_PASS 2
#define FTP_CONN_GET_LOGIN_PARAM_ACCT 3
struct ftp_conn_hooks
{
void (*choose_syshooks) (struct ftp_conn *conn, const char *syst);
void (*cntl_debug) (struct ftp_conn *conn, int type, const char *txt);
void (*opened) (struct ftp_conn *conn);
error_t (*get_login_param) (struct ftp_conn *conn, int type, char **txt);
void (*closed) (struct ftp_conn *conn);
error_t (*init) (struct ftp_conn *conn);
void (*fini) (struct ftp_conn *conn);
int (*interrupt_check) (struct ftp_conn *conn);
};
struct ftp_conn
{
const struct ftp_conn_params *params;
const struct ftp_conn_hooks *hooks;
struct ftp_conn_syshooks syshooks;
int syshooks_valid : 1;
int control;
char *line;
size_t line_sz;
size_t line_offs;
size_t line_len;
char *reply_txt;
size_t reply_txt_sz;
char *cwd;
const char *type;
void *hook;
int use_passive : 1;
struct sockaddr *actv_data_addr;
};
struct ftp_conn_params
{
void *addr;
size_t addr_len;
int addr_type;
char *user, *pass, *acct;
};
extern error_t ftp_conn_unix_pasv_addr (struct ftp_conn *conn, const char *txt,
struct sockaddr **addr);
extern error_t ftp_conn_unix_interp_err (struct ftp_conn *conn, const char *txt,
const error_t *poss_errs);
extern error_t ftp_conn_unix_start_get_stats (struct ftp_conn *conn,
const char *name,
int contents, int *fd,
void **state);
extern error_t ftp_conn_unix_cont_get_stats (struct ftp_conn *conn,
int fd, void *state,
ftp_conn_add_stat_fun_t add_stat,
void *hook);
error_t ftp_conn_unix_append_name (struct ftp_conn *conn,
const char *dir, const char *name,
char **composite);
error_t ftp_conn_unix_basename (struct ftp_conn *conn, char **name);
extern struct ftp_conn_syshooks ftp_conn_unix_syshooks;
error_t
ftp_conn_get_raw_reply (struct ftp_conn *conn,
int *reply, const char **reply_txt);
error_t
ftp_conn_get_reply (struct ftp_conn *conn, int *reply, const char **reply_txt);
error_t
ftp_conn_cmd (struct ftp_conn *conn, const char *cmd, const char *arg,
int *reply, const char **reply_txt);
error_t
ftp_conn_cmd_reopen (struct ftp_conn *conn, const char *cmd, const char *arg,
int *reply, const char **reply_txt);
void ftp_conn_abort (struct ftp_conn *conn);
void ftp_conn_set_syshooks (struct ftp_conn *conn,
struct ftp_conn_syshooks *syshooks);
error_t ftp_conn_open (struct ftp_conn *conn);
void ftp_conn_close (struct ftp_conn *conn);
extern error_t ftp_conn_validate_syshooks (struct ftp_conn *conn);
#if defined(__USE_EXTERN_INLINES) || defined(FTP_CONN_DEFINE_EI)
FTP_CONN_EI error_t
ftp_conn_validate_syshooks (struct ftp_conn *conn)
{
if (conn->syshooks_valid)
return 0;
else
return ftp_conn_open (conn);
}
#endif
error_t ftp_conn_create (const struct ftp_conn_params *params,
const struct ftp_conn_hooks *hooks,
struct ftp_conn **conn);
void ftp_conn_free (struct ftp_conn *conn);
error_t
ftp_conn_start_transfer (struct ftp_conn *conn,
const char *cmd, const char *arg,
const error_t *poss_errs,
int *data);
error_t ftp_conn_finish_transfer (struct ftp_conn *conn);
error_t ftp_conn_start_retrieve (struct ftp_conn *conn, const char *name, int *data);
error_t ftp_conn_start_list (struct ftp_conn *conn, const char *name, int *data);
error_t ftp_conn_start_dir (struct ftp_conn *conn, const char *name, int *data);
error_t ftp_conn_start_store (struct ftp_conn *conn, const char *name, int *data);
error_t
ftp_conn_rmt_transfer (struct ftp_conn *src_conn,
const char *src_cmd, const char *src_name,
const int *src_poss_errs,
struct ftp_conn *dst_conn, const char *dst_name);
error_t
ftp_conn_rmt_copy (struct ftp_conn *src_conn, const char *src_name,
struct ftp_conn *dst_conn, const char *dst_name);
error_t ftp_conn_get_cwd (struct ftp_conn *conn, char **cwd);
error_t ftp_conn_cwd (struct ftp_conn *conn, const char *cwd);
error_t ftp_conn_cdup (struct ftp_conn *conn);
error_t ftp_conn_set_type (struct ftp_conn *conn, const char *type);
error_t ftp_conn_start_get_stats (struct ftp_conn *conn,
const char *name, int contents,
int *fd, void **state);
error_t ftp_conn_cont_get_stats (struct ftp_conn *conn, int fd, void *state,
ftp_conn_add_stat_fun_t add_stat, void *hook);
error_t ftp_conn_get_stats (struct ftp_conn *conn,
const char *name, int contents,
ftp_conn_add_stat_fun_t add_stat, void *hook);
typedef error_t (*ftp_conn_add_name_fun_t) (const char *name, void *hook);
error_t ftp_conn_start_get_names (struct ftp_conn *conn,
const char *name, int *fd, void **state);
error_t ftp_conn_cont_get_names (struct ftp_conn *conn, int fd, void *state,
ftp_conn_add_name_fun_t add_name, void *hook);
error_t ftp_conn_get_names (struct ftp_conn *conn, const char *name,
ftp_conn_add_name_fun_t add_name, void *hook);
error_t ftp_conn_append_name (struct ftp_conn *conn,
const char *dir, const char *name,
char **composite);
error_t ftp_conn_basename (struct ftp_conn *conn,
const char *composite, char **base);
#endif