#ifndef __FTPCONN_PRIV_H__
#define __FTPCONN_PRIV_H__
#include <features.h>
#ifdef FTP_CONN_DEFINE_EI
#define FTP_CONN_EI
#else
#define FTP_CONN_EI __extern_inline
#endif
#define REPLY_DELAY	120
#define REPLY_OK	200
#define REPLY_SYSTYPE	215
#define REPLY_HELLO	220
#define REPLY_ABORT_OK	225
#define REPLY_TRANS_OK	226
#define REPLY_PASV_OK	227
#define REPLY_LOGIN_OK	230
#define REPLY_FCMD_OK	250
#define REPLY_DIR_NAME	257
#define REPLY_NEED_PASS	331
#define REPLY_NEED_ACCT 332
#define REPLY_CLOSED	421
#define REPLY_ABORTED	426
#define REPLY_BAD_CMD	500
#define REPLY_BAD_ARG	501
#define REPLY_UNIMP_CMD	502
#define REPLY_UNIMP_ARG	504
#define REPLY_NO_LOGIN	530
#define REPLY_NO_ACCT	532
#define REPLY_NO_SPACE	552
#define REPLY_IS_PRELIM(rep) ((rep) >= 100 && (rep) < 200)
#define REPLY_IS_SUCCESS(rep) ((rep) >= 200 && (rep) < 300)
#define REPLY_IS_INCOMPLETE(rep) ((rep) >= 300 && (rep) < 400)
#define REPLY_IS_TRANSIENT(rep) ((rep) >= 400 && (rep) < 500)
#define REPLY_IS_FAILURE(rep) ((rep) >= 500 && (rep) < 600)
extern error_t unexpected_reply (struct ftp_conn *conn, int reply, const char *reply_txt,
const error_t *poss_errs);
#if defined(__USE_EXTERN_INLINES) || defined(FTP_CONN_DEFINE_EI)
FTP_CONN_EI error_t
unexpected_reply (struct ftp_conn *conn, int reply, const char *reply_txt,
const error_t *poss_errs)
{
if (reply == REPLY_CLOSED)
return EPIPE;
else if (reply == REPLY_UNIMP_CMD || reply == REPLY_UNIMP_ARG)
return EOPNOTSUPP;
else if (reply == REPLY_BAD_ARG)
return EINVAL;
else if (REPLY_IS_FAILURE (reply) && reply_txt
&& conn->syshooks.interp_err && poss_errs)
return (*conn->syshooks.interp_err) (conn, reply_txt, poss_errs);
else if (REPLY_IS_TRANSIENT (reply))
return EAGAIN;
else
return EGRATUITOUS;
}
#endif
extern const error_t ftp_conn_poss_file_errs[];
error_t ftp_conn_get_pasv_addr (struct ftp_conn *conn, struct sockaddr **addr);
error_t ftp_conn_send_actv_addr (struct ftp_conn *conn, struct sockaddr *addr);
#endif