#include <unistd.h>
#include <errno.h>
#include <ftpconn.h>
#include "priv.h"
error_t
ftp_conn_rmt_transfer (struct ftp_conn *src_conn,
const char *src_cmd, const char *src_name,
const int *src_poss_errs,
struct ftp_conn *dst_conn, const char *dst_name)
{
struct sockaddr *src_addr;
error_t err = ftp_conn_get_pasv_addr (src_conn, &src_addr);
if (! err)
{
err = ftp_conn_send_actv_addr (dst_conn, src_addr);
if (! err)
{
int reply;
const char *txt;
err = ftp_conn_cmd (src_conn, src_cmd, src_name, 0, 0);
if (! err)
{
err = ftp_conn_cmd (dst_conn, "stor", dst_name, &reply, &txt);
if (! err)
{
if (REPLY_IS_PRELIM (reply))
{
err = ftp_conn_get_reply (src_conn, &reply, &txt);
if (!err && !REPLY_IS_PRELIM (reply))
err = unexpected_reply (src_conn, reply, txt,
src_poss_errs);
if (err)
ftp_conn_abort (dst_conn);
else
err = ftp_conn_finish_transfer (dst_conn);
}
else
err = unexpected_reply (dst_conn, reply, txt,
ftp_conn_poss_file_errs);
}
if (err)
ftp_conn_close (src_conn);
else
err = ftp_conn_finish_transfer (src_conn);
}
}
free (src_addr);
}
return err;
}
error_t
ftp_conn_rmt_copy (struct ftp_conn *src_conn, const char *src_name,
struct ftp_conn *dst_conn, const char *dst_name)
{
return
ftp_conn_rmt_transfer (src_conn, "retr", src_name, ftp_conn_poss_file_errs,
dst_conn, dst_name);
}