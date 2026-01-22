#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <netinet/in.h>
#include <ftpconn.h>
#include "priv.h"
static error_t
ftp_conn_start_open_actv_data (struct ftp_conn *conn, int *data)
{
error_t err = 0;
int dcq;
struct sockaddr *addr = conn->actv_data_addr;
socklen_t addr_len = sizeof *addr;
if (! addr)
{
addr = conn->actv_data_addr = malloc (sizeof (struct sockaddr_in));
if (! addr)
return ENOMEM;
if (conn->control < 0)
err = EBADF;
else if (getsockname (conn->control, addr, &addr_len) < 0)
err = errno;
if (err == EBADF || err == EPIPE)
{
err = ftp_conn_open (conn);
if (!err && getsockname (conn->control, addr, &addr_len) < 0)
err = errno;
}
if (err)
{
free (addr);
conn->actv_data_addr = 0;
return err;
}
}
dcq = socket (AF_INET, SOCK_STREAM, 0);
if (dcq < 0)
return errno;
((struct sockaddr_in *)addr)->sin_port = 0;
if (!err && bind (dcq, addr, addr_len) < 0)
err = errno;
if (!err && getsockname (dcq, addr, &addr_len) < 0)
err = errno;
if (!err && listen (dcq, 1) < 0)
err = errno;
if (err)
close (dcq);
else
err = ftp_conn_send_actv_addr (conn, conn->actv_data_addr);
if (! err)
*data = dcq;
return err;
}
static error_t
ftp_conn_finish_open_actv_data (struct ftp_conn *conn, int *data)
{
struct sockaddr_in rmt_addr;
socklen_t rmt_addr_len = sizeof rmt_addr;
int real = accept (*data, &rmt_addr, &rmt_addr_len);
close (*data);
if (real < 0)
return errno;
*data = real;
return 0;
}
static void
ftp_conn_abort_open_actv_data (struct ftp_conn *conn, int data)
{
close (data);
}
static error_t
ftp_conn_start_open_data (struct ftp_conn *conn, int *data)
{
error_t err;
if (conn->use_passive)
{
struct sockaddr *addr;
err = ftp_conn_get_pasv_addr (conn, &addr);
if (! err)
{
int dsock = socket (PF_INET, SOCK_STREAM, 0);
if (dsock < 0)
err = errno;
else if (connect (dsock, addr, addr->sa_len) < 0)
{
err = errno;
close (dsock);
}
else
*data = dsock;
free (addr);
}
}
else
err = EAGAIN;
if (err)
{
conn->use_passive = 0;
err = ftp_conn_start_open_actv_data (conn, data);
}
return err;
}
static error_t
ftp_conn_finish_open_data (struct ftp_conn *conn, int *data)
{
if (conn->use_passive)
return 0;
else
return ftp_conn_finish_open_actv_data (conn, data);
}
static void
ftp_conn_abort_open_data (struct ftp_conn *conn, int data)
{
if (conn->use_passive)
close (data);
else
return ftp_conn_abort_open_actv_data (conn, data);
}
error_t
ftp_conn_start_transfer (struct ftp_conn *conn,
const char *cmd, const char *arg,
const error_t *poss_errs,
int *data)
{
error_t err = ftp_conn_start_open_data (conn, data);
if (! err)
{
int reply;
const char *txt;
err = ftp_conn_cmd (conn, cmd, arg, &reply, &txt);
if (!err && !REPLY_IS_PRELIM (reply))
err = unexpected_reply (conn, reply, txt, poss_errs);
if (err)
ftp_conn_abort_open_data (conn, *data);
else
err = ftp_conn_finish_open_data (conn, data);
}
return err;
}
error_t
ftp_conn_finish_transfer (struct ftp_conn *conn)
{
int reply;
error_t err = ftp_conn_get_reply (conn, &reply, 0);
if (!err && reply != REPLY_TRANS_OK && reply != REPLY_FCMD_OK)
err = unexpected_reply (conn, reply, 0, 0);
return err;
}
error_t
ftp_conn_start_retrieve (struct ftp_conn *conn, const char *name, int *data)
{
if (! name)
return EINVAL;
return
ftp_conn_start_transfer (conn, "retr", name, ftp_conn_poss_file_errs, data);
}
error_t
ftp_conn_start_list (struct ftp_conn *conn, const char *name, int *data)
{
return
ftp_conn_start_transfer (conn, "nlst", name, ftp_conn_poss_file_errs, data);
}
error_t
ftp_conn_start_dir (struct ftp_conn *conn, const char *name, int *data)
{
return
ftp_conn_start_transfer (conn, "list", name, ftp_conn_poss_file_errs, data);
}
error_t
ftp_conn_start_store (struct ftp_conn *conn, const char *name, int *data)
{
if (! name)
return EINVAL;
return
ftp_conn_start_transfer (conn, "stor", name, ftp_conn_poss_file_errs, data);
}