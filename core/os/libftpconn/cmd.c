#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <arpa/telnet.h>
#include <ftpconn.h>
#include "priv.h"
static error_t
_write (int fd, const void *buf, size_t len)
{
while (len > 0)
{
ssize_t wr = write (fd, buf, len);
if (wr < 0)
return errno;
else if (wr == 0)
return EPIPE;
buf += wr;
len -= wr;
}
return 0;
}
static error_t
_skip_write (int fd, const void *buf, size_t len, size_t *skip)
{
size_t sk = *skip;
error_t err = 0;
if (len > sk)
{
err = _write (fd, buf + sk, len - sk);
*skip = 0;
}
else
*skip = sk - len;
return err;
}
static error_t
_long_cmd (int fd, const char *cmd, const char *arg, size_t skip)
{
error_t err = _skip_write (fd, cmd, strlen (cmd), &skip);
if (!err && arg)
{
err = _skip_write (fd, " ", 1, &skip);
if (! err)
err = _skip_write (fd, arg, strlen (arg), &skip);
}
if (! err)
err = _skip_write (fd, "\r\n", 2, &skip);
return err;
}
error_t
ftp_conn_cmd (struct ftp_conn *conn, const char *cmd, const char *arg,
int *reply, const char **reply_txt)
{
error_t err = 0;
if (conn->control < 0)
err = EPIPE;
else
{
char buf[200];
size_t out =
snprintf (buf, sizeof buf, arg ? "%s %s\r\n" : "%s\r\n", cmd, arg);
err = _write (conn->control, buf, out);
if (!err && conn->hooks && conn->hooks->cntl_debug && out >= 2)
{
buf[out - 2] = '\0';
(* conn->hooks->cntl_debug) (conn, FTP_CONN_CNTL_DEBUG_CMD, buf);
}
if (!err && out == sizeof buf)
err = _long_cmd (conn->control, cmd, arg, sizeof buf);
}
if (!err && (reply || reply_txt))
err = ftp_conn_get_reply (conn, reply, reply_txt);
return err;
}
error_t
ftp_conn_cmd_reopen (struct ftp_conn *conn, const char *cmd, const char *arg,
int *reply, const char **reply_txt)
{
int _reply;
error_t err;
err = ftp_conn_cmd (conn, cmd, arg, &_reply, reply_txt);
if (err == EPIPE || (!err && _reply == REPLY_CLOSED))
{
err = ftp_conn_open (conn);
if (! err)
err = ftp_conn_cmd (conn, cmd, arg, reply, reply_txt);
}
else if (reply)
*reply = _reply;
return err;
}
void
ftp_conn_abort (struct ftp_conn *conn)
{
if (conn->control >= 0)
{
static const char ip[] = { IAC, IP, IAC };
static const char abor[] = { DM, 'a', 'b', 'o', 'r', '\r', '\n' };
if (conn->hooks && conn->hooks->cntl_debug)
(* conn->hooks->cntl_debug) (conn, FTP_CONN_CNTL_DEBUG_CMD, "abor");
if (send (conn->control, ip, sizeof ip, MSG_OOB) == sizeof ip
&& write (conn->control, abor, sizeof abor) == sizeof abor)
{
int reply;
do
ftp_conn_get_raw_reply (conn, &reply, 0);
while (reply == REPLY_ABORTED);
if (reply != REPLY_TRANS_OK && reply != REPLY_ABORT_OK)
ftp_conn_close (conn);
}
else
ftp_conn_close (conn);
}
}