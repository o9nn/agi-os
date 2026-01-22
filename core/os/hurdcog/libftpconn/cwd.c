#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <ftpconn.h>
#include "priv.h"
static error_t
_cache_cwd (struct ftp_conn *conn, int reopen)
{
int reply;
const char *txt;
error_t err =
(reopen ? ftp_conn_cmd_reopen : ftp_conn_cmd) (conn, "pwd", 0, &reply, &txt);
if (! err)
{
if (reply == REPLY_DIR_NAME)
{
char *cwd = malloc (strlen (txt));
if (! cwd)
err = ENOMEM;
else if (sscanf (txt, "\"%[^\"]\"", cwd) != 1)
{
free (cwd);
err = EGRATUITOUS;
}
else
{
if (conn->cwd)
free (conn->cwd);
conn->cwd = cwd;
}
}
else
err = unexpected_reply (conn, reply, txt, 0);
}
return err;
}
error_t
ftp_conn_get_cwd (struct ftp_conn *conn, char **cwd)
{
error_t err = 0;
if (! conn->cwd)
err = _cache_cwd (conn, 1);
if (! err)
{
*cwd = strdup (conn->cwd);
if (! *cwd)
err = ENOMEM;
}
return err;
}
error_t
ftp_conn_cwd (struct ftp_conn *conn, const char *cwd)
{
error_t err = 0;
if (conn->cwd && strcmp (conn->cwd, cwd) == 0)
err = 0;
else
{
int reply;
const char *txt;
err = ftp_conn_cmd_reopen (conn, "cwd", cwd, &reply, &txt);
if (! err)
{
if (reply == REPLY_FCMD_OK)
err = _cache_cwd (conn, 0);
else
err = unexpected_reply (conn, reply, txt, ftp_conn_poss_file_errs);
}
}
return err;
}
error_t
ftp_conn_cdup (struct ftp_conn *conn)
{
int reply;
const char *txt;
error_t err = ftp_conn_cmd_reopen (conn, "cdup", 0, &reply, &txt);
if (! err)
{
if (reply == REPLY_OK)
err = _cache_cwd (conn, 0);
else
err = unexpected_reply (conn, reply, txt, ftp_conn_poss_file_errs);
}
return err;
}