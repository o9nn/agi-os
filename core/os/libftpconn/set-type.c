#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <ftpconn.h>
#include "priv.h"
error_t
ftp_conn_set_type (struct ftp_conn *conn, const char *type)
{
error_t err = 0;
if (! type)
return EINVAL;
if (!conn->type || strcmp (type, conn->type) != 0)
{
type = strdup (type);
if (! type)
err = ENOMEM;
else
{
int reply;
error_t err = ftp_conn_cmd (conn, "type", type, &reply, 0);
if (!err && reply != REPLY_OK && reply != REPLY_CLOSED)
err = unexpected_reply (conn, reply, 0, 0);
if (!err || err == EPIPE)
{
if (conn->type)
free ((char *)conn->type);
conn->type = type;
}
}
}
return err;
}