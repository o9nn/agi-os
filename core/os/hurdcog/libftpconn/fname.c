#include <string.h>
#include "ftpconn.h"
error_t
ftp_conn_append_name (struct ftp_conn *conn,
const char *dir, const char *name,
char **composite)
{
error_t err = ftp_conn_validate_syshooks (conn);
if (err)
return err;
else if (conn->syshooks.append_name)
return (*conn->syshooks.append_name) (conn, dir, name, composite);
else
return EOPNOTSUPP;
}
error_t
ftp_conn_basename (struct ftp_conn *conn, const char *composite, char **base)
{
error_t err = ftp_conn_validate_syshooks (conn);
if (err)
return err;
if (conn->syshooks.basename)
{
size_t in_size = strlen (composite) + 1;
char *in = strdup (composite), *out = in;
if (! in)
return ENOMEM;
err = (*conn->syshooks.basename) (conn, &out);
if (err || out != in)
{
if (!err && out >= in && out < in + in_size)
out = strdup (out);
free (in);
}
if (! err)
*base = out;
return err;
}
else
return EOPNOTSUPP;
}