#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <ftpconn.h>
error_t
ftp_conn_create (const struct ftp_conn_params *params,
const struct ftp_conn_hooks *hooks,
struct ftp_conn **conn)
{
error_t err;
struct ftp_conn *new = malloc (sizeof (struct ftp_conn));
if (! new)
return ENOMEM;
new->control = -1;
new->line = 0;
new->line_sz = 0;
new->line_offs = 0;
new->line_len = 0;
new->reply_txt = 0;
new->reply_txt_sz = 0;
new->params = params;
new->hooks = hooks;
new->syshooks_valid = 0;
new->use_passive = 1;
new->actv_data_addr = 0;
new->cwd = 0;
new->type = 0;
memset (&new->syshooks, 0, sizeof new->syshooks);
if (new->hooks && new->hooks->init)
err = (*new->hooks->init) (new);
else
err = 0;
if (err)
ftp_conn_free (new);
else
*conn = new;
return err;
}
void
ftp_conn_free (struct ftp_conn *conn)
{
ftp_conn_close (conn);
if (conn->hooks && conn->hooks->fini)
(* conn->hooks->fini) (conn);
if (conn->line)
free (conn->line);
if (conn->reply_txt)
free (conn->reply_txt);
if (conn->actv_data_addr)
free (conn->actv_data_addr);
free (conn);
}