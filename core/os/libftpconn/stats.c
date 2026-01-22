#include <unistd.h>
#include <errno.h>
#include <ftpconn.h>
error_t
ftp_conn_start_get_stats (struct ftp_conn *conn,
const char *name, int contents,
int *fd, void **state)
{
if (conn->syshooks.start_get_stats)
return
(*conn->syshooks.start_get_stats) (conn, name, contents, fd, state);
else
return EOPNOTSUPP;
}
error_t
ftp_conn_cont_get_stats (struct ftp_conn *conn, int fd, void *state,
ftp_conn_add_stat_fun_t add_stat, void *hook)
{
if (conn->syshooks.cont_get_stats)
return (*conn->syshooks.cont_get_stats) (conn, fd, state, add_stat, hook);
else
return EOPNOTSUPP;
}
error_t
ftp_conn_get_stats (struct ftp_conn *conn,
const char *name, int contents,
ftp_conn_add_stat_fun_t add_stat, void *hook)
{
int fd;
void *state;
error_t err = ftp_conn_start_get_stats (conn, name, contents, &fd, &state);
if (err)
return err;
do
err = ftp_conn_cont_get_stats (conn, fd, state, add_stat, hook);
while (err == EAGAIN);
return err;
}