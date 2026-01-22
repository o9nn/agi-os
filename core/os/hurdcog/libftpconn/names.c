#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <ftpconn.h>
struct get_names_state
{
char *name;
size_t name_len;
size_t name_alloced;
int name_partial;
size_t buf_len;
char buf[7000];
};
error_t
ftp_conn_start_get_names (struct ftp_conn *conn,
const char *name, int *fd, void **state)
{
error_t err;
struct get_names_state *s = malloc (sizeof (struct get_names_state));
if (! s)
return ENOMEM;
err = ftp_conn_start_list (conn, name, fd);
if (err)
free (s);
else
{
s->name = 0;
s->name_len = s->name_alloced = 0;
s->name_partial = 0;
s->buf_len = 0;
*state = s;
}
return err;
}
error_t
ftp_conn_cont_get_names (struct ftp_conn *conn, int fd, void *state,
ftp_conn_add_name_fun_t add_name, void *hook)
{
char *p, *nl;
ssize_t rd;
size_t name_len;
error_t err = 0;
struct get_names_state *s = state;
int (*icheck) (struct ftp_conn *conn) = conn->hooks->interrupt_check;
rd = read (fd, s->buf + s->buf_len, sizeof (s->buf) - s->buf_len);
if (rd < 0)
{
err = errno;
goto finished;
}
if (icheck && (*icheck) (conn))
{
err = EINTR;
goto finished;
}
if (rd == 0)
if (s->buf_len == 0)
goto finished;
else
nl = s->buf + s->buf_len;
else
{
nl = memchr (s->buf + s->buf_len, '\n', rd);
s->buf_len += rd;
}
if (!nl && s->buf_len < sizeof (s->buf))
return EAGAIN;
p = s->buf;
do
{
name_len = (nl ? nl - p : s->buf + s->buf_len - p);
if (name_len > 0 && p[name_len - 1] == '\r')
name_len--;
if (name_len > 0)
{
size_t old_len = s->name_len;
size_t total_len = old_len + name_len + 1;
if (total_len > s->name_alloced)
{
char *new_name = realloc (s->name, total_len);
if (! new_name)
goto enomem;
s->name = new_name;
s->name_alloced = total_len;
}
strncpy (s->name + old_len, p, name_len);
s->name[old_len + name_len] = '\0';
s->name_len = total_len - 1;
}
if (nl)
{
char *name = s->name;
if (conn->syshooks.basename)
{
err = (*conn->syshooks.basename) (conn, &name);
if (err)
goto finished;
}
err = (*add_name) (name, hook);
if (name < s->name || name > s->name + s->name_len)
free (name);
if (err)
goto finished;
s->name_len = 0;
s->name_partial = 0;
p = nl + 1;
nl = memchr (p, '\n', s->buf + s->buf_len - p);
}
else
{
s->name_partial = 1;
p += name_len;
}
}
while (nl);
s->buf_len -= (p - s->buf);
if (s->buf_len > 0)
memmove (s->buf, p, s->buf_len);
return EAGAIN;
enomem:
err = ENOMEM;
finished:
if (s->name)
free (s->name);
free (s);
close (fd);
if (err && rd > 0)
ftp_conn_abort (conn);
else if (err)
ftp_conn_finish_transfer (conn);
else
err = ftp_conn_finish_transfer (conn);
return err;
}
error_t
ftp_conn_get_names (struct ftp_conn *conn, const char *name,
ftp_conn_add_name_fun_t add_name, void *hook)
{
int fd;
void *state;
error_t err = ftp_conn_start_get_names (conn, name, &fd, &state);
if (err)
return err;
do
err = ftp_conn_cont_get_names (conn, fd, state, add_name, hook);
while (err == EAGAIN);
return err;
}