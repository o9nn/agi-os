#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <ctype.h>
#include <ftpconn.h>
#include "priv.h"
static inline error_t
ftp_conn_add_reply_txt (struct ftp_conn *conn, size_t *offs,
const char *str, size_t len)
{
if (*offs + len + 1 > conn->reply_txt_sz)
{
size_t new_sz = *offs + len + 50;
char *new = realloc (conn->reply_txt, new_sz);
if (! new)
return ENOMEM;
conn->reply_txt = new;
conn->reply_txt_sz = new_sz;
}
bcopy (str, conn->reply_txt + *offs, len);
conn->reply_txt[*offs + len] = '\0';
*offs += len;
return 0;
}
static inline error_t
ftp_conn_getline (struct ftp_conn *conn, const char **line, size_t *line_len)
{
char *l = conn->line;
size_t offs = conn->line_offs, len = conn->line_len, sz = conn->line_sz;
int (*icheck) (struct ftp_conn *conn) = conn->hooks->interrupt_check;
for (;;)
{
int rd;
if (offs < len)
{
char *nl = memchr (l + offs, '\n', len - offs);
if (nl)
{
*line = l + offs;
offs = nl + 1 - l;
if (nl > *line && nl[-1] == '\r')
nl--;
*nl = '\0';
*line_len = nl - *line;
if (offs == len)
conn->line_offs = conn->line_len = 0;
else
conn->line_offs = offs;
return 0;
}
}
if (offs > (len << 2) && offs < len)
{
len -= offs;
bcopy (l + offs, l, len - offs);
offs = conn->line_offs = 0;
conn->line_len = len;
}
if (len == sz)
{
sz = sz + len ?: 50;
l = realloc (l, sz);
if (! l)
return ENOMEM;
conn->line = l;
conn->line_sz = sz;
}
rd = read (conn->control, l + len, sz - len);
if (rd < 0)
return errno;
else if (rd == 0)
{
*line = l + offs;
*line_len = 0;
return 0;
}
len += rd;
conn->line_len = len;
if (icheck && (*icheck) (conn))
return EINTR;
}
}
inline error_t
ftp_conn_get_raw_reply (struct ftp_conn *conn, int *reply,
const char **reply_txt)
{
size_t reply_txt_offs = 0;
int multi = 0;
if (!reply && !reply_txt)
return 0;
do
{
const char *l = NULL;
size_t len = 0;
error_t err = ftp_conn_getline (conn, &l, &len);
if (err)
return err;
if (!multi && len == 0)
return EPIPE;
#define ACCUM(txt, len) \
do { \
if (reply_txt) \
{ \
error_t err = \
ftp_conn_add_reply_txt (conn, &reply_txt_offs, txt, len); \
if (err) \
return err; \
} \
} while (0)
if (conn->hooks && conn->hooks->cntl_debug)
(*conn->hooks->cntl_debug) (conn, FTP_CONN_CNTL_DEBUG_REPLY, l);
if (isdigit (l[0]) && isdigit (l[1]) && isdigit (l[2]))
{
int code = (l[0] - '0')*100 + (l[1] - '0')*10 + (l[2] - '0');
if (multi && code != multi)
return EGRATUITOUS;
if (l[3] == '-')
multi = code;
else if (l[3] != ' ')
return EGRATUITOUS;
else
{
multi = 0;
if (reply)
*reply = code;
}
ACCUM (l + 4, len - 4);
}
else if (multi)
ACCUM (l, len);
else
return EGRATUITOUS;
}
while (multi);
if (reply_txt)
*reply_txt = conn->reply_txt;
return 0;
}
error_t
ftp_conn_get_reply (struct ftp_conn *conn, int *reply, const char **reply_txt)
{
int code;
error_t err;
do
err = ftp_conn_get_raw_reply (conn, &code, reply_txt);
while (!err && code == REPLY_ABORT_OK);
if (!err && reply)
*reply = code;
return err;
}