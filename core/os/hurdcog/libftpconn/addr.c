#include <unistd.h>
#include <errno.h>
#include <netinet/in.h>
#include <ftpconn.h>
#include "priv.h"
error_t
ftp_conn_get_pasv_addr (struct ftp_conn *conn, struct sockaddr **addr)
{
int reply;
const char *txt;
error_t err = ftp_conn_cmd_reopen (conn, "pasv", 0, &reply, &txt);
if (! err)
{
if (reply == REPLY_PASV_OK)
err = (*(conn->syshooks.pasv_addr ?: ftp_conn_unix_pasv_addr))
(conn, txt, addr);
else
err = unexpected_reply (conn, reply, txt, 0);
}
return err;
}
error_t
ftp_conn_send_actv_addr (struct ftp_conn *conn, struct sockaddr *addr)
{
error_t err;
if (addr == 0)
err = EINVAL;
else if (addr->sa_family != AF_INET)
err = EAFNOSUPPORT;
else
{
char buf[50];
int reply;
unsigned char *a =
(unsigned char *)&((struct sockaddr_in *)addr)->sin_addr.s_addr;
unsigned char *p =
(unsigned char *)&((struct sockaddr_in *)addr)->sin_port;
snprintf (buf, sizeof buf, "%d,%d,%d,%d,%d,%d",
a[0], a[1], a[2], a[3], p[0], p[1]);
err = ftp_conn_cmd_reopen (conn, "port", buf, &reply, 0);
if (! err)
{
if (reply == REPLY_OK)
err = 0;
else
err = unexpected_reply (conn, reply, 0, 0);
}
}
return err;
}