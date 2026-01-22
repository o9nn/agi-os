#include "lwip-hurd.h"
#include <assert.h>
#include <refcount.h>
#include <lwip/sockets.h>
error_t
make_sockaddr_port (int sock,
int peer,
mach_port_t * addr, mach_msg_type_name_t * addrtype)
{
struct sockaddr_storage buf;
int buflen = sizeof buf;
error_t err;
struct sock_addr *addrstruct;
if (peer)
err =
lwip_getpeername (sock, (struct sockaddr *) &buf,
(socklen_t *) & buflen);
else
err =
lwip_getsockname (sock, (struct sockaddr *) &buf,
(socklen_t *) & buflen);
if (err)
return -err;
err = ports_create_port (addrport_class, lwip_bucket,
(offsetof (struct sock_addr, address)
+buflen), &addrstruct);
if (!err)
{
addrstruct->address.sa.sa_family = buf.ss_family;
addrstruct->address.sa.sa_len = buflen;
memcpy (addrstruct->address.sa.sa_data,
((struct sockaddr *) &buf)->sa_data,
buflen - offsetof (struct sockaddr, sa_data));
*addr = ports_get_right (addrstruct);
*addrtype = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (addrstruct);
}
return err;
}
struct socket *
sock_alloc (void)
{
struct socket *sock;
sock = calloc (1, sizeof *sock);
if (!sock)
return 0;
sock->sockno = -1;
sock->identity = MACH_PORT_NULL;
refcount_init (&sock->refcnt, 1);
return sock;
}
void
sock_release (struct socket *sock)
{
if (refcount_deref (&sock->refcnt) != 0)
return;
if (sock->sockno > -1)
lwip_close (sock->sockno);
if (sock->identity != MACH_PORT_NULL)
mach_port_destroy (mach_task_self (), sock->identity);
free (sock);
}
struct sock_user *
make_sock_user (struct socket *sock, int isroot, int noinstall, int consume)
{
error_t err;
struct sock_user *user;
assert_backtrace (sock->refcnt != 0);
if (noinstall)
err = ports_create_port_noinstall (socketport_class, lwip_bucket,
sizeof (struct sock_user), &user);
else
err = ports_create_port (socketport_class, lwip_bucket,
sizeof (struct sock_user), &user);
if (err)
{
errno = err;
return 0;
}
if (!consume)
refcount_ref (&sock->refcnt);
user->isroot = isroot;
user->sock = sock;
return user;
}
void
clean_socketport (void *arg)
{
struct sock_user *const user = arg;
sock_release (user->sock);
}
void
clean_addrport (void *arg)
{
}