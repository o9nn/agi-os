#define _HACK_ERRNO_H
#include <assert-backtrace.h>
#include "pfinet.h"
#include <linux/socket.h>
#include <linux/net.h>
#ifndef NPROTO
#define NPROTO (PF_INET + 1)
#endif
struct net_proto_family *net_families[NPROTO];
int
sock_register (struct net_proto_family *fam)
{
assert_backtrace (fam->family < NPROTO);
net_families[fam->family] = fam;
return 0;
}
struct socket *
sock_alloc (void)
{
static ino_t nextino;
struct socket *sock;
pthread_cond_t *c;
sock = malloc (sizeof *sock + sizeof (pthread_cond_t));
if (!sock)
return 0;
c = (void *) &sock[1];
pthread_cond_init (c, NULL);
memset (sock, 0, sizeof *sock);
sock->state = SS_UNCONNECTED;
sock->identity = MACH_PORT_NULL;
sock->refcnt = 1;
sock->wait = (void *) c;
if (nextino == 0)
nextino = 2;
sock->st_ino = nextino++;
return sock;
}
struct sock_user *
make_sock_user (struct socket *sock, int isroot, int noinstall, int consume)
{
error_t err;
struct sock_user *user;
assert_backtrace (sock->refcnt != 0);
if (noinstall)
err = ports_create_port_noinstall (socketport_class, pfinet_bucket,
sizeof (struct sock_user), &user);
else
err = ports_create_port (socketport_class, pfinet_bucket,
sizeof (struct sock_user), &user);
if (err)
{
errno = err;
return 0;
}
if (! consume)
++sock->refcnt;
user->isroot = isroot;
user->sock = sock;
return user;
}
void
sock_release (struct socket *sock)
{
if (--sock->refcnt != 0)
return;
if (sock->state != SS_UNCONNECTED)
sock->state = SS_DISCONNECTING;
if (sock->ops)
sock->ops->release(sock, NULL);
if (sock->identity != MACH_PORT_NULL)
mach_port_destroy (mach_task_self (), sock->identity);
free (sock);
}
void
clean_socketport (void *arg)
{
struct sock_user *const user = arg;
pthread_mutex_lock (&global_lock);
sock_release (user->sock);
pthread_mutex_unlock (&global_lock);
}