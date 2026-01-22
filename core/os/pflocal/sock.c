#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <hurd/pipe.h>
#include "sock.h"
#include "sserver.h"
#include "connq.h"
error_t
sock_acquire_read_pipe (struct sock *sock, struct pipe **pipe)
{
error_t err = 0;
pthread_mutex_lock (&sock->lock);
*pipe = sock->read_pipe;
if (*pipe != NULL)
if (   !(sock->pipe_class->flags & PIPE_CLASS_CONNECTIONLESS)
&& !(sock->flags & PFLOCAL_SOCK_CONNECTED))
err = ENOTCONN;
else
pipe_acquire_reader (*pipe);
else if (sock->flags & PFLOCAL_SOCK_SHUTDOWN_READ)
err = EPIPE;
else
err = ENOTCONN;
pthread_mutex_unlock (&sock->lock);
return err;
}
error_t
sock_acquire_write_pipe (struct sock *sock, struct pipe **pipe)
{
error_t err = 0;
pthread_mutex_lock (&sock->lock);
*pipe = sock->write_pipe;
if (*pipe != NULL)
pipe_acquire_writer (*pipe);
else if (sock->flags & PFLOCAL_SOCK_SHUTDOWN_WRITE)
err = EPIPE;
else if (sock->pipe_class->flags & PIPE_CLASS_CONNECTIONLESS)
err = EDESTADDRREQ;
else
err = ENOTCONN;
pthread_mutex_unlock (&sock->lock);
return err;
}
error_t
sock_create (struct pipe_class *pipe_class, mode_t mode, struct sock **sock)
{
error_t err;
struct sock *new = malloc (sizeof (struct sock));
if (new == NULL)
return ENOMEM;
err = pipe_create (pipe_class, &new->read_pipe);
if (err)
{
free (new);
return err;
}
pipe_add_reader (new->read_pipe);
new->refs = 0;
new->flags = 0;
new->write_pipe = NULL;
new->req_write_limit = 0;
new->mode = mode;
new->id = MACH_PORT_NULL;
new->listen_queue = NULL;
new->connect_queue = NULL;
new->pipe_class = pipe_class;
new->addr = NULL;
new->uid = getuid ();
new->gid = getgid ();
memset (&new->change_time, 0, sizeof (new->change_time));
pthread_mutex_init (&new->lock, NULL);
*sock = new;
return 0;
}
void
sock_free (struct sock *sock)
{
sock_shutdown (sock, PFLOCAL_SOCK_SHUTDOWN_READ | PFLOCAL_SOCK_SHUTDOWN_WRITE);
if (sock->id != MACH_PORT_NULL)
mach_port_destroy (mach_task_self (), sock->id);
if (sock->listen_queue)
connq_destroy (sock->listen_queue);
free (sock);
}
void
_sock_norefs (struct sock *sock)
{
assert_backtrace (sock->addr == NULL);
pthread_mutex_unlock (&sock->lock);
sock_free (sock);
}
error_t
sock_clone (struct sock *template, struct sock **sock)
{
error_t err = sock_create (template->pipe_class, template->mode, sock);
if (err)
return err;
(*sock)->flags =
template->flags & ~(PFLOCAL_SOCK_CONNECTED | PFLOCAL_SOCK_NONBLOCK);
return 0;
}
struct port_class *sock_user_port_class;
static void
sock_user_clean (void *vuser)
{
struct sock_user *user = vuser;
sock_deref (user->sock);
}
error_t
sock_create_port (struct sock *sock, mach_port_t *port)
{
struct sock_user *user;
error_t err =
ports_create_port (sock_user_port_class, sock_port_bucket,
sizeof (struct sock_user), &user);
if (err)
return err;
ensure_sock_server ();
pthread_mutex_lock (&sock->lock);
sock->refs++;
pthread_mutex_unlock (&sock->lock);
user->sock = sock;
*port = ports_get_right (user);
ports_port_deref (user);
return 0;
}
struct addr
{
struct port_info pi;
struct sock *sock;
pthread_mutex_t lock;
};
struct port_class *addr_port_class;
static void
addr_unbind (void *vaddr)
{
struct sock *sock;
struct addr *addr = vaddr;
pthread_mutex_lock (&addr->lock);
sock = addr->sock;
if (sock)
{
pthread_mutex_lock (&sock->lock);
sock->addr = NULL;
addr->sock = NULL;
ports_port_deref_weak (addr);
pthread_mutex_unlock (&sock->lock);
sock_deref (sock);
}
pthread_mutex_unlock (&addr->lock);
}
static void
addr_clean (void *vaddr)
{
struct addr *addr = vaddr;
assert_backtrace (addr->sock == NULL);
}
inline error_t
addr_create (struct addr **addr)
{
error_t err =
ports_create_port (addr_port_class, sock_port_bucket,
sizeof (struct addr), addr);
if (! err)
{
ensure_sock_server ();
(*addr)->sock = NULL;
pthread_mutex_init (&(*addr)->lock, NULL);
}
return err;
}
error_t
sock_bind (struct sock *sock, struct addr *addr)
{
error_t err = 0;
struct addr *old_addr;
if (addr)
pthread_mutex_lock (&addr->lock);
pthread_mutex_lock (&sock->lock);
old_addr = sock->addr;
if (addr && old_addr)
err = EINVAL;
else if (!addr && !old_addr)
err = EINVAL;
else if (addr && addr->sock)
err = EADDRINUSE;
else if (addr)
addr->sock = sock;
else
old_addr->sock = NULL;
if (! err)
{
sock->addr = addr;
if (addr)
{
sock->refs++;
ports_port_ref_weak (addr);
}
if (old_addr)
{
sock->refs--;
ports_port_deref_weak (old_addr);
assert_backtrace (sock->refs > 0);
}
}
pthread_mutex_unlock (&sock->lock);
if (addr)
pthread_mutex_unlock (&addr->lock);
return err;
}
static inline error_t
ensure_addr (struct sock *sock, struct addr **addr)
{
error_t err = 0;
if (! sock->addr)
{
err = addr_create (&sock->addr);
if (!err)
{
sock->addr->sock = sock;
sock->refs++;
ports_port_ref_weak (sock->addr);
}
}
else
ports_port_ref (sock->addr);
if (!err)
*addr = sock->addr;
return err;
}
error_t
addr_get_sock (struct addr *addr, struct sock **sock)
{
pthread_mutex_lock (&addr->lock);
*sock = addr->sock;
if (*sock)
{
pthread_mutex_lock (&(*sock)->lock);
(*sock)->refs++;
pthread_mutex_unlock (&(*sock)->lock);
}
pthread_mutex_unlock (&addr->lock);
return *sock ? 0 : EADDRNOTAVAIL;
}
error_t
sock_get_addr (struct sock *sock, struct addr **addr)
{
error_t err;
pthread_mutex_lock (&sock->lock);
err = ensure_addr (sock, addr);
pthread_mutex_unlock (&sock->lock);
return err;
}
static pthread_mutex_t socket_pair_lock;
error_t
sock_connect (struct sock *sock1, struct sock *sock2)
{
error_t err = 0;
struct pipe *old_sock1_write_pipe = NULL;
void connect (struct sock *wr, struct sock *rd)
{
if (!(   (wr->flags & PFLOCAL_SOCK_SHUTDOWN_WRITE)
|| (rd->flags & PFLOCAL_SOCK_SHUTDOWN_READ)))
{
struct pipe *pipe = rd->read_pipe;
assert_backtrace (pipe);
pipe_add_writer (pipe);
wr->write_pipe = pipe;
if (pipe->write_limit < wr->req_write_limit)
pipe->write_limit = wr->req_write_limit;
}
}
if (sock1->pipe_class != sock2->pipe_class)
return EOPNOTSUPP;
pthread_mutex_lock (&socket_pair_lock);
pthread_mutex_lock (&sock1->lock);
if (sock1 != sock2)
pthread_mutex_lock (&sock2->lock);
if ((sock1->flags & PFLOCAL_SOCK_CONNECTED) || (sock2->flags & PFLOCAL_SOCK_CONNECTED))
err = EISCONN;
else
{
old_sock1_write_pipe = sock1->write_pipe;
connect (sock1, sock2);
if (! (sock1->pipe_class->flags & PIPE_CLASS_CONNECTIONLESS))
{
sock1->flags |= PFLOCAL_SOCK_CONNECTED;
if (sock1 != sock2)
{
connect (sock2, sock1);
sock2->flags |= PFLOCAL_SOCK_CONNECTED;
}
}
}
if (sock1 != sock2)
pthread_mutex_unlock (&sock2->lock);
pthread_mutex_unlock (&sock1->lock);
pthread_mutex_unlock (&socket_pair_lock);
if (old_sock1_write_pipe)
pipe_remove_writer (old_sock1_write_pipe);
return err;
}
void
sock_shutdown (struct sock *sock, unsigned flags)
{
unsigned old_flags;
struct pipe *read_pipe = NULL;
struct pipe *write_pipe = NULL;
pthread_mutex_lock (&sock->lock);
old_flags = sock->flags;
sock->flags |= flags;
if (flags & PFLOCAL_SOCK_SHUTDOWN_READ && !(old_flags & PFLOCAL_SOCK_SHUTDOWN_READ))
{
read_pipe = sock->read_pipe;
sock->read_pipe = NULL;
}
if (flags & PFLOCAL_SOCK_SHUTDOWN_WRITE && !(old_flags & PFLOCAL_SOCK_SHUTDOWN_WRITE))
{
write_pipe = sock->write_pipe;
sock->write_pipe = NULL;
if (write_pipe)
sock->req_write_limit = write_pipe->write_limit;
}
pthread_mutex_unlock (&sock->lock);
if (read_pipe)
pipe_remove_reader (read_pipe);
if (write_pipe)
pipe_remove_writer (write_pipe);
}
error_t
sock_global_init (void)
{
sock_port_bucket = ports_create_bucket ();
sock_user_port_class = ports_create_class (sock_user_clean, NULL);
addr_port_class = ports_create_class (addr_clean, addr_unbind);
return 0;
}
error_t
sock_global_shutdown (void)
{
int num_ports = ports_count_bucket (sock_port_bucket);
ports_enable_bucket (sock_port_bucket);
return (num_ports == 0 ? 0 : EBUSY);
}