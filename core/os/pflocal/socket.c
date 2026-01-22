#include <sys/socket.h>
#include <hurd/pipe.h>
#include "sock.h"
#include "connq.h"
#include "socket_S.h"
kern_return_t
S_socket_connect2 (struct sock_user *user1, struct sock_user *user2)
{
error_t err;
if (!user1 || !user2)
return EOPNOTSUPP;
err = sock_connect (user1->sock, user2->sock);
if (!err && user1->sock->pipe_class->flags & PIPE_CLASS_CONNECTIONLESS)
err = sock_connect (user2->sock, user1->sock);
mach_port_deallocate (mach_task_self (), user2->pi.port_right);
return err;
}
static error_t
ensure_connq (struct sock *sock)
{
error_t err = 0;
pthread_mutex_lock (&sock->lock);
if (!sock->listen_queue)
err = connq_create (&sock->listen_queue);
pthread_mutex_unlock (&sock->lock);
return err;
}
kern_return_t
S_socket_listen (struct sock_user *user, int queue_limit)
{
error_t err;
if (!user)
return EOPNOTSUPP;
if (queue_limit < 0)
return EINVAL;
err = ensure_connq (user->sock);
if (!err)
err = connq_set_length (user->sock->listen_queue, queue_limit);
return err;
}
kern_return_t
S_socket_connect (struct sock_user *user, struct addr *addr)
{
error_t err;
struct sock *peer;
int deref = 1;
if (! addr)
return ECONNREFUSED;
mach_port_deallocate (mach_task_self (),
((struct port_info *)addr)->port_right);
if (! user)
return EOPNOTSUPP;
err = addr_get_sock (addr, &peer);
if (err == EADDRNOTAVAIL)
err = ECONNREFUSED;
else if (!err)
{
struct sock *sock = user->sock;
struct connq *cq = peer->listen_queue;
if (sock->pipe_class->flags & PIPE_CLASS_CONNECTIONLESS)
err = sock_connect (sock, peer);
else if (cq)
{
pthread_mutex_lock (&sock->lock);
if (sock->connect_queue)
err = EALREADY;
else if (sock->flags & PFLOCAL_SOCK_CONNECTED)
err = EISCONN;
else
{
sock->connect_queue = cq;
pthread_mutex_unlock (&sock->lock);
err = connq_connect (peer->listen_queue,
sock->flags & PFLOCAL_SOCK_NONBLOCK);
if (!err)
{
struct sock *server;
err = sock_clone (peer, &server);
if (!err)
{
err = sock_connect (sock, server);
if (!err)
{
deref = 0;
connq_connect_complete (peer->listen_queue, server);
}
else
sock_free (server);
}
if (err)
connq_connect_cancel (peer->listen_queue);
}
pthread_mutex_lock (&sock->lock);
sock->connect_queue = NULL;
}
pthread_mutex_unlock (&sock->lock);
}
else
err = ECONNREFUSED;
if (deref)
sock_deref (peer);
}
return err;
}
kern_return_t
S_socket_accept (struct sock_user *user,
mach_port_t *port, mach_msg_type_name_t *port_type,
mach_port_t *peer_addr_port,
mach_msg_type_name_t *peer_addr_port_type)
{
error_t err;
struct sock *sock;
if (!user)
return EOPNOTSUPP;
sock = user->sock;
err = ensure_connq (sock);
if (!err)
{
struct timespec noblock = {0, 0};
struct sock *peer_sock;
err = connq_listen (sock->listen_queue,
(sock->flags & PFLOCAL_SOCK_NONBLOCK) ? &noblock : NULL,
&peer_sock);
if (!err)
{
struct addr *peer_addr;
sock_deref (sock);
*port_type = MACH_MSG_TYPE_MAKE_SEND;
err = sock_create_port (peer_sock, port);
if (!err)
err = sock_get_addr (peer_sock, &peer_addr);
if (!err)
{
*peer_addr_port = ports_get_right (peer_addr);
*peer_addr_port_type = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (peer_addr);
}
else
{
}
}
}
return err;
}
kern_return_t
S_socket_bind (struct sock_user *user, struct addr *addr)
{
if (! addr)
return EADDRNOTAVAIL;
mach_port_deallocate (mach_task_self (),
((struct port_info *)addr)->port_right);
if (! user)
return EOPNOTSUPP;
return sock_bind (user->sock, addr);
}
kern_return_t
S_socket_shutdown (struct sock_user *user, int what)
{
if (! user)
return EOPNOTSUPP;
sock_shutdown (user->sock,
(what != 1 ? PFLOCAL_SOCK_SHUTDOWN_READ : 0)
| (what != 0 ? PFLOCAL_SOCK_SHUTDOWN_WRITE : 0));
return 0;
}
kern_return_t
S_socket_name (struct sock_user *user,
mach_port_t *addr_port, mach_msg_type_name_t *addr_port_type)
{
error_t err;
struct addr *addr;
if (!user)
return EOPNOTSUPP;
err = sock_get_addr (user->sock, &addr);
if (err)
return err;
*addr_port = ports_get_right (addr);
*addr_port_type = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (addr);
return 0;
}
kern_return_t
S_socket_peername (struct sock_user *user,
mach_port_t *addr_port,
mach_msg_type_name_t *addr_port_type)
{
return EOPNOTSUPP;
if (!user)
return EOPNOTSUPP;
*addr_port_type = MACH_MSG_TYPE_MAKE_SEND;
}
kern_return_t
S_socket_send (struct sock_user *user, struct addr *dest_addr, int flags,
const_data_t data, mach_msg_type_number_t data_len,
const mach_port_t *ports, mach_msg_type_number_t num_ports,
const_data_t control, mach_msg_type_number_t control_len,
vm_size_t *amount)
{
error_t err = 0;
int noblock;
struct pipe *pipe;
struct sock *sock, *dest_sock;
struct addr *source_addr;
if (!user)
return EOPNOTSUPP;
sock = user->sock;
if (flags & MSG_OOB)
return EOPNOTSUPP;
if (dest_addr)
{
err = addr_get_sock (dest_addr, &dest_sock);
if (err == EADDRNOTAVAIL)
err = ECONNREFUSED;
if (err)
return err;
if (sock->pipe_class != dest_sock->pipe_class)
err = EINVAL;
}
else
dest_sock = 0;
if (!err && sock->pipe_class->flags & PIPE_CLASS_CONNECTIONLESS)
err = sock_get_addr (sock, &source_addr);
else
source_addr = NULL;
if (!err)
{
if (dest_sock)
err = sock_acquire_read_pipe (dest_sock, &pipe);
else
err = sock_acquire_write_pipe (sock, &pipe);
if (!err)
{
noblock = (user->sock->flags & PFLOCAL_SOCK_NONBLOCK)
|| (flags & MSG_DONTWAIT);
err = pipe_send (pipe, noblock, source_addr, data, data_len,
control, control_len, ports, num_ports,
amount);
if (dest_sock)
pipe_release_reader (pipe);
else
pipe_release_writer (pipe);
}
if (err)
{
if (source_addr)
ports_port_deref (source_addr);
while (num_ports-- > 0)
mach_port_deallocate (mach_task_self (), *ports++);
}
}
if (dest_sock)
sock_deref (dest_sock);
return err;
}
kern_return_t
S_socket_recv (struct sock_user *user,
mach_port_t *addr, mach_msg_type_name_t *addr_type,
int in_flags,
data_t *data, mach_msg_type_name_t *data_len,
mach_port_t **ports, mach_msg_type_name_t *ports_type,
mach_msg_type_name_t *num_ports,
data_t *control, mach_msg_type_name_t *control_len,
int *out_flags, vm_size_t amount)
{
error_t err;
unsigned flags;
int noblock;
struct pipe *pipe;
void *source_addr = NULL;
if (!user)
return EOPNOTSUPP;
if (in_flags & MSG_OOB)
return EINVAL;
flags = in_flags & MSG_PEEK;
err = sock_acquire_read_pipe (user->sock, &pipe);
if (err == EPIPE)
{
err = 0;
*data_len = 0;
if (num_ports)
*num_ports = 0;
if (control_len)
*control_len = 0;
}
else if (!err)
{
size_t data_size = *data_len;
size_t control_size = *control_len;
size_t ports_size = *num_ports;
noblock = (user->sock->flags & PFLOCAL_SOCK_NONBLOCK)
|| (in_flags & MSG_DONTWAIT);
err =
pipe_recv (pipe, noblock, &flags, &source_addr, data, &data_size,
amount, control, &control_size, ports, &ports_size);
pipe_release_reader (pipe);
if (!err)
{
*data_len = data_size;
*control_len = control_size;
*num_ports = ports_size;
}
}
if (!err)
{
*addr_type = MACH_MSG_TYPE_MAKE_SEND;
*ports_type = MACH_MSG_TYPE_MOVE_SEND;
if (source_addr)
{
*addr = ports_get_right (source_addr);
ports_port_deref (source_addr);
}
else
*addr = MACH_PORT_NULL;
}
*out_flags = 0;
return err;
}
kern_return_t
S_socket_getopt (struct sock_user *user,
int level, int opt,
data_t *value,
mach_msg_type_name_t *value_len)
{
int ret = 0;
struct pipe *pipe;
struct sock *sock;
if (!user)
return EOPNOTSUPP;
sock = user->sock;
pthread_mutex_lock (&sock->lock);
switch (level)
{
case SOL_SOCKET:
switch (opt)
{
case SO_TYPE:
if (*value_len < sizeof (int))
{
ret = EINVAL;
break;
}
*(int *)*value = sock->pipe_class->sock_type;
*value_len = sizeof (int);
break;
case SO_RCVBUF:
if (*value_len < sizeof (int))
{
ret = EINVAL;
break;
}
pipe = sock->read_pipe;
if (!pipe)
{
ret = ENOTCONN;
break;
}
*(int *)*value = pipe->write_limit;
*value_len = sizeof (int);
break;
case SO_SNDBUF:
if (*value_len < sizeof (int))
{
ret = EINVAL;
break;
}
pipe = sock->write_pipe;
if (pipe)
*(int *)*value = pipe->write_limit;
else
*(int *)*value = sock->req_write_limit;
*value_len = sizeof (int);
break;
case SO_ERROR:
if (*value_len < sizeof (short))
{
*(char*)*value = 0;
*value_len = sizeof(char);
}
else if (*value_len < sizeof (int))
{
*(short*)*value = 0;
*value_len = sizeof(short);
}
else
{
*(int*)*value = 0;
*value_len = sizeof(int);
}
break;
default:
ret = ENOPROTOOPT;
break;
}
break;
default:
ret = ENOPROTOOPT;
break;
}
pthread_mutex_unlock (&sock->lock);
return ret;
}
kern_return_t
S_socket_setopt (struct sock_user *user,
int level, int opt, const_data_t value,
mach_msg_type_name_t value_len)
{
int ret = 0;
struct pipe *pipe;
struct sock *sock;
if (!user)
return EOPNOTSUPP;
sock = user->sock;
pthread_mutex_lock (&sock->lock);
switch (level)
{
case SOL_SOCKET:
switch (opt)
{
case SO_RCVBUF:
{
int new, old;
if (value_len < sizeof (int))
{
ret = EINVAL;
break;
}
new = *(int *)value;
if (new <= 0)
{
ret = EINVAL;
break;
}
if (new > PFLOCAL_WRITE_LIMIT_MAX)
new = PFLOCAL_WRITE_LIMIT_MAX;
pipe = sock->read_pipe;
if (!pipe)
{
ret = ENOTCONN;
break;
}
pthread_mutex_lock (&pipe->lock);
old = pipe->write_limit;
pipe->write_limit = new;
if (new > old)
_pipe_wake_writers (pipe);
pthread_mutex_unlock (&pipe->lock);
break;
}
case SO_SNDBUF:
{
int new, old;
if (value_len < sizeof (int))
{
ret = EINVAL;
break;
}
new = *(int *)value;
if (new <= 0)
{
ret = EINVAL;
break;
}
if (new > PFLOCAL_WRITE_LIMIT_MAX)
new = PFLOCAL_WRITE_LIMIT_MAX;
pipe = sock->write_pipe;
if (!pipe)
{
sock->req_write_limit = new;
break;
}
pthread_mutex_lock (&pipe->lock);
old = pipe->write_limit;
pipe->write_limit = new;
if (new > old)
_pipe_wake_writers (pipe);
pthread_mutex_unlock (&pipe->lock);
break;
}
default:
ret = ENOPROTOOPT;
break;
}
break;
default:
ret = ENOPROTOOPT;
break;
}
pthread_mutex_unlock (&sock->lock);
return ret;
}