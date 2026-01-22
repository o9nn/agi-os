#include <stddef.h>
#include <sys/socket.h>
#include <hurd/pipe.h>
#include <hurd/trivfs.h>
#include "sock.h"
#include "socket_S.h"
kern_return_t
S_socket_create (trivfs_protid_t pf,
int sock_type, int protocol,
mach_port_t *port, mach_msg_type_name_t *port_type)
{
error_t err;
struct sock *sock;
struct pipe_class *pipe_class;
mode_t mode;
switch (protocol)
{
case 0:
mode = S_IFSOCK;
break;
case S_IFCHR:
case S_IFSOCK:
case S_IFIFO:
mode = protocol;
break;
default:
return EPROTONOSUPPORT;
}
switch (sock_type)
{
case SOCK_STREAM:
pipe_class = stream_pipe_class; break;
case SOCK_DGRAM:
pipe_class = dgram_pipe_class; break;
case SOCK_SEQPACKET:
pipe_class = seqpack_pipe_class; break;
default:
return EPROTOTYPE;
}
err = sock_create (pipe_class, mode, &sock);
if (!err)
{
err = sock_create_port (sock, port);
if (err)
sock_free (sock);
else
*port_type = MACH_MSG_TYPE_MAKE_SEND;
}
if (!err)
{
if (pf->user->uids->num > 0)
sock->uid = pf->user->uids->ids[0];
if (pf->user->gids->num > 0)
sock->gid = pf->user->gids->ids[0];
}
return err;
}
kern_return_t
S_socket_create_address (mach_port_t pf, int sockaddr_type,
const_data_t data,
mach_msg_type_number_t data_len,
mach_port_t *addr_port,
mach_msg_type_name_t *addr_port_type)
{
return EOPNOTSUPP;
}
kern_return_t
S_socket_fabricate_address (mach_port_t pf,
int sockaddr_type,
mach_port_t *addr_port,
mach_msg_type_name_t *addr_port_type)
{
error_t err;
struct addr *addr;
if (sockaddr_type != AF_LOCAL)
return EAFNOSUPPORT;
err = addr_create (&addr);
if (err)
return err;
*addr_port = ports_get_right (addr);
*addr_port_type = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (addr);
return 0;
}
kern_return_t
S_socket_whatis_address (struct addr *addr,
int *sockaddr_type,
data_t *sockaddr,
mach_msg_type_number_t *sockaddr_len)
{
socklen_t addr_len = (offsetof (struct sockaddr, sa_data) + 1);
if (! addr)
return EOPNOTSUPP;
*sockaddr_type = AF_LOCAL;
if (*sockaddr_len < addr_len)
*sockaddr = mmap (0, addr_len, PROT_READ|PROT_WRITE, MAP_ANON, 0, 0);
((struct sockaddr *) *sockaddr)->sa_len = addr_len;
((struct sockaddr *) *sockaddr)->sa_family = *sockaddr_type;
((struct sockaddr *) *sockaddr)->sa_data[0] = 0;
*sockaddr_len = addr_len;
return 0;
}