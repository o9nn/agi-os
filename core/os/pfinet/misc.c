#include "pfinet.h"
#include <string.h>
#include <stddef.h>
#include <linux/socket.h>
#include <linux/net.h>
error_t
make_sockaddr_port (struct socket *sock,
int peer,
mach_port_t *addr,
mach_msg_type_name_t *addrtype)
{
union { struct sockaddr_storage storage; struct sockaddr sa; } buf;
int buflen = sizeof buf;
error_t err;
struct sock_addr *addrstruct;
err = (*sock->ops->getname) (sock, &buf.sa, &buflen, peer);
if (err)
return -err;
err = ports_create_port (addrport_class, pfinet_bucket,
(offsetof (struct sock_addr, address)
+ buflen), &addrstruct);
if (!err)
{
addrstruct->address.sa_family = buf.sa.sa_family;
addrstruct->address.sa_len = buflen;
memcpy (addrstruct->address.sa_data, buf.sa.sa_data,
buflen - offsetof (struct sockaddr, sa_data));
*addr = ports_get_right (addrstruct);
*addrtype = MACH_MSG_TYPE_MAKE_SEND;
}
ports_port_deref (addrstruct);
return 0;
}
void
clean_addrport (void *arg)
{
}