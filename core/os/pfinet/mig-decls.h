#ifndef __PFINET_MIG_DECLS_H__
#define __PFINET_MIG_DECLS_H__
#include "pfinet.h"
typedef struct sock_user *sock_user_t;
typedef struct sock_addr *sock_addr_t;
static inline struct sock_user * __attribute__ ((unused))
begin_using_socket_port (mach_port_t port)
{
return ports_lookup_port (pfinet_bucket, port, socketport_class);
}
static inline struct sock_user * __attribute__ ((unused))
begin_using_socket_payload (uintptr_t payload)
{
return ports_lookup_payload (pfinet_bucket, payload, socketport_class);
}
static inline void __attribute__ ((unused))
end_using_socket_port (struct sock_user *user)
{
if (user)
ports_port_deref (user);
}
static inline struct sock_addr * __attribute__ ((unused))
begin_using_sockaddr_port (mach_port_t port)
{
return ports_lookup_port (pfinet_bucket, port, addrport_class);
}
static inline struct sock_addr * __attribute__ ((unused))
begin_using_sockaddr_payload (uintptr_t payload)
{
return ports_lookup_payload (pfinet_bucket, payload, addrport_class);
}
static inline void __attribute__ ((unused))
end_using_sockaddr_port (struct sock_addr *addr)
{
if (addr)
ports_port_deref (addr);
}
#endif