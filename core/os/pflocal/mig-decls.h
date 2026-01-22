#ifndef __MIG_DECLS_H__
#define __MIG_DECLS_H__
#include "sock.h"
typedef struct sock_user *sock_user_t;
typedef struct addr *addr_t;
static inline sock_user_t __attribute__ ((unused))
begin_using_sock_user_port(mach_port_t port)
{
return (sock_user_t)ports_lookup_port (0, port, sock_user_port_class);
}
static inline sock_user_t __attribute__ ((unused))
begin_using_sock_user_payload (uintptr_t payload)
{
return ports_lookup_payload (NULL, payload, sock_user_port_class);
}
static inline void __attribute__ ((unused))
end_using_sock_user_port (sock_user_t sock_user)
{
if (sock_user != NULL)
ports_port_deref (sock_user);
}
static inline addr_t __attribute__ ((unused))
begin_using_addr_port(mach_port_t port)
{
return (addr_t)ports_lookup_port (0, port, addr_port_class);
}
static inline addr_t __attribute__ ((unused))
begin_using_addr_payload (uintptr_t payload)
{
return ports_lookup_payload (NULL, payload, addr_port_class);
}
static inline void __attribute__ ((unused))
end_using_addr_port (addr_t addr)
{
if (addr != NULL)
ports_port_deref (addr);
}
#endif