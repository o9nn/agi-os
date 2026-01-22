#ifndef __LIBPORTS_MIG_DECLS_H__
#define __LIBPORTS_MIG_DECLS_H__
#include "ports.h"
static inline struct port_info * __attribute__ ((unused))
begin_using_port_info_port (mach_port_t port)
{
return ports_lookup_port (0, port, 0);
}
static inline struct port_info * __attribute__ ((unused))
begin_using_port_info_payload (uintptr_t payload)
{
return ports_lookup_payload (0, payload, 0);
}
static inline void __attribute__ ((unused))
end_using_port_info (struct port_info *p)
{
if (p)
ports_port_deref (p);
}
#endif