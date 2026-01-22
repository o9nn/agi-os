#ifndef __EXEC_MIG_DECLS_H__
#define __EXEC_MIG_DECLS_H__
#include "priv.h"
static inline struct bootinfo * __attribute__ ((unused))
begin_using_bootinfo_port (mach_port_t port)
{
return ports_lookup_port (port_bucket, port, execboot_portclass);
}
static inline struct bootinfo * __attribute__ ((unused))
begin_using_bootinfo_payload (uintptr_t payload)
{
return ports_lookup_payload (port_bucket, payload, execboot_portclass);
}
static inline void __attribute__ ((unused))
end_using_bootinfo (struct bootinfo *b)
{
if (b)
ports_port_deref (b);
}
#endif