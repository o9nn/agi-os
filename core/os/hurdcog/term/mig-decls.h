#ifndef __TERM_MIG_DECLS_H__
#define __TERM_MIG_DECLS_H__
#include <hurd/ports.h>
#include "term.h"
static inline struct port_info * __attribute__ ((unused))
begin_using_ctty_port (mach_port_t port)
{
return ports_lookup_port (term_bucket, port, cttyid_class);
}
static inline struct port_info * __attribute__ ((unused))
begin_using_ctty_payload (uintptr_t payload)
{
return ports_lookup_payload (term_bucket, payload, cttyid_class);
}
static inline void __attribute__ ((unused))
end_using_ctty (struct port_info *p)
{
if (p)
ports_port_deref (p);
}
#endif