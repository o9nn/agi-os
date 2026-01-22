#ifndef __LIBPAGER_MIG_DECLS_H__
#define __LIBPAGER_MIG_DECLS_H__
#include "priv.h"
typedef struct pager *pager_t;
static inline struct pager * __attribute__ ((unused))
begin_using_pager (mach_port_t port)
{
return ports_lookup_port (0, port, _pager_class);
}
static inline struct pager * __attribute__ ((unused))
begin_using_pager_payload (uintptr_t payload)
{
return ports_lookup_payload (0, payload, _pager_class);
}
static inline void __attribute__ ((unused))
end_using_pager (struct pager *p)
{
if (p)
ports_port_deref (p);
}
#endif