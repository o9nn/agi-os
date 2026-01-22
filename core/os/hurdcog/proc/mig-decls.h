#ifndef __MIG_DECLS_H__
#define __MIG_DECLS_H__
#include "proc.h"
static inline struct proc * __attribute__ ((unused))
begin_using_proc_port (mach_port_t port)
{
struct proc *p;
p = ports_lookup_port (proc_bucket, port, proc_class);
if (p && p->p_dead)
ports_port_deref (p);
return (!p || p->p_dead) ? NULL : p;
}
static inline struct proc * __attribute__ ((unused))
begin_using_proc_payload (uintptr_t payload)
{
struct proc *p;
p = ports_lookup_payload (proc_bucket, payload, proc_class);
if (p && p->p_dead)
ports_port_deref (p);
return (!p || p->p_dead) ? 0 : p;
}
static inline void __attribute__ ((unused))
end_using_proc (struct proc *p)
{
if (p)
ports_port_deref (p);
}
typedef struct exc* exc_t;
static inline exc_t __attribute__ ((unused))
begin_using_exc_port (mach_port_t port)
{
return ports_lookup_port (NULL, port, exc_class);
}
static inline exc_t __attribute__ ((unused))
begin_using_exc_payload (uintptr_t payload)
{
return ports_lookup_payload (NULL, payload, exc_class);
}
static inline void __attribute__ ((unused))
end_using_exc (exc_t exc)
{
if (exc != NULL)
ports_port_deref (exc);
}
#endif