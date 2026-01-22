#include "ports.h"
#include <assert-backtrace.h>
void
ports_port_deref (void *portstruct)
{
struct port_info *pi = portstruct;
struct references result;
if (pi->class->dropweak_routine)
{
refcounts_demote (&pi->refcounts, &result);
if (result.hard == 0 && result.weak > 1)
(*pi->class->dropweak_routine) (pi);
refcounts_deref_weak (&pi->refcounts, &result);
}
else
refcounts_deref (&pi->refcounts, &result);
if (result.hard == 0 && result.weak == 0)
_ports_complete_deallocate (pi);
}