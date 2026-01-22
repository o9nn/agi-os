#include "ports.h"
#include <assert-backtrace.h>
void
ports_port_deref_weak (void *portstruct)
{
struct port_info *pi = portstruct;
struct references result;
refcounts_deref_weak (&pi->refcounts, &result);
if (result.hard == 0 && result.weak == 0)
_ports_complete_deallocate (pi);
}