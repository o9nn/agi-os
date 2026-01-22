#include "ports.h"
#include <assert-backtrace.h>
void
ports_port_ref (void *portstruct)
{
struct port_info *pi = portstruct;
refcounts_ref (&pi->refcounts, NULL);
}