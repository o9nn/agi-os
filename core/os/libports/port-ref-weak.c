#include "ports.h"
#include <assert-backtrace.h>
void
ports_port_ref_weak (void *portstruct)
{
struct port_info *pi = portstruct;
refcounts_ref_weak (&pi->refcounts, NULL);
}