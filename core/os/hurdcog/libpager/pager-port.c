#include "priv.h"
mach_port_t
pager_get_port (struct pager *p)
{
return ports_get_right (p);
}