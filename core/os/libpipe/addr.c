#include <hurd/ports.h>
void pipe_dealloc_addr (void *addr)
{
ports_port_deref (addr);
}