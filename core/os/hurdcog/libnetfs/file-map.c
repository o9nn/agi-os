#include "netfs.h"
mach_port_t __attribute__ ((weak))
netfs_get_filemap (struct node *np, vm_prot_t prot)
{
errno = EOPNOTSUPP;
return MACH_PORT_NULL;
}