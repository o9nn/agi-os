#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_async (struct protid *cred __attribute__ ((unused)),
mach_port_t notify __attribute__ ((unused)),
mach_port_t *idport __attribute__ ((unused)),
mach_msg_type_name_t *idport_type
__attribute__ ((unused)))
{
return EOPNOTSUPP;
}