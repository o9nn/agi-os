#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_get_icky_async_id (struct protid *cred,
mach_port_t *idport,
mach_msg_type_name_t *idport_type)
{
if (!cred)
return EOPNOTSUPP;
*idport = MACH_PORT_NULL;
*idport_type = MACH_MSG_TYPE_COPY_SEND;
return 0;
}