#include "priv.h"
#include "trivfs_io_S.h"
#include <assert-backtrace.h>
kern_return_t
trivfs_S_io_get_icky_async_id (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
mach_port_t *id,
mach_msg_type_name_t *idtype)
{
return EOPNOTSUPP;
}