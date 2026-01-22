#include "priv.h"
#include "trivfs_io_S.h"
#include <assert-backtrace.h>
kern_return_t
trivfs_S_io_get_openmodes (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
int *bits)
{
if (!cred)
return EOPNOTSUPP;
else
{
*bits = cred->po->openmodes;
return 0;
}
}