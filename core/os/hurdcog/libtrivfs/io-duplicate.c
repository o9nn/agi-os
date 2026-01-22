#include "priv.h"
#include "trivfs_io_S.h"
#include <string.h>
kern_return_t
trivfs_S_io_duplicate (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
mach_port_t *newport,
mach_msg_type_name_t *newporttype)
{
error_t err;
struct trivfs_protid *newcred;
if (!cred)
return EOPNOTSUPP;
err = trivfs_protid_dup (cred, &newcred);
if (!err)
{
*newport = ports_get_right (newcred);
*newporttype = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (newcred);
}
return err;
}