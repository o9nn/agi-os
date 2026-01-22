#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_getlinknode (struct protid *user,
file_t *port,
mach_msg_type_name_t *porttype)
{
if (!user)
return EOPNOTSUPP;
if (user->po->np == netfs_root_node)
return EBUSY;
*port = ports_get_right (user);
*porttype = MACH_MSG_TYPE_MAKE_SEND;
return 0;
}