#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_file_getlinknode (struct protid *cred,
file_t *port,
mach_msg_type_name_t *portpoly)
{
struct node *np;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
if (np == diskfs_root_node)
return EBUSY;
*port = ports_get_right (cred);
*portpoly = MACH_MSG_TYPE_MAKE_SEND;
return 0;
}