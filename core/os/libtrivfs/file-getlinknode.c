#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_getlinknode (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
mach_port_t *linknode,
mach_msg_type_name_t *linknodetype)
{
if (!cred)
return EOPNOTSUPP;
*linknode = cred->realnode;
*linknodetype = MACH_MSG_TYPE_COPY_SEND;
return 0;
}