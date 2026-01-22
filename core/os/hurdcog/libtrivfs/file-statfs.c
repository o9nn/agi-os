#include "priv.h"
#include "trivfs_fs_S.h"
#include <string.h>
#include <unistd.h>
kern_return_t
trivfs_S_file_statfs (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
struct statfs *stb)
{
if (!trivfs_fsid)
trivfs_fsid = getpid();
memset (stb, 0, sizeof(struct statfs));
stb->f_type = trivfs_fstype;
stb->f_fsid = trivfs_fsid;
return 0;
}