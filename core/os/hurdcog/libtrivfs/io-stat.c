#include "priv.h"
#include "trivfs_io_S.h"
#include <unistd.h>
kern_return_t
trivfs_S_io_stat (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
struct stat *st)
{
error_t err;
if (!cred)
return EOPNOTSUPP;
err = io_stat (cred->realnode, st);
if (!err)
{
if (! trivfs_fsid)
trivfs_fsid = getpid();
st->st_fstype = trivfs_fstype;
st->st_fsid = trivfs_fsid;
st->st_mode = (st->st_mode & ~S_IFMT & ~S_ITRANS) | S_IFCHR | S_IROOT;
trivfs_modify_stat (cred, st);
}
return err;
}