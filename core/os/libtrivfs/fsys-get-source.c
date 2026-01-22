#include "priv.h"
#include "trivfs_fsys_S.h"
kern_return_t
trivfs_S_fsys_get_source (struct trivfs_control *fsys,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
string_t source)
{
return fsys ? trivfs_get_source (source, 1024 ) : EOPNOTSUPP;
}