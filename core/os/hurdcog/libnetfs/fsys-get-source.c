#include "priv.h"
#include "fsys_S.h"
kern_return_t
netfs_S_fsys_get_source (struct netfs_control *fsys,
mach_port_t reply,
mach_msg_type_name_t reply_type,
string_t source)
{
if (! fsys)
return EOPNOTSUPP;
return netfs_get_source (source, 1024 );
}