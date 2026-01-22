#include "priv.h"
#include "fsys_S.h"
kern_return_t
diskfs_S_fsys_get_source (struct diskfs_control *fsys,
mach_port_t reply,
mach_msg_type_name_t replytype,
string_t source)
{
if (! fsys)
return EOPNOTSUPP;
return diskfs_get_source (source, 1024 );
}