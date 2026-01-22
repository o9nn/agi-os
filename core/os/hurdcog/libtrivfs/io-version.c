#include <stdio.h>
#include "priv.h"
#include "trivfs_io_S.h"
kern_return_t
trivfs_S_io_server_version (trivfs_protid_t obj,
mach_port_t reply,
mach_msg_type_name_t replytype,
string_t name,
int *maj,
int *min,
int *edit)
{
if (!obj)
return EOPNOTSUPP;
if (!&trivfs_server_name || !&trivfs_server_version)
return EOPNOTSUPP;
snprintf (name, sizeof (string_t), "%s %s",
trivfs_server_name, trivfs_server_version);
return 0;
}