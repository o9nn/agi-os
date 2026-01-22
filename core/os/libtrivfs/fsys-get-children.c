#include "priv.h"
#include "trivfs_fsys_S.h"
kern_return_t
trivfs_S_fsys_get_children (struct trivfs_control *fsys,
mach_port_t reply,
mach_msg_type_name_t replyPoly,
char **names,
mach_msg_type_number_t *names_len,
mach_port_t **controls,
mach_msg_type_name_t *controlsPoly,
mach_msg_type_number_t *controlsCnt)
{
return EOPNOTSUPP;
}