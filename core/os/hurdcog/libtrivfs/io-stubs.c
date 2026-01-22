#include "priv.h"
#include "trivfs_io_S.h"
kern_return_t __attribute__((weak))
trivfs_S_io_map_cntl (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
mach_port_t *obj,
mach_msg_type_name_t *objtype)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_io_get_conch (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_io_release_conch (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_io_eofnotify (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_io_prenotify (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
vm_offset_t start,
vm_offset_t end)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_io_postnotify (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
vm_offset_t start,
vm_offset_t end)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_io_readsleep (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_io_sigio (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype)
{
return EOPNOTSUPP;
}
kern_return_t __attribute__((weak))
trivfs_S_io_readnotify (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype)
{
return EOPNOTSUPP;
}