#include "priv.h"
#include "trivfs_io_S.h"
kern_return_t
trivfs_S_io_identity (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
mach_port_t *idport,
mach_msg_type_name_t *idport_type,
mach_port_t *fsidport,
mach_msg_type_name_t *fsidport_type,
ino_t *fileno)
{
error_t err;
struct stat st;
if (!cred)
return EOPNOTSUPP;
err = io_stat (cred->realnode, &st);
if (err)
return err;
trivfs_modify_stat (cred, &st);
*idport = cred->po->cntl->file_id;
*idport_type = MACH_MSG_TYPE_MAKE_SEND;
*fsidport = cred->po->cntl->filesys_id;
*fsidport_type = MACH_MSG_TYPE_MAKE_SEND;
*fileno = st.st_ino;
return 0;
}