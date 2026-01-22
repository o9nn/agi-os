#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_identity (struct protid *cred,
mach_port_t *id,
mach_msg_type_name_t *idtype,
mach_port_t *fsys,
mach_msg_type_name_t *fsystype,
ino_t *fileno)
{
struct node *np;
error_t err;
ino_t inum;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
inum = np->dn_stat.st_ino;
pthread_mutex_unlock (&np->lock);
err = fshelp_get_identity (diskfs_port_bucket, inum, id);
if (! err)
{
if (cred->po->shadow_root && cred->po->shadow_root != diskfs_root_node)
{
err = fshelp_get_identity (diskfs_port_bucket,
cred->po->shadow_root->dn_stat.st_ino,
fsys);
if (err)
mach_port_deallocate (mach_task_self (), *id);
}
else
*fsys = diskfs_fsys_identity;
}
if (! err)
{
*idtype = MACH_MSG_TYPE_MAKE_SEND;
*fsystype = MACH_MSG_TYPE_MAKE_SEND;
*fileno = inum;
}
return err;
}