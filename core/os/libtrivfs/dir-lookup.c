#include "priv.h"
#include <assert-backtrace.h>
#include <fcntl.h>
#include <string.h>
kern_return_t
trivfs_S_dir_lookup (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
const_string_t filename,
int flags,
mode_t mode,
retry_type *retry_type,
char *retry_name,
mach_port_t *retrypt,
mach_msg_type_name_t *retrypt_type)
{
int perms;
error_t err;
struct trivfs_protid *newcred;
if (!cred)
return EOPNOTSUPP;
if (filename[0])
return ENOTDIR;
flags &= O_HURD;
flags &= ~(O_CREAT|O_EXCL|O_NOLINK|O_NOTRANS);
if (! trivfs_check_access_hook)
file_check_access (cred->realnode, &perms);
else
(*trivfs_check_access_hook) (cred->po->cntl, cred->user,
cred->realnode, &perms);
if ((flags & (O_READ|O_WRITE|O_EXEC) & perms)
!= (flags & (O_READ|O_WRITE|O_EXEC)))
return EACCES;
err = 0;
if (trivfs_check_open_hook)
err = (*trivfs_check_open_hook) (cred->po->cntl, cred->user, flags);
if (!err)
{
struct iouser *user;
err = iohelp_dup_iouser (&user, cred->user);
if (err)
return err;
err = trivfs_open (cred->po->cntl, user, flags,
cred->realnode, &newcred);
if (err)
iohelp_free_iouser (user);
else
mach_port_mod_refs (mach_task_self (), cred->realnode,
MACH_PORT_RIGHT_SEND, +1);
}
if (err)
return err;
*retry_type = FS_RETRY_NORMAL;
*retry_name = '\0';
*retrypt = ports_get_right (newcred);
*retrypt_type = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (newcred);
return 0;
}