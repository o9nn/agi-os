#include "priv.h"
#include "fsys_reply_U.h"
#include "trivfs_fsys_S.h"
#include <assert-backtrace.h>
#include <fcntl.h>
#include <string.h>
kern_return_t
trivfs_S_fsys_getroot (struct trivfs_control *cntl,
mach_port_t reply_port,
mach_msg_type_name_t reply_port_type,
mach_port_t dotdot,
const id_t *uids,
mach_msg_type_number_t nuids,
const id_t *gids,
mach_msg_type_number_t ngids,
int flags,
retry_type *do_retry,
string_t retry_name,
mach_port_t *newpt,
mach_msg_type_name_t *newpttype)
{
int perms;
error_t err = 0;
mach_port_t new_realnode;
struct trivfs_protid *cred;
struct iouser *user;
if (!cntl)
return EOPNOTSUPP;
if (trivfs_getroot_hook)
{
err = (*trivfs_getroot_hook) (cntl, reply_port, reply_port_type, dotdot,
uids, nuids, gids, ngids, flags,
do_retry, retry_name, newpt, newpttype);
if (err != EAGAIN)
return err;
}
if ((flags & O_WRITE & trivfs_allow_open) != (flags & O_WRITE))
return EROFS;
if ((flags & (O_READ|O_WRITE|O_EXEC) & trivfs_allow_open)
!= (flags & (O_READ|O_WRITE|O_EXEC)))
return EACCES;
flags &= O_HURD;
flags &= ~(O_CREAT|O_EXCL|O_NOLINK|O_NOTRANS);
struct idvec idvec = {
.ids = (id_t*) uids,
.num = nuids,
.alloced = nuids,
};
if (_is_privileged (&idvec))
err = io_duplicate (cntl->underlying, &new_realnode);
else
err = io_restrict_auth (cntl->underlying,
&new_realnode, uids, nuids, gids, ngids);
if (err)
return err;
err = iohelp_create_complex_iouser (&user, uids, nuids, gids, ngids);
if (err)
return err;
if (! trivfs_check_access_hook)
file_check_access (new_realnode, &perms);
else
(*trivfs_check_access_hook) (cntl, user, new_realnode, &perms);
if ((flags & (O_READ|O_WRITE|O_EXEC) & perms)
!= (flags & (O_READ|O_WRITE|O_EXEC)))
err = EACCES;
if (!err && trivfs_check_open_hook)
err = (*trivfs_check_open_hook) (cntl, user, flags);
if (!err)
{
if (! trivfs_open_hook)
{
err = trivfs_open (cntl, user, flags, new_realnode, &cred);
if (!err)
mach_port_deallocate (mach_task_self (), dotdot);
}
else
err = (*trivfs_open_hook) (cntl, user, dotdot, flags, new_realnode,
&cred);
}
if (err)
{
mach_port_deallocate (mach_task_self (), new_realnode);
iohelp_free_iouser (user);
}
else
{
*do_retry = FS_RETRY_NORMAL;
*retry_name = '\0';
*newpt = ports_get_right (cred);
*newpttype = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (cred);
}
return err;
}