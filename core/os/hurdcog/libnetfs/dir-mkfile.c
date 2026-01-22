#include "netfs.h"
#include "misc.h"
#include "fs_S.h"
kern_return_t
netfs_S_dir_mkfile (struct protid *diruser, int flags, mode_t mode,
mach_port_t *newfile, mach_msg_type_name_t *newfiletype)
{
error_t err;
struct node *np;
struct iouser *user;
struct protid *newpi;
pthread_mutex_lock (&diruser->po->np->lock);
err = netfs_attempt_mkfile (diruser->user, diruser->po->np, mode, &np);
if (!err)
{
flags &= ~OPENONLY_STATE_MODES;
err = iohelp_dup_iouser (&user, diruser->user);
if (! err)
{
newpi = netfs_make_protid (netfs_make_peropen (np, flags,
diruser->po),
user);
if (newpi)
{
*newfile = ports_get_right (newpi);
*newfiletype = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (newpi);
}
else
{
err = errno;
iohelp_free_iouser (user);
}
}
netfs_nput (np);
}
return err;
}