#include "priv.h"
#include "fs_S.h"
#include <fcntl.h>
kern_return_t
diskfs_S_dir_mkfile (struct protid *cred,
int flags,
mode_t mode,
mach_port_t *newnode,
mach_msg_type_name_t *newnodetype)
{
struct node *dnp, *np;
error_t err;
struct protid *newpi;
struct peropen *newpo;
if (!cred)
return EOPNOTSUPP;
if (diskfs_check_readonly ())
return EROFS;
dnp = cred->po->np;
pthread_mutex_lock (&dnp->lock);
if (!S_ISDIR (dnp->dn_stat.st_mode))
{
pthread_mutex_unlock (&dnp->lock);
return ENOTDIR;
}
err = fshelp_access (&dnp->dn_stat, S_IWRITE, cred->user);
if (err)
{
pthread_mutex_unlock (&dnp->lock);
return err;
}
mode &= ~(S_IFMT | S_ISPARE | S_ISVTX | S_ITRANS);
mode |= S_IFREG;
err = diskfs_create_node (dnp, 0, mode, &np, cred, 0);
pthread_mutex_unlock (&dnp->lock);
if (diskfs_synchronous)
{
diskfs_file_update (dnp, 1);
diskfs_file_update (np, 1);
}
if (err)
return err;
flags &= ~OPENONLY_STATE_MODES;
err = diskfs_make_peropen (np, flags, cred->po, &newpo);
if (! err)
{
err = diskfs_create_protid (newpo, cred->user, &newpi);
if (err)
diskfs_release_peropen (newpo);
}
if (! err)
{
*newnode = ports_get_right (newpi);
*newnodetype = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (newpi);
}
diskfs_nput (np);
return err;
}