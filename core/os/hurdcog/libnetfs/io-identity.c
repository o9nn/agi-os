#include "netfs.h"
#include "io_S.h"
kern_return_t
netfs_S_io_identity (struct protid *cred,
mach_port_t *id,
mach_msg_type_name_t *idtype,
mach_port_t *fsys,
mach_msg_type_name_t *fsystype,
ino_t *fileno)
{
struct node *np;
error_t err;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
err = netfs_validate_stat (np, cred->user);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
err = fshelp_get_identity (netfs_port_bucket, np->nn_stat.st_ino, id);
if (err)
{
pthread_mutex_unlock (&np->lock);
return err;
}
*idtype = MACH_MSG_TYPE_MAKE_SEND;
*fsys = netfs_fsys_identity;
*fsystype = MACH_MSG_TYPE_MAKE_SEND;
*fileno = np->nn_stat.st_ino;
pthread_mutex_unlock (&np->lock);
return 0;
}