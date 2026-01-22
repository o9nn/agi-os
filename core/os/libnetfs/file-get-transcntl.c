#include "priv.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_get_translator_cntl (struct protid *cred,
mach_port_t *ctl,
mach_msg_type_name_t *ctltype)
{
struct node *np;
error_t err;
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
err = fshelp_isowner (&np->nn_stat, cred->user);
if (!err)
err = fshelp_fetch_control (&np->transbox, ctl);
if (!err && *ctl == MACH_PORT_NULL)
err = ENXIO;
if (!err)
*ctltype = MACH_MSG_TYPE_MOVE_SEND;
pthread_mutex_unlock (&np->lock);
return err;
}