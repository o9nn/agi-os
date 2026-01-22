#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_revoke (struct protid *cred)
{
error_t err;
struct node *np;
error_t
iterator_function (void *port)
{
struct protid *user = port;
if ((user != cred)
&& (user->po->np == np))
ports_destroy_right (user);
return 0;
}
if (!cred)
return EOPNOTSUPP;
np = cred->po->np;
pthread_mutex_lock (&np->lock);
err = fshelp_isowner (&np->dn_stat, cred->user);
pthread_mutex_unlock (&np->lock);
if (err)
return err;
ports_inhibit_bucket_rpcs (diskfs_port_bucket);
ports_class_iterate (diskfs_protid_class, iterator_function);
ports_resume_bucket_rpcs (diskfs_port_bucket);
return 0;
}