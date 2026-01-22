#include "priv.h"
#include "io_S.h"
kern_return_t
diskfs_S_io_restrict_auth (struct protid *cred,
mach_port_t *newport,
mach_msg_type_name_t *newportpoly,
const uid_t *uids,
mach_msg_type_name_t nuids,
const gid_t *gids,
mach_msg_type_name_t ngids)
{
error_t err;
struct iouser *user;
struct protid *newpi;
if (!cred)
return EOPNOTSUPP;
err = iohelp_restrict_iouser (&user, cred->user, uids, nuids, gids, ngids);
if (err)
return err;
refcount_ref (&cred->po->refcnt);
err = diskfs_create_protid (cred->po, user, &newpi);
if (! err)
{
*newport = ports_get_right (newpi);
*newportpoly = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (newpi);
}
else
refcount_deref (&cred->po->refcnt);
iohelp_free_iouser (user);
return err;
}