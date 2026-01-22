#include "netfs.h"
#include "io_S.h"
kern_return_t
netfs_S_io_restrict_auth (struct protid *user,
mach_port_t *newport,
mach_msg_type_name_t *newporttype,
const uid_t *uids,
mach_msg_type_number_t nuids,
const gid_t *gids,
mach_msg_type_number_t ngids)
{
error_t err;
struct protid *newpi;
struct iouser *new_user;
if (!user)
return EOPNOTSUPP;
err = iohelp_restrict_iouser (&new_user, user->user,
uids, nuids, gids, ngids);
if (err)
return err;
refcount_ref (&user->po->refcnt);
newpi = netfs_make_protid (user->po, new_user);
if (newpi)
{
*newport = ports_get_right (newpi);
*newporttype = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (newpi);
}
else
{
refcount_deref (&user->po->refcnt);
iohelp_free_iouser (new_user);
err = ENOMEM;
}
return err;
}