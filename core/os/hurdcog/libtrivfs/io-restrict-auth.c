#include "priv.h"
#include "trivfs_io_S.h"
#include <string.h>
static inline int
listmember (const uid_t *list, uid_t query, mach_msg_type_number_t n)
{
mach_msg_type_number_t i;
for (i = 0; i < n; i++)
if (list[i] == query)
return 1;
return 0;
}
kern_return_t
trivfs_S_io_restrict_auth (struct trivfs_protid *cred,
mach_port_t reply,
mach_msg_type_name_t replytype,
mach_port_t *newport,
mach_msg_type_name_t *newporttype,
const uid_t *uids,
mach_msg_type_number_t nuids,
const uid_t *gids,
mach_msg_type_number_t ngids)
{
unsigned int i;
error_t err;
struct trivfs_protid *newcred;
struct idvec *uvec, *gvec;
struct iouser *user;
if (!cred)
return EOPNOTSUPP;
if (cred->isroot)
{
err = iohelp_create_complex_iouser (&user, uids, nuids, gids, ngids);
if (err)
return err;
}
else
{
uvec = make_idvec ();
if (! uvec)
return ENOMEM;
gvec = make_idvec ();
if (! gvec)
{
idvec_free (uvec);
return ENOMEM;
}
for (i = 0; i < cred->user->uids->num; i++)
if (listmember (uids, cred->user->uids->ids[i], nuids))
{
err = idvec_add (uvec, cred->user->uids->ids[i]);
if (err)
goto out;
}
for (i = 0; i < cred->user->gids->num; i++)
if (listmember (gids, cred->user->gids->ids[i], ngids))
{
err = idvec_add (gvec, cred->user->gids->ids[i]);
if (err)
goto out;
}
err = iohelp_create_iouser (&user, uvec, gvec);
if (err)
{
out:
idvec_free (uvec);
idvec_free (gvec);
return err;
}
}
err = ports_create_port (cred->po->cntl->protid_class,
cred->po->cntl->protid_bucket,
sizeof (struct trivfs_protid),
&newcred);
if (err)
{
iohelp_free_iouser (user);
return err;
}
newcred->po = cred->po;
refcount_ref (&newcred->po->refcnt);
newcred->isroot = cred->isroot && _is_privileged (user->uids);
newcred->user = user;
newcred->hook = cred->hook;
err = io_restrict_auth (cred->realnode, &newcred->realnode,
user->uids->ids, user->uids->num,
user->gids->ids, user->gids->num);
if (!err && trivfs_protid_create_hook)
{
err = (*trivfs_protid_create_hook) (newcred);
if (err)
mach_port_deallocate (mach_task_self (), newcred->realnode);
}
if (err)
newcred->realnode = MACH_PORT_NULL;
else
{
*newport = ports_get_right (newcred);
*newporttype = MACH_MSG_TYPE_MAKE_SEND;
}
ports_port_deref (newcred);
return 0;
}