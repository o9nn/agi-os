#include "iohelp.h"
static inline int
listmember (const uid_t *list, uid_t query, int n)
{
int i;
for (i = 0; i < n; i++)
if (list[i] == query)
return 1;
return 0;
}
error_t
iohelp_restrict_iouser (struct iouser **new_user,
const struct iouser *old_user,
const uid_t *uids, int nuids,
const gid_t *gids, int ngids)
{
if (idvec_contains (old_user->uids, 0))
return iohelp_create_complex_iouser (new_user, uids, nuids, gids, ngids);
else
{
struct idvec *uvec, *gvec;
unsigned int i;
error_t err;
uvec = make_idvec ();
if (! uvec)
return ENOMEM;
gvec = make_idvec ();
if (! gvec)
{
idvec_free (uvec);
return ENOMEM;
}
for (i = 0; i < old_user->uids->num; i++)
if (listmember (uids, old_user->uids->ids[i], nuids))
{
err = idvec_add (uvec, old_user->uids->ids[i]);
if (err)
goto out;
}
for (i = 0; i < old_user->gids->num; i++)
if (listmember (gids, old_user->gids->ids[i], ngids))
{
err = idvec_add (gvec, old_user->gids->ids[i]);
if (err)
goto out;
}
err = iohelp_create_iouser (new_user, uvec, gvec);
if (err)
{
out:
idvec_free (uvec);
idvec_free (gvec);
}
return err;
}
}