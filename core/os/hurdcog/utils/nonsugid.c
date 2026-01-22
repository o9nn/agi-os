#include <errno.h>
#include <idvec.h>
#include <hurd.h>
error_t
get_nonsugid_ids (struct idvec *uids, struct idvec *gids)
{
if (uids->num == 0 && gids->num == 0)
{
error_t err = 0;
auth_t auth;
struct idvec *p_eff_uids = make_idvec ();
struct idvec *p_eff_gids = make_idvec ();
if (!p_eff_uids || !p_eff_gids)
err = ENOMEM;
auth = getauth ();
if (! err)
err = idvec_merge_auth (p_eff_uids, uids, p_eff_gids, gids, auth);
if (! err)
{
idvec_delete (p_eff_uids, 0);
idvec_delete (p_eff_gids, 0);
idvec_delete (uids, 1);
idvec_delete (gids, 1);
if (! err)
err = idvec_merge (uids, p_eff_uids);
if (! err)
err = idvec_merge (gids, p_eff_gids);
}
mach_port_deallocate (mach_task_self (), auth);
if (p_eff_uids)
idvec_free (p_eff_uids);
if (p_eff_gids)
idvec_free (p_eff_gids);
return err;
}
else
return 0;
}