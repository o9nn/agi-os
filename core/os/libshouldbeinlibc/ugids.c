#include <stdlib.h>
#include <string.h>
#include "idvec.h"
#include "ugids.h"
struct ugids *
make_ugids (void)
{
struct ugids *u = malloc (sizeof (struct ugids));
if (u)
memset (u, 0, sizeof *u);
return u;
}
error_t
ugids_add_uid (struct ugids *ugids, uid_t uid, int avail)
{
return idvec_add_new (avail ? &ugids->avail_uids : &ugids->eff_uids, uid);
}
error_t
ugids_add_gid (struct ugids *ugids, gid_t gid, int avail)
{
error_t err =
idvec_add_new (avail ? &ugids->avail_gids : &ugids->eff_gids, gid);
if (! err)
idvec_remove (avail ? &ugids->imp_avail_gids : &ugids->imp_eff_gids,
0, gid);
return err;
}
error_t
ugids_add_user (struct ugids *ugids, uid_t uid, int avail)
{
error_t err;
struct idvec imp_gids = IDVEC_INIT;
uid_t uids_ids[] = { uid };
struct idvec uids = { uids_ids, 1 };
struct idvec *gids = avail ? &ugids->avail_gids : &ugids->eff_gids;
idvec_merge_implied_gids (&imp_gids, &uids);
idvec_subtract (&imp_gids, gids);
err = idvec_add_new (avail ? &ugids->avail_uids : &ugids->eff_uids, uid);
if (! err)
err =
idvec_merge (avail ? &ugids->avail_gids : &ugids->eff_gids,
&imp_gids);
if (! err)
err = idvec_merge ((avail
? &ugids->imp_avail_gids
: &ugids->imp_eff_gids),
&imp_gids);
idvec_fini (&imp_gids);
return err;
}