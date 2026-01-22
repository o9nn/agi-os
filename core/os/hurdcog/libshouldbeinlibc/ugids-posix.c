#include <stdlib.h>
#include "ugids.h"
error_t
ugids_set_posix_user (struct ugids *ugids, uid_t uid)
{
error_t err;
struct idvec imp_gids = IDVEC_INIT;
uid_t uids_ids[] = { uid };
struct idvec uids = { uids_ids, 1 };
error_t update_real (struct idvec *avail_ids, uid_t id)
{
if (avail_ids->num == 0
|| !idvec_tail_contains (avail_ids, 1, avail_ids->ids[0]))
return idvec_insert (avail_ids, 0, id);
else
avail_ids->ids[0] = id;
return 0;
}
idvec_merge_implied_gids (&imp_gids, &uids);
err = idvec_insert_only (&ugids->eff_uids, 0, uid);
if (! err)
err = update_real (&ugids->avail_uids, uid);
if (! err)
err = idvec_insert_only (&ugids->avail_uids, 1, uid);
if (!err && imp_gids.num > 0)
{
gid_t gid = imp_gids.ids[0];
int gid_was_avail = idvec_contains (&ugids->avail_gids, gid);
idvec_subtract (&imp_gids, &ugids->eff_gids);
if (! err)
err = idvec_insert_only (&ugids->eff_gids, 0, gid);
if (! err)
err = update_real (&ugids->avail_gids, gid);
if (! err)
err = idvec_insert_only (&ugids->avail_gids, 1, gid);
if (!err && !gid_was_avail)
err = idvec_add (&ugids->imp_avail_gids, gid);
if (! err)
err = idvec_merge (&ugids->eff_gids, &imp_gids);
if (! err)
err = idvec_merge (&ugids->imp_eff_gids, &imp_gids);
}
idvec_fini (&imp_gids);
return err;
}