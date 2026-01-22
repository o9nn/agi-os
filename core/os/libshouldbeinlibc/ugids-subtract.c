#include <errno.h>
#include "idvec.h"
#include "ugids.h"
static
error_t _sub_gids (struct idvec *gids, struct idvec *gids_imp,
const struct idvec *sub, const struct idvec *sub_imp)
{
error_t err;
struct idvec delta = IDVEC_INIT;
struct idvec delta_suppress = IDVEC_INIT;
err = idvec_set (&delta, sub);
if (! err)
err = idvec_set (&delta_suppress, sub_imp);
if (! err)
{
idvec_subtract (&delta_suppress, gids_imp);
idvec_subtract (&delta, &delta_suppress);
idvec_subtract (gids, &delta);
}
idvec_fini (&delta);
idvec_fini (&delta_suppress);
return err;
}
static
error_t _sub (struct idvec *uids, struct idvec *gids, struct idvec *gids_imp,
const struct idvec *sub_uids,
const struct idvec *sub_gids, const struct idvec *sub_gids_imp)
{
error_t err;
struct idvec new_uids = IDVEC_INIT;
struct idvec no_sub_gids = IDVEC_INIT;
struct idvec new_sub_gids = IDVEC_INIT;
struct idvec new_sub_gids_imp = IDVEC_INIT;
err = idvec_set (&new_uids, uids);
if (! err)
err = idvec_set (&new_sub_gids, sub_gids);
if (! err)
err = idvec_set (&new_sub_gids_imp, sub_gids_imp);
if (! err)
{
idvec_subtract (&new_uids, sub_uids);
err = idvec_merge_implied_gids (&no_sub_gids, &new_uids);
if (! err)
{
idvec_keep (&no_sub_gids, gids_imp);
idvec_keep (&no_sub_gids, sub_gids_imp);
idvec_subtract (&new_sub_gids, &no_sub_gids);
idvec_subtract (&new_sub_gids_imp, &no_sub_gids);
err = _sub_gids (gids, gids_imp, &new_sub_gids, &new_sub_gids_imp);
if (! err)
err = idvec_set (uids, &new_uids);
}
}
idvec_fini (&new_uids);
idvec_fini (&no_sub_gids);
idvec_fini (&new_sub_gids);
idvec_fini (&new_sub_gids_imp);
return err;
}
error_t
ugids_subtract (struct ugids *ugids, const struct ugids *sub)
{
error_t err =
_sub (&ugids->eff_uids, &ugids->eff_gids, &ugids->imp_eff_gids,
&sub->eff_uids, &sub->eff_gids, &sub->imp_eff_gids);
if (! err)
err = _sub (&ugids->avail_uids, &ugids->avail_gids, &ugids->imp_avail_gids,
&sub->avail_uids, &sub->avail_gids, &sub->imp_avail_gids);
return err;
}