#include <errno.h>
#include "idvec.h"
#include "ugids.h"
static error_t
_merge_gids (struct idvec *gids, struct idvec *gids_imp,
const struct idvec *new, const struct idvec *new_imp)
{
error_t err;
struct idvec gids_strong = IDVEC_INIT;
struct idvec new_strong = IDVEC_INIT;
err = idvec_set (&gids_strong, gids);
if (! err)
err = idvec_set (&new_strong, new);
if (! err)
{
idvec_subtract (&gids_strong, gids_imp);
idvec_subtract (&new_strong, new_imp);
err = idvec_merge (gids, new);
if (! err)
{
err = idvec_merge (gids_imp, new_imp);
if (! err)
{
idvec_subtract (gids_imp, &gids_strong);
idvec_subtract (gids_imp, &new_strong);
}
}
}
idvec_fini (&gids_strong);
idvec_fini (&new_strong);
return err;
}
error_t
ugids_merge (struct ugids *ugids, const struct ugids *new)
{
error_t err;
err = idvec_merge (&ugids->eff_uids, &new->eff_uids);
if (! err)
err = idvec_merge (&ugids->avail_uids, &new->avail_uids);
if (! err)
err = _merge_gids (&ugids->eff_gids, &ugids->imp_eff_gids,
&new->eff_gids, &new->imp_eff_gids);
if (! err)
err = _merge_gids (&ugids->avail_gids, &ugids->imp_avail_gids,
&new->avail_gids, &new->imp_avail_gids);
return err;
}
error_t
ugids_set (struct ugids *ugids, const struct ugids *new)
{
idvec_clear (&ugids->eff_uids);
idvec_clear (&ugids->eff_gids);
idvec_clear (&ugids->avail_uids);
idvec_clear (&ugids->avail_gids);
idvec_clear (&ugids->imp_eff_gids);
idvec_clear (&ugids->imp_avail_gids);
return ugids_merge (ugids, new);
}
error_t
ugids_save (struct ugids *ugids)
{
error_t err = idvec_merge (&ugids->avail_uids, &ugids->eff_uids);
if (! err)
err = _merge_gids (&ugids->avail_gids, &ugids->imp_avail_gids,
&ugids->eff_gids, &ugids->imp_eff_gids);
if (! err)
{
idvec_clear (&ugids->eff_uids);
idvec_clear (&ugids->eff_gids);
idvec_clear (&ugids->imp_eff_gids);
}
return err;
}