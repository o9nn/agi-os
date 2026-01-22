#include <errno.h>
#include "idvec.h"
#include "ugids.h"
error_t
ugids_imply_all (struct ugids *ugids)
{
error_t err;
err = idvec_merge_implied_gids (&ugids->imp_eff_gids, &ugids->eff_uids);
if (! err)
err =
idvec_merge_implied_gids (&ugids->imp_avail_gids, &ugids->avail_uids);
return err;
}