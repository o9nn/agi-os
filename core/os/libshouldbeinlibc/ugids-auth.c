#include <hurd.h>
#include "idvec.h"
#include "ugids.h"
error_t
ugids_make_auth (const struct ugids *ugids,
const auth_t *from, size_t num_from,
auth_t *auth)
{
auth_t cur_auth = getauth ();
error_t err =
auth_makeauth (cur_auth, (auth_t *)from, MACH_MSG_TYPE_COPY_SEND, num_from,
ugids->eff_uids.ids, ugids->eff_uids.num,
ugids->avail_uids.ids, ugids->avail_uids.num,
ugids->eff_gids.ids, ugids->eff_gids.num,
ugids->avail_gids.ids, ugids->avail_gids.num,
auth);
mach_port_deallocate (mach_task_self (), cur_auth);
return err;
}
error_t
ugids_merge_auth (struct ugids *ugids, auth_t auth)
{
return
idvec_merge_auth (&ugids->eff_uids, &ugids->avail_uids,
&ugids->eff_gids, &ugids->avail_gids,
auth);
}