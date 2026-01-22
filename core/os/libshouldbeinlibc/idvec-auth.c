#include <mach.h>
#include <sys/mman.h>
#include <hurd/auth.h>
#include <errno.h>
#include "idvec.h"
error_t
idvec_merge_auth (struct idvec *eff_uids, struct idvec *avail_uids,
struct idvec *eff_gids, struct idvec *avail_gids,
auth_t auth)
{
error_t err;
uid_t eff_uid_buf[10], avail_uid_buf[20];
uid_t *_eff_uids = eff_uid_buf, *_avail_uids = avail_uid_buf;
mach_msg_type_number_t num_eff_uids = 10, num_avail_uids = 20;
uid_t eff_gid_buf[10], avail_gid_buf[20];
uid_t *_eff_gids = eff_gid_buf, *_avail_gids = avail_gid_buf;
mach_msg_type_number_t num_eff_gids = 10, num_avail_gids = 20;
err = auth_getids (auth,
&_eff_uids, &num_eff_uids, &_avail_uids, &num_avail_uids,
&_eff_gids, &num_eff_gids, &_avail_gids, &num_avail_gids);
if (err)
return err;
if (eff_uids)
err = idvec_grow (eff_uids, num_eff_uids);
if (avail_uids && !err)
err = idvec_grow (avail_uids, num_avail_uids);
if (eff_gids && !err)
err = idvec_grow (eff_gids, num_eff_gids);
if (avail_gids && !err)
err = idvec_grow (avail_gids, num_avail_gids);
if (!err)
{
if (eff_uids)
idvec_merge_ids (eff_uids, _eff_uids, num_eff_uids);
if (avail_uids)
idvec_merge_ids (avail_uids, _avail_uids, num_avail_uids);
if (eff_gids)
idvec_merge_ids (eff_gids, _eff_gids, num_eff_gids);
if (avail_gids)
idvec_merge_ids (avail_gids, _avail_gids, num_avail_gids);
}
if (_eff_uids != eff_uid_buf)
munmap ((caddr_t) _eff_uids, num_eff_uids * sizeof (uid_t));
if (_avail_uids != avail_uid_buf)
munmap ((caddr_t) _avail_uids, num_avail_uids * sizeof (uid_t));
if (_eff_gids != eff_gid_buf)
munmap ((caddr_t) _eff_gids, num_eff_gids * sizeof (gid_t));
if (_avail_gids != avail_gid_buf)
munmap ((caddr_t) _avail_gids, num_avail_gids * sizeof (gid_t));
return err;
}