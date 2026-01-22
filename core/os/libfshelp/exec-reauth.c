#include <hurd/io.h>
#include <hurd/process.h>
#include <hurd/auth.h>
#include <idvec.h>
#include "fshelp.h"
extern error_t
exec_reauth (auth_t auth, int secure, int must_reauth,
mach_port_t *ports, mach_msg_type_number_t num_ports,
mach_port_t *fds, mach_msg_type_number_t num_fds);
error_t
fshelp_exec_reauth (int suid, uid_t uid, int sgid, gid_t gid,
auth_t auth,
error_t
(*get_file_ids)(struct idvec *uids, struct idvec *gids),
mach_port_t *ports, mach_msg_type_number_t num_ports,
mach_port_t *fds, mach_msg_type_number_t num_fds,
int *secure)
{
error_t err = 0;
int _secure = 0;
if (suid || sgid)
{
int already_root = 0;
auth_t newauth;
struct idvec *eff_uids = make_idvec (), *avail_uids = make_idvec ();
struct idvec *eff_gids = make_idvec (), *avail_gids = make_idvec ();
if (!eff_uids || !avail_uids || !eff_gids || !avail_gids)
goto abandon_suid;
err = idvec_merge_auth (eff_uids, avail_uids, eff_gids, avail_gids,
ports[INIT_PORT_AUTH]);
if (err)
goto abandon_suid;
already_root =
idvec_contains (eff_uids, 0) || idvec_contains (avail_uids, 0);
if (suid)
err = idvec_setid (eff_uids, avail_uids, uid, &_secure);
if (sgid && !err)
err = idvec_setid (eff_gids, avail_gids, gid, &_secure);
if (err)
goto abandon_suid;
err = auth_makeauth (auth, &ports[INIT_PORT_AUTH],
MACH_MSG_TYPE_COPY_SEND, 1,
eff_uids->ids, eff_uids->num,
avail_uids->ids, avail_uids->num,
eff_gids->ids, eff_gids->num,
avail_gids->ids, avail_gids->num,
&newauth);
if (err == EINVAL && get_file_ids)
{
idvec_clear (eff_uids);
idvec_clear (avail_uids);
idvec_clear (eff_gids);
idvec_clear (avail_gids);
err = (*get_file_ids)(eff_uids, eff_gids);
already_root = idvec_contains (eff_uids, 0);
if (suid && !err)
err = idvec_setid (eff_uids, avail_uids, uid, &_secure);
if (sgid && !err)
err = idvec_setid (eff_gids, avail_gids, gid, &_secure);
if (err)
goto abandon_suid;
err = auth_makeauth (auth, 0, MACH_MSG_TYPE_COPY_SEND, 1,
eff_uids->ids, eff_uids->num,
avail_uids->ids, avail_uids->num,
eff_gids->ids, eff_gids->num,
avail_gids->ids, avail_gids->num,
&newauth);
}
if (err)
goto abandon_suid;
if (already_root)
_secure = 0;
exec_reauth (newauth, _secure, 0, ports, num_ports, fds, num_fds);
err = proc_setowner (ports[INIT_PORT_PROC],
eff_uids->num > 0 ? eff_uids->ids[0] : 0,
!eff_uids->num);
if (err == EOPNOTSUPP)
err = 0;
abandon_suid:
if (eff_uids)
idvec_free (eff_uids);
if (avail_uids)
idvec_free (avail_uids);
if (eff_gids)
idvec_free (eff_gids);
if (avail_gids)
idvec_free (avail_gids);
}
if (secure)
*secure = _secure;
return err;
}