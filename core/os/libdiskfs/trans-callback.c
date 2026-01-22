#include "priv.h"
#include <fcntl.h>
#include <hurd/fshelp.h>
static error_t
_diskfs_translator_callback1_fn (void *cookie1, void *cookie2,
uid_t *uid, gid_t *gid,
char **argz, mach_msg_type_number_t *argz_len)
{
error_t err;
struct node *np = cookie1;
if (! (np->dn_stat.st_mode & S_IPTRANS))
return ENOENT;
err = diskfs_get_translator (np, argz, argz_len);
if (err)
{
assert_backtrace (err != EOPNOTSUPP);
return err;
}
*uid = np->dn_stat.st_uid;
*gid = np->dn_stat.st_gid;
return 0;
}
static error_t
_diskfs_translator_callback2_fn (void *cookie1, void *cookie2,
int flags,
mach_port_t *underlying,
mach_msg_type_name_t *underlying_type)
{
struct node *np = cookie1;
struct fshelp_stat_cookie2 *statc = cookie2;
struct protid *cred;
struct peropen *po;
error_t err;
struct iouser *user;
err = iohelp_create_simple_iouser (&user, np->dn_stat.st_uid,
np->dn_stat.st_gid);
if (err)
return err;
err = diskfs_make_peropen (np, flags, statc->next, &po);
if (! err)
{
err = diskfs_create_protid (po, user, &cred);
if (err)
diskfs_release_peropen (po);
}
iohelp_free_iouser (user);
if (! err)
{
*underlying = ports_get_right (cred);
*underlying_type = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (cred);
}
return err;
}
fshelp_fetch_root_callback1_t _diskfs_translator_callback1 =
_diskfs_translator_callback1_fn;
fshelp_fetch_root_callback2_t _diskfs_translator_callback2 =
_diskfs_translator_callback2_fn;