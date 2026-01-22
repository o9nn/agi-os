#include "priv.h"
#include <fcntl.h>
#include <hurd/fshelp.h>
static error_t
_netfs_translator_callback1_fn (void *cookie1, void *cookie2,
uid_t *uid, gid_t *gid,
char **argz, mach_msg_type_number_t *argz_len)
{
error_t err;
struct node *np = cookie1;
if (! (np->nn_stat.st_mode & S_IPTRANS))
return ENOENT;
err = netfs_get_translator (np, argz, argz_len);
if (err)
{
assert_backtrace (err != EOPNOTSUPP);
return err;
}
*uid = np->nn_stat.st_uid;
*gid = np->nn_stat.st_gid;
return 0;
}
static error_t
_netfs_translator_callback2_fn (void *cookie1, void *cookie2, int flags,
mach_port_t *underlying,
mach_msg_type_name_t *underlying_type)
{
error_t err;
struct protid *cred;
struct node *node = cookie1;
struct fshelp_stat_cookie2 *statc = cookie2;
struct iouser *user;
struct peropen *po;
err = iohelp_create_simple_iouser (&user, node->nn_stat.st_uid,
node->nn_stat.st_gid);
if (err)
return err;
po = netfs_make_peropen (node, flags, statc->next);
if (! po)
{
err = errno;
iohelp_free_iouser (user);
return err;
}
cred = netfs_make_protid (po, user);
if (cred)
{
*underlying = ports_get_right (cred);
*underlying_type = MACH_MSG_TYPE_MAKE_SEND;
ports_port_deref (cred);
return 0;
}
else
{
err = errno;
netfs_release_peropen (po);
iohelp_free_iouser (user);
return err;
}
}
fshelp_fetch_root_callback1_t _netfs_translator_callback1 =
_netfs_translator_callback1_fn;
fshelp_fetch_root_callback2_t _netfs_translator_callback2 =
_netfs_translator_callback2_fn;