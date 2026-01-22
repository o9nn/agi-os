#include "priv.h"
error_t
diskfs_clear_directory (struct node *dp,
struct node *pdp,
struct protid *cred)
{
error_t err;
struct dirstat *ds = alloca (diskfs_dirstat_size);
struct node *np;
err = diskfs_lookup (dp, ".", REMOVE, &np, ds, cred);
assert_backtrace (err != ENOENT);
if (!err)
{
assert_backtrace (np == dp);
err = diskfs_dirremove (dp, np, ".", ds);
diskfs_nrele (np);
}
else
diskfs_drop_dirstat (dp, ds);
if (err)
return err;
dp->dn_stat.st_nlink--;
dp->dn_set_ctime = 1;
err = diskfs_lookup (dp, "..", REMOVE | SPEC_DOTDOT, &np, ds, cred);
assert_backtrace (err != ENOENT);
if (!err)
{
assert_backtrace (np == pdp);
err = diskfs_dirremove (dp, np, "..", ds);
}
else
diskfs_drop_dirstat (dp, ds);
if (err)
return err;
pdp->dn_stat.st_nlink--;
pdp->dn_set_ctime = 1;
diskfs_truncate (dp, 0);
return err;
}