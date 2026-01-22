#include "priv.h"
static error_t
checkpath(struct node *source,
struct node *target,
struct protid *cred)
{
error_t err;
struct node *np, *newnp;
for (newnp = target, err = 0;
;
err = diskfs_lookup (np, "..", LOOKUP | SPEC_DOTDOT, &newnp, 0, cred))
{
if (err)
{
diskfs_nput (np);
return err;
}
np = newnp;
if (np == source)
{
diskfs_nput (np);
return EINVAL;
}
if (np == diskfs_root_node || np == cred->po->shadow_root)
{
diskfs_nput (np);
return 0;
}
}
}
error_t
diskfs_rename_dir (struct node *fdp, struct node *fnp, const char *fromname,
struct node *tdp, const char *toname,
struct protid *fromcred, struct protid *tocred, int excl)
{
error_t err;
struct node *tnp, *tmpnp;
void *buf = alloca (diskfs_dirstat_size);
struct dirstat *ds;
struct dirstat *tmpds;
pthread_mutex_lock (&tdp->lock);
diskfs_nref (tdp);
err = checkpath (fnp, tdp, tocred);
if (err)
return err;
pthread_mutex_lock (&fdp->lock);
if (fdp != tdp)
pthread_mutex_lock (&tdp->lock);
ds = buf;
err = diskfs_lookup (tdp, toname, RENAME, &tnp, ds, tocred);
assert_backtrace (err != EAGAIN);
if (err && err != ENOENT)
goto out;
if (tnp && excl)
{
err = EEXIST;
goto out;
}
if (tnp == fnp)
{
diskfs_drop_dirstat (tdp, ds);
diskfs_nput (tnp);
pthread_mutex_unlock (&tdp->lock);
if (fdp != tdp)
pthread_mutex_unlock (&fdp->lock);
return 0;
}
tmpds = alloca (diskfs_dirstat_size);
err = diskfs_lookup (fdp, fromname, REMOVE, &tmpnp, tmpds, fromcred);
assert_backtrace (!tmpnp || tmpnp == fnp);
if (tmpnp)
diskfs_nrele (tmpnp);
diskfs_drop_dirstat (fdp, tmpds);
if (err)
goto out;
if (tnp)
{
if (! S_ISDIR(tnp->dn_stat.st_mode))
err = ENOTDIR;
else if (!diskfs_dirempty (tnp, tocred))
err = ENOTEMPTY;
}
if (err && err != ENOENT)
goto out;
if (fdp != tdp)
{
if (tdp->dn_stat.st_nlink == diskfs_link_max - 1)
{
err = EMLINK;
goto out;
}
tdp->dn_stat.st_nlink++;
tdp->dn_set_ctime = 1;
if (diskfs_synchronous)
diskfs_node_update (tdp, 1);
tmpds = alloca (diskfs_dirstat_size);
err = diskfs_lookup (fnp, "..", RENAME | SPEC_DOTDOT,
&tmpnp, tmpds, fromcred);
assert_backtrace (err != ENOENT);
if (err)
{
diskfs_drop_dirstat (fnp, tmpds);
goto out;
}
assert_backtrace (tmpnp == fdp);
err = diskfs_dirrewrite (fnp, fdp, tdp, "..", tmpds);
if (diskfs_synchronous)
diskfs_file_update (fnp, 1);
if (err)
goto out;
fdp->dn_stat.st_nlink--;
fdp->dn_set_ctime = 1;
if (diskfs_synchronous)
diskfs_node_update (fdp, 1);
}
if (fnp->dn_stat.st_nlink == diskfs_link_max - 1)
{
pthread_mutex_unlock (&fnp->lock);
diskfs_drop_dirstat (tdp, ds);
pthread_mutex_unlock (&tdp->lock);
if (tnp)
diskfs_nput (tnp);
return EMLINK;
}
fnp->dn_stat.st_nlink++;
fnp->dn_set_ctime = 1;
diskfs_node_update (fnp, diskfs_synchronous);
if (tnp)
{
err = diskfs_dirrewrite (tdp, tnp, fnp, toname, ds);
ds = 0;
if (!err)
{
tnp->dn_stat.st_nlink--;
tnp->dn_set_ctime = 1;
}
diskfs_clear_directory (tnp, tdp, tocred);
if (diskfs_synchronous)
diskfs_file_update (tnp, 1);
}
else
{
err = diskfs_direnter (tdp, toname, fnp, ds, tocred);
if (diskfs_synchronous)
diskfs_file_update (tdp, 1);
}
if (err)
goto out;
ds = buf;
pthread_mutex_unlock (&fnp->lock);
err = diskfs_lookup (fdp, fromname, REMOVE, &tmpnp, ds, fromcred);
assert_backtrace (!tmpnp || tmpnp == fnp);
if (tmpnp)
diskfs_nrele (tmpnp);
if (err)
{
assert_backtrace (!tmpnp);
fnp = NULL;
goto out;
}
diskfs_dirremove (fdp, fnp, fromname, ds);
ds = 0;
fnp->dn_stat.st_nlink--;
fnp->dn_set_ctime = 1;
if (diskfs_synchronous)
{
diskfs_file_update (fdp, 1);
diskfs_node_update (fnp, 1);
}
out:
if (tdp)
pthread_mutex_unlock (&tdp->lock);
if (tnp)
diskfs_nput (tnp);
if (fdp && fdp != tdp)
pthread_mutex_unlock (&fdp->lock);
if (fnp)
pthread_mutex_unlock (&fnp->lock);
if (ds)
diskfs_drop_dirstat (tdp, ds);
return err;
}