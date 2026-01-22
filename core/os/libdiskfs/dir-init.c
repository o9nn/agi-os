#include "priv.h"
error_t
diskfs_init_dir (struct node *dp, struct node *pdp, struct protid *cred)
{
struct dirstat *ds = alloca (diskfs_dirstat_size);
struct node *foo;
error_t err;
static uid_t zero = 0;
static struct idvec vec = {&zero, 1, 1};
static struct iouser user = {&vec, &vec, 0};
struct protid lookupcred = {{ .refcounts = { .references = {1, 0}}},
&user, cred->po, 0, 0};
if (pdp->dn_stat.st_nlink == diskfs_link_max - 1)
return EMLINK;
dp->dn_stat.st_nlink++;
dp->dn_set_ctime = 1;
err = diskfs_lookup (dp, ".", CREATE, &foo, ds, &lookupcred);
assert_backtrace (err == ENOENT);
err = diskfs_direnter (dp, ".", dp, ds, cred);
if (err)
{
dp->dn_stat.st_nlink--;
dp->dn_set_ctime = 1;
return err;
}
pdp->dn_stat.st_nlink++;
pdp->dn_set_ctime = 1;
err = diskfs_lookup (dp, "..", CREATE, &foo, ds, &lookupcred);
assert_backtrace (err == ENOENT);
err = diskfs_direnter (dp, "..", pdp, ds, cred);
if (err)
{
pdp->dn_stat.st_nlink--;
pdp->dn_set_ctime = 1;
return err;
}
diskfs_node_update (dp, diskfs_synchronous);
return 0;
}