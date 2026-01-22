#include "priv.h"
error_t
diskfs_direnter (struct node *dp,
const char *name,
struct node *np,
struct dirstat *ds,
struct protid *cred)
{
error_t err;
err = diskfs_direnter_hard (dp, name, np, ds, cred);
if (err)
return err;
if (dp->dirmod_reqs)
diskfs_notice_dirchange (dp, DIR_CHANGED_NEW, name);
diskfs_enter_lookup_cache (dp, np, name);
return 0;
}