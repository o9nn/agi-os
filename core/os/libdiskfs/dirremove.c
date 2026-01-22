#include "priv.h"
error_t
diskfs_dirremove (struct node *dp,
struct node *np,
const char *name,
struct dirstat *ds)
{
error_t err;
diskfs_purge_lookup_cache (dp, np);
err = diskfs_dirremove_hard (dp, ds);
if (!err && dp->dirmod_reqs)
diskfs_notice_dirchange (dp, DIR_CHANGED_UNLINK, name);
return err;
}