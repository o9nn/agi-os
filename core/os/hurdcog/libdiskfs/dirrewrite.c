#include "priv.h"
error_t diskfs_dirrewrite (struct node *dp,
struct node *oldnp,
struct node *np,
const char *name,
struct dirstat *ds)
{
error_t err;
diskfs_purge_lookup_cache (dp, oldnp);
err = diskfs_dirrewrite_hard (dp, np, ds);
if (err)
return err;
if (dp->dirmod_reqs)
diskfs_notice_dirchange (dp, DIR_CHANGED_RENUMBER, name);
diskfs_enter_lookup_cache (dp, np, name);
return 0;
}