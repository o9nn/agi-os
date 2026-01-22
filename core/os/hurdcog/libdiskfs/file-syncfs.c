#include "priv.h"
#include "fs_S.h"
#include <hurd/fsys.h>
kern_return_t
diskfs_S_file_syncfs (struct protid *cred,
int wait,
int dochildren)
{
if (!cred)
return EOPNOTSUPP;
if (dochildren)
{
error_t err;
char *n = NULL;
size_t n_len = 0;
mach_port_t *c;
size_t c_count, i;
err = fshelp_get_active_translators (&n, &n_len, &c, &c_count);
if (err)
return err;
free(n);
for (i = 0; i < c_count; i++)
fsys_syncfs (c[i], wait, 1);
free(c);
if (err)
return err;
}
if (diskfs_synchronous)
wait = 1;
if (! diskfs_readonly)
{
diskfs_sync_everything (wait);
diskfs_set_hypermetadata (wait, 0);
}
return 0;
}