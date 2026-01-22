#include "netfs.h"
#include "fs_S.h"
#include <hurd/fsys.h>
kern_return_t
netfs_S_file_syncfs (struct protid *user,
int wait,
int dochildren)
{
error_t err;
if (!user)
return EOPNOTSUPP;
if (dochildren)
{
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
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_syncfs (user->user, wait);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}