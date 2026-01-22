#include <fcntl.h>
#include "hostmux.h"
void
netfs_node_norefs (struct node *node)
{
if (node->nn->name)
node->nn->name->node = 0;
free (node->nn);
free (node);
}
error_t
netfs_attempt_create_file (struct iouser *user, struct node *dir,
const char *name, mode_t mode, struct node **node)
{
*node = 0;
pthread_mutex_unlock (&dir->lock);
return EOPNOTSUPP;
}
error_t
netfs_check_open_permissions (struct iouser *user, struct node *node,
int flags, int newnode)
{
error_t err = 0;
if (flags & O_READ)
err = fshelp_access (&node->nn_stat, S_IREAD, user);
if (!err && (flags & O_WRITE))
err = fshelp_access (&node->nn_stat, S_IWRITE, user);
if (!err && (flags & O_EXEC))
err = fshelp_access (&node->nn_stat, S_IEXEC, user);
return err;
}
error_t
netfs_attempt_utimes (struct iouser *cred, struct node *node,
struct timespec *atime, struct timespec *mtime)
{
error_t err = fshelp_isowner (&node->nn_stat, cred);
int flags = TOUCH_CTIME;
if (! err)
{
if (atime)
node->nn_stat.st_atim = *atime;
if (mtime)
node->nn_stat.st_mtim = *mtime;
fshelp_touch (&node->nn_stat, flags, hostmux_maptime);
}
return err;
}
error_t
netfs_report_access (struct iouser *cred, struct node *node, int *types)
{
*types = 0;
if (fshelp_access (&node->nn_stat, S_IREAD, cred) == 0)
*types |= O_READ;
if (fshelp_access (&node->nn_stat, S_IWRITE, cred) == 0)
*types |= O_WRITE;
if (fshelp_access (&node->nn_stat, S_IEXEC, cred) == 0)
*types |= O_EXEC;
return 0;
}
error_t
netfs_validate_stat (struct node *node, struct iouser *cred)
{
return 0;
}
error_t
netfs_attempt_sync (struct iouser *cred, struct node *node, int wait)
{
return 0;
}