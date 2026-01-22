#include "netfs.h"
#include "fs_S.h"
#include <fcntl.h>
#include <sys/file.h>
kern_return_t
netfs_S_file_lock (struct protid *user,
int flags)
{
error_t err;
struct flock64 lock;
struct node *node;
int openstat;
mach_port_t rendezvous = MACH_PORT_NULL;
if (!user)
return EOPNOTSUPP;
lock.l_whence = SEEK_SET;
lock.l_start = 0;
lock.l_len = 0;
if (flags & LOCK_UN)
lock.l_type = F_UNLCK;
else if (flags & LOCK_SH)
lock.l_type = F_RDLCK;
else if (flags & LOCK_EX)
lock.l_type = F_WRLCK;
else
return EINVAL;
openstat = user->po->openstat;
if (openstat & (O_RDONLY|O_WRONLY|O_EXEC)) openstat |= O_RDONLY|O_WRONLY;
node = user->po->np;
pthread_mutex_lock (&node->lock);
err = fshelp_rlock_tweak (&node->userlock, &node->lock,
&user->po->lock_status, openstat,
0, 0, flags & LOCK_NB ? F_SETLK64 : F_SETLKW64,
&lock, rendezvous);
pthread_mutex_unlock (&node->lock);
return err;
}