#include "priv.h"
#include "io_S.h"
#include <fcntl.h>
kern_return_t
diskfs_S_io_select (struct protid *cred,
int *type)
{
if (!cred)
return EOPNOTSUPP;
*type &= ~SELECT_URG;
return 0;
}
kern_return_t
diskfs_S_io_select_timeout (struct protid *cred,
struct timespec ts,
int *type)
{
return diskfs_S_io_select (cred, type);
}