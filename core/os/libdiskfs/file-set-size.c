#include "priv.h"
#include "fs_S.h"
#include <fcntl.h>
kern_return_t
diskfs_S_file_set_size (struct protid *cred,
off_t size)
{
CHANGE_NODE_FIELD (cred,
({
if (!(cred->po->openstat & O_WRITE) || (size < 0))
err = EINVAL;
else if (size < np->dn_stat.st_size)
{
err = diskfs_truncate (np, size);
if (!err && np->filemod_reqs)
diskfs_notice_filechange (np,
FILE_CHANGED_TRUNCATE,
0, size);
}
else if (size > np->dn_stat.st_size)
{
err = diskfs_grow (np, size, cred);
if (! err)
{
np->dn_stat.st_size = size;
np->dn_set_ctime = np->dn_set_mtime = 1;
if (np->filemod_reqs)
diskfs_notice_filechange (np,
FILE_CHANGED_EXTEND,
0, size);
}
}
else
err = 0;
}));
}