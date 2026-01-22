#include <unistd.h>
#include "priv.h"
#include "io_S.h"
#include <dirent.h>
#include <limits.h>
kern_return_t
diskfs_S_io_pathconf (struct protid *cred,
int name,
int *value)
{
if (!cred)
return EOPNOTSUPP;
switch (name)
{
case _PC_LINK_MAX:
*value = diskfs_link_max;
break;
case _PC_MAX_CANON:
case _PC_MAX_INPUT:
case _PC_PIPE_BUF:
case _PC_VDISABLE:
case _PC_SOCK_MAXBUF:
case _PC_PATH_MAX:
case _PC_REC_MAX_XFER_SIZE:
case _PC_REC_INCR_XFER_SIZE:
case _PC_SYMLINK_MAX:
*value = -1;
break;
case _PC_NAME_MAX:
#define D_NAMLEN_MAX (UCHAR_MAX * sizeof (((struct dirent *) 0)->d_namlen))
if (diskfs_name_max > D_NAMLEN_MAX || diskfs_name_max < 0)
diskfs_name_max = D_NAMLEN_MAX;
*value = diskfs_name_max;
break;
case _PC_NO_TRUNC:
case _PC_2_SYMLINKS:
*value = 1;
break;
case _PC_CHOWN_RESTRICTED:
case _PC_SYNC_IO:
case _PC_ASYNC_IO:
*value = 1;
break;
case _PC_PRIO_IO:
*value = 0;
break;
case _PC_FILESIZEBITS:
*value = 32;
break;
case _PC_REC_MIN_XFER_SIZE:
case _PC_REC_XFER_ALIGN:
case _PC_ALLOC_SIZE_MIN:
*value = vm_page_size;
break;
default:
return EINVAL;
}
return 0;
}