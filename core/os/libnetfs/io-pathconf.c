#include <unistd.h>
#include "netfs.h"
#include "io_S.h"
kern_return_t
netfs_S_io_pathconf (struct protid *user,
int name,
int *value)
{
if (!user)
return EOPNOTSUPP;
switch (name)
{
case _PC_LINK_MAX:
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
*value = 1024;
break;
case _PC_CHOWN_RESTRICTED:
case _PC_NO_TRUNC:
case _PC_2_SYMLINKS:
*value = 1;
break;
case _PC_PRIO_IO:
case _PC_SYNC_IO:
case _PC_ASYNC_IO:
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