#include <string.h>
#include <sys/statvfs.h>
#include "priv.h"
#include "fs_S.h"
#ifndef ST_RELATIME
#  define ST_RELATIME 64
#endif
kern_return_t
diskfs_S_file_statfs (struct protid *file,
fsys_statfsbuf_t *statbuf)
{
if (!file)
return EOPNOTSUPP;
memset (statbuf, 0, sizeof *statbuf);
if (diskfs_readonly)
statbuf->f_flag |= ST_RDONLY;
if (_diskfs_nosuid)
statbuf->f_flag |= ST_NOSUID;
if (_diskfs_noexec)
statbuf->f_flag |= ST_NOEXEC;
if (diskfs_synchronous)
statbuf->f_flag |= ST_SYNCHRONOUS;
if (_diskfs_noatime)
statbuf->f_flag |= ST_NOATIME;
else if (_diskfs_relatime)
statbuf->f_flag |= ST_RELATIME;
diskfs_set_statfs (statbuf);
statbuf->f_namelen = diskfs_name_max;
return 0;
}