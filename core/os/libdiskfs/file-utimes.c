#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_file_utimes (struct protid *cred,
time_value_t atime,
time_value_t mtime)
{
struct timespec atim, mtim;
if (atime.microseconds == -1)
{
atim.tv_sec = 0;
atim.tv_nsec = UTIME_NOW;
}
else
TIME_VALUE_TO_TIMESPEC (&atime, &atim);
if (mtime.microseconds == -1)
{
mtim.tv_sec = 0;
mtim.tv_nsec = UTIME_NOW;
}
else
TIME_VALUE_TO_TIMESPEC (&mtime, &mtim);
return diskfs_S_file_utimens (cred, atim, mtim);
}
kern_return_t
diskfs_S_file_utimens (struct protid *cred,
struct timespec atime,
struct timespec mtime)
{
CHANGE_NODE_FIELD (cred,
({
if (!(err = fshelp_isowner (&np->dn_stat, cred->user)))
{
diskfs_set_node_times (np);
if (atime.tv_nsec == UTIME_NOW)
np->dn_set_atime = 1;
else if (atime.tv_nsec == UTIME_OMIT)
;
else
{
np->dn_stat.st_atim = atime;
np->dn_set_atime = 0;
}
if (mtime.tv_nsec == UTIME_NOW)
np->dn_set_mtime = 1;
else if (mtime.tv_nsec == UTIME_OMIT)
;
else
{
np->dn_stat.st_mtim = mtime;
np->dn_set_mtime = 0;
}
np->dn_set_ctime = 1;
if (np->filemod_reqs)
diskfs_notice_filechange (np,
FILE_CHANGED_META,
0, 0);
}
}));
}