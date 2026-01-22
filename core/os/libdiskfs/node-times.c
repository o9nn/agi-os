#include "priv.h"
#include <maptime.h>
int
atime_should_update (struct node *np)
{
struct timeval t;
if (_diskfs_noatime)
return 0;
if (_diskfs_relatime)
{
if (np->dn_stat.st_mtim.tv_sec >= np->dn_stat.st_atim.tv_sec)
return 1;
if (np->dn_stat.st_ctim.tv_sec >= np->dn_stat.st_atim.tv_sec)
return 1;
maptime_read (diskfs_mtime, &t);
if ((long)(t.tv_sec - np->dn_stat.st_atim.tv_sec) >= 24 * 60 * 60)
return 1;
return 0;
}
return 1;
}
void
diskfs_set_node_atime (struct node *np)
{
if (!diskfs_check_readonly () && atime_should_update (np))
np->dn_set_atime = 1;
}
void
diskfs_set_node_times (struct node *np)
{
struct timeval t;
if (!np->dn_set_mtime && !np->dn_set_atime && !np->dn_set_ctime)
return;
maptime_read (diskfs_mtime, &t);
if (np->dn_set_mtime)
{
np->dn_stat.st_mtim.tv_sec = t.tv_sec;
np->dn_stat.st_mtim.tv_nsec = t.tv_usec * 1000;
np->dn_stat_dirty = 1;
np->dn_set_mtime = 0;
}
if (np->dn_set_atime)
{
np->dn_stat.st_atim.tv_sec = t.tv_sec;
np->dn_stat.st_atim.tv_nsec = t.tv_usec * 1000;
np->dn_stat_dirty = 1;
np->dn_set_atime = 0;
}
if (np->dn_set_ctime)
{
np->dn_stat.st_ctim.tv_sec = t.tv_sec;
np->dn_stat.st_ctim.tv_nsec = t.tv_usec * 1000;
np->dn_stat_dirty = 1;
np->dn_set_ctime = 0;
}
}