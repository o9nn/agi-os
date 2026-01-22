#include <stdio.h>
#include <argz.h>
#include "priv.h"
error_t
diskfs_append_std_options (char **argz, size_t *argz_len)
{
error_t err;
extern int diskfs_sync_interval;
if (diskfs_readonly)
err = argz_add (argz, argz_len, "--readonly");
else
err = argz_add (argz, argz_len, "--writable");
if (!err && _diskfs_nosuid)
err = argz_add (argz, argz_len, "--no-suid");
if (!err && _diskfs_noexec)
err = argz_add (argz, argz_len, "--no-exec");
if (!err && _diskfs_noatime)
err = argz_add (argz, argz_len, "--no-atime");
else if (!err && _diskfs_relatime)
err = argz_add (argz, argz_len, "--relatime");
if (!err && _diskfs_no_inherit_dir_group)
err = argz_add (argz, argz_len, "--no-inherit-dir-group");
if (! err)
{
if (diskfs_synchronous)
err = argz_add (argz, argz_len, "--sync");
else if (DEFAULT_SYNC_INTERVAL != diskfs_sync_interval)
{
if (diskfs_sync_interval == 0)
err = argz_add (argz, argz_len, "--no-sync");
else
{
char buf[80];
sprintf (buf, "--sync=%d", diskfs_sync_interval);
err = argz_add (argz, argz_len, buf);
}
}
}
return err;
}