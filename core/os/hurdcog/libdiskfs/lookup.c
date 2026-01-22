#include "priv.h"
#include <string.h>
error_t
diskfs_lookup (struct node *dp, const char *name, enum lookup_type type,
struct node **np, struct dirstat *ds, struct protid *cred)
{
error_t err;
struct node *cached;
if (type == REMOVE || type == RENAME)
assert_backtrace (np);
if (!S_ISDIR (dp->dn_stat.st_mode))
{
if (ds)
diskfs_null_dirstat (ds);
return ENOTDIR;
}
while (*name == '/')
name++;
if (name[0] == '\0')
{
if (ds)
diskfs_null_dirstat (ds);
return EINVAL;
}
else
{
char *p = strchr (name, '/');
if (p != 0)
{
*p = '\0';
do
++p;
while (*p == '/');
if (*p != '\0')
{
if (ds)
diskfs_null_dirstat (ds);
return EINVAL;
}
}
}
err = fshelp_access (&dp->dn_stat, S_IEXEC, cred->user);
if (err)
{
if (ds)
diskfs_null_dirstat (ds);
return err;
}
if (dp == cred->po->shadow_root
&& name[0] == '.' && name[1] == '.' && name[2] == '\0')
{
if (ds)
diskfs_null_dirstat (ds);
return EAGAIN;
}
if (type == LOOKUP)
cached = diskfs_check_lookup_cache (dp, name);
else
cached = 0;
if (cached == (struct node *)-1)
{
if (np)
*np = 0;
return ENOENT;
}
else if (cached)
{
if (np)
*np = cached;
else
if (cached == dp)
diskfs_nrele (cached);
else
diskfs_nput (cached);
if (ds)
diskfs_null_dirstat (ds);
}
else
{
err = diskfs_lookup_hard (dp, name, type, np, ds, cred);
if (err && err != ENOENT)
return err;
if (type == RENAME
|| (type == CREATE && err == ENOENT)
|| (type == REMOVE && err != ENOENT))
{
error_t err2;
if (diskfs_name_max > 0 && strlen (name) > diskfs_name_max)
err2 = ENAMETOOLONG;
else
err2 = fshelp_checkdirmod (&dp->dn_stat,
(err || !np) ? 0 : &(*np)->dn_stat,
cred->user);
if (err2)
{
if (np && !err)
{
if (*np == dp)
diskfs_nrele (*np);
else
diskfs_nput (*np);
*np = 0;
}
return err2;
}
}
if ((type == LOOKUP || type == CREATE) && !err && np)
diskfs_enter_lookup_cache (dp, *np, name);
else if (type == LOOKUP && err == ENOENT)
diskfs_enter_lookup_cache (dp, 0, name);
}
return err;
}