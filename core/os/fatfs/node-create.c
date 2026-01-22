#include <hurd/diskfs.h>
int _diskfs_no_inherit_dir_group;
error_t
diskfs_create_node (struct node *dir,
const char *name,
mode_t mode,
struct node **newnode,
struct protid *cred,
struct dirstat *ds)
{
struct node *np;
error_t err;
uid_t newuid;
gid_t newgid;
if (diskfs_check_readonly ())
{
*newnode = NULL;
return EROFS;
}
err = diskfs_alloc_node (dir, mode, newnode);
if (err)
{
if (name)
diskfs_drop_dirstat (dir, ds);
*newnode = NULL;
return err;
}
np = *newnode;
if (cred->user->uids->num)
newuid = cred->user->uids->ids[0];
else
{
newuid = dir->dn_stat.st_uid;
mode &= ~S_ISUID;
}
err = diskfs_validate_owner_change (np, newuid);
if (err)
goto change_err;
np->dn_stat.st_uid = newuid;
if (np->author_tracks_uid)
np->dn_stat.st_author = newuid;
if (!_diskfs_no_inherit_dir_group)
{
newgid = dir->dn_stat.st_gid;
if (!idvec_contains (cred->user->gids, newgid))
mode &= ~S_ISGID;
}
else
{
if (dir->dn_stat.st_mode & S_ISGID)
{
newgid = dir->dn_stat.st_gid;
if (S_ISDIR (mode))
mode |= S_ISGID;
else
{
if (!idvec_contains (cred->user->gids, newgid))
mode &= ~S_ISGID;
}
}
else
{
if (cred->user->gids->num)
newgid = cred->user->gids->ids[0];
else
{
newgid = dir->dn_stat.st_gid;
mode &= ~S_ISGID;
}
}
}
err = diskfs_validate_group_change (np, newgid);
if (err)
goto change_err;
np->dn_stat.st_gid = newgid;
np->dn_stat.st_rdev = 0;
np->dn_stat.st_nlink = !!name;
err = diskfs_validate_mode_change (np, mode);
if (err)
goto change_err;
np->dn_stat.st_mode = mode;
np->dn_stat.st_blocks = 0;
np->dn_stat.st_size = 0;
np->dn_stat.st_flags = 0;
np->dn_set_atime = 1;
np->dn_set_mtime = 1;
np->dn_set_ctime = 1;
diskfs_node_update (np, 1);
if (err)
{
change_err:
np->dn_stat.st_mode = 0;
np->dn_stat.st_nlink = 0;
if (name)
diskfs_drop_dirstat (dir, ds);
*newnode = NULL;
return err;
}
if (name)
{
err = diskfs_direnter (dir, name, np, ds, cred);
if (err)
{
np->dn_stat.st_nlink = 0;
np->dn_set_ctime = 1;
diskfs_nput (np);
}
if (S_ISDIR (mode))
err = diskfs_init_dir (np, dir, cred);
if (err)
{
struct dirstat *ds = alloca (diskfs_dirstat_size);
struct node *foo;
error_t err;
np->dn_stat.st_nlink = 0;
err = diskfs_lookup (dir, name, REMOVE, &foo, ds, cred);
if (err)
{
*newnode = NULL;
return err;
}
err = diskfs_dirremove (dir, foo, name, ds);
if (err)
{
diskfs_nput (np);
*newnode = NULL;
return err;
}
}
diskfs_node_update (np, 1);
}
if (err)
*newnode = NULL;
return err;
}