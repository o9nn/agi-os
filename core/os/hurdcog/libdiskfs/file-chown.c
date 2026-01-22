#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_file_chown (struct protid *cred,
uid_t uid,
gid_t gid)
{
if (uid == (uid_t) -1 && gid == (gid_t) -1)
return 0;
CHANGE_NODE_FIELD (cred,
({
err = fshelp_isowner (&np->dn_stat, cred->user);
if (err
|| (((uid != (uid_t) -1
&& !idvec_contains (cred->user->uids, uid))
|| (gid != (gid_t) -1
&& !idvec_contains (cred->user->gids, gid)))
&& !idvec_contains (cred->user->uids, 0)))
err = EPERM;
else
{
if (uid != (uid_t) -1)
err = diskfs_validate_owner_change (np, uid);
if (!err && gid != (gid_t) -1)
err = diskfs_validate_group_change (np, gid);
if (!err)
{
if (uid != (uid_t) -1)
{
np->dn_stat.st_uid = uid;
if (np->author_tracks_uid)
np->dn_stat.st_author = uid;
}
if (gid != (gid_t) -1)
np->dn_stat.st_gid = gid;
np->dn_set_ctime = 1;
if (np->filemod_reqs)
diskfs_notice_filechange(np,
FILE_CHANGED_META,
0, 0);
}
}
}));
}