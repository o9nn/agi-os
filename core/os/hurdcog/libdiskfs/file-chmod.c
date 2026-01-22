#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_file_chmod (struct protid *cred,
mode_t mode)
{
mode &= ~(S_IFMT | S_ISPARE | S_ITRANS);
CHANGE_NODE_FIELD (cred,
({
if (!(err = fshelp_isowner (&np->dn_stat, cred->user)))
{
if (!idvec_contains (cred->user->uids, 0))
{
if (!S_ISDIR (np->dn_stat.st_mode))
mode &= ~S_ISVTX;
if (!idvec_contains (cred->user->gids,
np->dn_stat.st_gid))
mode &= ~S_ISGID;
if (!idvec_contains (cred->user->uids,
np->dn_stat.st_uid))
mode &= ~S_ISUID;
}
mode |= (np->dn_stat.st_mode
& (S_IFMT | S_ISPARE | S_ITRANS));
err = diskfs_validate_mode_change (np, mode);
if (!err)
{
np->dn_stat.st_mode = mode;
np->dn_set_ctime = 1;
if (np->filemod_reqs)
diskfs_notice_filechange (np,
FILE_CHANGED_META,
0, 0);
}
}
}));
}