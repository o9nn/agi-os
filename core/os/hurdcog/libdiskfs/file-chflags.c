#include "priv.h"
#include "fs_S.h"
kern_return_t
diskfs_S_file_chflags (struct protid *cred,
int flags)
{
#define HI(X)	((X) & 0xffff0000u)
CHANGE_NODE_FIELD (cred,
({
if ((HI (flags) != HI (np->dn_stat.st_flags))
&& ! idvec_contains (cred->user->uids, 0))
return EPERM;
err = fshelp_isowner (&np->dn_stat, cred->user);
if (!err)
err = diskfs_validate_flags_change (np, flags);
if (!err)
{
np->dn_stat.st_flags = flags;
np->dn_set_ctime = 1;
}
if (!err && np->filemod_reqs)
diskfs_notice_filechange(np, FILE_CHANGED_META,
0, 0);
}));
#undef HI
}