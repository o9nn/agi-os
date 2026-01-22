#include "fshelp.h"
error_t
fshelp_isowner (struct stat *st, struct iouser *user)
{
if (idvec_contains (user->uids, st->st_uid)
|| idvec_contains (user->uids, 0)
|| (idvec_contains (user->gids, st->st_gid)
&& idvec_contains (user->uids, st->st_gid)))
return 0;
else
return EPERM;
}