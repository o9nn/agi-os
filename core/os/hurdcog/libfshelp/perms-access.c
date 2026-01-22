#include "fshelp.h"
error_t
fshelp_access (struct stat *st, int op, struct iouser *user)
{
int gotit;
if (idvec_contains (user->uids, 0))
gotit = (op != S_IEXEC) || !S_ISREG(st->st_mode) || (st->st_mode & (S_IXUSR | S_IXGRP | S_IXOTH));
else if (user->uids->num == 0 && (st->st_mode & S_IUSEUNK))
gotit = st->st_mode & (op << S_IUNKSHIFT);
else if (!fshelp_isowner (st, user))
gotit = st->st_mode & op;
else if (idvec_contains (user->gids, st->st_gid))
gotit = st->st_mode & (op >> 3);
else
gotit = st->st_mode & (op >> 6);
return gotit ? 0 : EACCES;
}