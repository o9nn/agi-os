#include <unistd.h>
#include "fshelp.h"
error_t
fshelp_iscontroller (struct stat *st, struct iouser *user)
{
if (idvec_contains (user->uids, 0)
|| idvec_contains (user->uids, st->st_uid)
|| idvec_contains (user->uids, geteuid ()))
return 0;
return EPERM;
}