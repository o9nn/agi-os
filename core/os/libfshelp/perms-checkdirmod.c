#include "fshelp.h"
error_t
fshelp_checkdirmod (struct stat *dir, struct stat *st, struct iouser *user)
{
error_t err;
err = fshelp_access (dir, S_IWRITE, user);
if (err)
return err;
if ((dir->st_mode & S_ISVTX) && st
&& fshelp_isowner (dir, user) && fshelp_isowner (st, user))
return EACCES;
return 0;
}