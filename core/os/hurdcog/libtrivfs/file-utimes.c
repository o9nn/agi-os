#include "priv.h"
#include "trivfs_fs_S.h"
kern_return_t
trivfs_S_file_utimes (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
time_value_t atime, time_value_t mtime)
{
return cred ? file_utimes (cred->realnode, atime, mtime) : EOPNOTSUPP;
}
kern_return_t
trivfs_S_file_utimens (struct trivfs_protid *cred,
mach_port_t reply, mach_msg_type_name_t reply_type,
struct timespec atime, struct timespec mtime)
{
kern_return_t err;
if (!cred)
return EOPNOTSUPP;
#ifdef HAVE_FILE_UTIMENS
err = file_utimens (cred->realnode, atime, mtime);
if (err == EMIG_BAD_ID || err == EOPNOTSUPP)
#endif
{
time_value_t atim, mtim;
if (atime.tv_nsec == UTIME_NOW)
{
atim.seconds = -1;
atim.microseconds = -1;
}
else
TIMESPEC_TO_TIME_VALUE (&atim, &atime);
if (mtime.tv_nsec == UTIME_NOW)
{
mtim.seconds = -1;
mtim.microseconds = -1;
}
else
TIMESPEC_TO_TIME_VALUE (&mtim, &mtime);
err = file_utimes (cred->realnode, atim, mtim);
}
return err;
}