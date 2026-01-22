#include "priv.h"
error_t
trivfs_set_atime (struct trivfs_control *cntl)
{
error_t err;
#ifdef HAVE_FILE_UTIMENS
struct timespec atime;
struct timespec mtime;
atime.tv_sec = 0;
atime.tv_nsec = UTIME_NOW;
mtime.tv_sec = 0;
mtime.tv_nsec = UTIME_OMIT;
err = file_utimens (cntl->underlying, atime, mtime);
if (err == MIG_BAD_ID || err == EOPNOTSUPP)
#endif
{
struct stat st;
time_value_t atim, mtim;
io_stat (cntl->underlying, &st);
TIMESPEC_TO_TIME_VALUE (&atim, &st.st_atim);
mtim.seconds = -1;
mtim.microseconds = -1;
err = file_utimes (cntl->underlying, atim, mtim);
}
return err;
}
error_t
trivfs_set_mtime (struct trivfs_control *cntl)
{
error_t err;
#ifdef HAVE_FILE_UTIMENS
struct timespec atime;
struct timespec mtime;
atime.tv_sec = 0;
atime.tv_nsec = UTIME_OMIT;
mtime.tv_sec = 0;
mtime.tv_nsec = UTIME_NOW;
err = file_utimens (cntl->underlying, atime, mtime);
if (err == MIG_BAD_ID || err == EOPNOTSUPP)
#endif
{
struct stat st;
time_value_t atim, mtim;
io_stat (cntl->underlying, &st);
atim.seconds = -1;
atim.microseconds = -1;
TIMESPEC_TO_TIME_VALUE (&mtim, &st.st_mtim);
err = file_utimes (cntl->underlying, atim, mtim);
}
return err;
}