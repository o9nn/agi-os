#include "netfs.h"
#include "fs_S.h"
kern_return_t
netfs_S_file_utimes (struct protid *user,
time_value_t atimein,
time_value_t mtimein)
{
struct timespec atim, mtim;
if (atimein.microseconds == -1)
{
atim.tv_sec = 0;
atim.tv_nsec = UTIME_NOW;
}
else
TIME_VALUE_TO_TIMESPEC (&atimein, &atim);
if (mtimein.microseconds == -1)
{
mtim.tv_sec = 0;
mtim.tv_nsec = UTIME_NOW;
}
else
TIME_VALUE_TO_TIMESPEC (&mtimein, &mtim);
return netfs_S_file_utimens (user, atim, mtim);
}
kern_return_t
netfs_S_file_utimens (struct protid *user,
struct timespec atimein,
struct timespec mtimein)
{
error_t err;
struct timeval t;
if (!user)
return EOPNOTSUPP;
if (atimein.tv_nsec == UTIME_NOW || mtimein.tv_nsec == UTIME_NOW)
{
maptime_read (netfs_mtime, &t);
if (atimein.tv_nsec == UTIME_NOW)
TIMEVAL_TO_TIMESPEC (&t, &atimein);
if (mtimein.tv_nsec == UTIME_NOW)
TIMEVAL_TO_TIMESPEC (&t, &mtimein);
}
pthread_mutex_lock (&user->po->np->lock);
err = netfs_attempt_utimes (user->user, user->po->np,
(atimein.tv_nsec == UTIME_OMIT) ? 0 : &atimein,
(mtimein.tv_nsec == UTIME_OMIT) ? 0 : &mtimein);
pthread_mutex_unlock (&user->po->np->lock);
return err;
}