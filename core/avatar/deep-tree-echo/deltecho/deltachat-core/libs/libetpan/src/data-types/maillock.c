#ifdef HAVE_CONFIG_H
#	include <config.h>
#endif
#include "maillock.h"
#include "libetpan-config.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#ifdef HAVE_UNISTD_H
#	include <unistd.h>
#endif
#include <stdio.h>
#include <time.h>
#include <string.h>
#ifdef WIN32
#	include "win_etpan.h"
#endif
#ifdef HAVE_LIBLOCKFILE
#include <lockfile.h>
#endif
#define LOCKTO_RM	300
#define LOCKTO_GLOB	400
#ifdef WIN32
#	define F_RDLCK	0
#	define F_WRLCK	1
#	include <sys/locking.h>
#endif
static int lock_common(const char * filename, int fd, short locktype)
{
#ifdef WIN32
time_t start;
#ifdef SEB_TRY
int reslock;
lseek( fd, 0L, SEEK_SET );
reslock = _locking( fd, _LK_NBLCK, LONG_MAX);
if (reslock == 0) return 0;
if (errno != EACCES) return -1;
time(&start);
while (1) {
time_t now;
sleep( 5);
reslock = _locking( fd, _LK_NBLCK, LONG_MAX);
if (reslock == 0) return 0;
if (errno != EACCES) return -1;
time(&now);
if (now > start + LOCKTO_GLOB) {
return -1;
#else
if (fd != -1) {
lseek( fd, 0L, SEEK_SET );
if (_locking( fd, _LK_NBLCK, LONG_MAX) == 0) return 0;
time(&start);
while (1) {
time_t now;
sleep( 5);
if (_locking( fd, _LK_NBLCK, LONG_MAX) == 0) return 0;
time(&now);
if (now > start + LOCKTO_GLOB) {
return -1;
}
#endif
}
}
return 0;
#else
char lockfilename[PATH_MAX];
#ifndef HAVE_LIBLOCKFILE
struct flock lock;
int statfailed = 0;
time_t start;
int r;
#endif
int res;
if (strlen(filename) + 6 > PATH_MAX) {
res = -1;
goto err;
}
snprintf(lockfilename, PATH_MAX, "%s.lock", filename);
#ifdef HAVE_LIBLOCKFILE
return lockfile_create(lockfilename, LOCKTO_GLOB, 0);
#else
if (fd != -1) {
lock.l_start = 0;
lock.l_len = 0;
lock.l_pid = getpid();
lock.l_type = locktype;
lock.l_whence = SEEK_SET;
r = fcntl(fd, F_SETLKW, &lock);
if (r < 0) {
}
}
time(&start);
while (1) {
int fd2;
struct stat st;
time_t now;
time(&now);
if (now > start + LOCKTO_GLOB) {
res = -1;
goto unlock;
}
fd2 = open(lockfilename, O_WRONLY|O_EXCL|O_CREAT, 0);
if (fd2 >= 0) {
r = (int) write(fd2, "0", 2);
close(fd2);
break;
}
sleep(5);
if (stat(lockfilename, &st) < 0) {
if (statfailed++ > 5) {
res = -1;
goto unlock;
}
continue;
}
statfailed = 0;
time(&now);
if (now < st.st_ctime + LOCKTO_RM)
continue;
if (unlink(lockfilename) < 0) {
res = -1;
goto unlock;
}
#if 0
sleep(5);
#endif
}
return 0;
unlock:
if (fd != -1) {
lock.l_start = 0;
lock.l_len = 0;
lock.l_pid = getpid();
lock.l_type = F_UNLCK;
lock.l_whence = SEEK_SET;
r = fcntl(fd, F_SETLK, &lock);
if (r < 0) {
}
}
#endif
err:
return res;
#endif
}
static int unlock_common(const char * filename, int fd)
{
#ifdef WIN32
if (fd != -1) {
lseek( fd, 0L, SEEK_SET );
_locking( fd, _LK_UNLCK, LONG_MAX);
}
return 0;
#else
char lockfilename[PATH_MAX];
#ifndef HAVE_LIBLOCKFILE
struct flock lock;
int r;
#endif
if (strlen(filename) + 6 > PATH_MAX)
return -1;
snprintf(lockfilename, PATH_MAX, "%s.lock", filename);
#ifdef HAVE_LIBLOCKFILE
return lockfile_remove(lockfilename);
#else
unlink(lockfilename);
if (fd != -1) {
lock.l_start = 0;
lock.l_len = 0;
lock.l_pid = getpid();
lock.l_type = F_UNLCK;
lock.l_whence = SEEK_SET;
r = fcntl(fd, F_SETLK, &lock);
if (r < 0) {
}
}
return 0;
#endif
#endif
}
int maillock_read_lock(const char * filename, int fd)
{
return lock_common(filename, fd, F_RDLCK);
}
int maillock_read_unlock(const char * filename, int fd)
{
return unlock_common(filename, fd);
}
int maillock_write_lock(const char * filename, int fd)
{
return lock_common(filename, fd, F_WRLCK);
}
int maillock_write_unlock(const char * filename, int fd)
{
return unlock_common(filename, fd);
}