#include <sys/errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "paxdir.h"
#ifdef BSD_SYSV
#if defined(accel) || defined(sun) || defined(vax)
#define DIRBLKSIZ 512
#else
#ifdef alliant
#define DIRBLKSIZ 4096
#else
#ifdef gould
#define DIRBLKSIZ 1024
#else
#ifdef ns32000
#define DIRBLKSIZ 2600
#else
#define DIRBLKSIZ 4096
#endif
#endif
#endif
#endif
#define MAXNAMELEN 255
struct direct {
unsigned long d_fileno;
unsigned short d_reclen;
unsigned short d_namlen;
char d_name[MAXNAMELEN + 1];
};
#define DIRSIZ( dp ) ((sizeof(struct direct) - (MAXNAMELEN+1) \
+ sizeof(long) + (dp)->d_namlen) \
/ sizeof(long) * sizeof(long))
#else
#include <sys/dir.h>
#ifdef SYSV3
#undef MAXNAMLEN
#endif
#ifdef d_ino
#undef d_ino
#else
#define d_fileno d_ino
#endif
#endif
#ifdef UNK
#ifndef UFS
#include "***** ERROR ***** UNK applies only to UFS"
#endif
#include <signal.h>
#endif
#if defined(UFS) + defined(BFS) + defined(NFS) != 1
#include "***** ERROR ***** exactly one of UFS, BFS, or NFS must be defined"
#endif
#ifdef UFS
#define RecLen( dp ) (sizeof(struct direct))
#else
#define RecLen( dp ) ((dp)->d_reclen)
#endif
#ifdef NFS
#ifdef BSD_SYSV
#define getdirentries _getdirentries
#endif
extern int getdirentries();
static long dummy;
#define GetBlock( fd, buf, n ) getdirentries( fd, buf, (unsigned)n, &dummy )
#else
#ifdef BSD_SYSV
#define read _read
#endif
extern int read();
#define GetBlock( fd, buf, n ) read( fd, buf, (unsigned)n )
#endif
#ifdef UNK
extern int _getdents();
#endif
extern char *strncpy();
extern int fstat();
extern OFFSET lseek();
extern int errno;
#ifndef DIRBLKSIZ
#define DIRBLKSIZ 4096
#endif
#ifndef NULL
#define NULL 0
#endif
#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif
#ifndef S_ISDIR
#define S_ISDIR( mode ) (((mode) & S_IFMT) == S_IFDIR)
#endif
#ifndef SEEK_CUR
#define SEEK_CUR 1
#endif
#ifdef BSD_SYSV
#define open _open
#endif
extern int getdents();
typedef char *pointer;
extern void free();
extern pointer malloc();
extern int
open(), close(), fstat();
extern int errno;
extern OFFSET lseek();
#ifndef SEEK_SET
#define SEEK_SET 0
#endif
typedef int bool;
#define false 0
#define true 1
#ifndef NULL
#define NULL 0
#endif
#ifndef O_RDONLY
#define O_RDONLY 0
#endif
#ifndef S_ISDIR
#define S_ISDIR( mode ) (((mode) & S_IFMT) == S_IFDIR)
#endif
#ifdef __STDC__
DIR *opendir(char *dirname)
#else
DIR *opendir(dirname)
char *dirname;
#endif
{
register DIR *dirp;
register int fd;
struct stat sbuf;
if ((fd = open(dirname, O_RDONLY)) < 0)
return ((DIR *)NULL);
if (fstat(fd, &sbuf) != 0 || !S_ISDIR(sbuf.st_mode)) {
close(fd);
errno = ENOTDIR;
return ((DIR *)NULL);
}
if ((dirp = (DIR *) malloc(sizeof(DIR))) == (DIR *)NULL
|| (dirp->dd_buf = (char *) malloc((unsigned) DIRBUF)) == (char *)NULL
) {
register int serrno = errno;
if (dirp != (DIR *)NULL)
free((pointer) dirp);
close(fd);
errno = serrno;
return ((DIR *)NULL);
}
dirp->dd_fd = fd;
dirp->dd_loc = dirp->dd_size = 0;
return dirp;
}
#ifdef __STDC__
int closedir(register DIR *dirp)
#else
int closedir(dirp)
register DIR *dirp;
#endif
{
register int fd;
if ( dirp == (DIR *)NULL || dirp->dd_buf == (char *)NULL ) {
errno = EFAULT;
return -1;
}
fd = dirp->dd_fd;
free( (pointer)dirp->dd_buf );
free( (pointer)dirp );
return close( fd );
}
#ifdef __STDC__
struct dirent *readdir(register DIR *dirp)
#else
struct dirent *readdir(dirp)
register DIR *dirp;
#endif
{
register struct dirent *dp;
if (dirp == (DIR *)NULL || dirp->dd_buf == (char *)NULL) {
errno = EFAULT;
return (struct dirent *)NULL;
}
do {
if (dirp->dd_loc >= dirp->dd_size)
dirp->dd_loc = dirp->dd_size = 0;
if (dirp->dd_size == 0
&& (dirp->dd_size =
getdents(dirp->dd_fd, dirp->dd_buf, (unsigned) DIRBUF)
) <= 0
)
return ((struct dirent *)NULL);
dp = (struct dirent *) & dirp->dd_buf[dirp->dd_loc];
dirp->dd_loc += dp->d_reclen;
}
while (dp->d_ino == 0L);
return dp;
}
#ifdef __STDC__
void seekdir(register DIR *dirp, register OFFSET loc)
#else
void seekdir(dirp, loc)
register DIR *dirp;
register OFFSET loc;
#endif
{
register bool rewind;
if (dirp == (DIR *)NULL || dirp->dd_buf == (char *)NULL) {
errno = EFAULT;
return;
}
if (dirp->dd_loc >= dirp->dd_size
|| ((struct dirent *) & dirp->dd_buf[dirp->dd_loc])->d_off > loc
)
dirp->dd_loc = 0;
for (rewind = true;;) {
register struct dirent *dp;
if ((dirp->dd_loc < dirp->dd_size
|| readdir(dirp) != (struct dirent *)NULL
&& (dirp->dd_loc = 0, true)
)
&& (dp = (struct dirent *) & dirp->dd_buf[dirp->dd_loc])->d_off
<= loc
) {
for ( ;
(char *) dp < &dirp->dd_buf[dirp->dd_size];
dp = (struct dirent *) ((char *) dp + dp->d_reclen)
)
if (dp->d_off == loc) {
dirp->dd_loc =
(char *) dp - dirp->dd_buf;
return;
}
rewind = false;
dirp->dd_loc = dirp->dd_size;
} else
if (!rewind) {
errno = EINVAL;
return;
} else {
rewind = false;
dirp->dd_loc = dirp->dd_size = 0;
if (lseek(dirp->dd_fd, (OFFSET) 0, SEEK_SET)
!= 0
)
return;
if (loc == 0)
return;
}
}
}
#ifdef __STDC__
OFFSET telldir(DIR *dirp)
#else
OFFSET telldir(dirp)
DIR *dirp;
#endif
{
if (dirp == (DIR *)NULL || dirp->dd_buf == (char *)NULL) {
errno = EFAULT;
return -1;
}
if (dirp->dd_loc < dirp->dd_size)
return ((struct dirent *) & dirp->dd_buf[dirp->dd_loc])->d_off;
else
return lseek(dirp->dd_fd, (OFFSET) 0, SEEK_CUR);
}
#ifdef UFS
#ifdef __STDC__
static int NameLen(char *name)
#else
static int NameLen(name)
char *name;
#endif
{
register char *s;
register char *stop = &name[DIRSIZ];
for (s = &name[1];
*s != '\0'
&& ++s < stop;
);
return s - name;
}
#else
extern int strlen();
#define NameLen( name ) strlen( name )
#endif
#ifdef UNK
static enum {
maybe, no, yes
} state = maybe;
#ifdef __STDC__
static void sig_catch(int sig)
#else
static void sig_catch(sig)
int sig;
#endif
{
state = no;
}
#endif
#ifdef __STDC__
int getdents(int fildes, char *buf, unsigned nbyte)
#else
int getdents(fildes, buf, nbyte)
int fildes;
char *buf;
unsigned nbyte;
#endif
{
int serrno;
OFFSET offset;
struct stat statb;
union {
#ifdef UFS
char dblk[DIRBLKSIZ + 1];
#else
char dblk[DIRBLKSIZ];
#endif
struct direct dummy;
} u;
register struct direct *dp;
register struct dirent *bp;
#ifdef UNK
switch (state) {
SIG_T (*shdlr)();
register int retval;
case yes:
return _getdents(fildes, buf, nbyte);
case maybe:
shdlr = signal(SIGSYS, sig_catch);
retval = _getdents(fildes, buf, nbyte);
signal(SIGSYS, shdlr);
if (state == maybe) {
state = yes;
return retval;
}
}
#endif
if (buf == (char *)NULL
#ifdef ATT_SPEC
|| (unsigned long) buf % sizeof(long) != 0
#endif
) {
errno = EFAULT;
return -1;
}
if (fstat(fildes, &statb) != 0) {
return -1;
}
if (!S_ISDIR(statb.st_mode)) {
errno = ENOTDIR;
return -1;
}
if ((offset = lseek(fildes, (OFFSET) 0, SEEK_CUR)) < 0) {
return -1;
}
#ifdef BFS
if ((unsigned long) offset % DIRBLKSIZ != 0) {
errno = ENOENT;
return -1;
}
#endif
serrno = errno;
for (bp = (struct dirent *) buf; bp == (struct dirent *) buf;) {
int size;
do {
size = GetBlock(fildes, u.dblk, DIRBLKSIZ);
} while (size == -1 && errno == EINTR);
if (size <= 0) {
return size;
}
for (dp = (struct direct *) u.dblk;
(char *) dp < &u.dblk[size];
dp = (struct direct *) ((char *) dp + RecLen(dp))
) {
#ifndef UFS
if (dp->d_reclen <= 0) {
errno = EIO;
return -1;
}
#endif
if (dp->d_fileno != 0) {
register int reclen =
DIRENTSIZ(NameLen(dp->d_name));
if ((char *) bp + reclen > &buf[nbyte]) {
errno = EINVAL;
return -1;
}
bp->d_ino = dp->d_fileno;
bp->d_off = offset + ((char *) dp - u.dblk);
bp->d_reclen = reclen;
{
#ifdef UFS
register char save = dp->d_name[DIRSIZ];
dp->d_name[DIRSIZ] = '\0';
#endif
strncpy(bp->d_name, dp->d_name, reclen - DIRENTBASESIZ);
#ifdef UFS
dp->d_name[DIRSIZ] = save;
#endif
}
bp = (struct dirent *) ((char *) bp + reclen);
}
}
#ifndef BFS
if ((char *) dp > &u.dblk[size]) {
errno = EIO;
return -1;
}
#endif
}
errno = serrno;
return (char *) bp - buf;
}