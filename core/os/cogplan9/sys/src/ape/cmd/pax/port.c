#ifndef lint
static char *ident = "$Id: port.c,v 1.2 89/02/12 10:05:35 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#if !defined(mc300) && !defined(mc500) && !defined(mc700) && !defined(BSD) && !defined(_POSIX_SOURCE)
#ifdef __STDC__
int mkdir(char *dpath, int dmode)
#else
int mkdir(dpath, dmode)
char *dpath;
int dmode;
#endif
{
int cpid, status;
Stat statbuf;
extern int errno;
if (STAT(dpath, &statbuf) == 0) {
errno = EEXIST;
return (-1);
}
if (errno != ENOENT)
return (-1);
switch (cpid = fork()) {
case -1:
return (-1);
case 0:
status = umask(0);
status = umask(status | (0777 & ~dmode));
execl("/bin/mkdir", "mkdir", dpath, (char *) 0);
_exit(-1);
default:
while (cpid != wait(&status)) {
}
}
if (TERM_SIGNAL(status) != 0 || TERM_VALUE(status) != 0) {
errno = EIO;
return (-1);
}
return (0);
}
#ifdef __STDC__
int rmdir(char *dpath)
#else
int rmdir(dpath)
char *dpath;
#endif
{
int cpid, status;
Stat statbuf;
extern int errno;
if (STAT(dpath, &statbuf) == -1) {
return (-1);
}
switch (cpid = fork()) {
case -1:
return (-1);
case 0:
execl("/bin/rmdir", "rmdir", dpath, (char *) 0);
_exit(-1);
default:
while (cpid != wait(&status)) {
}
}
if (TERM_SIGNAL(status) != 0 || TERM_VALUE(status) != 0) {
errno = EIO;
return (-1);
}
return (0);
}
#endif