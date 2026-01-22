#ifndef lint
static char *ident = "$Id: names.c,v 1.2 89/02/12 10:05:05 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#define myuid	( my_uid < 0? (my_uid = getuid()): my_uid )
#define	mygid	( my_gid < 0? (my_gid = getgid()): my_gid )
static int      saveuid = -993;
static char     saveuname[TUNMLEN];
static int      my_uid = -993;
static int      savegid = -993;
static char     savegname[TGNMLEN];
static int      my_gid = -993;
#ifdef __STDC__
char *finduname(int uuid)
#else
char *finduname(uuid)
int             uuid;
#endif
{
struct passwd  *pw;
if (uuid != saveuid) {
saveuid = uuid;
saveuname[0] = '\0';
pw = getpwuid(uuid);
if (pw) {
strncpy(saveuname, pw->pw_name, TUNMLEN);
}
}
return(saveuname);
}
#ifdef __STDC__
int finduid(char *uname)
#else
int finduid(uname)
char            *uname;
#endif
{
struct passwd  *pw;
extern struct passwd *getpwnam();
if (uname[0] != saveuname[0]
||0 != strncmp(uname, saveuname, TUNMLEN)) {
strncpy(saveuname, uname, TUNMLEN);
pw = getpwnam(uname);
if (pw) {
saveuid = pw->pw_uid;
} else {
saveuid = myuid;
}
}
return (saveuid);
}
#ifdef __STDC__
char *findgname(int ggid)
#else
char *findgname(ggid)
int             ggid;
#endif
{
struct group   *gr;
if (ggid != savegid) {
savegid = ggid;
savegname[0] = '\0';
#ifndef _POSIX_SOURCE
setgrent();
#endif
gr = getgrgid(ggid);
if (gr) {
strncpy(savegname, gr->gr_name, TGNMLEN);
}
}
return(savegname);
}
#ifdef __STDC__
int findgid(char *gname)
#else
int findgid(gname)
char           *gname;
#endif
{
struct group   *gr;
if (gname[0] != savegname[0] || strncmp(gname, savegname, TUNMLEN) != 0) {
strncpy(savegname, gname, TUNMLEN);
gr = getgrnam(gname);
if (gr) {
savegid = gr->gr_gid;
} else {
savegid = mygid;
}
}
return (savegid);
}