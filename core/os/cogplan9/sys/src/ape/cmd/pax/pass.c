#ifndef lint
static char *ident = "$Id: pass.c,v 1.3 89/02/12 10:29:51 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
int pass(char *dirname)
#else
int pass(dirname)
char	*dirname;
#endif
{
char            name[PATH_MAX + 1];
int             fd;
Stat            sb;
while (name_next(name, &sb) >= 0 && (fd = openin(name, &sb)) >= 0) {
if (rplhead != (Replstr *)NULL) {
rpl_name(name);
}
if (get_disposition("pass", name) || get_newname(name, sizeof(name))) {
if (fd) {
close(fd);
}
continue;
}
if (passitem(name, &sb, fd, dirname)) {
close(fd);
}
if (f_verbose) {
fprintf(stderr, "%s/%s\n", dirname, name);
}
}
}
#ifdef __STDC__
int passitem(char *from, Stat *asb, int ifd, char *dir)
#else
int passitem(from, asb, ifd, dir)
char           *from;
Stat           *asb;
int             ifd;
char           *dir;
#endif
{
int             ofd;
time_t          tstamp[2];
char            to[PATH_MAX + 1];
if (nameopt(strcat(strcat(strcpy(to, dir), "/"), from)) < 0) {
return (-1);
}
if (asb->sb_nlink > 1) {
linkto(to, asb);
}
if (f_link && islink(from, asb) == (Link *)NULL) {
linkto(from, asb);
}
if ((ofd = openout(to, asb, islink(to, asb), 1)) < 0) {
return (-1);
}
if (ofd > 0) {
passdata(from, ifd, to, ofd);
}
tstamp[0] = asb->sb_atime;
tstamp[1] = f_mtime ? asb->sb_mtime : time((time_t *) 0);
utime(to, tstamp);
return (ifd);
}