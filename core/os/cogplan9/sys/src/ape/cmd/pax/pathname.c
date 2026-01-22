#ifndef lint
static char *ident = "$Id: pathname.c,v 1.2 89/02/12 10:05:13 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
int dirneed(char *name)
#else
int dirneed(name)
char           *name;
#endif
{
char           *cp;
char           *last;
int             ok;
static Stat     sb;
last = (char *)NULL;
for (cp = name; *cp;) {
if (*cp++ == '/') {
last = cp;
}
}
if (last == (char *)NULL) {
return (STAT(".", &sb));
}
*--last = '\0';
ok = STAT(*name ? name : ".", &sb) == 0
? ((sb.sb_mode & S_IFMT) == S_IFDIR)
: (f_dir_create && dirneed(name) == 0 && dirmake(name, &sb) == 0);
*last = '/';
return (ok ? 0 : -1);
}
#ifdef __STDC__
int nameopt(char *begin)
#else
int nameopt(begin)
char           *begin;
#endif
{
char           *name;
char           *item;
int             idx;
int             absolute;
char           *element[PATHELEM];
absolute = (*(name = begin) == '/');
idx = 0;
for (;;) {
if (idx == PATHELEM) {
warn(begin, "Too many elements");
return (-1);
}
while (*name == '/') {
++name;
}
if (*name == '\0') {
break;
}
element[idx] = item = name;
while (*name && *name != '/') {
++name;
}
if (*name) {
*name++ = '\0';
}
if (strcmp(item, "..") == 0) {
if (idx == 0) {
if (!absolute) {
++idx;
}
} else if (strcmp(element[idx - 1], "..") == 0) {
++idx;
} else {
--idx;
}
} else if (strcmp(item, ".") != 0) {
++idx;
}
}
if (idx == 0) {
element[idx++] = absolute ? "" : ".";
}
element[idx] = (char *)NULL;
name = begin;
if (absolute) {
*name++ = '/';
}
for (idx = 0; item = element[idx]; ++idx, *name++ = '/') {
while (*item) {
*name++ = *item++;
}
}
*--name = '\0';
return (idx);
}
#ifdef __STDC__
int dirmake(char *name, Stat *asb)
#else
int dirmake(name, asb)
char           *name;
Stat           *asb;
#endif
{
if (mkdir(name, (int) (asb->sb_mode & S_IPOPN)) < 0) {
return (-1);
}
if (asb->sb_mode & S_IPEXE) {
chmod(name, (int) (asb->sb_mode & S_IPERM));
}
if (f_owner) {
chown(name, (int) asb->sb_uid, (int) asb->sb_gid);
}
return (0);
}