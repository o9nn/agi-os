#ifndef lint
static char *ident = "$Id: link.c,v 1.2 89/02/12 10:04:38 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#define LINKHASH(ino) (linkbase + (ino) % NEL(linkbase))
#define NEL(a) (sizeof(a) / sizeof(*(a)))
static Link *linkbase[256];
#ifdef __STDC__
Link *linkfrom(char *name, Stat *asb)
#else
Link *linkfrom(name, asb)
char *name;
Stat *asb;
#endif
{
Link *linkp;
Link *linknext;
Path *path;
Path *pathnext;
Link **abase;
for (linkp = *(abase = LINKHASH(asb->sb_ino)); linkp; linkp = linknext) {
if (linkp->l_nlink == 0) {
if (linkp->l_name) {
free((char *) linkp->l_name);
}
if (linknext = linkp->l_forw) {
linknext->l_back = linkp->l_back;
}
if (linkp->l_back) {
linkp->l_back->l_forw = linkp->l_forw;
}
free((char *) linkp);
*abase = (Link *)NULL;
} else if (linkp->l_ino == asb->sb_ino && linkp->l_dev == asb->sb_dev) {
for (path = linkp->l_path; path; path = pathnext) {
if (strcmp(path->p_name, name) == 0) {
--linkp->l_nlink;
if (path->p_name) {
free(path->p_name);
}
if (pathnext = path->p_forw) {
pathnext->p_back = path->p_back;
}
if (path->p_back) {
path->p_back->p_forw = pathnext;
}
if (linkp->l_path == path) {
linkp->l_path = pathnext;
}
free(path);
return (linkp);
}
pathnext = path->p_forw;
}
return((Link *)NULL);
} else {
linknext = linkp->l_forw;
}
}
return ((Link *)NULL);
}
#ifdef __STDC__
Link *islink(char *name, Stat *asb)
#else
Link *islink(name, asb)
char *name;
Stat *asb;
#endif
{
Link *linkp;
Link *linknext;
for (linkp = *(LINKHASH(asb->sb_ino)); linkp; linkp = linknext) {
if (linkp->l_ino == asb->sb_ino && linkp->l_dev == asb->sb_dev) {
if (strcmp(name, linkp->l_name) == 0) {
return ((Link *)NULL);
}
return (linkp);
} else {
linknext = linkp->l_forw;
}
}
return ((Link *)NULL);
}
#ifdef __STDC__
Link *linkto(char *name, Stat *asb)
#else
Link *linkto(name, asb)
char *name;
Stat *asb;
#endif
{
Link *linkp;
Link *linknext;
Path *path;
Link **abase;
for (linkp = *(LINKHASH(asb->sb_ino)); linkp; linkp = linknext) {
if (linkp->l_ino == asb->sb_ino && linkp->l_dev == asb->sb_dev) {
if ((path = (Path *) mem_get(sizeof(Path))) == (Path *)NULL ||
(path->p_name = mem_str(name)) == (char *)NULL) {
return((Link *)NULL);
}
if (path->p_forw = linkp->l_path) {
if (linkp->l_path->p_forw) {
linkp->l_path->p_forw->p_back = path;
}
} else {
linkp->l_path = path;
}
path->p_back = (Path *)NULL;
return(linkp);
} else {
linknext = linkp->l_forw;
}
}
if ((asb->sb_mode & S_IFMT) == S_IFDIR
|| (linkp = (Link *) mem_get(sizeof(Link))) == (Link *)NULL
|| (linkp->l_name = mem_str(name)) == (char *)NULL) {
return ((Link *)NULL);
}
linkp->l_dev = asb->sb_dev;
linkp->l_ino = asb->sb_ino;
linkp->l_nlink = asb->sb_nlink - 1;
linkp->l_size = asb->sb_size;
linkp->l_path = (Path *)NULL;
if (linkp->l_forw = *(abase = LINKHASH(asb->sb_ino))) {
linkp->l_forw->l_back = linkp;
} else {
*abase = linkp;
}
linkp->l_back = (Link *)NULL;
return (linkp);
}
#ifdef __STDC__
void linkleft(void)
#else
void linkleft()
#endif
{
Link *lp;
Link **base;
for (base = linkbase; base < linkbase + NEL(linkbase); ++base) {
for (lp = *base; lp; lp = lp->l_forw) {
if (lp->l_nlink) {
warn(lp->l_path->p_name, "Unseen link(s)");
}
}
}
}