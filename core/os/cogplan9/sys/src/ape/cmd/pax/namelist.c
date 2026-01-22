#ifndef lint
static char *ident = "$Id: namelist.c,v 1.6 89/02/13 09:14:48 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
struct nm_list {
struct nm_list *next;
short           length;
char            found;
char            firstch;
char            re;
char            name[1];
};
struct dirinfo {
char            dirname[PATH_MAX + 1];
OFFSET	    where;
struct dirinfo *next;
};
static struct dirinfo *stack_head = (struct dirinfo *)NULL;
#ifndef __STDC__
static void pushdir();
static struct dirinfo *popdir();
#else
static void pushdir(struct dirinfo *info);
static struct dirinfo *popdir(void);
#endif
static struct nm_list *namelast;
static struct nm_list *namelist;
#ifdef __STDC__
void add_name(char *name)
#else
void add_name(name)
char           *name;
#endif
{
int             i;
struct nm_list *p;
i = strlen(name);
p = (struct nm_list *) malloc((unsigned) (i + sizeof(struct nm_list)));
if (!p) {
fatal("cannot allocate memory for namelist entry\n");
}
p->next = (struct nm_list *)NULL;
p->length = i;
strncpy(p->name, name, i);
p->name[i] = '\0';
p->found = 0;
p->firstch = isalpha(name[0]);
if (strchr(name, '*') || strchr(name, '[') || strchr(name, '?')) {
p->re = 1;
}
if (namelast) {
namelast->next = p;
}
namelast = p;
if (!namelist) {
namelist = p;
}
}
#ifdef __STDC__
int name_match(char *p)
#else
int name_match(p)
char           *p;
#endif
{
struct nm_list *nlp;
int             len;
if ((nlp = namelist) == 0) {
return (1);
}
len = strlen(p);
for (; nlp != 0; nlp = nlp->next) {
if (nlp->firstch && nlp->name[0] != p[0]) {
continue;
}
if (nlp->re) {
if (wildmat(nlp->name, p)) {
nlp->found = 1;
return (1);
}
continue;
}
if (nlp->length <= len
&& (p[nlp->length] == '\0' || p[nlp->length] == '/')
&& strncmp(p, nlp->name, nlp->length) == 0) {
nlp->found = 1;
return (1);
}
}
return (0);
}
#ifdef __STDC__
void names_notfound(void)
#else
void names_notfound()
#endif
{
struct nm_list *nlp;
for (nlp = namelist; nlp != 0; nlp = nlp->next) {
if (!nlp->found) {
fprintf(stderr, "%s: %s not found in archive\n",
myname, nlp->name);
}
free(nlp);
}
namelist = (struct nm_list *)NULL;
namelast = (struct nm_list *)NULL;
}
#ifdef __STDC__
void name_init(int argc, char **argv)
#else
void name_init(argc, argv)
int             argc;
char          **argv;
#endif
{
n_argc = argc;
n_argv = argv;
}
#ifdef __STDC__
int name_next(char *name, Stat *statbuf)
#else
int name_next(name, statbuf)
char           *name;
Stat           *statbuf;
#endif
{
int             err = -1;
static int      in_subdir = 0;
static DIR     *dirp;
struct dirent  *d;
static struct dirinfo *curr_dir;
int			len;
do {
if (names_from_stdin) {
if (lineget(stdin, name) < 0) {
return (-1);
}
if (nameopt(name) < 0) {
continue;
}
} else {
if (in_subdir) {
if ((d = readdir(dirp)) != (struct dirent *)NULL) {
if (strcmp(d->d_name, ".") == 0 ||
strcmp(d->d_name, "..") == 0) {
continue;
}
if (strlen(d->d_name) +
strlen(curr_dir->dirname) >= PATH_MAX) {
warn("name too long", d->d_name);
continue;
}
strcpy(name, curr_dir->dirname);
strcat(name, d->d_name);
} else {
closedir(dirp);
in_subdir--;
curr_dir = popdir();
if (in_subdir) {
errno = 0;
if ((dirp=opendir(curr_dir->dirname)) == (DIR *)NULL) {
warn(curr_dir->dirname, "error opening directory (1)");
in_subdir--;
}
seekdir(dirp, curr_dir->where);
}
continue;
}
} else if (optind >= n_argc) {
return (-1);
} else {
strcpy(name, n_argv[optind++]);
}
}
if ((err = LSTAT(name, statbuf)) < 0) {
warn(name, strerror());
continue;
}
if (!names_from_stdin && (statbuf->sb_mode & S_IFMT) == S_IFDIR) {
if (in_subdir) {
curr_dir->where = telldir(dirp);
pushdir(curr_dir);
closedir(dirp);
}
in_subdir++;
if ((curr_dir = (struct dirinfo *) mem_get(sizeof(struct dirinfo)))
== (struct dirinfo *)NULL) {
exit(2);
}
strcpy(curr_dir->dirname, name);
len = strlen(curr_dir->dirname);
while (len >= 1 && curr_dir->dirname[len - 1] == '/') {
len--;
}
curr_dir->dirname[len++] = '/';
curr_dir->dirname[len] = '\0';
curr_dir->where = 0;
errno = 0;
do {
if ((dirp = opendir(curr_dir->dirname)) == (DIR *)NULL) {
warn(curr_dir->dirname, "error opening directory (2)");
if (in_subdir > 1) {
curr_dir = popdir();
}
in_subdir--;
err = -1;
continue;
} else {
seekdir(dirp, curr_dir->where);
}
} while (in_subdir && (! dirp));
}
} while (err < 0);
return (0);
}
#ifdef __STDC__
void name_gather(void)
#else
void name_gather()
#endif
{
while (optind < n_argc) {
add_name(n_argv[optind++]);
}
}
#ifdef __STDC__
static void pushdir(struct dirinfo *info)
#else
static void pushdir(info)
struct dirinfo	*info;
#endif
{
if  (stack_head == (struct dirinfo *)NULL) {
stack_head = info;
stack_head->next = (struct dirinfo *)NULL;
} else {
info->next = stack_head;
stack_head = info;
}
}
#ifdef __STDC__
static struct dirinfo *popdir(void)
#else
static struct dirinfo *popdir()
#endif
{
struct dirinfo	*tmp;
if (stack_head == (struct dirinfo *)NULL) {
return((struct dirinfo *)NULL);
} else {
tmp = stack_head;
stack_head = stack_head->next;
}
return(tmp);
}