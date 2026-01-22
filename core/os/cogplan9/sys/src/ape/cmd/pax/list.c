#ifndef lint
static char *ident = "$Id: list.c,v 1.2 89/02/12 10:04:43 mark Exp $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#define ISODIGIT(c) (((c) >= '0') && ((c) <= '7'))
#ifdef __STDC__
static void cpio_entry(char *, Stat *);
static void tar_entry(char *, Stat *);
static void pax_entry(char *, Stat *);
static void print_mode(ushort);
static long from_oct(int digs, char *where);
#else
static void cpio_entry();
static void tar_entry();
static void pax_entry();
static void print_mode();
static long from_oct();
#endif
static char *monnames[] = {
"Jan", "Feb", "Mar", "Apr", "May", "Jun",
"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
};
#ifdef __STDC__
int read_header(char *name, Stat *asb)
#else
int read_header(name, asb)
char *name;
Stat *asb;
#endif
{
int i;
long sum;
long recsum;
Link *link;
char *p;
char hdrbuf[BLOCKSIZE];
memset((char *)asb, 0, sizeof(Stat));
if (buf_read(hdrbuf, BLOCKSIZE) != 0) {
return (EOF);
}
strcpy(name, hdrbuf);
recsum = from_oct(8, &hdrbuf[148]);
sum = 0;
p = hdrbuf;
for (i = 0 ; i < 500; i++) {
sum += 0xFF & *p++;
}
for (i = 0; i < 8; i++) {
sum -= 0xFF & hdrbuf[148 + i];
}
sum += ' ' * 8;
if (sum == 8 * ' ') {
return (2);
}
if (sum == recsum) {
if (hdrbuf[156] != LNKTYPE) {
asb->sb_size = from_oct(1 + 12, &hdrbuf[124]);
}
asb->sb_mtime = from_oct(1 + 12, &hdrbuf[136]);
asb->sb_mode = from_oct(8, &hdrbuf[100]);
if (strcmp(&hdrbuf[257], TMAGIC) == 0) {
head_standard = 1;
#ifdef NONAMES
asb->sb_uid = from_oct(8, &hdrbuf[108]);
asb->sb_gid = from_oct(8, &hdrbuf[116]);
#else
asb->sb_uid = finduid(&hdrbuf[265]);
asb->sb_gid = findgid(&hdrbuf[297]);
#endif
switch (hdrbuf[156]) {
case BLKTYPE:
case CHRTYPE:
#ifndef _POSIX_SOURCE
asb->sb_rdev = makedev(from_oct(8, &hdrbuf[329]),
from_oct(8, &hdrbuf[337]));
#endif
break;
default:
break;
}
} else {
head_standard = 0;
asb->sb_uid = from_oct(8, &hdrbuf[108]);
asb->sb_gid = from_oct(8, &hdrbuf[116]);
}
switch (hdrbuf[156]) {
case REGTYPE:
case AREGTYPE:
if (name[strlen(name) - 1] == '/') {
name[strlen(name) - 1] = '\0';
asb->sb_mode |= S_IFDIR;
} else {
asb->sb_mode |= S_IFREG;
}
break;
case LNKTYPE:
asb->sb_nlink = 2;
linkto(&hdrbuf[157], asb);
linkto(name, asb);
asb->sb_mode |= S_IFREG;
break;
case BLKTYPE:
asb->sb_mode |= S_IFBLK;
break;
case CHRTYPE:
asb->sb_mode |= S_IFCHR;
break;
case DIRTYPE:
asb->sb_mode |= S_IFDIR;
break;
#ifdef S_IFLNK
case SYMTYPE:
asb->sb_mode |= S_IFLNK;
strcpy(asb->sb_link, &hdrbuf[157]);
break;
#endif
#ifdef S_IFIFO
case FIFOTYPE:
asb->sb_mode |= S_IFIFO;
break;
#endif
#ifdef S_IFCTG
case CONTTYPE:
asb->sb_mode |= S_IFCTG;
break;
#endif
}
return (1);
}
return (0);
}
#ifdef __STDC__
void print_entry(char *name, Stat *asb)
#else
void print_entry(name, asb)
char *name;
Stat *asb;
#endif
{
switch (ar_interface) {
case TAR:
tar_entry(name, asb);
break;
case CPIO:
cpio_entry(name, asb);
break;
case PAX: pax_entry(name, asb);
break;
}
}
#ifdef __STDC__
static void cpio_entry(char *name, Stat *asb)
#else
static void cpio_entry(name, asb)
char *name;
Stat *asb;
#endif
{
struct tm *atm;
Link *from;
struct passwd *pwp;
struct group *grp;
if (f_list && f_verbose) {
fprintf(msgfile, "%-7o", asb->sb_mode);
atm = localtime(&asb->sb_mtime);
if (pwp = getpwuid((int) USH(asb->sb_uid))) {
fprintf(msgfile, "%-6s", pwp->pw_name);
} else {
fprintf(msgfile, "%-6u", USH(asb->sb_uid));
}
fprintf(msgfile,"%7ld  %3s %2d %02d:%02d:%02d %4d  ",
asb->sb_size, monnames[atm->tm_mon],
atm->tm_mday, atm->tm_hour, atm->tm_min,
atm->tm_sec, atm->tm_year + 1900);
}
fprintf(msgfile, "%s", name);
if ((asb->sb_nlink > 1) && (from = islink(name, asb))) {
fprintf(msgfile, " linked to %s", from->l_name);
}
#ifdef S_IFLNK
if ((asb->sb_mode & S_IFMT) == S_IFLNK) {
fprintf(msgfile, " symbolic link to %s", asb->sb_link);
}
#endif
putc('\n', msgfile);
}
#ifdef __STDC__
static void tar_entry(char *name, Stat *asb)
#else
static void tar_entry(name, asb)
char *name;
Stat *asb;
#endif
{
struct tm *atm;
int i;
int mode;
char *symnam = "NULL";
Link *link;
if ((mode = asb->sb_mode & S_IFMT) == S_IFDIR) {
return;
}
if (f_extract) {
switch (mode) {
#ifdef S_IFLNK
case S_IFLNK:
i = readlink(name, symnam, PATH_MAX - 1);
if (i < 0) {
warn("can't read symbolic link", strerror());
} else {
symnam[i] = '\0';
fprintf(msgfile, "x %s symbolic link to %s\n", name, symnam);
}
break;
#endif
case S_IFREG:
if ((asb->sb_nlink > 1) && (link = islink(name, asb))) {
fprintf(msgfile, "%s linked to %s\n", name, link->l_name);
} else {
fprintf(msgfile, "x %s, %ld bytes, %d tape blocks\n",
name, asb->sb_size, ROUNDUP(asb->sb_size,
BLOCKSIZE) / BLOCKSIZE);
}
}
} else if (f_append || f_create) {
switch (mode) {
#ifdef S_IFLNK
case S_IFLNK:
i = readlink(name, symnam, PATH_MAX - 1);
if (i < 0) {
warn("can't read symbolic link", strerror());
} else {
symnam[i] = '\0';
fprintf(msgfile, "a %s symbolic link to %s\n", name, symnam);
}
break;
#endif
case S_IFREG:
fprintf(msgfile, "a %s ", name);
if ((asb->sb_nlink > 1) && (link = islink(name, asb))) {
fprintf(msgfile, "link to %s\n", link->l_name);
} else {
fprintf(msgfile, "%ld Blocks\n",
ROUNDUP(asb->sb_size, BLOCKSIZE) / BLOCKSIZE);
}
break;
}
} else if (f_list) {
if (f_verbose) {
atm = localtime(&asb->sb_mtime);
print_mode(asb->sb_mode);
fprintf(msgfile," %d/%d %6d %3s %2d %02d:%02d %4d %s",
asb->sb_uid, asb->sb_gid, asb->sb_size,
monnames[atm->tm_mon], atm->tm_mday, atm->tm_hour,
atm->tm_min, atm->tm_year + 1900, name);
} else {
fprintf(msgfile, "%s", name);
}
switch (mode) {
#ifdef S_IFLNK
case S_IFLNK:
i = readlink(name, symnam, PATH_MAX - 1);
if (i < 0) {
warn("can't read symbolic link", strerror());
} else {
symnam[i] = '\0';
fprintf(msgfile, " symbolic link to %s", symnam);
}
break;
#endif
case S_IFREG:
if ((asb->sb_nlink > 1) && (link = islink(name, asb))) {
fprintf(msgfile, " linked to %s", link->l_name);
}
break;
}
fputc('\n', msgfile);
} else {
fprintf(msgfile, "? %s %ld blocks\n", name,
ROUNDUP(asb->sb_size, BLOCKSIZE) / BLOCKSIZE);
}
}
#ifdef __STDC__
static void pax_entry(char *name, Stat *asb)
#else
static void pax_entry(name, asb)
char *name;
Stat *asb;
#endif
{
struct tm *atm;
Link *from;
struct passwd *pwp;
struct group *grp;
if (f_list && f_verbose) {
print_mode(asb->sb_mode);
fprintf(msgfile, " %2d", asb->sb_nlink);
atm = localtime(&asb->sb_mtime);
if (pwp = getpwuid((int) USH(asb->sb_uid))) {
fprintf(msgfile, " %-8s", pwp->pw_name);
} else {
fprintf(msgfile, " %-8u", USH(asb->sb_uid));
}
if (grp = getgrgid((int) USH(asb->sb_gid))) {
fprintf(msgfile, " %-8s", grp->gr_name);
} else {
fprintf(msgfile, " %-8u", USH(asb->sb_gid));
}
switch (asb->sb_mode & S_IFMT) {
case S_IFBLK:
case S_IFCHR:
fprintf(msgfile, "\t%3d, %3d",
major(asb->sb_rdev), minor(asb->sb_rdev));
break;
case S_IFREG:
fprintf(msgfile, "\t%8ld", asb->sb_size);
break;
default:
fprintf(msgfile, "\t        ");
}
fprintf(msgfile," %3s %2d %02d:%02d ",
monnames[atm->tm_mon], atm->tm_mday,
atm->tm_hour, atm->tm_min);
}
fprintf(msgfile, "%s", name);
if ((asb->sb_nlink > 1) && (from = islink(name, asb))) {
fprintf(msgfile, " == %s", from->l_name);
}
#ifdef S_IFLNK
if ((asb->sb_mode & S_IFMT) == S_IFLNK) {
fprintf(msgfile, " -> %s", asb->sb_link);
}
#endif
putc('\n', msgfile);
}
#ifdef __STDC__
static void print_mode(ushort mode)
#else
static void print_mode(mode)
ushort mode;
#endif
{
if (ar_interface != TAR) {
switch (mode & S_IFMT) {
case S_IFDIR:
putc('d', msgfile);
break;
#ifdef S_IFLNK
case S_IFLNK:
putc('l', msgfile);
break;
#endif
case S_IFBLK:
putc('b', msgfile);
break;
case S_IFCHR:
putc('c', msgfile);
break;
#ifdef S_IFIFO
case S_IFIFO:
putc('p', msgfile);
break;
#endif
case S_IFREG:
default:
putc('-', msgfile);
break;
}
}
putc(mode & 0400 ? 'r' : '-', msgfile);
putc(mode & 0200 ? 'w' : '-', msgfile);
putc(mode & 0100
? mode & 04000 ? 's' : 'x'
: mode & 04000 ? 'S' : '-', msgfile);
putc(mode & 0040 ? 'r' : '-', msgfile);
putc(mode & 0020 ? 'w' : '-', msgfile);
putc(mode & 0010
? mode & 02000 ? 's' : 'x'
: mode & 02000 ? 'S' : '-', msgfile);
putc(mode & 0004 ? 'r' : '-', msgfile);
putc(mode & 0002 ? 'w' : '-', msgfile);
putc(mode & 0001
? mode & 01000 ? 't' : 'x'
: mode & 01000 ? 'T' : '-', msgfile);
}
#ifdef __STDC__
static long from_oct(int digs, char *where)
#else
static long from_oct(digs, where)
int digs;
char *where;
#endif
{
long value;
while (isspace(*where)) {
where++;
if (--digs <= 0) {
return(-1);
}
}
value = 0;
while (digs > 0 && ISODIGIT(*where)) {
value = (value << 3) | (*where++ - '0');
--digs;
}
if (digs > 0 && *where && !isspace(*where)) {
return(-1);
}
return(value);
}