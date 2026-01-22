#ifndef lint
static char *ident = "$Id: create.c,v 1.3 89/02/12 10:29:37 mark Exp Locker: mark $";
static char *copyright = "Copyright (c) 1989 Mark H. Colburn.\nAll rights reserved.\n";
#endif
#include "pax.h"
#ifdef __STDC__
static void writetar(char *, Stat *);
static void writecpio(char *, Stat *);
static char tartype(int);
#else
static void writetar();
static void writecpio();
static char tartype();
#endif
#ifdef __STDC__
int create_archive(void)
#else
int create_archive()
#endif
{
char name[PATH_MAX + 1];
Stat sb;
int fd;
while (name_next(name, &sb) != -1) {
if ((fd = openin(name, &sb)) < 0) {
continue;
}
if (rplhead != (Replstr *)NULL) {
rpl_name(name);
if (strlen(name) == 0) {
continue;
}
}
if (get_disposition("add", name) || get_newname(name, sizeof(name))) {
if (fd) {
close(fd);
}
continue;
}
if (!f_link && sb.sb_nlink > 1) {
if (islink(name, &sb)) {
sb.sb_size = 0;
}
linkto(name, &sb);
}
if (ar_format == TAR) {
writetar(name, &sb);
} else {
writecpio(name, &sb);
}
if (fd) {
outdata(fd, name, sb.sb_size);
}
if (f_verbose) {
print_entry(name, &sb);
}
}
write_eot();
close_archive();
return (0);
}
#ifdef __STDC__
static void writetar(char *name, Stat *asb)
#else
static void writetar(name, asb)
char *name;
Stat *asb;
#endif
{
char *p;
char *prefix = (char *)NULL;
int i;
int sum;
char hdr[BLOCKSIZE];
Link *from;
memset(hdr, 0, BLOCKSIZE);
if (strlen(name) > 255) {
warn(name, "name too long");
return;
}
if (strlen(name) > 100) {
prefix = name;
name += 155;
while (name > prefix && *name != '/') {
name--;
}
if (name == prefix) {
warn(prefix, "Name too long");
return;
}
*name++ = '\0';
}
#ifdef S_IFLNK
if ((asb->sb_mode & S_IFMT) == S_IFLNK) {
strcpy(&hdr[157], asb->sb_link);
asb->sb_size = 0;
}
#endif
strcpy(hdr, name);
sprintf(&hdr[100], "%06o \0", asb->sb_mode & ~S_IFMT);
sprintf(&hdr[108], "%06o \0", asb->sb_uid);
sprintf(&hdr[116], "%06o \0", asb->sb_gid);
sprintf(&hdr[124], "%011lo ", (long) asb->sb_size);
sprintf(&hdr[136], "%011lo ", (long) asb->sb_mtime);
strncpy(&hdr[148], "        ", 8);
hdr[156] = tartype(asb->sb_mode);
if (asb->sb_nlink > 1 && (from = linkfrom(name, asb)) != (Link *)NULL) {
strcpy(&hdr[157], from->l_name);
hdr[156] = LNKTYPE;
}
strcpy(&hdr[257], TMAGIC);
strncpy(&hdr[263], TVERSION, 2);
strcpy(&hdr[265], finduname((int) asb->sb_uid));
strcpy(&hdr[297], findgname((int) asb->sb_gid));
sprintf(&hdr[329], "%06o \0", major(asb->sb_rdev));
sprintf(&hdr[337], "%06o \0", minor(asb->sb_rdev));
if (prefix != (char *)NULL) {
strncpy(&hdr[345], prefix, 155);
}
sum = 0;
p = hdr;
for (i = 0; i < 500; i++) {
sum += 0xFF & *p++;
}
sprintf(&hdr[148], "%06o \0", sum);
outwrite(hdr, BLOCKSIZE);
}
#ifdef __STDC__
static char tartype(int mode)
#else
static char tartype(mode)
int mode;
#endif
{
switch (mode & S_IFMT) {
#ifdef S_IFCTG
case S_IFCTG:
return(CONTTYPE);
#endif
case S_IFDIR:
return (DIRTYPE);
#ifdef S_IFLNK
case S_IFLNK:
return (SYMTYPE);
#endif
#ifdef S_IFFIFO
case S_IFIFO:
return (FIFOTYPE);
#endif
#ifdef S_IFCHR
case S_IFCHR:
return (CHRTYPE);
#endif
#ifdef S_IFBLK
case S_IFBLK:
return (BLKTYPE);
#endif
default:
return (REGTYPE);
}
}
#ifdef __STDC__
static void writecpio(char *name, Stat *asb)
#else
static void writecpio(name, asb)
char *name;
Stat *asb;
#endif
{
uint namelen;
char header[M_STRLEN + H_STRLEN + 1];
namelen = (uint) strlen(name) + 1;
strcpy(header, M_ASCII);
sprintf(header + M_STRLEN, "%06o%06o%06o%06o%06o",
USH(asb->sb_dev), USH(asb->sb_ino), USH(asb->sb_mode),
USH(asb->sb_uid), USH(asb->sb_gid));
sprintf(header + M_STRLEN + 30, "%06o%06o%011lo%06o%011lo",
#ifdef _POSIX_SOURCE
USH(asb->sb_nlink), USH(0),
#else
USH(asb->sb_nlink), USH(asb->sb_rdev),
#endif
f_mtime ? asb->sb_mtime : time((time_t *) 0),
namelen, asb->sb_size);
outwrite(header, M_STRLEN + H_STRLEN);
outwrite(name, namelen);
#ifdef S_IFLNK
if ((asb->sb_mode & S_IFMT) == S_IFLNK) {
outwrite(asb->sb_link, (uint) asb->sb_size);
}
#endif
}