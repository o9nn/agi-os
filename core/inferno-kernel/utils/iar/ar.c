#include <lib9.h>
#include <bio.h>
#include <mach.h>
#include <ar.h>
typedef struct Arsymref
{
char *name;
int type;
int len;
long offset;
struct Arsymref *next;
} Arsymref;
typedef struct Armember
{
struct Armember *next;
struct ar_hdr hdr;
long size;
long date;
void *member;
} Armember;
typedef struct Arfile
{
int paged;
char *fname;
int fd;
long size;
Armember *head;
Armember *tail;
Arsymref *sym;
} Arfile;
typedef struct Hashchain
{
char *name;
struct Hashchain *next;
} Hashchain;
#define NHASH 1024
#define HEADER_IO(cmd, f, h) cmd(f, h.name, sizeof(h.name)) != sizeof(h.name)\
|| cmd(f, h.date, sizeof(h.date)) != sizeof(h.date)\
|| cmd(f, h.uid, sizeof(h.uid)) != sizeof(h.uid)\
|| cmd(f, h.gid, sizeof(h.gid)) != sizeof(h.gid)\
|| cmd(f, h.mode, sizeof(h.mode)) != sizeof(h.mode)\
|| cmd(f, h.size, sizeof(h.size)) != sizeof(h.size)\
|| cmd(f, h.fmag, sizeof(h.fmag)) != sizeof(h.fmag)
char *man = "mrxtdpq";
char *opt = "uvnbailo";
char artemp[] = "/tmp/vXXXXX";
char movtemp[] = "/tmp/v1XXXXX";
char tailtemp[] = "/tmp/v2XXXXX";
char symdef[] = "__.SYMDEF";
int aflag;
int bflag;
int cflag;
int oflag;
int uflag;
int vflag;
Arfile *astart, *amiddle, *aend;
int allobj = 1;
int symdefsize;
int dupfound;
Hashchain *hash[NHASH];
#define ARNAMESIZE sizeof(astart->tail->hdr.name)
char poname[ARNAMESIZE+1];
char *file;
Biobuf bout;
Biobuf bar;
void arcopy(Biobuf*, Arfile*, Armember*);
int arcreate(char*);
void arfree(Arfile*);
void arinsert(Arfile*, Armember*);
char *armalloc(int);
void armove(Biobuf*, Arfile*, Armember*);
void arread(Biobuf*, Armember*, int);
void arstream(int, Arfile*);
int arwrite(int, Armember*);
int bamatch(char*, char*);
int duplicate(char*);
Armember *getdir(Biobuf*);
int getspace(void);
void install(char*, Arfile*, Arfile*, Arfile*, int);
void longt(Armember*);
int match(int, char**);
void mesg(int, char*);
char *myctime(long);
Arfile *newtempfile(char*);
Armember *newmember(void);
void objsym(Sym*, void*);
int openar(char*, int, int);
int page(Arfile*);
void pmode(long);
void rl(int);
void scanobj(Biobuf*, Arfile*, int);
void ar_select(int*, long);
void setcom(void(*)(char*, int, char**));
void skip(Biobuf*, long);
int symcomp(void*, void*);
void trim(char*, char*, int);
void usage(void);
void wrerr(void);
void wrsym(Biobuf*, int, Arsymref*);
void arcmd(char*, int, char**);
void dcmd(char*, int, char**);
void xcmd(char*, int, char**);
void tcmd(char*, int, char**);
void pcmd(char*, int, char**);
void mcmd(char*, int, char**);
void qcmd(char*, int, char**);
void (*comfun)(char*, int, char**);
void
main(int argc, char *argv[])
{
char *cp;
Binit(&bout, 1, OWRITE);
if(argc < 3)
usage();
for (cp = argv[1]; *cp; cp++) {
switch(*cp) {
case 'a': aflag = 1; break;
case 'b': bflag = 1; break;
case 'c': cflag = 1; break;
case 'd': setcom(dcmd); break;
case 'i': bflag = 1; break;
case 'l':
strcpy(artemp, "vXXXXX");
strcpy(movtemp, "v1XXXXX");
strcpy(tailtemp, "v2XXXXX");
break;
case 'm': setcom(mcmd); break;
case 'o': oflag = 1; break;
case 'p': setcom(pcmd); break;
case 'q': setcom(qcmd); break;
case 'r': setcom(arcmd); break;
case 't': setcom(tcmd); break;
case 'u': uflag = 1; break;
case 'v': vflag = 1; break;
case 'x': setcom(xcmd); break;
default:
fprint(2, "ar: bad option `%c'\n", *cp);
exits("error");
}
}
if (aflag && bflag) {
fprint(2, "ar: only one of 'a' and 'b' can be specified\n");
usage();
}
if(aflag || bflag) {
trim(argv[2], poname, sizeof(poname));
argv++;
argc--;
if(argc < 3)
usage();
}
if(comfun == 0) {
if(uflag == 0) {
fprint(2, "ar: one of [%s] must be specified\n", man);
usage();
}
setcom(arcmd);
}
cp = argv[2];
argc -= 3;
argv += 3;
(*comfun)(cp, argc, argv);
cp = 0;
while (argc--) {
if (*argv) {
fprint(2, "ar: %s not found\n", *argv);
cp = "error";
}
argv++;
}
if(allobj && dupfound)
exits("dup found");
exits(cp);
}
void
setcom(void (*fun)(char *, int, char**))
{
if(comfun != 0) {
fprint(2, "ar: only one of [%s] allowed\n", man);
usage();
}
comfun = fun;
}
void
arcmd(char *arname, int count, char **files)
{
int fd;
int i;
Arfile *ap;
Armember *bp;
Dir *d;
Biobuf *bfile;
fd = openar(arname, ORDWR, 1);
if (fd >= 0) {
Binit(&bar, fd, OREAD);
Bseek(&bar,seek(fd,0,1), 1);
}
astart = newtempfile(artemp);
ap = astart;
aend = 0;
for(i = 0; fd >= 0; i++) {
bp = getdir(&bar);
if (!bp)
break;
if (bamatch(file, poname)) {
aend = newtempfile(tailtemp);
ap = aend;
}
if (i == 0 && strcmp(file, symdef) == 0) {
skip(&bar, bp->size);
continue;
}
if (count && !match(count, files)) {
scanobj(&bar, ap, bp->size);
arcopy(&bar, ap, bp);
continue;
}
bfile = Bopen(file, OREAD);
if (!bfile) {
if (count != 0)
fprint(2, "ar: cannot open %s\n", file);
scanobj(&bar, ap, bp->size);
arcopy(&bar, ap, bp);
continue;
}
d = dirfstat(Bfildes(bfile));
if (d == nil)
fprint(2, "ar: cannot stat %s: %r\n", file);
if (uflag && (d == nil || d->mtime <= bp->date)) {
scanobj(&bar, ap, bp->size);
arcopy(&bar, ap, bp);
Bterm(bfile);
free(d);
continue;
}
mesg('r', file);
skip(&bar, bp->size);
scanobj(bfile, ap, d->length);
free(d);
armove(bfile, ap, bp);
Bterm(bfile);
}
if(fd >= 0)
close(fd);
for (i = 0; i < count; i++) {
file = files[i];
if(file == 0)
continue;
files[i] = 0;
bfile = Bopen(file, OREAD);
if (!bfile)
fprint(2, "ar: %s cannot open\n", file);
else {
mesg('a', file);
d = dirfstat(Bfildes(bfile));
if (d == nil)
fprint(2, "ar: can't stat %s: %r\n", file);
else {
scanobj(bfile, astart, d->length);
armove(bfile, astart, newmember());
free(d);
}
Bterm(bfile);
}
}
if(fd < 0 && !cflag)
install(arname, astart, 0, aend, 1);
else
install(arname, astart, 0, aend, 0);
}
void
dcmd(char *arname, int count, char **files)
{
Armember *bp;
int fd, i;
if (!count)
return;
fd = openar(arname, ORDWR, 0);
Binit(&bar, fd, OREAD);
Bseek(&bar,seek(fd,0,1), 1);
astart = newtempfile(artemp);
for (i = 0; bp = getdir(&bar); i++) {
if(match(count, files)) {
mesg('d', file);
skip(&bar, bp->size);
if (strcmp(file, symdef) == 0)
allobj = 0;
} else if (i == 0 && strcmp(file, symdef) == 0)
skip(&bar, bp->size);
else {
scanobj(&bar, astart, bp->size);
arcopy(&bar, astart, bp);
}
}
close(fd);
install(arname, astart, 0, 0, 0);
}
void
xcmd(char *arname, int count, char **files)
{
int fd, f, mode, i;
Armember *bp;
Dir dx;
fd = openar(arname, OREAD, 0);
Binit(&bar, fd, OREAD);
Bseek(&bar,seek(fd,0,1), 1);
i = 0;
while (bp = getdir(&bar)) {
if(count == 0 || match(count, files)) {
mode = strtoul(bp->hdr.mode, 0, 8) & 0777;
f = create(file, OWRITE, mode);
if(f < 0) {
fprint(2, "ar: %s cannot create\n", file);
skip(&bar, bp->size);
} else {
mesg('x', file);
arcopy(&bar, 0, bp);
if (write(f, bp->member, bp->size) < 0)
wrerr();
if(oflag) {
nulldir(&dx);
dx.atime = bp->date;
dx.mtime = bp->date;
if(dirwstat(file, &dx) < 0)
perror(file);
}
free(bp->member);
close(f);
}
free(bp);
if (count && ++i >= count)
break;
} else {
skip(&bar, bp->size);
free(bp);
}
}
close(fd);
}
void
pcmd(char *arname, int count, char **files)
{
int fd;
Armember *bp;
fd = openar(arname, OREAD, 0);
Binit(&bar, fd, OREAD);
Bseek(&bar,seek(fd,0,1), 1);
while(bp = getdir(&bar)) {
if(count == 0 || match(count, files)) {
if(vflag)
print("\n<%s>\n\n", file);
arcopy(&bar, 0, bp);
if (write(1, bp->member, bp->size) < 0)
wrerr();
} else
skip(&bar, bp->size);
free(bp);
}
close(fd);
}
void
mcmd(char *arname, int count, char **files)
{
int fd, i;
Arfile *ap;
Armember *bp;
if (count == 0)
return;
fd = openar(arname, ORDWR, 0);
Binit(&bar, fd, OREAD);
Bseek(&bar,seek(fd,0,1), 1);
astart = newtempfile(artemp);
amiddle = newtempfile(movtemp);
aend = 0;
ap = astart;
for (i = 0; bp = getdir(&bar); i++) {
if (bamatch(file, poname)) {
aend = newtempfile(tailtemp);
ap = aend;
}
if(match(count, files)) {
mesg('m', file);
scanobj(&bar, amiddle, bp->size);
arcopy(&bar, amiddle, bp);
} else
if (ap == astart && i == 0 && strcmp(file, symdef) == 0)
skip(&bar, bp->size);
else {
scanobj(&bar, ap, bp->size);
arcopy(&bar, ap, bp);
}
}
close(fd);
if (poname[0] && aend == 0)
fprint(2, "ar: %s not found - files moved to end.\n", poname);
install(arname, astart, amiddle, aend, 0);
}
void
tcmd(char *arname, int count, char **files)
{
int fd;
Armember *bp;
char name[ARNAMESIZE+1];
fd = openar(arname, OREAD, 0);
Binit(&bar, fd, OREAD);
Bseek(&bar,seek(fd,0,1), 1);
while(bp = getdir(&bar)) {
if(count == 0 || match(count, files)) {
if(vflag)
longt(bp);
trim(file, name, ARNAMESIZE);
Bprint(&bout, "%s\n", name);
}
skip(&bar, bp->size);
free(bp);
}
close(fd);
}
void
qcmd(char *arname, int count, char **files)
{
int fd, i;
Armember *bp;
Biobuf *bfile;
if(aflag || bflag) {
fprint(2, "ar: abi not allowed with q\n");
exits("error");
}
fd = openar(arname, ORDWR, 1);
if (fd < 0) {
if(!cflag)
fprint(2, "ar: creating %s\n", arname);
fd = arcreate(arname);
}
Binit(&bar, fd, OREAD);
Bseek(&bar,seek(fd,0,1), 1);
Bseek(&bar, 0, 2);
bp = newmember();
for(i=0; i<count && files[i]; i++) {
file = files[i];
files[i] = 0;
bfile = Bopen(file, OREAD);
if(!bfile)
fprint(2, "ar: %s cannot open\n", file);
else {
mesg('q', file);
armove(bfile, 0, bp);
if (!arwrite(fd, bp))
wrerr();
free(bp->member);
bp->member = 0;
Bterm(bfile);
}
}
free(bp);
close(fd);
}
void
scanobj(Biobuf *b, Arfile *ap, int size)
{
int obj;
long offset;
Dir *d;
static int lastobj = -1;
if (!allobj)
return;
offset = Boffset(b);
obj = objtype(b, 0);
if (obj < 0) {
allobj = 0;
d = dirfstat(Bfildes(b));
if (d != nil && d->length == 0)
fprint(2, "ar: zero length file %s\n", file);
free(d);
Bseek(b, offset, 0);
return;
}
if (lastobj >= 0 && obj != lastobj) {
fprint(2, "ar: inconsistent object file %s\n", file);
allobj = 0;
Bseek(b, offset, 0);
return;
}
lastobj = obj;
if (!readar(b, obj, offset+size, 0)) {
fprint(2, "ar: invalid symbol reference in file %s\n", file);
allobj = 0;
Bseek(b, offset, 0);
return;
}
Bseek(b, offset, 0);
objtraverse(objsym, ap);
}
void
objsym(Sym *s, void *p)
{
int n;
Arsymref *as;
Arfile *ap;
if (s->type != 'T' && s->type != 'D')
return;
ap = (Arfile*)p;
as = (Arsymref*)armalloc(sizeof(Arsymref));
as->offset = ap->size;
n = strlen(s->name);
as->name = armalloc(n+1);
strcpy(as->name, s->name);
if(s->type == 'T' && duplicate(as->name)) {
dupfound = 1;
fprint(2, "duplicate text symbol: %s\n", as->name);
free(as->name);
free(as);
return;
}
as->type = s->type;
symdefsize += 4+(n+1)+1;
as->len = n;
as->next = ap->sym;
ap->sym = as;
}
int
duplicate(char *name)
{
Hashchain *p;
char *cp;
int h;
h = 0;
for(cp = name; *cp; h += *cp++)
h *= 1119;
if(h < 0)
h = ~h;
h %= NHASH;
for(p = hash[h]; p; p = p->next)
if(strcmp(p->name, name) == 0)
return 1;
p = (Hashchain*) armalloc(sizeof(Hashchain));
p->next = hash[h];
p->name = name;
hash[h] = p;
return 0;
}
int
openar(char *arname, int mode, int errok)
{
int fd;
char mbuf[SARMAG];
fd = open(arname, mode);
if(fd >= 0){
if(read(fd, mbuf, SARMAG) != SARMAG || strncmp(mbuf, ARMAG, SARMAG)) {
fprint(2, "ar: %s not in archive format\n", arname);
exits("error");
}
}else if(!errok){
fprint(2, "ar: cannot open %s: %r\n", arname);
exits("error");
}
return fd;
}
int
arcreate(char *arname)
{
int fd;
fd = create(arname, OWRITE, 0664);
if(fd < 0){
fprint(2, "ar: cannot create %s: %r\n", arname);
exits("error");
}
if(write(fd, ARMAG, SARMAG) != SARMAG)
wrerr();
return fd;
}
void
wrerr(void)
{
perror("ar: write error");
exits("error");
}
void
rderr(void)
{
perror("ar: read error");
exits("error");
}
void
phaseerr(int offset)
{
fprint(2, "ar: phase error at offset %d\n", offset);
exits("error");
}
void
usage(void)
{
fprint(2, "usage: ar [%s][%s] archive files ...\n", opt, man);
exits("error");
}
Armember *
getdir(Biobuf *b)
{
Armember *bp;
char *cp;
static char name[ARNAMESIZE+1];
bp = newmember();
if(HEADER_IO(Bread, b, bp->hdr)) {
free(bp);
return 0;
}
if(strncmp(bp->hdr.fmag, ARFMAG, sizeof(bp->hdr.fmag)))
phaseerr(Boffset(b));
strncpy(name, bp->hdr.name, sizeof(bp->hdr.name));
cp = name+sizeof(name)-1;
while(*--cp==' ')
;
cp[1] = '\0';
file = name;
bp->date = atol(bp->hdr.date);
bp->size = atol(bp->hdr.size);
return bp;
}
void
armove(Biobuf *b, Arfile *ap, Armember *bp)
{
char *cp;
Dir *d;
if ((d = dirfstat(Bfildes(b))) == nil) {
fprint(2, "ar: cannot stat %s: %r\n", file);
return;
}
trim(file, bp->hdr.name, sizeof(bp->hdr.name));
for (cp = strchr(bp->hdr.name, 0);
cp < bp->hdr.name+sizeof(bp->hdr.name); cp++)
*cp = ' ';
sprint(bp->hdr.date, "%-12ld", d->mtime);
sprint(bp->hdr.uid, "%-6d", 0);
sprint(bp->hdr.gid, "%-6d", 0);
sprint(bp->hdr.mode, "%-8lo", d->mode);
sprint(bp->hdr.size, "%-10lld", (vlong)d->length);
strncpy(bp->hdr.fmag, ARFMAG, 2);
bp->size = d->length;
bp->date = d->mtime;
arread(b, bp, bp->size);
if (d->length&0x01)
d->length++;
if (ap) {
arinsert(ap, bp);
ap->size += d->length+SAR_HDR;
}
free(d);
}
void
arcopy(Biobuf *b, Arfile *ap, Armember *bp)
{
int n;
n = bp->size;
if (n & 01)
n++;
arread(b, bp, n);
if (ap) {
arinsert(ap, bp);
ap->size += n+SAR_HDR;
}
}
void
skip(Biobuf *bp, long len)
{
if (len & 01)
len++;
Bseek(bp, len, 1);
}
void
install(char *arname, Arfile *astart, Arfile *amiddle, Arfile *aend, int createflag)
{
int fd;
if(allobj && dupfound) {
fprint(2, "%s not changed\n", arname);
return;
}
if(createflag)
fprint(2, "ar: creating %s\n", arname);
fd = arcreate(arname);
if(allobj)
rl(fd);
if (astart) {
arstream(fd, astart);
arfree(astart);
}
if (amiddle) {
arstream(fd, amiddle);
arfree(amiddle);
}
if (aend) {
arstream(fd, aend);
arfree(aend);
}
close(fd);
}
void
rl(int fd)
{
Biobuf b;
char *cp;
struct ar_hdr a;
long len;
Binit(&b, fd, OWRITE);
Bseek(&b,seek(fd,0,1), 0);
len = symdefsize;
if(len&01)
len++;
sprint(a.date, "%-12ld", time(0));
sprint(a.uid, "%-6d", 0);
sprint(a.gid, "%-6d", 0);
sprint(a.mode, "%-8o", 0644);
sprint(a.size, "%-10ld", len);
strncpy(a.fmag, ARFMAG, 2);
strcpy(a.name, symdef);
for (cp = strchr(a.name, 0);
cp < a.name+sizeof(a.name); cp++)
*cp = ' ';
if(HEADER_IO(Bwrite, &b, a))
wrerr();
len += Boffset(&b);
if (astart) {
wrsym(&b, len, astart->sym);
len += astart->size;
}
if(amiddle) {
wrsym(&b, len, amiddle->sym);
len += amiddle->size;
}
if(aend)
wrsym(&b, len, aend->sym);
if(symdefsize&0x01)
Bputc(&b, 0);
Bterm(&b);
}
void
wrsym(Biobuf *bp, int offset, Arsymref *as)
{
int off;
while(as) {
Bputc(bp, as->type);
off = as->offset+offset;
Bputc(bp, off);
Bputc(bp, off>>8);
Bputc(bp, off>>16);
Bputc(bp, off>>24);
if (Bwrite(bp, as->name, as->len+1) != as->len+1)
wrerr();
as = as->next;
}
}
int
match(int count, char **files)
{
int i;
char name[ARNAMESIZE+1];
for(i=0; i<count; i++) {
if(files[i] == 0)
continue;
trim(files[i], name, ARNAMESIZE);
if(strncmp(name, file, ARNAMESIZE) == 0) {
file = files[i];
files[i] = 0;
return 1;
}
}
return 0;
}
int
bamatch(char *file, char *pivot)
{
static int state = 0;
switch(state)
{
case 0:
if (aflag) {
if (strncmp(file, pivot, ARNAMESIZE) == 0)
state = 1;
} else if (bflag) {
if (strncmp(file, pivot, ARNAMESIZE) == 0) {
state = 2;
return 1;
}
}
break;
case 1:
state = 2;
return 1;
case 2:
break;
}
return 0;
}
void
mesg(int c, char *file)
{
if(vflag)
Bprint(&bout, "%c - %s\n", c, file);
}
void
trim(char *s, char *buf, int n)
{
char *p;
for(;;) {
p = strrchr(s, '/');
if (!p) {
strncpy(buf, s, n);
return;
}
if (p[1] != 0) {
strncpy(buf, p+1, n);
return;
}
*p = 0;
}
}
#define SUID 04000
#define SGID 02000
#define ROWN 0400
#define WOWN 0200
#define XOWN 0100
#define RGRP 040
#define WGRP 020
#define XGRP 010
#define ROTH 04
#define WOTH 02
#define XOTH 01
#define STXT 01000
void
longt(Armember *bp)
{
char *cp;
pmode(strtoul(bp->hdr.mode, 0, 8));
Bprint(&bout, "%3ld/%1ld", atol(bp->hdr.uid), atol(bp->hdr.gid));
Bprint(&bout, "%7ld", bp->size);
cp = myctime(bp->date);
Bprint(&bout, " %-12.12s %-4.4s ", cp+4, cp+24);
}
int m1[] = { 1, ROWN, 'r', '-' };
int m2[] = { 1, WOWN, 'w', '-' };
int m3[] = { 2, SUID, 's', XOWN, 'x', '-' };
int m4[] = { 1, RGRP, 'r', '-' };
int m5[] = { 1, WGRP, 'w', '-' };
int m6[] = { 2, SGID, 's', XGRP, 'x', '-' };
int m7[] = { 1, ROTH, 'r', '-' };
int m8[] = { 1, WOTH, 'w', '-' };
int m9[] = { 2, STXT, 't', XOTH, 'x', '-' };
int *m[] = { m1, m2, m3, m4, m5, m6, m7, m8, m9};
void
pmode(long mode)
{
int **mp;
for(mp = &m[0]; mp < &m[9];)
ar_select(*mp++, mode);
}
void
ar_select(int *ap, long mode)
{
int n;
n = *ap++;
while(--n>=0 && (mode&*ap++)==0)
ap++;
Bputc(&bout, *ap);
}
Arfile *
newtempfile(char *name)
{
Arfile *ap;
ap = (Arfile *) armalloc(sizeof(Arfile));
ap->fname = name;
return ap;
}
Armember *
newmember(void)
{
return (Armember *)armalloc(sizeof(Armember));
}
void
arread(Biobuf *b, Armember *bp, int n)
{
int i;
bp->member = armalloc(n);
i = Bread(b, bp->member, n);
if (i < 0) {
free(bp->member);
bp->member = 0;
rderr();
}
}
void
arinsert(Arfile *ap, Armember *bp)
{
bp->next = 0;
if (!ap->tail)
ap->head = bp;
else
ap->tail->next = bp;
ap->tail = bp;
}
void
arstream(int fd, Arfile *ap)
{
Armember *bp;
int i;
char buf[8192];
if (ap->paged) {
seek(ap->fd, 0, 0);
for (;;) {
i = read(ap->fd, buf, sizeof(buf));
if (i < 0)
rderr();
if (i == 0)
break;
if (write(fd, buf, i) != i)
wrerr();
}
close(ap->fd);
ap->paged = 0;
}
for (bp = ap->head; bp; bp = bp->next) {
if (!arwrite(fd, bp))
wrerr();
}
}
int
arwrite(int fd, Armember *bp)
{
int len;
if(HEADER_IO(write, fd, bp->hdr))
return 0;
len = bp->size;
if (len & 01)
len++;
if (write(fd, bp->member, len) != len)
return 0;
return 1;
}
int
page(Arfile *ap)
{
Armember *bp;
bp = ap->head;
if (!ap->paged) {
ap->fname = mktemp(ap->fname);
ap->fd = create(ap->fname, ORDWR|ORCLOSE, 0600);
if (ap->fd < 0) {
fprint(2,"ar: can't create temp file\n");
return 0;
}
ap->paged = 1;
}
if (!arwrite(ap->fd, bp))
return 0;
ap->head = bp->next;
if (ap->tail == bp)
ap->tail = bp->next;
free(bp->member);
free(bp);
return 1;
}
int
getspace(void)
{
if (astart && astart->head && page(astart))
return 1;
if (amiddle && amiddle->head && page(amiddle))
return 1;
if (aend && aend->head && page(aend))
return 1;
return 0;
}
void
arfree(Arfile *ap)
{
Armember *bp, *next;
for (bp = ap->head; bp; bp = next) {
next = bp->next;
if (bp->member)
free(bp->member);
free(bp);
}
free(ap);
}
char *
armalloc(int n)
{
char *cp;
do {
cp = malloc(n);
if (cp) {
memset(cp, 0, n);
return cp;
}
} while (getspace());
fprint(2, "ar: out of memory\n");
exits("malloc");
return 0;
}
char *
GetNameFromID(int id)
{
USED(id);
return "unknown";
}