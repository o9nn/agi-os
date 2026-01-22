#include <u.h>
#include <libc.h>
#include <bio.h>
#include <libsec.h>
#include "iso9660.h"
static long mode(Direc*, int);
static long nlink(Direc*);
static ulong suspdirflags(Direc*, int);
static ulong CputsuspCE(Cdimg *cd, vlong offset);
static int CputsuspER(Cdimg*, int);
static int CputsuspRR(Cdimg*, int, int);
static int CputsuspSP(Cdimg*, int);
static int Cputrripname(Cdimg*, char*, int, char*, int);
static int CputrripSL(Cdimg*, int, int, char*, int);
static int CputrripPX(Cdimg*, Direc*, int, int);
static int CputrripTF(Cdimg*, Direc*, int, int);
static void
setcelen(Cdimg *cd, vlong woffset, ulong len)
{
vlong o;
o = Cwoffset(cd);
Cwseek(cd, woffset);
Cputn(cd, len, 4);
Cwseek(cd, o);
}
typedef struct Cbuf Cbuf;
struct Cbuf {
int len;
uvlong ceoffset;
};
static int
freespace(Cbuf *cp)
{
return (254-28) - cp->len;
}
static Cbuf*
ensurespace(Cdimg *cd, int n, Cbuf *co, Cbuf *cn, int dowrite)
{
uvlong end;
if(co->len+n <= 254-28) {
co->len += n;
return co;
}
co->len += 28;
assert(co->len <= 254);
if(dowrite == 0) {
cn->len = n;
return cn;
}
end = Cwoffset(cd)+28;
if(cd->rrcontin+co->len == end) {
assert(cd->rrcontin != 0);
assert(co == cn);
cd->rrcontin += co->len;
setcelen(cd, co->ceoffset, co->len);
} else
assert(co != cn);
if(cd->rrcontin%Blocksize == 0
|| cd->rrcontin/Blocksize != (cd->rrcontin+256)/Blocksize) {
cd->rrcontin = (vlong)cd->nextblock * Blocksize;
cd->nextblock++;
}
cn->ceoffset = CputsuspCE(cd, cd->rrcontin);
assert(Cwoffset(cd) == end);
cn->len = n;
Cwseek(cd, cd->rrcontin);
assert(cd->rrcontin != 0);
return cn;
}
Cbuf*
Cputstring(Cdimg *cd, Cbuf *cp, Cbuf *cn, char *nm, char *p, int flags, int dowrite)
{
char buf[256], *q;
int free;
for(; p[0] != '\0'; p = q) {
cp = ensurespace(cd, 5+1, cp, cn, dowrite);
cp->len -= 5+1;
free = freespace(cp);
assert(5+1 <= free && free < 256);
strncpy(buf, p, free-5);
buf[free-5] = '\0';
q = p+strlen(buf);
p = buf;
ensurespace(cd, 5+strlen(p), cp, nil, dowrite);
Cputrripname(cd, nm, flags | (q[0] ? NMcontinue : 0), p, dowrite);
}
return cp;
}
int
Cputsysuse(Cdimg *cd, Direc *d, int dot, int dowrite, int initlen)
{
char buf[256], buf0[256], *nextpath, *p, *path, *q;
int flags, free, m, what;
uvlong o;
Cbuf cn, co, *cp;
assert(cd != nil);
assert((initlen&1) == 0);
if(dot == DTroot)
return 0;
co.len = initlen;
o = Cwoffset(cd);
assert(dowrite==0 || Cwoffset(cd) == o+co.len-initlen);
cp = &co;
if (dot == DTrootdot) {
m = CputsuspSP(cd, 0);
cp = ensurespace(cd, m, cp, &cn, dowrite);
CputsuspSP(cd, dowrite);
m = CputsuspER(cd, 0);
cp = ensurespace(cd, m, cp, &cn, dowrite);
CputsuspER(cd, dowrite);
}
what = RR_PX | RR_TF | RR_NM;
if(d != nil && (d->mode & CHLINK))
what |= RR_SL;
m = CputsuspRR(cd, what, 0);
cp = ensurespace(cd, m, cp, &cn, dowrite);
CputsuspRR(cd, what, dowrite);
if(what & RR_PX) {
m = CputrripPX(cd, d, dot, 0);
cp = ensurespace(cd, m, cp, &cn, dowrite);
CputrripPX(cd, d, dot, dowrite);
}
if(what & RR_NM) {
if(dot == DTiden)
p = d->name;
else if(dot == DTdotdot)
p = "..";
else
p = ".";
flags = suspdirflags(d, dot);
assert(dowrite==0 || cp != &co || Cwoffset(cd) == o+co.len-initlen);
cp = Cputstring(cd, cp, &cn, "NM", p, flags, dowrite);
}
if(what & RR_SL) {
for(path=d->symlink; path[0] != '\0'; path=nextpath) {
if((nextpath = strchr(path, '/')) == nil)
nextpath = path+strlen(path);
strncpy(buf0, path, nextpath-path);
buf0[nextpath-path] = '\0';
if(nextpath[0] == '/')
nextpath++;
p = buf0;
if(strcmp(p, "") == 0)
flags = NMroot;
else if(strcmp(p, ".") == 0)
flags = NMcurrent;
else if(strcmp(p, "..") == 0)
flags = NMparent;
else
flags = 0;
do {
cp = ensurespace(cd, 7+1, cp, &cn, dowrite);
cp->len -= 7+1;
free = freespace(cp);
assert(7+1 <= free && free < 256);
strncpy(buf, p, free-7);
buf[free-7] = '\0';
q = p+strlen(buf);
p = buf;
assert(7+strlen(p) <= free);
ensurespace(cd, 7+strlen(p), cp, nil, dowrite);
CputrripSL(cd, nextpath[0], flags | (q[0] ? NMcontinue : 0), p, dowrite);
p = q;
} while(p[0] != '\0');
}
}
assert(dowrite==0 || cp != &co || Cwoffset(cd) == o+co.len-initlen);
if(what & RR_TF) {
m = CputrripTF(cd, d, TFcreation|TFmodify|TFaccess|TFattributes, 0);
cp = ensurespace(cd, m, cp, &cn, dowrite);
CputrripTF(cd, d, TFcreation|TFmodify|TFaccess|TFattributes, dowrite);
}
assert(dowrite==0 || cp != &co || Cwoffset(cd) == o+co.len-initlen);
if(cp == &cn && dowrite) {
cd->rrcontin = Cwoffset(cd);
setcelen(cd, cn.ceoffset, cn.len);
Cwseek(cd, o+co.len-initlen);
}
if(co.len & 1) {
co.len++;
if(dowrite)
Cputc(cd, 0);
}
if(dowrite) {
if(Cwoffset(cd) != o+co.len-initlen)
fprint(2, "offset %llud o+co.len-initlen %llud\n",
Cwoffset(cd), o+co.len-initlen);
assert(Cwoffset(cd) == o+co.len-initlen);
} else
assert(Cwoffset(cd) == o);
assert(co.len <= 255);
return co.len - initlen;
}
static char SUSPrrip[10] = "RRIP_1991A";
static char SUSPdesc[84] = "RRIP <more garbage here>";
static char SUSPsrc[135] = "RRIP <more garbage here>";
static ulong
CputsuspCE(Cdimg *cd, vlong offset)
{
vlong o, x;
chat("writing SUSP CE record pointing to %ld, %ld\n",
offset/Blocksize, offset%Blocksize);
o = Cwoffset(cd);
Cputc(cd, 'C');
Cputc(cd, 'E');
Cputc(cd, 28);
Cputc(cd, 1);
Cputn(cd, offset/Blocksize, 4);
Cputn(cd, offset%Blocksize, 4);
x = Cwoffset(cd);
Cputn(cd, 0, 4);
assert(Cwoffset(cd) == o+28);
return x;
}
static int
CputsuspER(Cdimg *cd, int dowrite)
{
assert(cd != nil);
if(dowrite) {
chat("writing SUSP ER record\n");
Cputc(cd, 'E');
Cputc(cd, 'R');
Cputc(cd, 26);
Cputc(cd, 1);
Cputc(cd, 10);
Cputc(cd, 4);
Cputc(cd, 4);
Cputc(cd, 1);
Cputs(cd, SUSPrrip, 10);
Cputs(cd, SUSPdesc, 4);
Cputs(cd, SUSPsrc, 4);
}
return 8+10+4+4;
}
static int
CputsuspRR(Cdimg *cd, int what, int dowrite)
{
assert(cd != nil);
if(dowrite) {
Cputc(cd, 'R');
Cputc(cd, 'R');
Cputc(cd, 5);
Cputc(cd, 1);
Cputc(cd, what);
}
return 5;
}
static int
CputsuspSP(Cdimg *cd, int dowrite)
{
assert(cd!=0);
if(dowrite) {
chat("writing SUSP SP record\n");
Cputc(cd, 'S');
Cputc(cd, 'P');
Cputc(cd, 7);
Cputc(cd, 1);
Cputc(cd, 0xBE);
Cputc(cd, 0xEF);
Cputc(cd, 0);
}
return 7;
}
#ifdef NOTUSED
static int
CputsuspST(Cdimg *cd, int dowrite)
{
assert(cd!=0);
if(dowrite) {
Cputc(cd, 'S');
Cputc(cd, 'T');
Cputc(cd, 4);
Cputc(cd, 1);
}
return 4;
}
#endif
static ulong
suspdirflags(Direc *d, int dot)
{
uchar flags;
USED(d);
flags = 0;
switch(dot) {
default:
assert(0);
case DTdot:
case DTrootdot:
flags |= NMcurrent;
break;
case DTdotdot:
flags |= NMparent;
break;
case DTroot:
flags |= NMvolroot;
break;
case DTiden:
break;
}
return flags;
}
static int
Cputrripname(Cdimg *cd, char *nm, int flags, char *name, int dowrite)
{
int l;
l = strlen(name);
if(dowrite) {
Cputc(cd, nm[0]);
Cputc(cd, nm[1]);
Cputc(cd, l+5);
Cputc(cd, 1);
Cputc(cd, flags);
Cputs(cd, name, l);
}
return 5+l;
}
static int
CputrripSL(Cdimg *cd, int contin, int flags, char *name, int dowrite)
{
int l;
l = strlen(name);
if(dowrite) {
Cputc(cd, 'S');
Cputc(cd, 'L');
Cputc(cd, l+7);
Cputc(cd, 1);
Cputc(cd, contin ? 1 : 0);
Cputc(cd, flags);
Cputc(cd, l);
Cputs(cd, name, l);
}
return 7+l;
}
static int
CputrripPX(Cdimg *cd, Direc *d, int dot, int dowrite)
{
assert(cd!=0);
if(dowrite) {
Cputc(cd, 'P');
Cputc(cd, 'X');
Cputc(cd, 36);
Cputc(cd, 1);
Cputn(cd, mode(d, dot), 4);
Cputn(cd, nlink(d), 4);
Cputn(cd, d?d->uidno:0, 4);
Cputn(cd, d?d->gidno:0, 4);
}
return 36;
}
static int
CputrripTF(Cdimg *cd, Direc *d, int type, int dowrite)
{
int i, length;
assert(cd!=0);
assert(!(type & TFlongform));
length = 0;
for(i=0; i<7; i++)
if (type & (1<<i))
length++;
assert(length == 4);
if(dowrite) {
Cputc(cd, 'T');
Cputc(cd, 'F');
Cputc(cd, 5+7*length);
Cputc(cd, 1);
Cputc(cd, type);
if (type & TFcreation)
Cputdate(cd, d?d->ctime:0);
if (type & TFmodify)
Cputdate(cd, d?d->mtime:0);
if (type & TFaccess)
Cputdate(cd, d?d->atime:0);
if (type & TFattributes)
Cputdate(cd, d?d->ctime:0);
}
return 5+7*length;
}
#define NONPXMODES (DMDIR | DMAPPEND | DMEXCL | DMMOUNT)
#define POSIXMODEMASK (0177777)
#ifndef S_IFMT
#define S_IFMT (0170000)
#endif
#ifndef S_IFDIR
#define S_IFDIR (0040000)
#endif
#ifndef S_IFREG
#define S_IFREG (0100000)
#endif
#ifndef S_IFLNK
#define S_IFLNK (0120000)
#endif
#undef ISTYPE
#define ISTYPE(mode, mask) (((mode) & S_IFMT) == (mask))
#ifndef S_ISDIR
#define S_ISDIR(mode) ISTYPE(mode, S_IFDIR)
#endif
#ifndef S_ISREG
#define S_ISREG(mode) ISTYPE(mode, S_IREG)
#endif
#ifndef S_ISLNK
#define S_ISLNK(mode) ISTYPE(mode, S_ILNK)
#endif
static long
mode(Direc *d, int dot)
{
long mode;
if (!d)
return 0;
if ((dot != DTroot) && (dot != DTrootdot)) {
mode = (d->mode & ~(NONPXMODES));
if (d->mode & DMDIR)
mode |= S_IFDIR;
else if (d->mode & CHLINK)
mode |= S_IFLNK;
else
mode |= S_IFREG;
} else
mode = S_IFDIR | (0755);
mode &= POSIXMODEMASK;
assert(mode & (S_IFDIR|S_IFREG));
chat("writing PX record mode field %ulo with dot %d and name \"%s\"\n", mode, dot, d->name);
return mode;
}
static long
nlink(Direc *d)
{
int i;
long n;
if (!d)
return 0;
n = 1;
if (d->mode & DMDIR)
n++;
for(i=0; i<d->nchild; i++)
if (d->child[i].mode & DMDIR)
n++;
return n;
}