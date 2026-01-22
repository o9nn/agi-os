#include "all.h"
#include "io.h"
static void	dowormcopy(void);
static int	dodevcopy(void);
struct {
char*	icharp;
char*	charp;
int	error;
int	newconf;
int	modconf;
int	nextiter;
int	lastiter;
int	diriter;
Device*	lastcw;
Device*	devlist;
} f;
static Device* confdev;
static int copyworm = 0, copydev = 0;
static char *src, *dest;
static int resetparams;
Fspar fspar[] = {
{ "blocksize",	RBUFSIZE, },
{ "daddrbits",	sizeof(Off)*8, },
{ "indirblks",	NIBLOCK, },
{ "dirblks",	NDBLOCK, },
{ "namelen",	NAMELEN, },
{ nil, 0, },
};
int
devcmpr(Device *d1, Device *d2)
{
while (d1 != d2) {
if(d1 == nil || d2 == nil || d1->type != d2->type)
return 1;
switch(d1->type) {
default:
print("can't compare dev: %Z\n", d1);
panic("devcmp");
return 1;
case Devmcat:
case Devmlev:
case Devmirr:
d1 = d1->cat.first;
d2 = d2->cat.first;
while(d1 && d2) {
if(devcmpr(d1, d2))
return 1;
d1 = d1->link;
d2 = d2->link;
}
break;
case Devnone:
return 0;
case Devro:
d1 = d1->ro.parent;
d2 = d2->ro.parent;
break;
case Devjuke:
case Devcw:
if(devcmpr(d1->cw.c, d2->cw.c))
return 1;
d1 = d1->cw.w;
d2 = d2->cw.w;
break;
case Devfworm:
d1 = d1->fw.fw;
d2 = d2->fw.fw;
break;
case Devwren:
case Devworm:
case Devlworm:
if(d1->wren.ctrl == d2->wren.ctrl)
if(d1->wren.targ == d2->wren.targ)
if(d1->wren.lun == d2->wren.lun)
return 0;
return 1;
case Devpart:
if(d1->part.base == d2->part.base)
if(d1->part.size == d2->part.size) {
d1 = d1->part.d;
d2 = d2->part.d;
break;
}
return 1;
}
}
return 0;
}
void
cdiag(char *s, int c1)
{
f.charp--;
if(f.error == 0) {
print("config diag: %s -- <%c>\n", s, c1);
f.error = 1;
}
}
int
cnumb(void)
{
int c, n;
c = *f.charp++;
if(c == '<') {
n = f.nextiter;
if(n >= 0) {
f.nextiter = n+f.diriter;
if(n == f.lastiter) {
f.nextiter = -1;
f.lastiter = -1;
}
do {
c = *f.charp++;
} while (c != '>');
return n;
}
n = cnumb();
if(*f.charp++ != '-') {
cdiag("- expected", f.charp[-1]);
return 0;
}
c = cnumb();
if(*f.charp++ != '>') {
cdiag("> expected", f.charp[-1]);
return 0;
}
f.lastiter = c;
f.diriter = 1;
if(n > c)
f.diriter = -1;
f.nextiter = n+f.diriter;
return n;
}
if(!isascii(c) || !isdigit(c)) {
cdiag("number expected", c);
return 0;
}
n = 0;
while(isascii(c) && isdigit(c)) {
n = n*10 + (c-'0');
c = *f.charp++;
}
f.charp--;
return n;
}
Device*
config1(int c)
{
Device *d, *t;
int m;
d = malloc(sizeof(Device));
do {
t = config();
if(d->cat.first == 0)
d->cat.first = t;
else
d->cat.last->link = t;
d->cat.last = t;
if(f.error)
return devnone;
m = *f.charp;
if(c == '(' && m == ')')
d->type = Devmcat;
else if(c == '[' && m == ']')
d->type = Devmlev;
else if(c == '{' && m == '}')
d->type = Devmirr;
} while (d->type == 0);
f.charp++;
if(d->cat.first == d->cat.last)
d = d->cat.first;
return d;
}
static void
map(Device *d)
{
Map *map;
if (d->type != Devwren || d->wren.mapped)
return;
for (map = devmap; map != nil; map = map->next)
if (devcmpr(d, map->fdev) == 0)
break;
if (map == nil)
return;
if (access(map->to, AEXIST) >= 0)
{ print("map: mapped wren %Z to existing file %s\n", d, map->to);
d->wren.file = map->to;
}
else if (map->tdev != nil)
{ print("map: mapped wren %Z to dev %Z\n", d, map->tdev);
*d = *map->tdev;
}
else
print("bad mapping %Z to %s; no such file or device",
d, map->to);
d->wren.mapped = 1;
}
Device*
config(void)
{
int c, m;
Device *d;
char *icp;
if(f.error)
return devnone;
d = malloc(sizeof(Device));
c = *f.charp++;
switch(c) {
default:
cdiag("unknown type", c);
return devnone;
case '(':
case '[':
case '{':
return config1(c);
case 'f':
d->type = Devfworm;
d->fw.fw = config();
break;
case 'n':
d->type = Devnone;
break;
case 'w':
case 'r':
case 'l':
icp = f.charp;
d->type = Devwren;
d->wren.ctrl = 0;
d->wren.targ = cnumb();
d->wren.lun = 0;
m = *f.charp;
if(m == '.') {
f.charp++;
d->wren.lun = cnumb();
m = *f.charp;
if(m == '.') {
f.charp++;
d->wren.ctrl = d->wren.targ;
d->wren.targ = d->wren.lun;
d->wren.lun = cnumb();
}
}
if(f.nextiter >= 0)
f.charp = icp-1;
if(c == 'r')
d->type = Devworm;
else if(c == 'l')
d->type = Devlworm;
else
map(d);
break;
case 'o':
if(f.lastcw == 0) {
cdiag("no cw to match", c);
return devnone;
}
return f.lastcw->cw.ro;
case 'j':
d->type = Devjuke;
d->j.j = config();
d->j.m = config();
break;
case 'c':
d->type = Devcw;
d->cw.c = config();
d->cw.w = config();
d->cw.ro = malloc(sizeof(Device));
d->cw.ro->type = Devro;
d->cw.ro->ro.parent = d;
f.lastcw = d;
break;
case 'p':
d->type = Devpart;
d->part.d = config();
d->part.base = cnumb();
c = *f.charp++;
if(c != '.')
cdiag("dot expected", c);
d->part.size = cnumb();
break;
case 'x':
d->type = Devswab;
d->swab.d = config();
break;
}
d->dlink = f.devlist;
f.devlist = d;
return d;
}
Device*
iconfig(char *s)
{
Device *d;
f.nextiter = -1;
f.lastiter = -1;
f.error = 0;
f.icharp = s;
f.charp = f.icharp;
d = config();
if(*f.charp) {
cdiag("junk on end", *f.charp);
f.error = 1;
}
return d;
}
int
testconfig(char *s)
{
iconfig(s);
return f.error;
}
int
astrcmp(char *a, char *b)
{
int n, c;
n = strlen(b);
if(memcmp(a, b, n) != 0)
return 1;
c = a[n];
if(c == '\0')
return 0;
if(a[n+1])
return 1;
if(isascii(c) && isdigit(c))
return 0;
return 1;
}
static Fspar *
getpar(char *name)
{
Fspar *fsp;
for (fsp = fspar; fsp->name != nil; fsp++)
if (strcmp(name, fsp->name) == 0)
return fsp;
return nil;
}
void
mergeconf(Iobuf *p)
{
char word[Maxword+1];
char *cp;
Filsys *fs;
Fspar *fsp;
for (cp = p->iobuf; *cp != '\0'; cp++) {
cp = getwrd(word, cp);
if(word[0] == '\0')
break;
else if (word[0] == '#')
while (*cp != '\n' && *cp != '\0')
cp++;
else if(strcmp(word, "service") == 0) {
cp = getwrd(word, cp);
if(service[0] == 0)
strncpy(service, word, sizeof service);
} else if(strcmp(word, "ipauth") == 0)
cp = getwrd(word, cp);
else if(astrcmp(word, "ip") == 0)
cp = getwrd(word, cp);
else if(astrcmp(word, "ipgw") == 0)
cp = getwrd(word, cp);
else if(astrcmp(word, "ipsntp") == 0)
cp = getwrd(word, cp);
else if(astrcmp(word, "ipmask") == 0)
cp = getwrd(word, cp);
else if(strcmp(word, "filsys") == 0) {
cp = getwrd(word, cp);
for(fs = filsys; fs < filsys + nelem(filsys) - 1 &&
fs->name; fs++)
if(strcmp(fs->name, word) == 0)
break;
if (fs >= filsys + nelem(filsys) - 1)
panic("out of filsys structures");
if (fs->name && strcmp(fs->name, word) == 0 &&
fs->flags & FEDIT)
cp = getwrd(word, cp);
else {
fs->name = strdup(word);
cp = getwrd(word, cp);
if (word[0] == '\0')
fs->conf = nil;
else
fs->conf = strdup(word);
}
} else if ((fsp = getpar(word)) != nil) {
cp = getwrd(word, cp);
if (!isascii(word[0]) || !isdigit(word[0]))
print("bad %s value: %s", fsp->name, word);
else
fsp->declared = atol(word);
} else {
putbuf(p);
panic("unknown keyword in config block: %s", word);
}
if(*cp != '\n') {
putbuf(p);
panic("syntax error in config block at `%s'", word);
}
}
}
void
cmd_printconf(int, char *[])
{
char *p, *s;
Iobuf *iob;
iob = getbuf(confdev, 0, Brd);
if(iob == nil)
return;
if(checktag(iob, Tconfig, 0)){
putbuf(iob);
return;
}
print("config %s\n", nvrgetconfig());
for(s = p = iob->iobuf; *p != 0 && p < iob->iobuf+BUFSIZE; ){
if(*p++ != '\n')
continue;
if (strncmp(s, "ip", 2) != 0)
print("%.*s", (int)(p-s), s);
s = p;
}
if(p != s)
print("%.*s", (int)(p-s), s);
print("end\n");
putbuf(iob);
}
void
sysinit(void)
{
int error;
char *cp, *ep;
Device *d;
Filsys *fs;
Fspar *fsp;
Iobuf *p;
cons.chan = fs_chaninit(Devcon, 1, 0);
start:
devnone = iconfig("n");
cp = nvrgetconfig();
print("config %s\n", cp);
confdev = d = iconfig(cp);
devinit(d);
if(f.newconf) {
p = getbuf(d, 0, Bmod);
memset(p->iobuf, 0, RBUFSIZE);
settag(p, Tconfig, 0);
} else
p = getbuf(d, 0, Brd|Bmod);
if(!p || checktag(p, Tconfig, 0))
panic("config io");
mergeconf(p);
if (resetparams) {
for (fsp = fspar; fsp->name != nil; fsp++)
fsp->declared = 0;
resetparams = 0;
}
for (fsp = fspar; fsp->name != nil; fsp++) {
if (fsp->declared == 0) {
fsp->declared = fsp->actual;
f.modconf = 1;
}
if (fsp->declared != fsp->actual)
print("warning: config %s %ld != compiled-in %ld\n",
fsp->name, fsp->declared, fsp->actual);
}
if(f.modconf) {
memset(p->iobuf, 0, BUFSIZE);
p->flags |= Bmod|Bimm;
cp = p->iobuf;
ep = p->iobuf + RBUFSIZE - 1;
if(service[0])
cp = seprint(cp, ep, "service %s\n", service);
for(fs=filsys; fs->name; fs++)
if(fs->conf && fs->conf[0] != '\0')
cp = seprint(cp, ep, "filsys %s %s\n", fs->name,
fs->conf);
for (fsp = fspar; fsp->name != nil; fsp++)
cp = seprint(cp, ep, "%s %ld\n",
fsp->name, fsp->declared);
putbuf(p);
f.modconf = f.newconf = 0;
print("config block written\n");
goto start;
}
putbuf(p);
print("service    %s\n", service);
loop:
for(fs=filsys; fs->name; fs++)
if(fs->conf == nil || fs->conf[0] == '\0') {
for(; fs->name; fs++)
*fs = *(fs+1);
goto loop;
}
if(filsys[0].name == nil)
panic("no filsys");
error = 0;
for(fs=filsys; fs->name; fs++) {
print("filsys %s %s\n", fs->name, fs->conf);
fs->dev = iconfig(fs->conf);
if(f.error) {
error = 1;
continue;
}
}
if(error)
panic("fs config");
for(fs=filsys; fs->name; fs++) {
delay(3000);
print("sysinit: %s\n", fs->name);
if(fs->flags & FREAM)
devream(fs->dev, 1);
if(fs->flags & FRECOVER)
devrecover(fs->dev);
devinit(fs->dev);
}
if (copyworm) {
dowormcopy();
panic("copyworm bailed out!");
}
if (copydev)
if (dodevcopy() < 0)
panic("copydev failed!");
else
panic("copydev done.");
}
static int
userabort(char *msg)
{
USED(msg);
return 0;
}
static int
blockok(Device *d, Off a)
{
Iobuf *p = getbuf(d, a, Brd);
if (p == 0) {
print("i/o error reading %Z block %lld\n", d, (Wideoff)a);
return 0;
}
putbuf(p);
return 1;
}
static Device *
wormof(Device *dev)
{
Device *worm = dev, *cw;
if (dev->type == Devfworm) {
cw = dev->fw.fw;
if (cw != nil && cw->type == Devcw)
worm = cw->cw.w;
}
return worm;
}
static Devsize
writtensize(Device *worm)
{
Devsize lim = devsize(worm);
Iobuf *p;
print("devsize(%Z) = %lld\n", worm, (Wideoff)lim);
if (!blockok(worm, 0) || !blockok(worm, lim-1))
return 0;
delay(5*1000);
if (userabort("sanity checks"))
return 0;
while (lim > 0) {
if (userabort("sizing")) {
lim = 0;
break;
}
--lim;
p = getbuf(worm, lim, Brd);
if (p != 0) {
putbuf(p);
break;
}
}
print("limit(%Z) = %lld\n", worm, (Wideoff)lim);
if (lim <= 0)
return 0;
return lim + 1;
}
static void
dowormcopy(void)
{
Filsys *f1, *f2;
Device *fdev, *from, *to = nil;
Iobuf *p;
Off a;
Devsize lim;
f1 = fsstr("main");
if(f1 == nil)
panic("main file system missing");
fdev = f1->dev;
from = wormof(fdev);
if (from->type != Devfworm && from->type != Devcw) {
print("main file system is not a worm; copyworm may not do what you want!\n");
print("waiting for 20 seconds...\n");
delay(20000);
}
f2 = fsstr("output");
if(f2 == nil) {
print("no output file system - check only\n\n");
print("reading worm from %Z (worm %Z)\n", fdev, from);
} else {
to = f2->dev;
print("\ncopying worm from %Z (worm %Z) to %Z, starting in 8 seconds\n",
fdev, from, to);
delay(8000);
}
if (userabort("preparing to copy"))
return;
devinit(from);
if (0 && fdev != from) {
devinit(fdev);
print("debugging, sizing %Z first\n", fdev);
writtensize(fdev);
}
lim = writtensize(from);
if(lim == 0)
panic("no blocks to copy on %Z", from);
if (to) {
print("reaming %Z in 8 seconds\n", to);
delay(8000);
if (userabort("preparing to ream & copy"))
return;
devream(to, 0);
devinit(to);
print("copying worm: %lld blocks from %Z to %Z\n",
(Wideoff)lim, from, to);
}
for (a = 0; a < lim; a++) {
if (userabort("copy"))
break;
p = getbuf(from, a, Brd);
if (p == 0) {
print("%lld not written yet; can't read\n", (Wideoff)a);
continue;
}
if (to != 0 && devwrite(to, p->addr, p->iobuf) != 0) {
print("out block %lld: write error; bailing",
(Wideoff)a);
break;
}
putbuf(p);
if(a % 20000 == 0)
print("block %lld %T\n", (Wideoff)a, time(nil));
}
print("copied %lld blocks from %Z to %Z\n", (Wideoff)a, from, to);
sync("wormcopy");
delay(2000);
print("looping; reset the machine at any time.\n");
for (; ; )
continue;
}
static int
dodevcopy(void)
{
Device *from, *to;
Iobuf *p;
Off a;
Devsize lim, tosize;
from = iconfig(src);
if(f.error || from == nil) {
print("bad src device %s\n", src);
return -1;
}
to = iconfig(dest);
if(f.error || to == nil) {
print("bad dest device %s\n", dest);
return -1;
}
devinit(from);
lim = devsize(from);
if(lim == 0)
panic("no blocks to copy on %Z", from);
devinit(to);
tosize = devsize(to);
if(tosize == 0)
panic("no blocks to copy on %Z", to);
if (tosize < lim)
lim = tosize;
print("copy %Z to %Z in 8 seconds\n", from, to);
delay(8000);
if (userabort("preparing to copy"))
return -1;
print("copying dev: %lld blocks from %Z to %Z\n", (Wideoff)lim,
from, to);
for (a = 0; a < lim; a++) {
if (userabort("copy"))
break;
p = getbuf(from, a, Brd);
if (p == 0) {
print("%lld not written yet; can't read\n", (Wideoff)a);
continue;
}
if (to != 0 && devwrite(to, p->addr, p->iobuf) != 0) {
print("out block %lld: write error; bailing",
(Wideoff)a);
break;
}
putbuf(p);
if(a % 20000 == 0)
print("block %lld %T\n", (Wideoff)a, time(nil));
}
print("copied %lld blocks from %Z to %Z\n", (Wideoff)a, from, to);
sync("devcopy");
return 0;
}
static void
setconfig(char *dev)
{
if (dev != nil && !testconfig(dev))
nvrsetconfig(dev);
}
void
arginit(void)
{
int verb;
char *line;
char word[Maxword+1], *cp;
Filsys *fs;
if(nvrcheck() == 0) {
setconfig(conf.confdev);
if (!conf.configfirst)
return;
}
setconfig(conf.confdev);
for (;;) {
print("config: ");
if ((line = Brdline(&bin, '\n')) == nil)
return;
line[Blinelen(&bin)-1] = '\0';
cp = getwrd(word, line);
if (word[0] == '\0' || word[0] == '#')
continue;
if(strcmp(word, "end") == 0)
return;
if(strcmp(word, "halt") == 0)
exit();
if(strcmp(word, "queryjuke") == 0) {
getwrd(word, cp);
if(testconfig(word) == 0)
querychanger(iconfig(word));
continue;
}
if(strcmp(word, "allow") == 0) {
wstatallow = 1;
writeallow = 1;
continue;
}
if(strcmp(word, "copyworm") == 0) {
copyworm = 1;
continue;
}
if(strcmp(word, "copydev") == 0) {
cp = getwrd(word, cp);
if(testconfig(word))
continue;
src = strdup(word);
getwrd(word, cp);
if(testconfig(word))
continue;
dest = strdup(word);
copydev = 1;
continue;
}
if(strcmp(word, "noauth") == 0) {
noauth = !noauth;
continue;
}
if(strcmp(word, "noattach") == 0) {
noattach = !noattach;
continue;
}
if(strcmp(word, "readonly") == 0) {
readonly = 1;
continue;
}
if(strcmp(word, "ream") == 0) {
verb = FREAM;
goto gfsname;
}
if(strcmp(word, "recover") == 0) {
verb = FRECOVER;
goto gfsname;
}
if(strcmp(word, "filsys") == 0) {
verb = FEDIT;
goto gfsname;
}
if(strcmp(word, "nvram") == 0) {
getwrd(word, cp);
if(testconfig(word))
continue;
nvrsetconfig(word);
continue;
}
if(strcmp(word, "config") == 0) {
getwrd(word, cp);
if(!testconfig(word) && nvrsetconfig(word) == 0)
f.newconf = 1;
continue;
}
if(strcmp(word, "service") == 0) {
getwrd(word, cp);
strncpy(service, word, sizeof service);
f.modconf = 1;
continue;
}
if (strcmp(word, "resetparams") == 0) {
resetparams++;
continue;
}
if (strcmp(word, "ipauth") != 0 &&
astrcmp(word, "ip") != 0 &&
astrcmp(word, "ipgw") != 0 &&
astrcmp(word, "ipmask") != 0 &&
astrcmp(word, "ipsntp") != 0) {
print("unknown config command\n");
print("\ttype end to get out\n");
continue;
}
getwrd(word, cp);
f.modconf = 1;
continue;
gfsname:
cp = getwrd(word, cp);
for(fs=filsys; fs->name; fs++)
if(strcmp(word, fs->name) == 0)
break;
if (fs->name == nil) {
memset(fs, 0, sizeof *fs);
fs->name = strdup(word);
}
switch(verb) {
case FREAM:
if(strcmp(fs->name, "main") == 0)
wstatallow = 1;
case FRECOVER:
fs->flags |= verb;
break;
case FEDIT:
f.modconf = 1;
getwrd(word, cp);
fs->flags |= verb;
if(word[0] == 0)
fs->conf = nil;
else if(!testconfig(word))
fs->conf = strdup(word);
break;
}
}
}