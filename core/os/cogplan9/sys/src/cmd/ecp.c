#include <u.h>
#include <libc.h>
#include <ctype.h>
enum {
No = 0,
Yes,
Noseek = 0,
Mustseek,
Enone = 0,
Eio,
};
enum {
Defsectsz = 512,
Defblksz = 16*1024,
Mingoodblks = 3,
};
#define TTY "/dev/cons"
#define badsect(errno) ((errno) != Enone)
typedef uvlong Daddr;
typedef vlong Sdaddr;
typedef long Rdwrfn(int, void *, long);
typedef struct {
char *name;
int fd;
Daddr startsect;
int fast;
int seekable;
ulong maxconerrs;
ulong conerrs;
Daddr congoodblks;
Daddr harderrs;
Daddr lasterr;
Daddr lastgood;
} File;
char *argv0;
static int reblock = No, progress = No, swizzle = No;
static int reverse = No;
static ulong sectsz = Defsectsz;
static ulong blocksize = Defblksz;
static char *buf, *vfybuf;
static int blksects;
void
warning(char *s1, char *s2)
{
char err[100], msg[256];
char *np, *ep = msg + sizeof msg - 1;
errstr(err, sizeof err);
np = seprint(msg, ep, "%s: ", argv0);
np = seprint(np, ep, s1, s2);
errstr(err, sizeof err);
seprint(np, ep, ": %r\n");
fprint(2, "%s", msg);
}
int
eopen(char *file, int mode)
{
int fd = open(file, mode);
if (fd < 0)
sysfatal("can't open %s: %r", file);
return fd;
}
static int
confirm(File *src, File *dest)
{
int absent, n, tty = eopen(TTY, 2);
char c, junk;
Dir *stp;
if ((stp = dirstat(src->name)) == nil)
sysfatal("no input file %s: %r", src->name);
free(stp);
stp = dirstat(dest->name);
absent = (stp == nil);
free(stp);
fprint(2, "%s: copy %s to %s%s? ", argv0, src->name, dest->name,
(absent? " (missing)": ""));
n = read(tty, &c, 1);
junk = c;
if (n < 1)
c = 'n';
while (n > 0 && junk != '\n')
n = read(tty, &junk, 1);
close(tty);
if (isascii(c) && isupper(c))
c = tolower(c);
return c == 'y';
}
static char *
sectid(File *fp, Daddr sect)
{
static char sectname[256];
if (fp->startsect == 0)
snprint(sectname, sizeof sectname, "%s sector %llud",
fp->name, sect);
else
snprint(sectname, sizeof sectname,
"%s sector %llud (relative %llud)",
fp->name, sect + fp->startsect, sect);
return sectname;
}
static void
io_expl(File *fp, char *rw, Daddr sect)
{
if (reverse || fp->conerrs == 0) {
char msg[128];
snprint(msg, sizeof msg, "%s %s", rw, sectid(fp, sect));
warning("%s", msg);
} else if (fp->conerrs == 1)
fprint(2, "%s: ...\n", argv0);
}
static void
repos(File *fp, Daddr sect)
{
if (!fp->seekable)
sysfatal("%s: trying to seek on unseekable file", fp->name);
if (seek(fp->fd, (sect+fp->startsect)*sectsz, 0) == -1)
sysfatal("can't seek on %s: %r", fp->name);
}
static void
rewind(File *fp)
{
repos(fp, 0);
}
static char magic[] = "\235any old ☺ rubbish\173";
static char uniq[sizeof magic + 2*sizeof(ulong)];
static char *
putbe(char *p, ulong ul)
{
*p++ = ul>>24;
*p++ = ul>>16;
*p++ = ul>>8;
*p++ = ul;
return p;
}
static char *
addmagic(char *buff, int bytes)
{
char *p, *tail;
static ulong seq;
strcpy(uniq, magic);
p = putbe(uniq + sizeof magic - 1, time(0));
putbe(p, ++seq);
memcpy(buff, uniq, sizeof uniq);
tail = buff + bytes - sizeof uniq;
memcpy(tail, uniq, sizeof uniq);
return tail;
}
static int
ismagicok(char *buff, char *tail)
{
return memcmp(buff, uniq, sizeof uniq) == 0 ||
memcmp(tail, uniq, sizeof uniq) == 0;
}
static int
bio(File *fp, Rdwrfn *rdwr, char *buff, Daddr stsect, int sects, int mustseek)
{
int xfered;
char *tail;
ulong toread, bytes = sects * sectsz;
static int reblocked = 0;
if (mustseek) {
if (!fp->seekable)
sysfatal("%s: need to seek on unseekable file",
fp->name);
repos(fp, stsect);
}
if ((long)blocksize != blocksize || (long)bytes != bytes)
sysfatal("i/o count too big: %lud", bytes);
SET(tail);
if (rdwr == read)
tail = addmagic(buff, bytes);
werrstr("");
xfered = (*rdwr)(fp->fd, buff, bytes);
if (xfered == bytes) {
if (rdwr == read && ismagicok(buff, tail))
fprint(2, "%s: `good' read didn't change buffer\n",
argv0);
return Enone;
}
if (xfered < 0)
return Eio;
if (rdwr == write)
return Eio;
if (!reblock) {
memset(buff+xfered, '\0', bytes-xfered);
return Eio;
}
if (progress && !reblocked) {
fprint(2, "%s: reblocking input\n", argv0);
reblocked++;
}
for (toread = bytes - xfered; toread != 0; toread -= xfered) {
xfered = (*rdwr)(fp->fd, buff+bytes-toread, toread);
if (xfered <= 0)
break;
}
if (xfered < 0)
return Eio;
if (toread != 0)
memset(buff+bytes-toread, '\0', toread);
return Enone;
}
static int
toomanyerrs(File *fp, Daddr sect)
{
if (sect == fp->lasterr+1)
fp->conerrs++;
else
fp->conerrs = 0;
fp->lasterr = sect;
return fp->maxconerrs != 0 && fp->conerrs >= fp->maxconerrs &&
fp->lastgood == -1;
}
static void
ckendrange(File *fp)
{
if (!reverse && fp->conerrs > 0)
fprint(2, "%s: %lld: ... last bad sector in range\n",
argv0, fp->lasterr);
}
static int
transfer(File *fp, Rdwrfn *rdwr, char *buff, Daddr stsect, int sects,
int mustseek)
{
int res = bio(fp, rdwr, buff, stsect, sects, mustseek);
if (badsect(res)) {
fp->fast = 0;
fp->congoodblks = 0;
} else
fp->lastgood = stsect + sects - 1;
return res;
}
static void
bigxfer(File *fp, Rdwrfn *rdwr, char *buff, Daddr stsect, int sects,
int mustseek)
{
int i, badsects = 0, wasfast = fp->fast;
char *rw = (rdwr == read? "read": "write");
if (fp->fast) {
if (!badsect(transfer(fp, rdwr, buff, stsect, sects, mustseek)))
return;
if (progress)
fprint(2, "%s: breaking up big transfer on %s error "
"`%r' on %s\n", argv0, rw, sectid(fp, stsect));
}
for (i = 0; i < sects; i++)
if (badsect(transfer(fp, rdwr, buff+i*sectsz, stsect+i, 1,
Mustseek))) {
io_expl(fp, rw, stsect+i);
badsects++;
fp->harderrs++;
if (toomanyerrs(fp, stsect+i))
sysfatal("more than %lud consecutive I/O errors",
fp->maxconerrs);
} else {
ckendrange(fp);
fp->conerrs = 0;
}
if (badsects == 0) {
ckendrange(fp);
fp->conerrs = 0;
if (wasfast)
fprint(2, "%s: %s error on big transfer at %s but none "
"on retries!\n", argv0, rw, sectid(fp, stsect));
++fp->congoodblks;
if (fp->congoodblks >= Mingoodblks) {
fprint(2, "%s: %s: back to big transfers\n", argv0,
fp->name);
fp->fast = 1;
}
} else
repos(fp, stsect + sects);
}
static void
vrfyfailed(File *src, File *dest, Daddr stsect)
{
char *srcsect = strdup(sectid(src, stsect));
fprint(2, "%s: verify failed at %s (%s)\n", argv0, srcsect,
sectid(dest, stsect));
free(srcsect);
}
int
verify(File *src, File *dest, char *buff, char *buft, Daddr stsect,
int sectors)
{
int i, errors = 0;
for (i = 0; i < sectors; i++)
if (memcmp(buff + i*sectsz, buft + i*sectsz, sectsz) != 0)
errors++;
if (errors == 0)
return errors;
if (sectors == 1) {
vrfyfailed(src, dest, stsect);
return errors;
}
errors = 0;
for (i = 0; i < sectors; i++) {
int thissect = stsect + i;
if (badsect(bio(src, read, buff, thissect, 1, Mustseek)))
io_expl(src, "read", thissect);
if (badsect(bio(dest, read, buft, thissect, 1, Mustseek)))
io_expl(dest, "write", thissect);
if (memcmp(buff, buft, sectsz) != 0) {
vrfyfailed(src, dest, thissect);
++errors;
}
}
if (errors == 0) {
char *srcsect = strdup(sectid(src, stsect));
fprint(2, "%s: verification failed on big read at %s (%s) "
"but not on retries!\n", argv0, srcsect,
sectid(dest, stsect));
free(srcsect);
}
repos(src, stsect + sectors);
repos(dest, stsect + sectors);
return errors;
}
int
sectsleft(Daddr start, Daddr nsects, int maxxfr)
{
if (start + maxxfr <= nsects - 1)
return maxxfr;
else
return nsects - start;
}
enum {
Rotbits = 3,
};
void
swizzlebits(char *buff, int sects)
{
uchar *bp, *endbp;
endbp = (uchar *)(buff+sects*sectsz);
for (bp = (uchar *)buff; bp < endbp; bp++)
*bp = ~(*bp>>Rotbits | *bp<<(8-Rotbits));
}
static int
copysects(File *src, File *dest, Daddr stsect, Daddr nsects, int mustseek)
{
int xfrsects = sectsleft(stsect, nsects, blksects);
if (xfrsects > blksects) {
fprint(2, "%s: block size of %d is too big.\n", argv0, xfrsects);
exits("block size too big");
}
bigxfer(src, read, buf, stsect, xfrsects, mustseek);
if (swizzle)
swizzlebits(buf, xfrsects);
bigxfer(dest, write, buf, stsect, xfrsects, mustseek);
if (progress &&
(stsect < blksects*10 || stsect%(10*1024*1024/sectsz) == 0))
fprint(2, "%s: copied%s to relative sector %llud\n", argv0,
(swizzle? " swizzled": ""), stsect + xfrsects - 1);
return 0;
}
static int
vrfysects(File *src, File *dest, Daddr stsect, Daddr nsects, int mustseek)
{
int xfrsects = sectsleft(stsect, nsects, blksects);
if (xfrsects > blksects) {
fprint(2, "%s: block size of %d is too big.\n", argv0, xfrsects);
exits("block size too big");
}
bigxfer(src, read, buf, stsect, xfrsects, mustseek);
bigxfer(dest, read, vfybuf, stsect, xfrsects, mustseek);
return verify(src, dest, buf, vfybuf, stsect, xfrsects);
}
static void
setupfile(File *fp, int mode)
{
fp->fd = open(fp->name, mode);
if (fp->fd < 0)
sysfatal("can't open %s: %r", fp->name);
fp->seekable = (seek(fp->fd, 0, 1) >= 0);
if (fp->startsect != 0)
rewind(fp);
}
static Daddr
copyfile(File *src, File *dest, Daddr nsects, int plsverify)
{
Sdaddr stsect, vererrs = 0;
Dir *stp;
setupfile(src, OREAD);
if ((stp = dirstat(dest->name)) == nil) {
int fd = create(dest->name, ORDWR, 0666);
if (fd >= 0)
close(fd);
}
free(stp);
setupfile(dest, ORDWR);
if (progress)
fprint(2, "%s: copying first sectors\n", argv0);
if (reverse)
for (stsect = (nsects/blksects)*blksects; stsect >= 0;
stsect -= blksects)
vererrs += copysects(src, dest, stsect, nsects, Mustseek);
else {
for (stsect = 0; stsect < nsects; stsect += blksects)
vererrs += copysects(src, dest, stsect, nsects, Noseek);
ckendrange(src);
ckendrange(dest);
}
if (plsverify) {
fprint(2, "%s: copy done; verifying...\n", argv0);
rewind(src);
rewind(dest);
for (stsect = 0; stsect < nsects; stsect += blksects)
vererrs += vrfysects(src, dest, stsect, nsects, Noseek);
if (vererrs <= 0)
fprint(2, "%s: no", argv0);
else
fprint(2, "%s: %llud", argv0, vererrs);
fprint(2, " error%s during verification\n",
(vererrs != 1? "s": ""));
}
close(src->fd);
close(dest->fd);
return vererrs;
}
static void
usage(void)
{
fprint(2, "usage: %s [-bcprvZ][-B blocksz][-e errs][-s sectsz]"
"[-i issect][-o ossect] sectors from to\n", argv0);
exits("usage");
}
void
initfile(File *fp)
{
memset(fp, 0, sizeof *fp);
fp->fast = 1;
fp->lasterr = -1;
fp->lastgood = -1;
}
void
main(int argc, char **argv)
{
int errflg = 0, plsconfirm = No, plsverify = No;
long lval;
File src, dest;
Sdaddr sect;
initfile(&src);
initfile(&dest);
ARGBEGIN {
case 'b':
reblock = Yes;
break;
case 'B':
lval = atol(EARGF(usage()));
if (lval < 0)
usage();
blocksize = lval;
break;
case 'c':
plsconfirm = Yes;
break;
case 'e':
lval = atol(EARGF(usage()));
if (lval < 0)
usage();
src.maxconerrs = lval;
dest.maxconerrs = lval;
break;
case 'i':
sect = atoll(EARGF(usage()));
if (sect < 0)
usage();
src.startsect = sect;
break;
case 'o':
sect = atoll(EARGF(usage()));
if (sect < 0)
usage();
dest.startsect = sect;
break;
case 'p':
progress = Yes;
break;
case 'r':
reverse = Yes;
break;
case 's':
sectsz = atol(EARGF(usage()));
if (sectsz <= 0 || sectsz % 512 != 0)
usage();
break;
case 'v':
plsverify = Yes;
break;
case 'Z':
swizzle = Yes;
break;
default:
errflg++;
break;
} ARGEND
if (errflg || argc != 3)
usage();
if (blocksize <= 0 || blocksize % sectsz != 0)
sysfatal("block size not a multiple of sector size");
if (!isascii(argv[0][0]) || !isdigit(argv[0][0])) {
fprint(2, "%s: %s is not numeric\n", argv0, argv[0]);
exits("non-numeric sector count");
}
src.name = argv[1];
dest.name = argv[2];
blksects = blocksize / sectsz;
if (blksects < 1)
blksects = 1;
buf = malloc(blocksize);
vfybuf = malloc(blocksize);
if (buf == nil || vfybuf == nil)
sysfatal("out of memory: %r");
if (plsconfirm? confirm(&src, &dest): Yes)
copyfile(&src, &dest, atoll(argv[0]), plsverify);
exits(src.harderrs || dest.harderrs? "hard errors": 0);
}