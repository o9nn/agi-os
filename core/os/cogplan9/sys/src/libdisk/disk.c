#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ctype.h>
#include <disk.h>
static Disk*
mkwidth(Disk *disk)
{
char buf[40];
snprint(buf, sizeof buf, "%lld", disk->size);
disk->width = strlen(buf);
return disk;
}
typedef struct Table Table;
typedef struct Tentry Tentry;
struct Tentry {
uchar active;
uchar starth;
uchar starts;
uchar startc;
uchar type;
uchar endh;
uchar ends;
uchar endc;
uchar xlba[4];
uchar xsize[4];
};
enum {
Toffset = 446,
Magic0 = 0x55,
Magic1 = 0xAA,
NTentry = 4,
};
struct Table {
Tentry entry[NTentry];
uchar magic[2];
};
static int
partitiongeometry(Disk *disk)
{
char *rawname;
int i, h, rawfd, s;
uchar buf[512];
Table *t;
if(disk->c == 0 || disk->h == 0 || disk->s == 0)
return -1;
t = (Table*)(buf + Toffset);
rawname = malloc(strlen(disk->prefix) + 5);
if(rawname == nil)
return -1;
strcpy(rawname, disk->prefix);
strcat(rawname, "data");
rawfd = open(rawname, OREAD);
free(rawname);
if(rawfd >= 0
&& seek(rawfd, 0, 0) >= 0
&& readn(rawfd, buf, 512) == 512
&& t->magic[0] == Magic0
&& t->magic[1] == Magic1) {
close(rawfd);
} else {
if(rawfd >= 0)
close(rawfd);
if(seek(disk->fd, 0, 0) < 0
|| readn(disk->fd, buf, 512) != 512
|| t->magic[0] != Magic0
|| t->magic[1] != Magic1) {
return -1;
}
}
h = s = -1;
for(i=0; i<NTentry; i++) {
if(t->entry[i].type == 0)
continue;
t->entry[i].ends &= 63;
if(h == -1) {
h = t->entry[i].endh;
s = t->entry[i].ends;
} else {
if(h != t->entry[i].endh || s != t->entry[i].ends)
return -1;
}
}
if(h == -1)
return -1;
disk->h = h+1;
disk->s = s;
disk->c = disk->secs / (disk->h*disk->s);
disk->chssrc = Gpart;
return 0;
}
static int
drivergeometry(Disk *disk)
{
int m;
if(disk->c == 0 || disk->h == 0 || disk->s == 0)
return -1;
disk->chssrc = Gdisk;
if(disk->c < 1024)
return 0;
switch(disk->h) {
case 15:
disk->h = 255;
disk->c /= 17;
return 0;
default:
for(m = 2; m*disk->h < 256; m *= 2) {
if(disk->c/m < 1024) {
disk->c /= m;
disk->h *= m;
return 0;
}
}
disk->h = 255;
disk->s = 63;
disk->c = disk->secs / (disk->h * disk->s);
return 0;
}
}
static struct {
int h;
int s;
} guess[] = {
64, 32,
64, 63,
128, 63,
255, 63,
};
static int
guessgeometry(Disk *disk)
{
int i;
long c;
disk->chssrc = Gguess;
c = 1024;
for(i=0; i<nelem(guess); i++)
if(c*guess[i].h*guess[i].s >= disk->secs) {
disk->h = guess[i].h;
disk->s = guess[i].s;
disk->c = disk->secs / (disk->h * disk->s);
return 0;
}
disk->h = 255;
disk->s = 63;
disk->c = disk->secs / (disk->h * disk->s);
return 0;
}
static void
findgeometry(Disk *disk)
{
if(partitiongeometry(disk) < 0
&& drivergeometry(disk) < 0
&& guessgeometry(disk) < 0) {
print("we're completely confused about your disk; sorry\n");
assert(0);
}
}
static Disk*
openfile(Disk *disk)
{
Dir *d;
if((d = dirfstat(disk->fd)) == nil){
free(disk);
return nil;
}
disk->secsize = 512;
disk->size = d->length;
disk->secs = disk->size / disk->secsize;
disk->offset = 0;
free(d);
findgeometry(disk);
return mkwidth(disk);
}
static Disk*
opensd(Disk *disk)
{
Biobuf b;
char *p, *f[10];
int nf;
Binit(&b, disk->ctlfd, OREAD);
while(p = Brdline(&b, '\n')) {
p[Blinelen(&b)-1] = '\0';
nf = tokenize(p, f, nelem(f));
if(nf >= 3 && strcmp(f[0], "geometry") == 0) {
disk->secsize = strtoll(f[2], 0, 0);
if(nf >= 6) {
disk->c = strtol(f[3], 0, 0);
disk->h = strtol(f[4], 0, 0);
disk->s = strtol(f[5], 0, 0);
}
}
if(nf >= 4 && strcmp(f[0], "part") == 0 && strcmp(f[1], disk->part) == 0) {
disk->offset = strtoll(f[2], 0, 0);
disk->secs = strtoll(f[3], 0, 0) - disk->offset;
}
}
disk->size = disk->secs * disk->secsize;
if(disk->size <= 0) {
strcpy(disk->part, "");
disk->type = Tfile;
return openfile(disk);
}
findgeometry(disk);
return mkwidth(disk);
}
Disk*
opendisk(char *disk, int rdonly, int noctl)
{
char *p, *q;
Disk *d;
d = mallocz(sizeof(*d), 1);
if(d == nil)
return nil;
d->fd = d->wfd = d->ctlfd = -1;
d->rdonly = rdonly;
d->fd = open(disk, OREAD);
if(d->fd < 0) {
werrstr("cannot open disk file");
free(d);
return nil;
}
if(rdonly == 0) {
d->wfd = open(disk, OWRITE);
if(d->wfd < 0)
d->rdonly = 1;
}
if(noctl)
return openfile(d);
p = malloc(strlen(disk) + 4);
if(p == nil) {
close(d->wfd);
close(d->fd);
free(d);
return nil;
}
strcpy(p, disk);
if(strlen(p) >= 7) {
q = p+strlen(p)-7;
if(q[0] == 'f' && q[1] == 'd' && isdigit(q[2]) && strcmp(q+3, "disk") == 0) {
strcpy(q+3, "ctl");
if((d->ctlfd = open(p, ORDWR)) >= 0) {
*q = '\0';
d->prefix = p;
d->type = Tfloppy;
return openfile(d);
}
}
}
if(q = strrchr(p, '/'))
q++;
else
q = p;
strcpy(q, "ctl");
if((d->ctlfd = open(p, ORDWR)) >= 0) {
*q = '\0';
d->prefix = p;
d->type = Tsd;
d->part = strdup(disk+(q-p));
if(d->part == nil){
close(d->ctlfd);
close(d->wfd);
close(d->fd);
free(p);
free(d);
return nil;
}
return opensd(d);
}
*q = '\0';
d->prefix = p;
d->type = Tfile;
return openfile(d);
}