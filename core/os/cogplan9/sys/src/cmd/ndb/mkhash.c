#include <u.h>
#include <libc.h>
#include <bio.h>
#include <ndb.h>
uchar *ht;
ulong hlen;
Ndb *db;
ulong nextchain;
char*
syserr(void)
{
static char buf[ERRMAX];
errstr(buf, sizeof buf);
return buf;
}
void
enter(char *val, ulong dboff)
{
ulong h;
uchar *last;
ulong ptr;
h = ndbhash(val, hlen);
h *= NDBPLEN;
last = &ht[h];
ptr = NDBGETP(last);
if(ptr == NDBNAP){
NDBPUTP(dboff, last);
return;
}
if(ptr & NDBCHAIN){
for(;;){
ptr &= ~NDBCHAIN;
last = &ht[ptr+NDBPLEN];
ptr = NDBGETP(last);
if(ptr == NDBNAP){
NDBPUTP(dboff, last);
return;
}
if(!(ptr & NDBCHAIN)){
NDBPUTP(nextchain|NDBCHAIN, last);
break;
}
}
} else
NDBPUTP(nextchain|NDBCHAIN, last);
NDBPUTP(ptr, &ht[nextchain]);
NDBPUTP(dboff, &ht[nextchain + NDBPLEN]);
nextchain += 2*NDBPLEN;
}
uchar nbuf[16*1024];
void
main(int argc, char **argv)
{
Ndbtuple *t, *nt;
int n;
Dir *d;
uchar buf[8];
char file[128];
int fd;
ulong off;
uchar *p;
if(argc != 3){
fprint(2, "usage: mkhash file attribute\n");
exits("usage");
}
db = ndbopen(argv[1]);
if(db == 0){
fprint(2, "mkhash: can't open %s\n", argv[1]);
exits(syserr());
}
Binits(&db->b, Bfildes(&db->b), OREAD, nbuf, sizeof(nbuf));
n = 0;
while(nt = ndbparse(db)){
for(t = nt; t; t = t->entry){
if(strcmp(t->attr, argv[2]) == 0)
n++;
}
ndbfree(nt);
}
hlen = 2*n+1;
n = hlen*NDBPLEN + hlen*2*NDBPLEN;
ht = mallocz(n, 1);
if(ht == 0){
fprint(2, "mkhash: not enough memory\n");
exits(syserr());
}
for(p = ht; p < &ht[n]; p += NDBPLEN)
NDBPUTP(NDBNAP, p);
nextchain = hlen*NDBPLEN;
Bseek(&db->b, 0, 0);
off = 0;
while(nt = ndbparse(db)){
for(t = nt; t; t = t->entry){
if(strcmp(t->attr, argv[2]) == 0)
enter(t->val, off);
}
ndbfree(nt);
off = Boffset(&db->b);
}
snprint(file, sizeof(file), "%s.%s", argv[1], argv[2]);
fd = create(file, ORDWR, 0664);
if(fd < 0){
fprint(2, "mkhash: can't create %s\n", file);
exits(syserr());
}
NDBPUTUL(db->mtime, buf);
NDBPUTUL(hlen, buf+NDBULLEN);
if(write(fd, buf, NDBHLEN) != NDBHLEN){
fprint(2, "mkhash: writing %s\n", file);
exits(syserr());
}
if(write(fd, ht, nextchain) != nextchain){
fprint(2, "mkhash: writing %s\n", file);
exits(syserr());
}
close(fd);
d = dirstat(argv[1]);
if(d == nil || d->qid.path != db->qid.path
|| d->qid.vers != db->qid.vers){
fprint(2, "mkhash: %s changed underfoot\n", argv[1]);
remove(file);
exits("changed");
}
exits(0);
}