#include <u.h>
#include <libc.h>
#include <fcall.h>
#include <thread.h>
#include <9p.h>
#include "cifs.h"
static Pkt *
t2hdr(Session *s, Share *sp, int cmd)
{
Pkt *p;
p = cifshdr(s, sp, SMB_COM_TRANSACTION2);
p->tbase = pl16(p, 0);
pl16(p, 0);
pl16(p, 64);
pl16(p, (MTU - T2HDRLEN)-64);
p8(p, 0);
p8(p, 0);
pl16(p, 0);
pl32(p, 1000);
pl16(p, 0);
pl16(p, 0);
pl16(p, 0);
pl16(p, 0);
pl16(p, 0);
p8(p, 1);
p8(p, 0);
pl16(p, cmd);
pbytes(p);
p8(p, 0);
return p;
}
static void
pt2param(Pkt *p)
{
uchar *pos = p->pos;
assert(p->tbase != 0);
p->pos = p->tbase + 20;
pl16(p, (pos - p->buf) - NBHDRLEN);
p->tparam = p->pos = pos;
}
static void
pt2data(Pkt *p)
{
uchar *pos = p->pos;
assert(p->tbase != 0);
assert(p->tparam != 0);
p->pos = p->tbase +0;
pl16(p, pos - p->tparam);
p->pos = p->tbase +18;
pl16(p, pos - p->tparam);
p->pos = p->tbase +24;
pl16(p, (pos - p->buf) - NBHDRLEN);
p->tdata = p->pos = pos;
}
static int
t2rpc(Pkt *p)
{
int got;
uchar *pos;
assert(p->tbase != 0);
assert(p->tdata != 0);
pos = p->pos;
p->pos = p->tbase +2;
pl16(p, pos - p->tdata);
p->pos = p->tbase +22;
pl16(p, pos - p->tdata);
p->pos = pos;
if((got = cifsrpc(p)) == -1)
return -1;
gl16(p);
gl16(p);
gl16(p);
gl16(p);
p->tparam = p->buf +NBHDRLEN +gl16(p);
gl16(p);
gl16(p);
p->tdata = p->buf +NBHDRLEN +gl16(p);
gl16(p);
g8(p);
g8(p);
return got;
}
static void
gt2param(Pkt *p)
{
p->pos = p->tparam;
}
static void
gt2data(Pkt *p)
{
p->pos = p->tdata;
}
int
T2findfirst(Session *s, Share *sp, int slots, char *path, int *got,
long *resume, FInfo *fip)
{
int pktlen, i, n, sh;
uchar *next;
Pkt *p;
p = t2hdr(s, sp, TRANS2_FIND_FIRST2);
p8(p, 'D');
p8(p, ' ');
pt2param(p);
pl16(p, ATTR_HIDDEN|ATTR_SYSTEM|ATTR_DIRECTORY);
pl16(p, slots);
pl16(p, CIFS_SEARCH_RETURN_RESUME);
pl16(p, SMB_FIND_FILE_FULL_DIRECTORY_INFO);
pl32(p, 0);
ppath(p, path);
pt2data(p);
if((pktlen = t2rpc(p)) == -1){
free(p);
return -1;
}
s->lastfind = nsec();
gt2param(p);
sh = gl16(p);
*got = gl16(p);
gl16(p);
gl16(p);
gl16(p);
gt2data(p);
memset(fip, 0, slots * sizeof(FInfo));
for(i = 0; i < *got; i++){
next = p->pos;
next += gl32(p);
if(next - p->buf > pktlen){
*got = i;
break;
}
*resume = gl32(p);
fip[i].created = gvtime(p);
fip[i].accessed = gvtime(p);
fip[i].written = gvtime(p);
fip[i].changed = gvtime(p);
fip[i].size = gl64(p);
gl64(p);
fip[i].attribs = gl32(p);
n = gl32(p);
gl32(p);
gstr(p, fip[i].name, n);
p->pos = next;
}
free(p);
return sh;
}
int
T2findnext(Session *s, Share *sp, int slots, char *path, int *got,
long *resume, FInfo *fip, int sh)
{
Pkt *p;
int i, n;
uchar *next;
if(!(s->caps & CAP_NT_SMBS) && nsec() - s->lastfind < 200000000LL)
sleep(200);
p = t2hdr(s, sp, TRANS2_FIND_NEXT2);
p8(p, 'D');
p8(p, ' ');
pt2param(p);
pl16(p, sh);
pl16(p, slots);
pl16(p, SMB_FIND_FILE_FULL_DIRECTORY_INFO);
pl32(p, *resume);
pl16(p, CIFS_SEARCH_CONTINUE_FROM_LAST);
ppath(p, path);
pt2data(p);
if(t2rpc(p) == -1){
free(p);
return -1;
}
s->lastfind = nsec();
gt2param(p);
*got = gl16(p);
gl16(p);
gl16(p);
gl16(p);
gt2data(p);
memset(fip, 0, slots * sizeof(FInfo));
for(i = 0; i < *got; i++){
next = p->pos;
next += gl32(p);
*resume = gl32(p);
fip[i].created = gvtime(p);
fip[i].accessed = gvtime(p);
fip[i].written = gvtime(p);
fip[i].changed = gvtime(p);
fip[i].size = gl64(p);
gl64(p);
fip[i].attribs = gl32(p);
n = gl32(p);
gl32(p);
gstr(p, fip[i].name, n);
p->pos = next;
}
free(p);
return 0;
}
int
T2queryall(Session *s, Share *sp, char *path, FInfo *fip)
{
int n;
Pkt *p;
p = t2hdr(s, sp, TRANS2_QUERY_PATH_INFORMATION);
pt2param(p);
pl16(p, SMB_QUERY_FILE_ALL_INFO);
pl32(p, 0);
ppath(p, path);
pt2data(p);
if(t2rpc(p) == -1){
free(p);
return -1;
}
gt2data(p);
memset(fip, 0, sizeof(FInfo));
fip->created = gvtime(p);
fip->accessed = gvtime(p);
fip->written = gvtime(p);
fip->changed = gvtime(p);
fip->attribs = gl32(p);
gl32(p);
gl64(p);
fip->size = gl64(p);
gl32(p);
g8(p);
g8(p);
gl16(p);
gl32(p);
n = gl32(p);
if(n >= sizeof fip->name)
n = sizeof fip->name - 1;
gstr(p, fip->name, n);
free(p);
return 0;
}
int
T2querystandard(Session *s, Share *sp, char *path, FInfo *fip)
{
Pkt *p;
p = t2hdr(s, sp, TRANS2_QUERY_PATH_INFORMATION);
pt2param(p);
pl16(p, SMB_INFO_STANDARD);
pl32(p, 0);
ppath(p, path);
pt2data(p);
if(t2rpc(p) == -1){
free(p);
return -1;
}
gt2data(p);
memset(fip, 0, sizeof(FInfo));
fip->created = gdatetime(p);
fip->accessed = gdatetime(p);
fip->written = gdatetime(p);
fip->changed = fip->written;
fip->size = gl32(p);
gl32(p);
fip->attribs = gl16(p);
gl32(p);
free(p);
return 0;
}
int
T2setpathinfo(Session *s, Share *sp, char *path, FInfo *fip)
{
int rc;
Pkt *p;
p = t2hdr(s, sp, TRANS2_SET_PATH_INFORMATION);
pt2param(p);
pl16(p, SMB_INFO_STANDARD);
pl32(p, 0);
ppath(p, path);
pt2data(p);
pdatetime(p, fip->created);
pdatetime(p, fip->accessed);
pdatetime(p, fip->written);
pl32(p, fip->size);
pl32(p, 0);
pl16(p, fip->attribs);
pl32(p, 0);
pl16(p, 0);
rc = t2rpc(p);
free(p);
return rc;
}
int
T2setfilelength(Session *s, Share *sp, int fh, FInfo *fip)
{
int rc;
Pkt *p;
p = t2hdr(s, sp, TRANS2_SET_FILE_INFORMATION);
pt2param(p);
pl16(p, fh);
pl16(p, SMB_SET_FILE_END_OF_FILE_INFO);
pl16(p, 0);
pt2data(p);
pl64(p, fip->size);
pl32(p, 0);
pl16(p, 0);
rc = t2rpc(p);
free(p);
return rc;
}
int
T2fsvolumeinfo(Session *s, Share *sp, long *created, long *serialno,
char *label, int labellen)
{
Pkt *p;
long ct, sn, n;
p = t2hdr(s, sp, TRANS2_QUERY_FS_INFORMATION);
pt2param(p);
pl16(p, SMB_QUERY_FS_VOLUME_INFO);
pt2data(p);
if(t2rpc(p) == -1){
free(p);
return -1;
}
gt2data(p);
ct = gvtime(p);
sn = gl32(p);
n = gl32(p);
g8(p);
g8(p);
memset(label, 0, labellen);
if(n < labellen && n > 0)
gstr(p, label, n);
if(created)
*created = ct;
if(serialno)
*serialno = sn;
free(p);
return 0;
}
int
T2fssizeinfo(Session *s, Share *sp, uvlong *total, uvlong *unused)
{
Pkt *p;
uvlong t, f, n, b;
p = t2hdr(s, sp, TRANS2_QUERY_FS_INFORMATION);
pt2param(p);
pl16(p, SMB_QUERY_FS_SIZE_INFO);
pt2data(p);
if(t2rpc(p) == -1){
free(p);
return -1;
}
gt2data(p);
t = gl64(p);
f = gl64(p);
n = gl32(p);
b = gl32(p);
if(free)
*unused = f * n * b;
if(total)
*total = t * n * b;
free(p);
return 0;
}
int
T2getdfsreferral(Session *s, Share *sp, char *path, int *gflags, int *used,
Refer *re, int nent)
{
int i, vers, nret, len;
char tmp[1024];
uchar *base;
Pkt *p;
p = t2hdr(s, sp, TRANS2_GET_DFS_REFERRAL);
pt2param(p);
pl16(p, 3);
ppath(p, path);
pt2data(p);
if(t2rpc(p) == -1){
free(p);
return -1;
}
memset(re, 0, sizeof *re * nent);
gt2data(p);
*used = gl16(p) / 2;
nret = gl16(p);
*gflags = gl32(p);
for(i = 0; i < nret && i < nent && i < 16; i++){
base = p->pos;
vers = gl16(p);
len = gl16(p);
re[i].type = gl16(p);
re[i].flags = gl16(p);
switch(vers){
case 1:
re[i].prox = 0;
re[i].ttl = 5*60;
gstr(p, tmp, sizeof tmp);
re[i].addr = estrdup9p(tmp);
re[i].path = estrdup9p(tmp);
break;
case 2:
re[i].prox = gl32(p);
re[i].ttl = gl32(p);
goff(p, base, re[i].path, sizeof tmp);
re[i].path = estrdup9p(tmp);
goff(p, base, re[i].path, sizeof tmp);
goff(p, base, tmp, sizeof tmp);
re[i].addr = estrdup9p(tmp);
break;
case 3:
if(re[i].flags & DFS_REFERAL_LIST){
re[i].prox = 0;
re[i].ttl = gl32(p);
goff(p, base, tmp, sizeof tmp);
re[i].path = estrdup9p(tmp);
gl16(p);
goff(p, base, tmp, sizeof tmp);
re[i].addr = estrdup9p(tmp);
}
else{
re[i].prox = 0;
re[i].ttl = gl32(p);
goff(p, base, tmp, sizeof tmp);
re[i].path = estrdup9p(tmp);
gl16(p);
goff(p, base, tmp, sizeof tmp);
re[i].addr = estrdup9p(tmp);
gl16(p);
}
break;
default:
fprint(2, "%d - unsupported DFS infolevel\n", vers);
re[i].path = estrdup9p(tmp);
re[i].addr = estrdup9p(tmp);
break;
}
p->pos = base+len;
}
free(p);
return i;
}