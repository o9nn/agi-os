#include <u.h>
#include <libc.h>
#include <fcall.h>
#include <thread.h>
#include <9p.h>
#include "cifs.h"
static char magic[] = { 0xff, 'S', 'M', 'B' };
Session *
cifsdial(char *host, char *called, char *sysname)
{
int nbt, fd;
char *addr;
Session *s;
if(Debug)
fprint(2, "cifsdial: host=%s called=%s sysname=%s\n", host, called, sysname);
if((addr = netmkaddr(host, "tcp", "cifs")) == nil)
return nil;
nbt = 0;
if((fd = dial(addr, nil, nil, nil)) == -1){
nbt = 1;
if((fd = nbtdial(host, called, sysname)) == -1)
return nil;
}
s = emalloc9p(sizeof(Session));
memset(s, 0, sizeof(Session));
s->fd = fd;
s->nbt = nbt;
s->mtu = MTU;
s->pid = getpid();
s->mid = time(nil) ^ getpid();
s->uid = NO_UID;
s->seq = 0;
s->seqrun = 0;
s->secmode = SECMODE_SIGN_ENABLED;
s->flags2 = FL2_KNOWS_LONG_NAMES | FL2_HAS_LONG_NAMES | FL2_PAGEING_IO;
s->macidx = -1;
return s;
}
void
cifsclose(Session *s)
{
if(s->fd)
close(s->fd);
free(s);
}
Pkt *
cifshdr(Session *s, Share *sp, int cmd)
{
Pkt *p;
int sign, tid, dfs;
dfs = 0;
tid = NO_TID;
Active = IDLE_TIME;
werrstr("");
sign = s->secmode & SECMODE_SIGN_ENABLED? FL2_PACKET_SIGNATURES: 0;
if(sp){
tid = sp->tid;
}
p = emalloc9p(sizeof(Pkt) + MTU);
memset(p, 0, sizeof(Pkt) +MTU);
p->buf = (uchar *)p + sizeof(Pkt);
p->s = s;
qlock(&s->seqlock);
if(s->seqrun){
p->seq = s->seq;
s->seq = (s->seq + 2) % 0x10000;
}
qunlock(&s->seqlock);
nbthdr(p);
pmem(p, magic, nelem(magic));
p8(p, cmd);
pl32(p, 0);
p8(p, FL_CASELESS_NAMES | FL_CANNONICAL_NAMES);
pl16(p, s->flags2 | dfs | sign);
pl16(p, (s->pid >> 16) & 0xffff);
pl32(p, p->seq);
pl32(p, 0);
pl16(p, 0);
pl16(p, tid);
pl16(p, s->pid & 0xffff);
pl16(p, s->uid);
pl16(p, s->mid);
p->wordbase = p8(p, 0);
return p;
}
void
pbytes(Pkt *p)
{
int n;
assert(p->wordbase != nil);
assert(p->bytebase == nil);
n = p->pos - p->wordbase;
assert(n % 2 != 0);
*p->wordbase = n / 2;
p->bytebase = pl16(p, 0);
}
static void
dmp(int seq, uchar *buf)
{
int i;
if(seq == 99)
print("\n   ");
else
print("%+2d ", seq);
for(i = 0; i < 8; i++)
print("%02x ", buf[i] & 0xff);
print("\n");
}
int
cifsrpc(Pkt *p)
{
int flags2, got, err;
uint tid, uid, seq;
uchar *pos;
char m[nelem(magic)];
pos = p->pos;
if(p->bytebase){
p->pos = p->bytebase;
pl16(p, pos - (p->bytebase + 2));
}
p->pos = pos;
if(p->s->secmode & SECMODE_SIGN_ENABLED)
macsign(p, p->seq);
qlock(&p->s->rpclock);
got = nbtrpc(p);
qunlock(&p->s->rpclock);
if(got == -1)
return -1;
gmem(p, m, nelem(magic));
if(memcmp(m, magic, nelem(magic)) != 0){
werrstr("cifsrpc: bad magic number in packet %20ux%02ux%02ux%02ux",
m[0], m[1], m[2], m[3]);
return -1;
}
g8(p);
err = gl32(p);
g8(p);
flags2 = gl16(p);
gl16(p);
seq = gl32(p);
gl32(p);
gl16(p);
tid = gl16(p);
gl16(p);
uid = gl16(p);
gl16(p);
g8(p);
if(p->s->secmode & SECMODE_SIGN_ENABLED){
if(macsign(p, p->seq+1) != 0 && p->s->seqrun){
werrstr("cifsrpc: invalid packet signature");
print("MAC signature bad\n");
}
}else{
if(p->s->seqrun && seq != p->seq && seq != 0){
print("%ux != %ux bad sequence number\n", seq, p->seq);
return -1;
}
}
p->tid = tid;
if(p->s->uid == NO_UID)
p->s->uid = uid;
if(flags2 & FL2_NT_ERRCODES){
if((err & 0xF0000000) == 0xC0000000){
werrstr("%s", nterrstr(err));
return -1;
}
}else{
if(err){
werrstr("%s", doserrstr(err));
return -1;
}
}
return got;
}
int
CIFSnegotiate(Session *s, long *svrtime, char *domain, int domlen, char *cname,
int cnamlen)
{
int d, i;
char *ispeak = "NT LM 0.12";
static char *dialects[] = {
{ "NT LM 0.12" },
};
Pkt *p;
p = cifshdr(s, nil, SMB_COM_NEGOTIATE);
pbytes(p);
for(i = 0; i < nelem(dialects); i++){
p8(p, STR_DIALECT);
pstr(p, dialects[i]);
}
if(cifsrpc(p) == -1){
free(p);
return -1;
}
d = gl16(p);
if(d < 0 || d > nelem(dialects)){
werrstr("no CIFS dialect in common");
free(p);
return -1;
}
if(strcmp(dialects[d], ispeak) != 0){
werrstr("%s dialect unsupported", dialects[d]);
free(p);
return -1;
}
s->secmode = g8(p);
gl16(p);
gl16(p);
s->mtu = gl32(p);
gl32(p);
gl32(p);
s->caps = gl32(p);
*svrtime = gvtime(p);
s->tz = (short)gl16(p) * 60;
s->challen = g8(p);
gl16(p);
gmem(p, s->chal, s->challen);
gstr(p, domain, domlen);
{
char *cn = emalloc9p(cnamlen);
gstr(p, cn, cnamlen);
if(strlen(cn) > 0)
memcpy(cname, cn, cnamlen);
free(cn);
}
if(s->caps & CAP_UNICODE)
s->flags2 |= FL2_UNICODE;
free(p);
return 0;
}
int
CIFSsession(Session *s)
{
char os[64], *q;
Rune r;
Pkt *p;
enum {
mycaps = CAP_UNICODE | CAP_LARGE_FILES | CAP_NT_SMBS |
CAP_NT_FIND | CAP_STATUS32,
};
s->seqrun = 1;
p = cifshdr(s, nil, SMB_COM_SESSION_SETUP_ANDX);
p8(p, 0xFF);
p8(p, 0);
pl16(p, 0);
pl16(p, MTU);
pl16(p, 1);
pl16(p, 0);
pl32(p, 0);
if((s->secmode & SECMODE_PW_ENCRYPT) == 0) {
pl16(p, utflen(Sess->auth->resp[0])*2 + 2);
pl16(p, utflen(Sess->auth->resp[0])*2 + 2);
pl32(p, 0);
pl32(p, mycaps);
pbytes(p);
for(q = Sess->auth->resp[0]; *q; ){
q += chartorune(&r, q);
pl16(p, toupperrune(r));
}
pl16(p, 0);
for(q = Sess->auth->resp[0]; *q; ){
q += chartorune(&r, q);
pl16(p, r);
}
pl16(p, 0);
}else{
pl16(p, Sess->auth->len[0]);
pl16(p, Sess->auth->len[1]);
pl32(p, 0);
pl32(p, mycaps);
pbytes(p);
pmem(p, Sess->auth->resp[0], Sess->auth->len[0]);
pmem(p, Sess->auth->resp[1], Sess->auth->len[1]);
}
pstr(p, Sess->auth->user);
pstr(p, Sess->auth->windom);
pstr(p, "plan9");
pstr(p, argv0);
if(cifsrpc(p) == -1){
free(p);
return -1;
}
g8(p);
gl16(p);
Sess->isguest = gl16(p) & 1;
gl16(p);
gl16(p);
gstr(p, os, sizeof(os));
s->remos = estrdup9p(os);
free(p);
return 0;
}
CIFStreeconnect(Session *s, char *cname, char *tree, Share *sp)
{
int len;
char *resp, *path;
char zeros[24];
Pkt *p;
resp = Sess->auth->resp[0];
len  = Sess->auth->len[0];
if((s->secmode & SECMODE_USER) != SECMODE_USER){
memset(zeros, 0, sizeof(zeros));
resp = zeros;
len = sizeof(zeros);
}
p = cifshdr(s, nil, SMB_COM_TREE_CONNECT_ANDX);
p8(p, 0xFF);
p8(p, 0);
pl16(p, 0);
pl16(p, 0);
if((s->secmode & SECMODE_PW_ENCRYPT) == 0){
pl16(p, len+1);
pbytes(p);
pascii(p, resp);
}else{
pl16(p, len);
pbytes(p);
pmem(p, resp, len);
}
path = smprint("
strupr(path);
ppath(p, path);
free(path);
pascii(p, "?????");
if(cifsrpc(p) == -1){
free(p);
return -1;
}
g8(p);
g8(p);
gl16(p);
sp->options = g8(p);
sp->tid = p->tid;
free(p);
return 0;
}
int
CIFSlogoff(Session *s)
{
int rc;
Pkt *p;
p = cifshdr(s, nil, SMB_COM_LOGOFF_ANDX);
p8(p, 0xFF);
p8(p, 0);
pl16(p, 0);
pbytes(p);
rc = cifsrpc(p);
free(p);
return rc;
}
int
CIFStreedisconnect(Session *s, Share *sp)
{
int rc;
Pkt *p;
p = cifshdr(s, sp, SMB_COM_TREE_DISCONNECT);
pbytes(p);
rc = cifsrpc(p);
free(p);
return rc;
}
int
CIFSdeletefile(Session *s, Share *sp, char *name)
{
int rc;
Pkt *p;
p = cifshdr(s, sp, SMB_COM_DELETE);
pl16(p, ATTR_HIDDEN|ATTR_SYSTEM);
pbytes(p);
p8(p, STR_ASCII);
ppath(p, name);
rc = cifsrpc(p);
free(p);
return rc;
}
int
CIFSdeletedirectory(Session *s, Share *sp, char *name)
{
int rc;
Pkt *p;
p = cifshdr(s, sp, SMB_COM_DELETE_DIRECTORY);
pbytes(p);
p8(p, STR_ASCII);
ppath(p, name);
rc = cifsrpc(p);
free(p);
return rc;
}
int
CIFScreatedirectory(Session *s, Share *sp, char *name)
{
int rc;
Pkt *p;
p = cifshdr(s, sp, SMB_COM_CREATE_DIRECTORY);
pbytes(p);
p8(p, STR_ASCII);
ppath(p, name);
rc = cifsrpc(p);
free(p);
return rc;
}
int
CIFSrename(Session *s, Share *sp, char *old, char *new)
{
int rc;
Pkt *p;
p = cifshdr(s, sp, SMB_COM_RENAME);
pl16(p, ATTR_HIDDEN|ATTR_SYSTEM|ATTR_DIRECTORY);
pbytes(p);
p8(p, STR_ASCII);
ppath(p, old);
p8(p, STR_ASCII);
ppath(p, new);
rc = cifsrpc(p);
free(p);
return rc;
}
int
CIFS_NT_opencreate(Session *s, Share *sp, char *name, int flags, int options,
int attrs, int access, int share, int action, int *result, FInfo *fi)
{
Pkt *p;
int fh;
p = cifshdr(s, sp, SMB_COM_NT_CREATE_ANDX);
p8(p, 0xFF);
p8(p, 0);
pl16(p, 0);
p8(p, 0);
pl16(p, utflen(name) *2);
pl32(p, flags);
pl32(p, 0);
pl32(p, access);
pl64(p, 0);
pl32(p, attrs);
pl32(p, share);
pl32(p, action);
pl32(p, options);
pl32(p, SECURITY_IMPERSONATION);
p8(p, SECURITY_CONTEXT_TRACKING | SECURITY_EFFECTIVE_ONLY);
pbytes(p);
p8(p, 0);
ppath(p, name);
if(cifsrpc(p) == -1){
free(p);
return -1;
}
memset(fi, 0, sizeof(FInfo));
g8(p);
g8(p);
gl16(p);
g8(p);
fh = gl16(p);
*result = gl32(p);
gl64(p);
fi->accessed = gvtime(p);
fi->written = gvtime(p);
fi->changed = gvtime(p);
fi->attribs = gl32(p);
gl64(p);
fi->size = gl64(p);
free(p);
return fh;
}
CIFS_SMB_opencreate(Session *s, Share *sp, char *name, int access,
int attrs, int action, int *result)
{
Pkt *p;
int fh;
p = cifshdr(s, sp, SMB_COM_OPEN_ANDX);
p8(p, 0xFF);
p8(p, 0);
pl16(p, 0);
pl16(p, 0);
pl16(p, access);
pl16(p, ATTR_HIDDEN|ATTR_SYSTEM);
pl16(p, attrs);
pdatetime(p, 0);
pl16(p, action);
pl32(p, 0);
pl32(p, 0);
pl32(p, 0);
pbytes(p);
ppath(p, name);
if(cifsrpc(p) == -1){
free(p);
return -1;
}
g8(p);
g8(p);
gl16(p);
fh = gl16(p);
gl16(p);
gvtime(p);
gl32(p);
gl16(p);
gl16(p);
*result = gl16(p);
free(p);
return fh;
}
vlong
CIFSwrite(Session *s, Share *sp, int fh, uvlong off, void *buf, vlong n)
{
Pkt *p;
vlong got;
assert((n   & 0xffffffff00000000LL) == 0 || s->caps & CAP_LARGE_FILES);
assert((off & 0xffffffff00000000LL) == 0 || s->caps & CAP_LARGE_FILES);
assert(n < s->mtu - T2HDRLEN || s->caps & CAP_LARGE_WRITEX);
p = cifshdr(s, sp, SMB_COM_WRITE_ANDX);
p8(p, 0xFF);
p8(p, 0);
pl16(p, 0);
pl16(p, fh);
pl32(p, off & 0xffffffff);
pl32(p, 0);
pl16(p, s->nocache);
pl16(p, 0);
pl16(p, n >> 16);
pl16(p, n & 0xffffffff);
pl16(p, T2HDRLEN);
pl32(p, off >> 32);
pbytes(p);
p->pos = p->buf +T2HDRLEN +NBHDRLEN;
pmem(p, buf, n);
if(cifsrpc(p) == -1){
free(p);
return -1;
}
g8(p);
g8(p);
gl16(p);
got = gl16(p);
gl16(p);
got |= (gl16(p) << 16);
free(p);
return got;
}
vlong
CIFSread(Session *s, Share *sp, int fh, uvlong off, void *buf, vlong n,
vlong minlen)
{
int doff;
vlong got;
Pkt *p;
assert((n   & 0xffffffff00000000LL) == 0 || s->caps & CAP_LARGE_FILES);
assert((off & 0xffffffff00000000LL) == 0 || s->caps & CAP_LARGE_FILES);
assert(n < s->mtu - T2HDRLEN || s->caps & CAP_LARGE_READX);
p = cifshdr(s, sp, SMB_COM_READ_ANDX);
p8(p, 0xFF);
p8(p, 0);
pl16(p, 0);
pl16(p, fh);
pl32(p, off & 0xffffffff);
pl16(p, n);
pl16(p, minlen);
pl32(p, (uint)n >> 16);
pl16(p, 0);
pl32(p, off >> 32);
pbytes(p);
if(cifsrpc(p) == -1){
free(p);
return -1;
}
g8(p);
g8(p);
gl16(p);
gl16(p);
gl16(p);
gl16(p);
got = gl16(p);
doff = gl16(p);
got |= gl16(p) << 16;
p->pos = p->buf + doff + NBHDRLEN;
gmem(p, buf, got);
free(p);
return got;
}
int
CIFSflush(Session *s, Share *sp, int fh)
{
int rc;
Pkt *p;
p = cifshdr(s, sp, SMB_COM_FLUSH);
pl16(p, fh);
pbytes(p);
rc = cifsrpc(p);
free(p);
return rc;
}
int
CIFSclose(Session *s, Share *sp, int fh)
{
int rc;
Pkt *p;
p = cifshdr(s, sp, SMB_COM_CLOSE);
pl16(p, fh);
pl32(p, ~0L);
pbytes(p);
rc = cifsrpc(p);
free(p);
return rc;
}
int
CIFSfindclose2(Session *s, Share *sp, int sh)
{
int rc;
Pkt *p;
p = cifshdr(s, sp, SMB_COM_FIND_CLOSE2);
pl16(p, sh);
pbytes(p);
rc = cifsrpc(p);
free(p);
return rc;
}
int
CIFSecho(Session *s)
{
Pkt *p;
int rc;
p = cifshdr(s, nil, SMB_COM_ECHO);
pl16(p, 1);
pbytes(p);
pascii(p, "abcdefghijklmnopqrstuvwxyz");
rc = cifsrpc(p);
free(p);
return rc;
}
int
CIFSsetinfo(Session *s, Share *sp, char *path, FInfo *fip)
{
int rc;
Pkt *p;
p = cifshdr(s, sp, SMB_COM_SET_INFORMATION);
pl16(p, fip->attribs);
pl32(p, time(nil) - s->tz);
pl64(p, 0);
pl16(p, 0);
pbytes(p);
p8(p, STR_ASCII);
ppath(p, path);
rc = cifsrpc(p);
free(p);
return rc;
}