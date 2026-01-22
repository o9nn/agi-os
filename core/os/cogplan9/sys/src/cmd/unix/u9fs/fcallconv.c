#include <plan9.h>
#include <fcall.h>
#include <oldfcall.h>
extern int old9p;
static uint dumpsome(char*, char*, long);
static void fdirconv(char*, Dir*);
static char *qidtype(char*, uchar);
#define QIDFMT "(%.16llux %lud %s)"
int
fcallconv(va_list *arg, Fconv *f1)
{
Fcall *f;
int fid, type, tag, n, i;
char buf[512], tmp[200];
Dir *d;
Qid *q;
f = va_arg(*arg, Fcall*);
type = f->type;
fid = f->fid;
tag = f->tag;
switch(type){
case Tversion:
sprint(buf, "Tversion tag %ud msize %ud version '%s'", tag, f->msize, f->version);
break;
case Rversion:
sprint(buf, "Rversion tag %ud msize %ud version '%s'", tag, f->msize, f->version);
break;
case Tauth:
sprint(buf, "Tauth tag %ud afid %d uname %s aname %s", tag,
f->afid, f->uname, f->aname);
break;
case Rauth:
sprint(buf, "Rauth tag %ud qid " QIDFMT, tag,
f->aqid.path, f->aqid.vers, qidtype(tmp, f->aqid.type));
break;
case Tattach:
sprint(buf, "Tattach tag %ud fid %d afid %d uname %s aname %s", tag,
fid, f->afid, f->uname, f->aname);
break;
case Rattach:
sprint(buf, "Rattach tag %ud qid " QIDFMT, tag,
f->qid.path, f->qid.vers, qidtype(tmp, f->qid.type));
break;
case Rerror:
sprint(buf, "Rerror tag %ud ename %s", tag, f->ename);
break;
case Tflush:
sprint(buf, "Tflush tag %ud oldtag %ud", tag, f->oldtag);
break;
case Rflush:
sprint(buf, "Rflush tag %ud", tag);
break;
case Twalk:
n = sprint(buf, "Twalk tag %ud fid %d newfid %d nwname %d ", tag, fid, f->newfid, f->nwname);
for(i=0; i<f->nwname; i++)
n += sprint(buf+n, "%d:%s ", i, f->wname[i]);
break;
case Rwalk:
n = sprint(buf, "Rwalk tag %ud nwqid %ud ", tag, f->nwqid);
for(i=0; i<f->nwqid; i++){
q = &f->wqid[i];
n += sprint(buf+n, "%d:" QIDFMT " ", i,
q->path, q->vers, qidtype(tmp, q->type));
}
break;
case Topen:
sprint(buf, "Topen tag %ud fid %ud mode %d", tag, fid, f->mode);
break;
case Ropen:
sprint(buf, "Ropen tag %ud qid " QIDFMT " iounit %ud ", tag,
f->qid.path, f->qid.vers, qidtype(tmp, f->qid.type), f->iounit);
break;
case Tcreate:
sprint(buf, "Tcreate tag %ud fid %ud name %s perm %M mode %d",
tag, fid, f->name, (ulong)f->perm, f->mode);
break;
case Rcreate:
sprint(buf, "Rcreate tag %ud qid " QIDFMT " iounit %ud ", tag,
f->qid.path, f->qid.vers, qidtype(tmp, f->qid.type), f->iounit);
break;
case Tread:
sprint(buf, "Tread tag %ud fid %d offset %lld count %ud",
tag, fid, f->offset, f->count);
break;
case Rread:
n = sprint(buf, "Rread tag %ud count %ud ", tag, f->count);
dumpsome(buf+n, f->data, f->count);
break;
case Twrite:
n = sprint(buf, "Twrite tag %ud fid %d offset %lld count %ud ",
tag, fid, f->offset, f->count);
dumpsome(buf+n, f->data, f->count);
break;
case Rwrite:
sprint(buf, "Rwrite tag %ud count %ud", tag, f->count);
break;
case Tclunk:
sprint(buf, "Tclunk tag %ud fid %ud", tag, fid);
break;
case Rclunk:
sprint(buf, "Rclunk tag %ud", tag);
break;
case Tremove:
sprint(buf, "Tremove tag %ud fid %ud", tag, fid);
break;
case Rremove:
sprint(buf, "Rremove tag %ud", tag);
break;
case Tstat:
sprint(buf, "Tstat tag %ud fid %ud", tag, fid);
break;
case Rstat:
n = sprint(buf, "Rstat tag %ud ", tag);
if(f->nstat > sizeof tmp)
sprint(buf+n, " stat(%d bytes)", f->nstat);
else{
d = (Dir*)tmp;
(old9p?convM2Dold:convM2D)(f->stat, f->nstat, d, (char*)(d+1));
sprint(buf+n, " stat ");
fdirconv(buf+n+6, d);
}
break;
case Twstat:
n = sprint(buf, "Twstat tag %ud fid %ud", tag, fid);
if(f->nstat > sizeof tmp)
sprint(buf+n, " stat(%d bytes)", f->nstat);
else{
d = (Dir*)tmp;
(old9p?convM2Dold:convM2D)(f->stat, f->nstat, d, (char*)(d+1));
sprint(buf+n, " stat ");
fdirconv(buf+n+6, d);
}
break;
case Rwstat:
sprint(buf, "Rwstat tag %ud", tag);
break;
default:
sprint(buf, "unknown type %d", type);
}
strconv(buf, f1);
return(sizeof(Fcall*));
}
static char*
qidtype(char *s, uchar t)
{
char *p;
p = s;
if(t & QTDIR)
*p++ = 'd';
if(t & QTAPPEND)
*p++ = 'a';
if(t & QTEXCL)
*p++ = 'l';
if(t & QTMOUNT)
*p++ = 'm';
if(t & QTAUTH)
*p++ = 'A';
*p = '\0';
return s;
}
int
dirconv(va_list *arg, Fconv *f)
{
char buf[160];
fdirconv(buf, va_arg(*arg, Dir*));
strconv(buf, f);
return(sizeof(Dir*));
}
static void
fdirconv(char *buf, Dir *d)
{
char tmp[16];
sprint(buf, "'%s' '%s' '%s' '%s' "
"q " QIDFMT " m %#luo "
"at %ld mt %ld l %lld "
"t %d d %d",
d->name, d->uid, d->gid, d->muid,
d->qid.path, d->qid.vers, qidtype(tmp, d->qid.type), d->mode,
d->atime, d->mtime, d->length,
d->type, d->dev);
}
#define DUMPL 64
static uint
dumpsome(char *ans, char *buf, long count)
{
int i, printable;
char *p;
printable = 1;
if(count > DUMPL)
count = DUMPL;
for(i=0; i<count && printable; i++)
if((buf[i]<32 && buf[i] !='\n' && buf[i] !='\t') || (uchar)buf[i]>127)
printable = 0;
p = ans;
*p++ = '\'';
if(printable){
memmove(p, buf, count);
p += count;
}else{
for(i=0; i<count; i++){
if(i>0 && i%4==0)
*p++ = ' ';
sprint(p, "%2.2ux", (uchar)buf[i]);
p += 2;
}
}
*p++ = '\'';
*p = 0;
return p - ans;
}