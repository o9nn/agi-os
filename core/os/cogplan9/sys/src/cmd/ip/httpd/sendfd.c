#include <u.h>
#include <libc.h>
#include <auth.h>
#include "httpd.h"
#include "httpsrv.h"
static void printtype(Hio *hout, HContent *type, HContent *enc);
#define UNAUTHED "You are not authorized to see this area.\n"
#define NOCONTENT "No acceptable type of data is available.\n"
#define NOENCODE "No acceptable encoding of the contents is available.\n"
#define UNMATCHED "The entity requested does not match the existing entity.\n"
#define BADRANGE "No bytes are avaible for the range you requested.\n"
int
sendfd(HConnect *c, int fd, Dir *dir, HContent *type, HContent *enc)
{
Qid qid;
HRange *r;
HContents conts;
Hio *hout;
char *boundary, etag[32];
long mtime;
ulong tr;
int n, nw, multir, ok;
vlong wrote, length;
hout = &c->hout;
length = dir->length;
mtime = dir->mtime;
qid = dir->qid;
free(dir);
n = -1;
r = nil;
multir = 0;
boundary = nil;
if(c->req.vermaj){
if(type == nil && enc == nil){
conts = uriclass(c, c->req.uri);
type = conts.type;
enc = conts.encoding;
if(type == nil && enc == nil){
n = read(fd, c->xferbuf, HBufSize-1);
if(n > 0){
c->xferbuf[n] = '\0';
conts = dataclass(c, c->xferbuf, n);
type = conts.type;
enc = conts.encoding;
}
}
}
if(type == nil)
type = hmkcontent(c, "application", "octet-stream", nil);
snprint(etag, sizeof(etag), "\"%lluxv%lux\"", qid.path, qid.vers);
ok = checkreq(c, type, enc, mtime, etag);
if(ok <= 0){
close(fd);
return ok;
}
if(c->head.range == nil
|| c->head.ifrangeetag != nil && !etagmatch(1, c->head.ifrangeetag, etag)
|| c->head.ifrangedate != 0 && c->head.ifrangedate != mtime){
c->head.range = nil;
c->head.ifrangeetag = nil;
c->head.ifrangedate = 0;
}
if(c->head.range != nil){
c->head.range = fixrange(c->head.range, length);
if(c->head.range == nil){
if(c->head.ifrangeetag == nil && c->head.ifrangedate == 0){
hprint(hout, "%s 416 Request range not satisfiable\r\n", hversion);
hprint(hout, "Date: %D\r\n", time(nil));
hprint(hout, "Server: Plan9\r\n");
hprint(hout, "Content-Range: bytes */%lld\r\n", length);
hprint(hout, "Content-Length: %d\r\n", STRLEN(BADRANGE));
hprint(hout, "Content-Type: text/html\r\n");
if(c->head.closeit)
hprint(hout, "Connection: close\r\n");
else if(!http11(c))
hprint(hout, "Connection: Keep-Alive\r\n");
hprint(hout, "\r\n");
if(strcmp(c->req.meth, "HEAD") != 0)
hprint(hout, "%s", BADRANGE);
hflush(hout);
writelog(c, "Reply: 416 Request range not satisfiable\n");
close(fd);
return 1;
}
c->head.ifrangeetag = nil;
c->head.ifrangedate = 0;
}
}
if(c->head.range == nil)
hprint(hout, "%s 200 OK\r\n", hversion);
else
hprint(hout, "%s 206 Partial Content\r\n", hversion);
hprint(hout, "Server: Plan9\r\n");
hprint(hout, "Date: %D\r\n", time(nil));
hprint(hout, "ETag: %s\r\n", etag);
r = c->head.range;
if(r == nil)
hprint(hout, "Content-Length: %lld\r\n", length);
else if(r->next == nil){
hprint(hout, "Content-Range: bytes %ld-%ld/%lld\r\n", r->start, r->stop, length);
hprint(hout, "Content-Length: %ld\r\n", r->stop - r->start);
}else{
multir = 1;
boundary = hmkmimeboundary(c);
hprint(hout, "Content-Type: multipart/byteranges; boundary=%s\r\n", boundary);
}
if(c->head.ifrangeetag == nil){
hprint(hout, "Last-Modified: %D\r\n", mtime);
if(!multir)
printtype(hout, type, enc);
if(c->head.fresh_thresh)
hintprint(c, hout, c->req.uri, c->head.fresh_thresh, c->head.fresh_have);
}
if(c->head.closeit)
hprint(hout, "Connection: close\r\n");
else if(!http11(c))
hprint(hout, "Connection: Keep-Alive\r\n");
hprint(hout, "\r\n");
}
if(strcmp(c->req.meth, "HEAD") == 0){
if(c->head.range == nil)
writelog(c, "Reply: 200 file 0\n");
else
writelog(c, "Reply: 206 file 0\n");
hflush(hout);
close(fd);
return 1;
}
if(r == nil){
hflush(hout);
wrote = 0;
if(n > 0)
wrote = write(hout->fd, c->xferbuf, n);
if(n <= 0 || wrote == n){
while((n = read(fd, c->xferbuf, HBufSize)) > 0){
nw = write(hout->fd, c->xferbuf, n);
if(nw != n){
if(nw > 0)
wrote += nw;
break;
}
wrote += nw;
}
}
writelog(c, "Reply: 200 file %lld %lld\n", length, wrote);
close(fd);
if(length == wrote)
return 1;
return -1;
}
wrote = 0;
ok = 1;
for(; r != nil; r = r->next){
if(multir){
hprint(hout, "\r\n--%s\r\n", boundary);
printtype(hout, type, enc);
hprint(hout, "Content-Range: bytes %ld-%ld/%lld\r\n", r->start, r->stop, length);
hprint(hout, "Content-Length: %ld\r\n", r->stop - r->start);
hprint(hout, "\r\n");
}
hflush(hout);
if(seek(fd, r->start, 0) != r->start){
ok = -1;
break;
}
for(tr = r->stop - r->start + 1; tr; tr -= n){
n = tr;
if(n > HBufSize)
n = HBufSize;
if(read(fd, c->xferbuf, n) != n){
ok = -1;
goto breakout;
}
nw = write(hout->fd, c->xferbuf, n);
if(nw != n){
if(nw > 0)
wrote += nw;
ok = -1;
goto breakout;
}
wrote += nw;
}
}
breakout:;
if(r == nil){
if(multir){
hprint(hout, "--%s--\r\n", boundary);
hflush(hout);
}
writelog(c, "Reply: 206 partial content %lld %lld\n", length, wrote);
}else
writelog(c, "Reply: 206 partial content, early termination %lld %lld\n", length, wrote);
close(fd);
return ok;
}
static void
printtype(Hio *hout, HContent *type, HContent *enc)
{
hprint(hout, "Content-Type: %s/%s", type->generic, type->specific);
hprint(hout, "\r\n");
if(enc != nil)
hprint(hout, "Content-Encoding: %s\r\n", enc->generic);
}
int
etagmatch(int strong, HETag *tags, char *e)
{
char *s, *t;
for(; tags != nil; tags = tags->next){
if(strong && tags->weak)
continue;
s = tags->etag;
if(s[0] == '*' && s[1] == '\0')
return 1;
t = e + 1;
while(*t != '"'){
if(*s != *t)
break;
s++;
t++;
}
if(*s == '\0' && *t == '"')
return 1;
}
return 0;
}
static char *
acceptcont(char *s, char *e, HContent *ok, char *which)
{
char *sep;
if(ok == nil)
return seprint(s, e, "Your browser accepts any %s.<br>\n", which);
s = seprint(s, e, "Your browser accepts %s: ", which);
sep = "";
for(; ok != nil; ok = ok->next){
if(ok->specific)
s = seprint(s, e, "%s%s/%s", sep, ok->generic, ok->specific);
else
s = seprint(s, e, "%s%s", sep, ok->generic);
sep = ", ";
}
return seprint(s, e, ".<br>\n");
}
static int
notaccept(HConnect *c, HContent *type, HContent *enc, char *which)
{
Hio *hout;
char *s, *e;
hout = &c->hout;
e = &c->xferbuf[HBufSize];
s = c->xferbuf;
s = seprint(s, e, "<!DOCTYPE HTML PUBLIC \"-
s = seprint(s, e, "<html>\n<title>Unacceptable %s</title>\n<body>\n", which);
s = seprint(s, e, "Your browser will not accept this data, %H, because of its %s.<br>\n", c->req.uri, which);
s = seprint(s, e, "Its Content-Type is %s/%s", type->generic, type->specific);
if(enc != nil)
s = seprint(s, e, ", and Content-Encoding is %s", enc->generic);
s = seprint(s, e, ".<br>\n\n");
s = acceptcont(s, e, c->head.oktype, "Content-Type");
s = acceptcont(s, e, c->head.okencode, "Content-Encoding");
s = seprint(s, e, "</body>\n</html>\n");
hprint(hout, "%s 406 Not Acceptable\r\n", hversion);
hprint(hout, "Server: Plan9\r\n");
hprint(hout, "Date: %D\r\n", time(nil));
hprint(hout, "Content-Type: text/html\r\n");
hprint(hout, "Content-Length: %lud\r\n", s - c->xferbuf);
if(c->head.closeit)
hprint(hout, "Connection: close\r\n");
else if(!http11(c))
hprint(hout, "Connection: Keep-Alive\r\n");
hprint(hout, "\r\n");
if(strcmp(c->req.meth, "HEAD") != 0)
hwrite(hout, c->xferbuf, s - c->xferbuf);
writelog(c, "Reply: 406 Not Acceptable\nReason: %s\n", which);
return hflush(hout);
}
int
checkreq(HConnect *c, HContent *type, HContent *enc, long mtime, char *etag)
{
Hio *hout;
int m;
hout = &c->hout;
if(c->req.vermaj >= 1 && c->req.vermin >= 1 && !hcheckcontent(type, c->head.oktype, "Content-Type", 0))
return notaccept(c, type, enc, "Content-Type");
if(c->req.vermaj >= 1 && c->req.vermin >= 1 && !hcheckcontent(enc, c->head.okencode, "Content-Encoding", 0))
return notaccept(c, type, enc, "Content-Encoding");
m = etagmatch(1, c->head.ifnomatch, etag);
if(m && strcmp(c->req.meth, "GET") != 0 && strcmp(c->req.meth, "HEAD") != 0
|| c->head.ifunmodsince && c->head.ifunmodsince < mtime
|| c->head.ifmatch != nil && !etagmatch(1, c->head.ifmatch, etag)){
hprint(hout, "%s 412 Precondition Failed\r\n", hversion);
hprint(hout, "Server: Plan9\r\n");
hprint(hout, "Date: %D\r\n", time(nil));
hprint(hout, "Content-Type: text/html\r\n");
hprint(hout, "Content-Length: %d\r\n", STRLEN(UNMATCHED));
if(c->head.closeit)
hprint(hout, "Connection: close\r\n");
else if(!http11(c))
hprint(hout, "Connection: Keep-Alive\r\n");
hprint(hout, "\r\n");
if(strcmp(c->req.meth, "HEAD") != 0)
hprint(hout, "%s", UNMATCHED);
writelog(c, "Reply: 412 Precondition Failed\n");
return hflush(hout);
}
if(c->head.ifmodsince >= mtime
&& (m || c->head.ifnomatch == nil)){
hprint(hout, "%s 304 Not Modified\r\n", hversion);
hprint(hout, "Server: Plan9\r\n");
hprint(hout, "Date: %D\r\n", time(nil));
hprint(hout, "ETag: %s\r\n", etag);
if(c->head.closeit)
hprint(hout, "Connection: close\r\n");
else if(!http11(c))
hprint(hout, "Connection: Keep-Alive\r\n");
hprint(hout, "\r\n");
writelog(c, "Reply: 304 Not Modified\n");
return hflush(hout);
}
return 1;
}
HRange*
fixrange(HRange *h, long length)
{
HRange *r, *rr;
if(length == 0)
return nil;
rr = nil;
for(r = h; r != nil; r = r->next){
if(r->suffix){
r->start = length - r->stop;
if(r->start >= length)
r->start = 0;
r->stop = length - 1;
r->suffix = 0;
}
if(r->stop >= length)
r->stop = length - 1;
if(r->start > r->stop){
if(rr == nil)
h = r->next;
else
rr->next = r->next;
}else
rr = r;
}
if(h == nil)
return h;
r = h;
for(;;){
rr = r->next;
if(rr == nil)
break;
if(r->start <= rr->start && r->stop + 1 >= rr->start){
if(r->stop < rr->stop)
r->stop = rr->stop;
r->next = rr->next;
}else
r = rr;
}
return h;
}