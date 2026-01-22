#include <u.h>
#include <libc.h>
#include <draw.h>
#include <event.h>
#include <bio.h>
#include <ctype.h>
#include "page.h"
typedef struct PSInfo	PSInfo;
typedef struct Page	Page;
struct Page {
char *name;
int offset;
};
struct PSInfo {
GSInfo;
Rectangle bbox;
Page *page;
int npage;
int clueless;
long psoff;
char ctm[256];
};
static int	pswritepage(Document *d, int fd, int page);
static Image*	psdrawpage(Document *d, int page);
static char*	pspagename(Document*, int);
#define R(r) (r).min.x, (r).min.y, (r).max.x, (r).max.y
Rectangle
rdbbox(char *p)
{
Rectangle r;
int a;
char *f[4];
while(*p == ':' || *p == ' ' || *p == '\t')
p++;
if(tokenize(p, f, 4) != 4)
return Rect(0,0,0,0);
r = Rect(atoi(f[0]), atoi(f[1]), atoi(f[2]), atoi(f[3]));
r = canonrect(r);
if(Dx(r) <= 0 || Dy(r) <= 0)
return Rect(0,0,0,0);
if(truetoboundingbox)
return r;
if(chatty) fprint(2, "[%d %d %d %d] -> ", R(r));
a = Dx(r)*Dy(r);
if(a < 300*300){
} else if(Dx(r) <= 596 && r.max.x <= 596 && Dy(r) > 792 && Dy(r) <= 842 && r.max.y <= 842)
r = Rect(0, 0, 596, 842);
else {
if(Dx(r) <= 612 && r.max.x <= 612){
r.min.x = 0;
r.max.x = 612;
}
if(Dy(r) <= 792 && r.max.y <= 792){
r.min.y = 0;
r.max.y = 792;
}
}
if(chatty) fprint(2, "[%d %d %d %d]\n", R(r));
return r;
}
#define RECT(X) X.min.x, X.min.y, X.max.x, X.max.y
int
prefix(char *x, char *y)
{
return strncmp(x, y, strlen(y)) == 0;
}
void
repaginate(PSInfo *ps, int n)
{
int i, np, onp;
Page *page;
page = ps->page;
onp = ps->npage;
np = (ps->npage+n-1)/n;
if(chatty) {
for(i=0; i<=onp+1; i++)
print("page %d: %d\n", i, page[i].offset);
}
for(i=0; i<np; i++)
page[i] = page[n*i];
page[np] = page[onp];
page[np+1] = page[onp+1];
ps->npage = np;
if(chatty) {
for(i=0; i<=np+1; i++)
print("page %d: %d\n", i, page[i].offset);
}
}
Document*
initps(Biobuf *b, int argc, char **argv, uchar *buf, int nbuf)
{
Document *d;
PSInfo *ps;
char *p;
char *q, *r;
char eol;
char *nargv[1];
char fdbuf[20];
char tmp[32];
int fd;
int i;
int incomments;
int cantranslate;
int trailer=0;
int nesting=0;
int dumb=0;
int landscape=0;
long psoff;
long npage, mpage;
Page *page;
Rectangle bbox = Rect(0,0,0,0);
if(argc > 1) {
fprint(2, "can only view one ps file at a time\n");
return nil;
}
fprint(2, "reading through postscript...\n");
if(b == nil){
fd = spooltodisk(buf, nbuf, nil);
sprint(fdbuf, "/fd/%d", fd);
b = Bopen(fdbuf, OREAD);
if(b == nil){
fprint(2, "cannot open disk spool file\n");
wexits("Bopen temp");
}
nargv[0] = fdbuf;
argv = nargv;
}
Bseek(b, 0, 0);
psoff = 0;
eol = 0;
for(i=0; i<16; i++){
psoff = Boffset(b);
if(!(p = Brdline(b, eol='\n')) && !(p = Brdline(b, eol='\r'))) {
fprint(2, "cannot find end of first line\n");
wexits("initps");
}
if(p[0]=='\x1B')
p++, psoff++;
if(p[0] == '%' && p[1] == '!')
break;
}
if(i == 16){
werrstr("not ps");
return nil;
}
npage = 0;
mpage = 16;
page = emalloc(mpage*sizeof(*page));
memset(page, 0, mpage*sizeof(*page));
cantranslate = goodps;
incomments = 1;
Keepreading:
while(p = Brdline(b, eol)) {
if(p[0] == '%')
if(chatty > 1) fprint(2, "ps %.*s\n", utfnlen(p, Blinelen(b)-1), p);
if(npage == mpage) {
mpage *= 2;
page = erealloc(page, mpage*sizeof(*page));
memset(&page[npage], 0, npage*sizeof(*page));
}
if(p[0] != '%' || p[1] != '%')
continue;
if(prefix(p, "%%BeginDocument")) {
nesting++;
continue;
}
if(nesting > 0 && prefix(p, "%%EndDocument")) {
nesting--;
continue;
}
if(nesting)
continue;
if(prefix(p, "%%EndComment")) {
incomments = 0;
continue;
}
if(reverse == -1 && prefix(p, "%%PageOrder")) {
p[Blinelen(b)-1] = 0;
if(strstr(p, "Ascend"))
reverse = 0;
else if(strstr(p, "Descend"))
reverse = 1;
else if(strstr(p, "Special"))
dumb = 1;
p[Blinelen(b)-1] = '\n';
continue;
} else if(prefix(p, "%%Trailer")) {
incomments = 1;
page[npage].offset = Boffset(b)-Blinelen(b);
trailer = 1;
continue;
} else if(incomments && prefix(p, "%%Orientation")) {
if(strstr(p, "Landscape"))
landscape = 1;
} else if(incomments && Dx(bbox)==0 && prefix(p, q="%%BoundingBox")) {
bbox = rdbbox(p+strlen(q)+1);
if(chatty)
fprint(2, "document bbox [%d %d %d %d]\n",
RECT(bbox));
continue;
}
p[Blinelen(b)-1] = 0;
if((q=strstr(p, "initgraphics")) && ((r=strchr(p, '%'))==nil || r > q))
cantranslate = 0;
p[Blinelen(b)-1] = eol;
if(!prefix(p, "%%Page:"))
continue;
p[Blinelen(b)-1] = 0;
if(chatty) fprint(2, "page %s\n", p);
r = p+7;
while(*r == ' ' || *r == '\t')
r++;
q = r;
while(*q && *q != ' ' && *q != '\t')
q++;
free(page[npage].name);
if(*r) {
if(*r == '"' && *q == '"')
r++, q--;
if(*q)
*q = 0;
page[npage].name = estrdup(r);
*q = 'x';
} else {
snprint(tmp, sizeof tmp, "p %ld", npage+1);
page[npage].name = estrdup(tmp);
}
trailer = 0;
p[Blinelen(b)-1] = eol;
page[npage++].offset = Boffset(b)-Blinelen(b);
}
if(Blinelen(b) > 0){
fprint(2, "page: linelen %d\n", Blinelen(b));
Bseek(b, Blinelen(b), 1);
goto Keepreading;
}
if(Dx(bbox) == 0 || Dy(bbox) == 0)
bbox = Rect(0,0,612,792);
if(npage == 0) {
dumb = 1;
if(chatty) fprint(2, "don't know where pages are\n");
reverse = 0;
goodps = 0;
trailer = 0;
page[npage].name = "p 1";
page[npage++].offset = 0;
}
if(npage+2 > mpage) {
mpage += 2;
page = erealloc(page, mpage*sizeof(*page));
memset(&page[mpage-2], 0, 2*sizeof(*page));
}
if(!trailer)
page[npage].offset = Boffset(b);
Bseek(b, 0, 2);
page[npage+1].offset = Boffset(b);
d = emalloc(sizeof(*d));
ps = emalloc(sizeof(*ps));
ps->page = page;
ps->npage = npage;
ps->bbox = bbox;
ps->psoff = psoff;
d->extra = ps;
d->npage = ps->npage;
d->b = b;
d->drawpage = psdrawpage;
d->pagename = pspagename;
d->fwdonly = ps->clueless = dumb;
d->docname = argv[0];
if(spawngs(ps, "-dSAFER") < 0)
return nil;
if(!cantranslate)
bbox.min = ZP;
setdim(ps, bbox, ppi, landscape);
if(goodps){
pswritepage(d, ps->gsfd, -1);
waitgs(ps);
}
if(dumb) {
fprint(ps->gsfd, "(%s) run\n", argv[0]);
fprint(ps->gsfd, "(/fd/3) (w) file dup (THIS IS NOT A PLAN9 BITMAP 01234567890123456789012345678901234567890123456789\\n) writestring flushfile\n");
}
ps->bbox = bbox;
return d;
}
static int
pswritepage(Document *d, int fd, int page)
{
Biobuf *b = d->b;
PSInfo *ps = d->extra;
int t, n, i;
long begin, end;
char buf[8192];
if(page == -1)
begin = ps->psoff;
else
begin = ps->page[page].offset;
end = ps->page[page+1].offset;
if(chatty) {
fprint(2, "writepage(%d)... from #%ld to #%ld...\n",
page, begin, end);
}
Bseek(b, begin, 0);
t = end-begin;
n = sizeof(buf);
if(n > t) n = t;
while(t > 0 && (i=Bread(b, buf, n)) > 0) {
if(write(fd, buf, i) != i)
return -1;
t -= i;
if(n > t)
n = t;
}
return end-begin;
}
static Image*
psdrawpage(Document *d, int page)
{
PSInfo *ps = d->extra;
Image *im;
if(ps->clueless)
return readimage(display, ps->gsdfd, 0);
waitgs(ps);
if(goodps)
pswritepage(d, ps->gsfd, page);
else {
pswritepage(d, ps->gsfd, -1);
pswritepage(d, ps->gsfd, page);
pswritepage(d, ps->gsfd, d->npage);
}
write(ps->gsfd, "\n", 1);
im = readimage(display, ps->gsdfd, 0);
if(im == nil) {
fprint(2, "fatal: readimage error %r\n");
wexits("readimage");
}
waitgs(ps);
return im;
}
static char*
pspagename(Document *d, int page)
{
PSInfo *ps = (PSInfo *) d->extra;
return ps->page[page].name;
}