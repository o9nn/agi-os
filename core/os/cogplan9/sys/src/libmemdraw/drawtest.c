#include <u.h>
#include <libc.h>
#include <bio.h>
#include <draw.h>
#include <memdraw.h>
#define DBG if(0)
#define RGB2K(r,g,b) ((299*((ulong)(r))+587*((ulong)(g))+114*((ulong)(b)))/1000)
void drawonepixel(Memimage*, Point, Memimage*, Point, Memimage*, Point);
void verifyone(void);
void verifyline(void);
void verifyrect(void);
void verifyrectrepl(int, int);
void putpixel(Memimage *img, Point pt, ulong nv);
ulong rgbatopix(uchar, uchar, uchar, uchar);
char *dchan, *schan, *mchan;
int dbpp, sbpp, mbpp;
int drawdebug=0;
int seed;
int niters = 100;
int dbpp;
int sbpp;
int mbpp;
int dpm;
int nbytes;
int Xrange = 64;
int Yrange = 8;
Memimage *dst;
Memimage *src;
Memimage *mask;
Memimage *stmp;
Memimage *mtmp;
Memimage *ones;
uchar *dstbits;
uchar *srcbits;
uchar *maskbits;
ulong *savedstbits;
void
rdb(void)
{
}
int
iprint(char *fmt, ...)
{
int n;
va_list va;
char buf[1024];
va_start(va, fmt);
n = vseprint(buf, buf+sizeof buf, fmt, va) - buf;
va_end(va);
write(1,buf,n);
return 1;
}
void
main(int argc, char *argv[])
{
memimageinit();
seed = time(0);
ARGBEGIN{
case 'x':
Xrange = atoi(ARGF());
break;
case 'y':
Yrange = atoi(ARGF());
break;
case 'n':
niters = atoi(ARGF());
break;
case 's':
seed = atoi(ARGF());
break;
}ARGEND
dchan = "r8g8b8";
schan = "r8g8b8";
mchan = "r8g8b8";
switch(argc){
case 3: mchan = argv[2];
case 2: schan = argv[1];
case 1: dchan = argv[0];
case 0: break;
default: goto Usage;
Usage:
fprint(2, "usage: dtest [dchan [schan [mchan]]]\n");
exits("usage");
}
fprint(2, "%s -x %d -y %d -s 0x%x %s %s %s\n", argv0, Xrange, Yrange, seed, dchan, schan, mchan);
srand(seed);
dst = allocmemimage(Rect(0, 0, Xrange, Yrange), strtochan(dchan));
src = allocmemimage(Rect(0, 0, Xrange, Yrange), strtochan(schan));
mask = allocmemimage(Rect(0, 0, Xrange, Yrange), strtochan(mchan));
stmp = allocmemimage(Rect(0, 0, Xrange, Yrange), strtochan(schan));
mtmp = allocmemimage(Rect(0, 0, Xrange, Yrange), strtochan(mchan));
ones = allocmemimage(Rect(0, 0, Xrange, Yrange), strtochan(mchan));
if(dst==0 || src==0 || mask==0 || mtmp==0 || ones==0) {
Alloc:
fprint(2, "dtest: allocation failed: %r\n");
exits("alloc");
}
nbytes = (4*Xrange+4)*Yrange;
srcbits = malloc(nbytes);
dstbits = malloc(nbytes);
maskbits = malloc(nbytes);
savedstbits = malloc(nbytes);
if(dstbits==0 || srcbits==0 || maskbits==0 || savedstbits==0)
goto Alloc;
dbpp = dst->depth;
sbpp = src->depth;
mbpp = mask->depth;
dpm = 0xFF ^ (0xFF>>dbpp);
memset(ones->data->bdata, 0xFF, ones->width*sizeof(ulong)*Yrange);
fprint(2, "dtest: verify single pixel operation\n");
verifyone();
fprint(2, "dtest: verify full line non-replicated\n");
verifyline();
fprint(2, "dtest: verify full rectangle non-replicated\n");
verifyrect();
fprint(2, "dtest: verify full rectangle source replicated\n");
verifyrectrepl(1, 0);
fprint(2, "dtest: verify full rectangle mask replicated\n");
verifyrectrepl(0, 1);
fprint(2, "dtest: verify full rectangle source and mask replicated\n");
verifyrectrepl(1, 1);
exits(0);
}
static void
Bprintr5g6b5(Biobuf *bio, char*, ulong v)
{
int r,g,b;
r = (v>>11)&31;
g = (v>>5)&63;
b = v&31;
Bprint(bio, "%.2x%.2x%.2x", r,g,b);
}
static void
Bprintr5g5b5a1(Biobuf *bio, char*, ulong v)
{
int r,g,b,a;
r = (v>>11)&31;
g = (v>>6)&31;
b = (v>>1)&31;
a = v&1;
Bprint(bio, "%.2x%.2x%.2x%.2x", r,g,b,a);
}
void
dumpimage(char *name, Memimage *img, void *vdata, Point labelpt)
{
Biobuf b;
uchar *data;
uchar *p;
char *arg;
void (*fmt)(Biobuf*, char*, ulong);
int npr, x, y, nb, bpp;
ulong v, mask;
Rectangle r;
fmt = nil;
arg = nil;
switch(img->depth){
case 1:
case 2:
case 4:
fmt = (void(*)(Biobuf*,char*,ulong))Bprint;
arg = "%.1ux";
break;
case 8:
fmt = (void(*)(Biobuf*,char*,ulong))Bprint;
arg = "%.2ux";
break;
case 16:
arg = nil;
if(img->chan == RGB16)
fmt = Bprintr5g6b5;
else{
fmt = (void(*)(Biobuf*,char*,ulong))Bprint;
arg = "%.4ux";
}
break;
case 24:
fmt = (void(*)(Biobuf*,char*,ulong))Bprint;
arg = "%.6lux";
break;
case 32:
fmt = (void(*)(Biobuf*,char*,ulong))Bprint;
arg = "%.8lux";
break;
}
if(fmt == nil){
fprint(2, "bad format\n");
abort();
}
r = img->r;
Binit(&b, 2, OWRITE);
data = vdata;
bpp = img->depth;
Bprint(&b, "%s\t%d\tr %R clipr %R repl %d data %p *%P\n", name, r.min.x, r, img->clipr, (img->flags&Frepl) ? 1 : 0, vdata, labelpt);
mask = (1ULL<<bpp)-1;
for(y=0; y<Yrange; y++){
nb = 0;
v = 0;
p = data+(byteaddr(img, Pt(0,y))-(uchar*)img->data->bdata);
Bprint(&b, "%-4d\t", y);
for(x=0; x<Xrange; x++){
if(x==0)
Bprint(&b, "\t");
if(x != 0 && (x%8)==0)
Bprint(&b, " ");
npr = 0;
if(x==labelpt.x && y==labelpt.y){
Bprint(&b, "*");
npr++;
}
if(npr == 0)
Bprint(&b, " ");
while(nb < bpp){
v &= (1<<nb)-1;
v |= (ulong)(*p++) << nb;
nb += 8;
}
nb -= bpp;
fmt(&b, arg, (v>>nb)&mask);
}
Bprint(&b, "\n");
}
Bterm(&b);
}
void
checkone(Point p, Point sp, Point mp)
{
int delta;
uchar *dp, *sdp;
delta = (uchar*)byteaddr(dst, p)-(uchar*)dst->data->bdata;
dp = (uchar*)dst->data->bdata+delta;
sdp = (uchar*)savedstbits+delta;
if(memcmp(dp, sdp, (dst->depth+7)/8) != 0) {
fprint(2, "dtest: one bad pixel drawing at dst %P from source %P mask %P\n", p, sp, mp);
fprint(2, " %.2ux %.2ux %.2ux %.2ux should be %.2ux %.2ux %.2ux %.2ux\n",
dp[0], dp[1], dp[2], dp[3], sdp[0], sdp[1], sdp[2], sdp[3]);
fprint(2, "addresses dst %p src %p mask %p\n", dp, byteaddr(src, sp), byteaddr(mask, mp));
dumpimage("src", src, src->data->bdata, sp);
dumpimage("mask", mask, mask->data->bdata, mp);
dumpimage("origdst", dst, dstbits, p);
dumpimage("dst", dst, dst->data->bdata, p);
dumpimage("gooddst", dst, savedstbits, p);
abort();
}
}
#define RECTPTS(r) (r).min.x, (r).min.y, (r).max.x, (r).max.y
void
checkline(Rectangle r, Point sp, Point mp, int y, Memimage *stmp, Memimage *mtmp)
{
ulong *dp;
int nb;
ulong *saved;
dp = wordaddr(dst, Pt(0, y));
saved = savedstbits + y*dst->width;
if(dst->depth < 8)
nb = Xrange/(8/dst->depth);
else
nb = Xrange*(dst->depth/8);
if(memcmp(dp, saved, nb) != 0){
fprint(2, "dtest: bad line at y=%d; saved %p dp %p\n", y, saved, dp);
fprint(2, "draw dst %R src %P mask %P\n", r, sp, mp);
dumpimage("src", src, src->data->bdata, sp);
if(stmp) dumpimage("stmp", stmp, stmp->data->bdata, sp);
dumpimage("mask", mask, mask->data->bdata, mp);
if(mtmp) dumpimage("mtmp", mtmp, mtmp->data->bdata, mp);
dumpimage("origdst", dst, dstbits, r.min);
dumpimage("dst", dst, dst->data->bdata, r.min);
dumpimage("gooddst", dst, savedstbits, r.min);
abort();
}
}
void
fill(Memimage *img, uchar *ucbits)
{
int i, x, y;
ushort *up;
uchar alpha, r, g, b;
void *data;
if((img->flags&Falpha) == 0){
up = (ushort*)ucbits;
for(i=0; i<nbytes/2; i++)
*up++ = lrand() >> 7;
if(i+i != nbytes)
*(uchar*)up = lrand() >> 7;
}else{
data = img->data->bdata;
img->data->bdata = ucbits;
for(x=img->r.min.x; x<img->r.max.x; x++)
for(y=img->r.min.y; y<img->r.max.y; y++){
alpha = rand() >> 4;
r = rand()%(alpha+1);
g = rand()%(alpha+1);
b = rand()%(alpha+1);
putpixel(img, Pt(x,y), rgbatopix(r,g,b,alpha));
}
img->data->bdata = data;
}
}
void
verifyonemask(void)
{
Point dp, sp, mp;
fill(dst, dstbits);
fill(src, srcbits);
memmove(dst->data->bdata, dstbits, dst->width*sizeof(ulong)*Yrange);
memmove(src->data->bdata, srcbits, src->width*sizeof(ulong)*Yrange);
memmove(mask->data->bdata, maskbits, mask->width*sizeof(ulong)*Yrange);
dp.x = nrand(Xrange);
dp.y = nrand(Yrange);
sp.x = nrand(Xrange);
sp.y = nrand(Yrange);
mp.x = nrand(Xrange);
mp.y = nrand(Yrange);
drawonepixel(dst, dp, src, sp, mask, mp);
memmove(mask->data->bdata, maskbits, mask->width*sizeof(ulong)*Yrange);
memmove(savedstbits, dst->data->bdata, dst->width*sizeof(ulong)*Yrange);
memmove(dst->data->bdata, dstbits, dst->width*sizeof(ulong)*Yrange);
memimagedraw(dst, Rect(dp.x, dp.y, dp.x+1, dp.y+1), src, sp, mask, mp, SoverD);
memmove(mask->data->bdata, maskbits, mask->width*sizeof(ulong)*Yrange);
checkone(dp, sp, mp);
}
void
verifyone(void)
{
int i;
memset(maskbits, 0, nbytes);
for(i=0; i<niters; i++)
verifyonemask();
memset(maskbits, 0xFF, nbytes);
for(i=0; i<niters; i++)
verifyonemask();
for(i=0; i<niters; i++){
fill(mask, maskbits);
verifyonemask();
}
}
void
verifylinemask(void)
{
Point sp, mp, tp, up;
Rectangle dr;
int x;
fill(dst, dstbits);
fill(src, srcbits);
memmove(dst->data->bdata, dstbits, dst->width*sizeof(ulong)*Yrange);
memmove(src->data->bdata, srcbits, src->width*sizeof(ulong)*Yrange);
memmove(mask->data->bdata, maskbits, mask->width*sizeof(ulong)*Yrange);
dr.min.x = nrand(Xrange-1);
dr.min.y = nrand(Yrange-1);
dr.max.x = dr.min.x + 1 + nrand(Xrange-1-dr.min.x);
dr.max.y = dr.min.y + 1;
sp.x = nrand(Xrange);
sp.y = nrand(Yrange);
mp.x = nrand(Xrange);
mp.y = nrand(Yrange);
tp = sp;
up = mp;
for(x=dr.min.x; x<dr.max.x && tp.x<Xrange && up.x<Xrange; x++,tp.x++,up.x++)
memimagedraw(dst, Rect(x, dr.min.y, x+1, dr.min.y+1), src, tp, mask, up, SoverD);
memmove(savedstbits, dst->data->bdata, dst->width*sizeof(ulong)*Yrange);
memmove(dst->data->bdata, dstbits, dst->width*sizeof(ulong)*Yrange);
memimagedraw(dst, dr, src, sp, mask, mp, SoverD);
checkline(dr, drawrepl(src->r, sp), drawrepl(mask->r, mp), dr.min.y, nil, nil);
}
void
verifyline(void)
{
int i;
memset(maskbits, 0xFF, nbytes);
for(i=0; i<niters; i++)
verifylinemask();
memset(maskbits, 0, nbytes);
for(i=0; i<niters; i++)
verifylinemask();
for(i=0; i<niters; i++){
fill(mask, maskbits);
verifylinemask();
}
}
void
verifyrectmask(void)
{
Point sp, mp, tp, up;
Rectangle dr;
int x, y;
fill(dst, dstbits);
fill(src, srcbits);
memmove(dst->data->bdata, dstbits, dst->width*sizeof(ulong)*Yrange);
memmove(src->data->bdata, srcbits, src->width*sizeof(ulong)*Yrange);
memmove(mask->data->bdata, maskbits, mask->width*sizeof(ulong)*Yrange);
dr.min.x = nrand(Xrange-1);
dr.min.y = nrand(Yrange-1);
dr.max.x = dr.min.x + 1 + nrand(Xrange-1-dr.min.x);
dr.max.y = dr.min.y + 1 + nrand(Yrange-1-dr.min.y);
sp.x = nrand(Xrange);
sp.y = nrand(Yrange);
mp.x = nrand(Xrange);
mp.y = nrand(Yrange);
tp = sp;
up = mp;
for(y=dr.min.y; y<dr.max.y && tp.y<Yrange && up.y<Yrange; y++,tp.y++,up.y++){
for(x=dr.min.x; x<dr.max.x && tp.x<Xrange && up.x<Xrange; x++,tp.x++,up.x++)
memimagedraw(dst, Rect(x, y, x+1, y+1), src, tp, mask, up, SoverD);
tp.x = sp.x;
up.x = mp.x;
}
memmove(savedstbits, dst->data->bdata, dst->width*sizeof(ulong)*Yrange);
memmove(dst->data->bdata, dstbits, dst->width*sizeof(ulong)*Yrange);
memimagedraw(dst, dr, src, sp, mask, mp, SoverD);
for(y=0; y<Yrange; y++)
checkline(dr, drawrepl(src->r, sp), drawrepl(mask->r, mp), y, nil, nil);
}
void
verifyrect(void)
{
int i;
memset(maskbits, 0, nbytes);
for(i=0; i<niters; i++)
verifyrectmask();
memset(maskbits, 0xFF, nbytes);
for(i=0; i<niters; i++)
verifyrectmask();
for(i=0; i<niters; i++){
fill(mask, maskbits);
verifyrectmask();
}
}
Rectangle
randrect(void)
{
Rectangle r;
r.min.x = nrand(Xrange-1);
r.min.y = nrand(Yrange-1);
r.max.x = r.min.x + 1 + nrand(Xrange-1-r.min.x);
r.max.y = r.min.y + 1 + nrand(Yrange-1-r.min.y);
return r;
}
int
tilexy(int minx, int maxx, int x)
{
int sx;
sx = (x-minx) % (maxx-minx);
if(sx < 0)
sx += maxx-minx;
return sx+minx;
}
void
replicate(Memimage *i, Memimage *tmp)
{
Rectangle r, r1;
int x, y, nb;
r.min.x = nrand(Xrange-1);
r.min.y = nrand(Yrange-1);
switch(lrand()&0){
case 1:
r.max.x = r.min.x + 2;
r.max.y = r.min.y + 2;
if(r.max.x < Xrange && r.max.y < Yrange)
break;
case 0:
r.max.x = r.min.x + 1;
r.max.y = r.min.y + 1;
break;
default:
if(r.min.x+3 >= Xrange)
r.max.x = Xrange;
else
r.max.x = r.min.x+3 + nrand(Xrange-(r.min.x+3));
if(r.min.y+3 >= Yrange)
r.max.y = Yrange;
else
r.max.y = r.min.y+3 + nrand(Yrange-(r.min.y+3));
}
assert(r.min.x >= 0);
assert(r.max.x <= Xrange);
assert(r.min.y >= 0);
assert(r.max.y <= Yrange);
nb = tmp->width*sizeof(ulong)*Yrange;
memset(tmp->data->bdata, 0, nb);
memimagedraw(tmp, r, i, r.min, ones, r.min, SoverD);
memmove(i->data->bdata, tmp->data->bdata, nb);
memset(tmp->data->bdata, 0, nb);
x = -(tilexy(r.min.x, r.max.x, 0)-r.min.x);
for(; x<Xrange; x+=Dx(r)){
y = -(tilexy(r.min.y, r.max.y, 0)-r.min.y);
for(; y<Yrange; y+=Dy(r)){
r1.min.x = x;
r1.min.y = y;
r1.max.x = r1.min.x+Dx(r);
r1.max.y = r1.min.y+Dy(r);
memimagedraw(tmp, r1, i, r.min, ones, r.min, SoverD);
}
}
i->flags |= Frepl;
i->r = r;
i->clipr = randrect();
tmp->clipr = i->clipr;
}
void
verifyrectmaskrepl(int srcrepl, int maskrepl)
{
Point sp, mp, tp, up;
Rectangle dr;
int x, y;
Memimage *s, *m;
src->flags &= ~Frepl;
src->r = Rect(0, 0, Xrange, Yrange);
src->clipr = src->r;
stmp->flags &= ~Frepl;
stmp->r = Rect(0, 0, Xrange, Yrange);
stmp->clipr = src->r;
mask->flags &= ~Frepl;
mask->r = Rect(0, 0, Xrange, Yrange);
mask->clipr = mask->r;
mtmp->flags &= ~Frepl;
mtmp->r = Rect(0, 0, Xrange, Yrange);
mtmp->clipr = mask->r;
fill(dst, dstbits);
fill(src, srcbits);
memmove(dst->data->bdata, dstbits, dst->width*sizeof(ulong)*Yrange);
memmove(src->data->bdata, srcbits, src->width*sizeof(ulong)*Yrange);
memmove(mask->data->bdata, maskbits, mask->width*sizeof(ulong)*Yrange);
if(srcrepl){
replicate(src, stmp);
s = stmp;
}else
s = src;
if(maskrepl){
replicate(mask, mtmp);
m = mtmp;
}else
m = mask;
dr = randrect();
sp.x = nrand(Xrange);
sp.y = nrand(Yrange);
mp.x = nrand(Xrange);
mp.y = nrand(Yrange);
DBG print("smalldraws\n");
for(tp.y=sp.y,up.y=mp.y,y=dr.min.y; y<dr.max.y && tp.y<Yrange && up.y<Yrange; y++,tp.y++,up.y++)
for(tp.x=sp.x,up.x=mp.x,x=dr.min.x; x<dr.max.x && tp.x<Xrange && up.x<Xrange; x++,tp.x++,up.x++)
memimagedraw(dst, Rect(x, y, x+1, y+1), s, tp, m, up, SoverD);
memmove(savedstbits, dst->data->bdata, dst->width*sizeof(ulong)*Yrange);
memmove(dst->data->bdata, dstbits, dst->width*sizeof(ulong)*Yrange);
DBG print("bigdraw\n");
memimagedraw(dst, dr, src, sp, mask, mp, SoverD);
for(y=0; y<Yrange; y++)
checkline(dr, drawrepl(src->r, sp), drawrepl(mask->r, mp), y, srcrepl?stmp:nil, maskrepl?mtmp:nil);
}
void
verifyrectrepl(int srcrepl, int maskrepl)
{
int i;
memset(maskbits, 0xFF, nbytes);
for(i=0; i<niters; i++)
verifyrectmaskrepl(srcrepl, maskrepl);
memset(maskbits, 0, nbytes);
for(i=0; i<niters; i++)
verifyrectmaskrepl(srcrepl, maskrepl);
for(i=0; i<niters; i++){
fill(mask, maskbits);
verifyrectmaskrepl(srcrepl, maskrepl);
}
}
ulong
replbits(ulong v, int nhave, int nwant)
{
v &= (1<<nhave)-1;
for(; nhave<nwant; nhave*=2)
v |= v<<nhave;
v >>= (nhave-nwant);
return v & ((1<<nwant)-1);
}
void
pixtorgba(ulong v, uchar *r, uchar *g, uchar *b, uchar *a)
{
*a = v>>24;
*r = v>>16;
*g = v>>8;
*b = v;
}
ulong
rgbatopix(uchar r, uchar g, uchar b, uchar a)
{
return (a<<24)|(r<<16)|(g<<8)|b;
}
ulong
getpixel(Memimage *img, Point pt)
{
uchar r, g, b, a, *p;
int nbits, npack, bpp;
ulong v, c, rbits, bits;
r = g = b = 0;
a = ~0;
p = byteaddr(img, pt);
v = p[0]|(p[1]<<8)|(p[2]<<16)|(p[3]<<24);
bpp = img->depth;
if(bpp<8){
npack = 8/bpp;
v >>= 8 - bpp*(pt.x%npack+1);
v &= (1<<bpp)-1;
r = g = b = replbits(v, bpp, 8);
}else{
for(c=img->chan; c; c>>=8){
nbits = NBITS(c);
bits = v & ((1<<nbits)-1);
rbits = replbits(bits, nbits, 8);
v >>= nbits;
switch(TYPE(c)){
case CRed:
r = rbits;
break;
case CGreen:
g = rbits;
break;
case CBlue:
b = rbits;
break;
case CGrey:
r = g = b = rbits;
break;
case CAlpha:
a = rbits;
break;
case CMap:
p = img->cmap->cmap2rgb + 3*bits;
r = p[0];
g = p[1];
b = p[2];
break;
case CIgnore:
break;
default:
fprint(2, "unknown channel type %lud\n", TYPE(c));
abort();
}
}
}
return rgbatopix(r, g, b, a);
}
uchar
getgrey(Memimage *img, Point pt)
{
uchar r, g, b, a;
pixtorgba(getpixel(img, pt), &r, &g, &b, &a);
return RGB2K(r, g, b);
}
uchar
getmask(Memimage *img, Point pt)
{
if(img->flags&Falpha)
return getpixel(img, pt)>>24;
else
return getgrey(img, pt);
}
#undef DBG
#define DBG if(0)
void
putpixel(Memimage *img, Point pt, ulong nv)
{
uchar r, g, b, a, *p, *q;
ulong c, mask, bits, v;
int bpp, sh, npack, nbits;
pixtorgba(nv, &r, &g, &b, &a);
p = byteaddr(img, pt);
v = p[0]|(p[1]<<8)|(p[2]<<16)|(p[3]<<24);
bpp = img->depth;
DBG print("v %.8lux...", v);
if(bpp < 8){
npack = 8/bpp;
sh = bpp*(npack - pt.x%npack - 1);
bits = RGB2K(r,g,b);
DBG print("repl %lux 8 %d = %lux...", bits, bpp, replbits(bits, 8, bpp));
bits = replbits(bits, 8, bpp);
mask = (1<<bpp)-1;
DBG print("bits %lux mask %lux sh %d...", bits, mask, sh);
mask <<= sh;
bits <<= sh;
DBG print("(%lux & %lux) | (%lux & %lux)", v, ~mask, bits, mask);
v = (v & ~mask) | (bits & mask);
} else {
sh = 0;
for(c=img->chan; c; c>>=8){
nbits = NBITS(c);
switch(TYPE(c)){
case CRed:
bits = r;
break;
case CGreen:
bits = g;
break;
case CBlue:
bits = b;
break;
case CGrey:
bits = RGB2K(r, g, b);
break;
case CAlpha:
bits = a;
break;
case CIgnore:
bits = 0;
break;
case CMap:
q = img->cmap->rgb2cmap;
bits = q[(r>>4)*16*16+(g>>4)*16+(b>>4)];
break;
default:
SET(bits);
fprint(2, "unknown channel type %lud\n", TYPE(c));
abort();
}
DBG print("repl %lux 8 %d = %lux...", bits, nbits, replbits(bits, 8, nbits));
if(TYPE(c) != CMap)
bits = replbits(bits, 8, nbits);
mask = (1<<nbits)-1;
DBG print("bits %lux mask %lux sh %d...", bits, mask, sh);
bits <<= sh;
mask <<= sh;
v = (v & ~mask) | (bits & mask);
sh += nbits;
}
}
DBG print("v %.8lux\n", v);
p[0] = v;
p[1] = v>>8;
p[2] = v>>16;
p[3] = v>>24;
}
#undef DBG
#define DBG if(0)
void
drawonepixel(Memimage *dst, Point dp, Memimage *src, Point sp, Memimage *mask, Point mp)
{
uchar m, M, sr, sg, sb, sa, sk, dr, dg, db, da, dk;
pixtorgba(getpixel(dst, dp), &dr, &dg, &db, &da);
pixtorgba(getpixel(src, sp), &sr, &sg, &sb, &sa);
m = getmask(mask, mp);
M = 255-(sa*m)/255;
DBG print("dst %x %x %x %x src %x %x %x %x m %x = ", dr,dg,db,da, sr,sg,sb,sa, m);
if(dst->flags&Fgrey){
sk = RGB2K(sr, sg, sb);
dk = RGB2K(dr, dg, db);
dk = (sk*m + dk*M)/255;
dr = dg = db = dk;
da = (sa*m + da*M)/255;
}else{
dr = (sr*m + dr*M)/255;
dg = (sg*m + dg*M)/255;
db = (sb*m + db*M)/255;
da = (sa*m + da*M)/255;
}
DBG print("%x %x %x %x\n", dr,dg,db,da);
putpixel(dst, dp, rgbatopix(dr, dg, db, da));
}