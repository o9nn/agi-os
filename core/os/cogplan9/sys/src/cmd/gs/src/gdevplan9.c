#include "gdevprn.h"
#include "gsparam.h"
#include "gxlum.h"
#include "gxstdio.h"
#include <stdlib.h>
#define nil ((void*)0)
enum {
ERROR = -2
};
typedef struct WImage WImage;
typedef struct Rectangle Rectangle;
typedef struct Point Point;
struct Point {
int x;
int y;
};
struct Rectangle {
Point min;
Point max;
};
private Point ZP = { 0, 0 };
private WImage* initwriteimage(FILE *f, Rectangle r, char*, int depth);
private int writeimageblock(WImage *w, uchar *data, int ndata);
private int bytesperline(Rectangle, int);
private int rgb2cmap(int, int, int);
private long cmap2rgb(int);
#define X_DPI	100
#define Y_DPI	100
private dev_proc_map_rgb_color(plan9_rgb2cmap);
private dev_proc_map_color_rgb(plan9_cmap2rgb);
private dev_proc_open_device(plan9_open);
private dev_proc_close_device(plan9_close);
private dev_proc_print_page(plan9_print_page);
private dev_proc_put_params(plan9_put_params);
private dev_proc_get_params(plan9_get_params);
typedef struct plan9_device_s {
gx_device_common;
gx_prn_device_common;
int dither;
int ldepth;
int lastldepth;
int cmapcall;
} plan9_device;
enum {
Nbits = 8,
Bitmask = (1<<Nbits)-1,
};
private const gx_device_procs plan9_procs =
prn_color_params_procs(plan9_open, gdev_prn_output_page, gdev_prn_close,
plan9_rgb2cmap, plan9_cmap2rgb,
gdev_prn_get_params, gdev_prn_put_params);
plan9_device far_data gs_plan9_device =
{ prn_device_body(plan9_device, plan9_procs, "plan9",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0,0,0,0,
3,
Nbits*3,
(1<<Nbits)-1,
(1<<Nbits)-1,
1<<Nbits,
1<<Nbits,
plan9_print_page),
1,
};
private gx_color_index
plan9_rgb2cmap(gx_device *dev, gx_color_value *rgb)
{
gx_color_value r, g, b;
int shift;
plan9_device *idev;
ulong red, green, blue;
r = rgb[0];
g = rgb[1];
b = rgb[2];
idev = (plan9_device*) dev;
shift = gx_color_value_bits - Nbits;
red = r >> shift;
green = g >> shift;
blue = b >> shift;
if(red == green && green == blue) {
if(red == 0 || red == Bitmask)
;
else if(red == Bitmask/3 || red == 2*Bitmask/3) {
if(idev->ldepth < 1)
idev->ldepth = 1;
} else {
if(idev->ldepth < 2)
idev->ldepth = 2;
}
} else
idev->ldepth = 3;
idev->cmapcall = 1;
return (blue << (2*Nbits)) | (green << Nbits) | red;
}
private int
plan9_cmap2rgb(gx_device *dev, gx_color_index color,
gx_color_value rgb[3]) {
int shift, i;
plan9_device *idev;
if((ulong)color > 0xFFFFFF)
return_error(gs_error_rangecheck);
idev = (plan9_device*) dev;
shift = gx_color_value_bits - Nbits;
rgb[2] = ((color >> (2*Nbits)) & Bitmask) << shift;
rgb[1] = ((color >> Nbits) & Bitmask) << shift;
rgb[0] = (color & Bitmask) << shift;
return 0;
}
private int
plan9_put_param_int(gs_param_list *plist, gs_param_name pname, int *pv,
int minval, int maxval, int ecode)
{
int code, value;
switch(code = param_read_int(plist, pname, &value)) {
default:
return code;
case 1:
return ecode;
case 0:
if(value < minval || value > maxval)
param_signal_error(plist, pname, gs_error_rangecheck);
*pv = value;
return (ecode < 0 ? ecode : 1);
}
}
private int
plan9_get_params(gx_device *pdev, gs_param_list *plist)
{
int code;
plan9_device *idev;
idev = (plan9_device*) pdev;
if((code = gdev_prn_get_params(pdev, plist)) < 0
|| (code = param_write_int(plist, "Dither", &idev->dither)) < 0)
return code;
return code;
}
private int
plan9_put_params(gx_device * pdev, gs_param_list * plist)
{
int code;
int dither;
plan9_device *idev;
idev = (plan9_device*)pdev;
dither = idev->dither;
code = plan9_put_param_int(plist, "Dither", &dither, 0, 1, 0);
if(code < 0)
return code;
idev->dither = dither;
return 0;
}
extern void init_p9color(void);
private int
plan9_open(gx_device *dev)
{
int code;
plan9_device *idev;
idev = (plan9_device*) dev;
idev->cmapcall = 0;
idev->ldepth = 0;
init_p9color();
return gdev_prn_open(dev);
}
private int
plan9_print_page(gx_device_printer *pdev, FILE *f)
{
char *chanstr;
uchar *buf;
uchar *p;
WImage *w;
int bpl, y;
int x, xmod;
int ldepth;
int ppb[] = {8, 4, 2, 1};
int bpp[] = {1, 2, 4, 8};
int gsbpl;
int dither;
int depth;
ulong u;
ushort us;
Rectangle rect;
plan9_device *idev;
uchar *r;
gsbpl = gdev_prn_raster(pdev);
buf = gs_malloc(pdev->memory, gsbpl, 1, "plan9_print_page");
if(buf == nil) {
errprintf("out of memory\n");
return_error(gs_error_Fatal);
}
idev = (plan9_device *) pdev;
if(idev->cmapcall) {
idev->lastldepth = idev->ldepth;
idev->ldepth = 0;
idev->cmapcall = 0;
}
ldepth = idev->lastldepth;
dither = idev->dither;
if(pdev->color_info.anti_alias.graphics_bits || pdev->color_info.anti_alias.text_bits)
if(ldepth < 2)
ldepth = 2;
chanstr = nil;
depth = 0;
switch(ldepth){
case 0:
chanstr = "k1";
depth = 1;
break;
case 1:
return_error(gs_error_Fatal);
case 2:
chanstr = "k4";
depth = 4;
break;
case 3:
chanstr = "r8g8b8";
depth = 24;
break;
}
rect.min = ZP;
rect.max.x = pdev->width;
rect.max.y = pdev->height;
bpl = bytesperline(rect, depth);
w = initwriteimage(f, rect, chanstr, depth);
if(w == nil) {
errprintf("initwriteimage failed\n");
return_error(gs_error_Fatal);
}
for(y=0; y<pdev->height; y++) {
gdev_prn_get_bits(pdev, y, buf, &p);
r = p+2;
switch(depth){
default:
return_error(gs_error_Fatal);
case 1:
for(x=0; x<pdev->width; x++){
if((x%8) == 0)
p[x/8] = (*r>>4)&1;
else
p[x/8] = (p[x/8]<<1) | (*r>>4)&1;
r += 3;
}
break;
case 4:
for(x=0; x<pdev->width; x++){
if((x%2) == 0)
p[x/2] = (*r>>4) & 0xF;
else
p[x/2] = (p[x/2]<<4) | ((*r>>4)&0xF);
r += 3;
}
break;
case 24:
break;
}
xmod = pdev->width % ppb[ldepth];
if(xmod && ldepth<3)
p[(x-1)/ppb[ldepth]] <<= ((ppb[ldepth]-xmod)*bpp[ldepth]);
if(writeimageblock(w, p, bpl) == ERROR) {
gs_free(pdev->memory, buf, gsbpl, 1, "plan9_print_page");
return_error(gs_error_Fatal);
}
}
if(writeimageblock(w, nil, 0) == ERROR) {
gs_free(pdev->memory, buf, gsbpl, 1, "plan9_print_page");
return_error(gs_error_Fatal);
}
gs_free(pdev->memory, buf, gsbpl, 1, "plan9_print_page");
return 0;
}
#define	NMATCH	3
#define	NRUN	(NMATCH+31)
#define	NMEM	1024
#define	NDUMP	128
#define	NCBLOCK	6000
#define	HSHIFT	3
#define	NHASH	(1<<(HSHIFT*NMATCH))
#define	HMASK	(NHASH-1)
#define	hupdate(h, c)	((((h)<<HSHIFT)^(c))&HMASK)
typedef struct Dump	Dump;
typedef struct Hlist Hlist;
struct Hlist{
ulong p;
Hlist *next, *prev;
};
struct Dump {
int ndump;
uchar *dumpbuf;
uchar buf[1+NDUMP];
};
struct WImage {
FILE *f;
Rectangle origr, r;
int bpl;
uchar outbuf[NCBLOCK], *outp, *eout, *loutp;
uchar *inbuf;
uchar *ibase;
int minbuf;
int ninbuf;
ulong line;
Dump dump;
Hlist hash[NHASH];
Hlist chain[NMEM], *cp;
int h;
int needhash;
};
private void
zerohash(WImage *w)
{
memset(w->hash, 0, sizeof(w->hash));
memset(w->chain, 0, sizeof(w->chain));
w->cp=w->chain;
w->needhash = 1;
}
private int
addbuf(WImage *w, uchar *buf, int nbuf)
{
int n;
if(buf == nil || w->outp+nbuf > w->eout) {
if(w->loutp==w->outbuf){
errprintf("buffer too small for line\n");
return ERROR;
}
n=w->loutp-w->outbuf;
fprintf(w->f, "%11d %11d ", w->r.max.y, n);
fwrite(w->outbuf, 1, n, w->f);
w->r.min.y=w->r.max.y;
w->outp=w->outbuf;
w->loutp=w->outbuf;
zerohash(w);
return -1;
}
memmove(w->outp, buf, nbuf);
w->outp += nbuf;
return nbuf;
}
private int
flushdump(WImage *w)
{
int n = w->dump.ndump;
if(n == 0)
return 0;
w->dump.buf[0] = 0x80|(n-1);
if((n=addbuf(w, w->dump.buf, n+1)) == ERROR)
return ERROR;
if(n < 0)
return -1;
w->dump.ndump = 0;
return 0;
}
private void
updatehash(WImage *w, uchar *p, uchar *ep)
{
uchar *q;
Hlist *cp;
Hlist *hash;
int h;
hash = w->hash;
cp = w->cp;
h = w->h;
for(q=p; q<ep; q++) {
if(cp->prev)
cp->prev->next = cp->next;
cp->next = hash[h].next;
cp->prev = &hash[h];
cp->prev->next = cp;
if(cp->next)
cp->next->prev = cp;
cp->p = q - w->ibase;
if(++cp == w->chain+NMEM)
cp = w->chain;
if(&q[NMATCH] < &w->inbuf[w->ninbuf])
h = hupdate(h, q[NMATCH]);
}
w->cp = cp;
w->h = h;
}
private int
gobbleline(WImage *w)
{
int runlen, n, offs;
uchar *eline, *es, *best, *p, *s, *t;
Hlist *hp;
uchar buf[2];
int rv;
if(w->needhash) {
w->h = 0;
for(n=0; n!=NMATCH; n++)
w->h = hupdate(w->h, w->inbuf[w->line+n]);
w->needhash = 0;
}
w->dump.ndump=0;
eline=w->inbuf+w->line+w->bpl;
for(p=w->inbuf+w->line;p!=eline;){
es = (eline < p+NRUN) ? eline : p+NRUN;
best=nil;
runlen=0;
for(hp=w->hash[w->h].next;hp;hp=hp->next){
{	uchar *ss, *tt;
s = p+runlen;
t = w->ibase+hp->p+runlen;
for(ss=s, tt=t; ss>=p && *ss == *tt; ss--, tt--)
;
if(ss < p)
while(s<es && *s == *t)
s++, t++;
}
n = s-p;
if(n > runlen) {
runlen = n;
best = w->ibase+hp->p;
if(p+runlen == es)
break;
}
}
if(runlen<NMATCH){
if(w->dump.ndump==NDUMP) {
if((rv = flushdump(w)) == ERROR)
return ERROR;
if(rv < 0)
return 0;
}
w->dump.dumpbuf[w->dump.ndump++]=*p;
runlen=1;
}else{
if((rv = flushdump(w)) == ERROR)
return ERROR;
if(rv < 0)
return 0;
offs=p-best-1;
buf[0] = ((runlen-NMATCH)<<2)|(offs>>8);
buf[1] = offs&0xff;
if(addbuf(w, buf, 2) < 0)
return 0;
}
updatehash(w, p, p+runlen);
p += runlen;
}
if((rv = flushdump(w)) == ERROR)
return ERROR;
if(rv < 0)
return 0;
w->line += w->bpl;
w->loutp=w->outp;
w->r.max.y++;
return w->bpl;
}
private uchar*
shiftwindow(WImage *w, uchar *data, uchar *edata)
{
int n, m;
if(w->line > NMEM) {
n = w->line-NMEM;
memmove(w->inbuf, w->inbuf+n, w->ninbuf-n);
w->line -= n;
w->ibase -= n;
w->ninbuf -= n;
}
if(w->minbuf > w->ninbuf && edata > data) {
m = w->minbuf - w->ninbuf;
if(edata-data < m)
m = edata-data;
memmove(w->inbuf+w->ninbuf, data, m);
data += m;
w->ninbuf += m;
}
return data;
}
private WImage*
initwriteimage(FILE *f, Rectangle r, char *chanstr, int depth)
{
WImage *w;
int n, bpl;
bpl = bytesperline(r, depth);
if(r.max.y <= r.min.y || r.max.x <= r.min.x || bpl <= 0) {
errprintf("bad rectangle, ldepth");
return nil;
}
n = NMEM+NMATCH+NRUN+bpl*2;
w = malloc(n+sizeof(*w));
if(w == nil)
return nil;
w->inbuf = (uchar*) &w[1];
w->ibase = w->inbuf;
w->line = 0;
w->minbuf = n;
w->ninbuf = 0;
w->origr = r;
w->r = r;
w->r.max.y = w->r.min.y;
w->eout = w->outbuf+sizeof(w->outbuf);
w->outp = w->loutp = w->outbuf;
w->bpl = bpl;
w->f = f;
w->dump.dumpbuf = w->dump.buf+1;
w->dump.ndump = 0;
zerohash(w);
fprintf(f, "compressed\n%11s %11d %11d %11d %11d ",
chanstr, r.min.x, r.min.y, r.max.x, r.max.y);
return w;
}
private int
writeimageblock(WImage *w, uchar *data, int ndata)
{
uchar *edata;
if(data == nil) {
while(w->line < w->ninbuf)
if(gobbleline(w) == ERROR)
return ERROR;
addbuf(w, nil, 0);
if(w->r.min.y != w->origr.max.y) {
errprintf("not enough data supplied to writeimage\n");
}
free(w);
return 0;
}
edata = data+ndata;
data = shiftwindow(w, data, edata);
while(w->ninbuf >= w->line+w->bpl+NMATCH) {
if(gobbleline(w) == ERROR)
return ERROR;
data = shiftwindow(w, data, edata);
}
if(data != edata) {
fprintf(w->f, "data != edata.  uh oh\n");
return ERROR;
}
return 0;
}
static
int
unitsperline(Rectangle r, int d, int bitsperunit)
{
ulong l, t;
if(d <= 0 || d > 32)
abort();
if(r.min.x >= 0){
l = (r.max.x*d+bitsperunit-1)/bitsperunit;
l -= (r.min.x*d)/bitsperunit;
}else{
t = (-r.min.x*d+bitsperunit-1)/bitsperunit;
l = t+(r.max.x*d+bitsperunit-1)/bitsperunit;
}
return l;
}
int
wordsperline(Rectangle r, int d)
{
return unitsperline(r, d, 8*sizeof(ulong));
}
int
bytesperline(Rectangle r, int d)
{
return unitsperline(r, d, 8);
}