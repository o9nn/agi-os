#include <u.h>
#include <libc.h>
#include <draw.h>
#include <memdraw.h>
#include <bio.h>
#include "pslib.h"
struct iteminfo {
int itype;
int offset;
int width;
int ascent;
int font;
int line;
char *buf;
};
struct lineinfo {
int xorg;
int yorg;
int width;
int height;
int ascent;
};
char *noinit = "pslib not properly initialized";
static int boxes;
static int debug;
static int totitems;
static int totlines;
static int curfont;
static char *def_font;
static int def_font_type;
static int curfonttype;
static int pagestart;
static int started;
static int bps;
static int width;
static int height;
static int iwidth;
static int iheight;
static int xstart;
static int ystart;
static double xmagnification = 1.0, ymagnification = 1.0;
static int rotation = 0;
static int landscape = 0;
static char *Patch = nil;
char*
psinit(int box, int deb) {
debug = deb;
totlines=0;
totitems=0;
pagestart=0;
boxes=box;
curfont=0;
started=1;
return "";
}
static char *username;
int
preamble(Biobuf *ioutb, Rectangle bb) {
if (!started) return 1;
username = getuser();
if(bb.max.x == 0 && bb.max.y == 0) {
bb.max.x = 612;
bb.max.y = 792;
}
Bprint(ioutb, "%%!PS-Adobe-3.0\n");
Bprint(ioutb, "%%%%Creator: PsLib 1.0 (%s)\n",username);
Bprint(ioutb, "%%%%CreationDate: %s", ctime(time(nil)));
Bprint(ioutb, "%%%%Pages: (atend) \n");
Bprint(ioutb, "%%%%BoundingBox: %d %d %d %d\n", bb.min.x, bb.min.y, bb.max.x, bb.max.y);
Bprint(ioutb, "%%%%EndComments\n");
Bprint(ioutb, "%%%%BeginProlog\n");
Bprint(ioutb, "/doimage {\n");
Bprint(ioutb, "/grey exch def\n");
Bprint(ioutb, "/bps exch def\n");
Bprint(ioutb, "/width exch def\n");
Bprint(ioutb, "/height exch def\n");
Bprint(ioutb, "/xstart exch def\n");
Bprint(ioutb, "/ystart exch def\n");
Bprint(ioutb, "/iwidth exch def\n");
Bprint(ioutb, "/ascent exch def\n");
Bprint(ioutb, "/iheight exch def\n");
Bprint(ioutb, "gsave\n");
if(boxes)
Bprint(ioutb, "xstart ystart iwidth iheight rectstroke\n");
Bprint(ioutb, "bps 8 eq grey false eq and {\n");
Bprint(ioutb, " [/Indexed /DeviceRGB 255 <\n");
Bprint(ioutb, "  ffffff ffffaa ffff55 ffff00 ffaaff ffaaaa ffaa55 ffaa00 ff55ff ff55aa ff5555 ff5500\n");
Bprint(ioutb, "  ff00ff ff00aa ff0055 ff0000 ee0000 eeeeee eeee9e eeee4f eeee00 ee9eee ee9e9e ee9e4f\n");
Bprint(ioutb, "  ee9e00 ee4fee ee4f9e ee4f4f ee4f00 ee00ee ee009e ee004f dd0049 dd0000 dddddd dddd93\n");
Bprint(ioutb, "  dddd49 dddd00 dd93dd dd9393 dd9349 dd9300 dd49dd dd4993 dd4949 dd4900 dd00dd dd0093\n");
Bprint(ioutb, "  cc0088 cc0044 cc0000 cccccc cccc88 cccc44 cccc00 cc88cc cc8888 cc8844 cc8800 cc44cc\n");
Bprint(ioutb, "  cc4488 cc4444 cc4400 cc00cc aaffaa aaff55 aaff00 aaaaff bbbbbb bbbb5d bbbb00 aa55ff\n");
Bprint(ioutb, "  bb5dbb bb5d5d bb5d00 aa00ff bb00bb bb005d bb0000 aaffff 9eeeee 9eee9e 9eee4f 9eee00\n");
Bprint(ioutb, "  9e9eee aaaaaa aaaa55 aaaa00 9e4fee aa55aa aa5555 aa5500 9e00ee aa00aa aa0055 aa0000\n");
Bprint(ioutb, "  990000 93dddd 93dd93 93dd49 93dd00 9393dd 999999 99994c 999900 9349dd 994c99 994c4c\n");
Bprint(ioutb, "  994c00 9300dd 990099 99004c 880044 880000 88cccc 88cc88 88cc44 88cc00 8888cc 888888\n");
Bprint(ioutb, "  888844 888800 8844cc 884488 884444 884400 8800cc 880088 55ff55 55ff00 55aaff 5dbbbb\n");
Bprint(ioutb, "  5dbb5d 5dbb00 5555ff 5d5dbb 777777 777700 5500ff 5d00bb 770077 770000 55ffff 55ffaa\n");
Bprint(ioutb, "  4fee9e 4fee4f 4fee00 4f9eee 55aaaa 55aa55 55aa00 4f4fee 5555aa 666666 666600 4f00ee\n");
Bprint(ioutb, "  5500aa 660066 660000 4feeee 49dddd 49dd93 49dd49 49dd00 4993dd 4c9999 4c994c 4c9900\n");
Bprint(ioutb, "  4949dd 4c4c99 555555 555500 4900dd 4c0099 550055 550000 440000 44cccc 44cc88 44cc44\n");
Bprint(ioutb, "  44cc00 4488cc 448888 448844 448800 4444cc 444488 444444 444400 4400cc 440088 440044\n");
Bprint(ioutb, "  00ff00 00aaff 00bbbb 00bb5d 00bb00 0055ff 005dbb 007777 007700 0000ff 0000bb 000077\n");
Bprint(ioutb, "  333333 00ffff 00ffaa 00ff55 00ee4f 00ee00 009eee 00aaaa 00aa55 00aa00 004fee 0055aa\n");
Bprint(ioutb, "  006666 006600 0000ee 0000aa 000066 222222 00eeee 00ee9e 00dd93 00dd49 00dd00 0093dd\n");
Bprint(ioutb, "  009999 00994c 009900 0049dd 004c99 005555 005500 0000dd 000099 000055 111111 00dddd\n");
Bprint(ioutb, "  00cccc 00cc88 00cc44 00cc00 0088cc 008888 008844 008800 0044cc 004488 004444 004400\n");
Bprint(ioutb, "  0000cc 000088 000044 000000>\n");
Bprint(ioutb, " ] setcolorspace\n");
Bprint(ioutb, " /decodemat [0 255] def\n");
Bprint(ioutb, "}\n");
Bprint(ioutb, "{\n");
Bprint(ioutb, " grey true eq {\n");
Bprint(ioutb, "  [/DeviceGray] setcolorspace\n");
Bprint(ioutb, "  /decodemat [1 0] def\n");
Bprint(ioutb, " }\n");
Bprint(ioutb, " {\n");
Bprint(ioutb, "  [/DeviceRGB] setcolorspace\n");
Bprint(ioutb, "  /bps 8 def\n");
Bprint(ioutb, "  /decodemat [1 0 1 0 1 0] def\n");
Bprint(ioutb, " }\n");
Bprint(ioutb, " ifelse\n");
Bprint(ioutb, "}\n");
Bprint(ioutb, "ifelse\n");
Bprint(ioutb, "/xmagnification %g def\n", xmagnification);
Bprint(ioutb, "/ymagnification %g def\n", ymagnification);
Bprint(ioutb, "/rotation %d def\n", rotation);
Bprint(ioutb, "xstart ystart translate rotation rotate\n");
Bprint(ioutb, "iwidth xmagnification mul iheight ymagnification mul scale\n");
Bprint(ioutb, "<<\n");
Bprint(ioutb, " /ImageType 1\n");
Bprint(ioutb, " /Width width \n");
Bprint(ioutb, " /Height height \n");
Bprint(ioutb, " /BitsPerComponent bps %% bits/sample\n");
Bprint(ioutb, " /Decode decodemat %% Brazil/Inferno cmap or DeviceGray value\n");
Bprint(ioutb, " /ImageMatrix [width 0 0 height neg 0 height]\n");
Bprint(ioutb, " /DataSource currentfile /ASCII85Decode filter\n");
Bprint(ioutb, ">> \n");
Bprint(ioutb, "image\n");
Bprint(ioutb, "grestore\n");
Bprint(ioutb, "} def\n");
Bprint(ioutb, "%%%%EndProlog\n");
if (Patch != nil)
Bprint(ioutb, "%s\n", Patch);
return 0;
}
int
trailer(Biobuf *ioutb ,int pages) {
if(!started)
return 1;
Bprint(ioutb, "%%%%Trailer\n%%%%Pages: %d\n%%%%EOF\n", pages);
return 0;
}
void
printnewpage(int pagenum, int end, Biobuf *ioutb)
{
if (!started) return;
if (end){
if (boxes){
Bprint(ioutb, "18 18 moveto 594 18 lineto 594 774 lineto 18 774 lineto closepath stroke\n");
}
Bprint(ioutb, "showpage\n%%%%EndPage %d %d\n", pagenum, pagenum);
} else
Bprint(ioutb, "%%%%Page: %d %d\n", pagenum, pagenum);
}
void
cmap2ascii85(uchar *b, uchar *c) {
int i;
unsigned long i1;
b--;
c--;
i1 = (b[1]<<24)+(b[2]<<16)+(b[3]<<8)+b[4];
if(i1 == 0){
c[1] = 'z';
c[2] = '\0';
return;
}
for(i=0; i<=4; i++){
c[5-i] = '!' + (i1 % 85);
i1 /= 85;
}
c[6] = '\0';
}
static uchar *arr = nil;
ulong onesbits = ~0;
void
imagebits(Biobuf *ioutb, Memimage *im)
{
int spb;
int bitoff;
int j, n, n4, i, bpl, nrest;
int lsf;
uchar c85[6], *data, *src, *dst;
Memimage *tmp;
Rectangle r;
tmp = nil;
if (debug)
fprint(2, "imagebits, r=%d %d %d %d, depth=%d\n",
im->r.min.x, im->r.min.y, im->r.max.x, im->r.max.y, im->depth);
width = Dx(im->r);
height = Dy(im->r);
bps = im->depth;
bitoff = 0;
if (bps < 8) {
spb = 8 / bps;
bitoff = (im->r.min.x % spb) * bps;
}
if (bitoff != 0) {
r = im->r;
r.min.x -= bitoff/im->depth;
r.max.x -= bitoff/im->depth;
tmp = allocmemimage(r, im->chan);
if(tmp == nil){
fprint(2, "p9bitpost: allocmemimage failed: %r\n");
exits("alloc");
}
memimagedraw(tmp, r, im, im->r.min, nil, ZP, S);
im = tmp;
}
lsf = 0;
bpl = bytesperline(im->r, im->depth);
n = bpl*Dy(im->r);
data = malloc(n);
if(data == nil){
fprint(2, "p9bitpost: malloc failed: %r\n");
exits("malloc");
}
for(i=0; i<Dy(im->r); i++){
dst = data+bpl*i;
src = byteaddr(im, Pt(im->r.min.x, im->r.min.y+i));
for(j=0; j<bpl; j++)
*dst++ = 255 - *src++;
}
n4 = (n / 4) * 4;
for (i = 0; i < n4; i += 4){
cmap2ascii85(data+i, c85);
lsf += strlen((char *)c85);
Bprint(ioutb, "%s", (char *)c85);
if (lsf > 74) {
Bprint(ioutb, "\n");
lsf = 0;
}
}
nrest = n - n4;
if (nrest != 0) {
uchar foo[4];
for (i=0; i<nrest; i++)
foo[i] = data[n4+i];
for (i=nrest; i<4; i++)
foo[i] = '\0';
cmap2ascii85(foo, c85);
if (strcmp((char *)c85, "z") == 0 )
strcpy((char *)c85, "!!!!!");
Bprint(ioutb, "%.*s", nrest+1, (char *)c85);
}
Bprint(ioutb, "\n~>");
Bprint(ioutb, "\n");
freememimage(tmp);
}
int
image2psfile(int fd, Memimage *im, int dpi) {
Rectangle r;
Rectangle bbox;
int e;
int xmargin = 36;
int ymargin = 36;
double paperaspectratio;
double imageaspectratio;
Biobuf ioutb;
Memimage *tmp;
if(im->depth >= 8 && im->chan != CMAP8 && im->chan != GREY8){
tmp = allocmemimage(im->r, strtochan("b8g8r8"));
if(tmp == nil)
return 1;
memimagedraw(tmp, tmp->r, im, im->r.min, nil, ZP, S);
freememimage(im);
im = tmp;
}
Binit(&ioutb, fd, OWRITE);
r = im->r;
width = Dx(r);
height = Dy(r);
imageaspectratio = (double) width / (double) height;
if (landscape) {
paperaspectratio = ((double)paperlength - (ymargin * 2)) / ((double)paperwidth - (xmargin * 2));
if (dpi > 0) {
iwidth = width * 72 / dpi;
iheight = height * 72 / dpi;
} else if (imageaspectratio > paperaspectratio) {
iwidth = paperlength - (ymargin * 2);
iheight = iwidth / imageaspectratio;
} else {
iheight = paperwidth - (xmargin * 2);
iwidth = iheight * imageaspectratio;
}
xstart = paperwidth - xmargin - (iheight * ymagnification);
ystart = paperlength - ymargin;
rotation = -90;
} else {
paperaspectratio = ((double)paperwidth - (xmargin * 2)) / ((double)paperlength - (ymargin * 2));
if (dpi > 0) {
iwidth = width * 72 / dpi;
iheight = height * 72 / dpi;
} else if (imageaspectratio > paperaspectratio) {
iwidth = paperwidth - (xmargin * 2);
iheight = iwidth / imageaspectratio;
} else {
iheight = paperlength - (ymargin * 2);
iwidth = iheight * imageaspectratio;
}
xstart = xmargin;
ystart = paperlength - ymargin - (iheight * ymagnification);
rotation = 0;
}
bbox = Rect(xstart,ystart,xstart+iwidth,ystart+iheight);
e = preamble(&ioutb, bbox);
if(e != 0)
return e;
Bprint(&ioutb, "%%%%Page: 1\n%%%%BeginPageSetup\n");
Bprint(&ioutb, "/pgsave save def\n");
Bprint(&ioutb, "%%%%EndPageSetup\n");
bps = im->depth;
Bprint(&ioutb, "%d 0 %d %d %d %d %d %d %s doimage\n", iheight, iwidth, ystart, xstart, height, width, bps, im->flags&Fgrey ? "true" : "false");
imagebits(&ioutb, im);
Bprint(&ioutb, "pgsave restore\nshowpage\n");
e = trailer(&ioutb, 1);
if(e != 0)
return e;
Bterm(&ioutb);
return 0;
}
void
psopt(char *s, void *val)
{
if(s == nil)
return;
if(strcmp("xmagnification", s) == 0)
xmagnification = *((double *)val);
if(strcmp("ymagnification", s) == 0)
ymagnification = *((double *)val);
if(strcmp("landscape", s) == 0)
landscape = *((int *)val);
if(strcmp("Patch", s) == 0)
Patch = *((char **)val);
}