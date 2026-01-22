#include	<u.h>
#include	<libc.h>
#include	<ctype.h>
#include	<draw.h>
#include	<event.h>
#include	<cursor.h>
#include	<stdio.h>
#define Never	0xffffffff
#define LOG2  0.301029995664
#define Button_bit(b)	(1 << ((b)-1))
enum {
But1	= Button_bit(1),
But2	= Button_bit(2),
But3	= Button_bit(3),
};
int cantmv = 1;
int plotdots;
int top_border, bot_border, lft_border, rt_border;
int lft_border0;
int top_left, top_right;
int Mv_delay = 400;
int Dotrad = 2;
int framewd=1;
int framesep=1;
int outersep=1;
Point sdigit;
Point smaxch;
double underscan = .05;
double fuzz = 6;
int tick_len = 15;
FILE* logfil = 0;
#define labdigs  3
#define digs10pow 1000
#define axis_color  clr_im(DLtblue)
char* str_insert(char* buf, char* s, int n)
{
int blen, slen = strlen(s) + 1;
if (slen >= n)
{strncpy(buf,s,n); buf[n-1]='\0'; return buf;}
blen = strlen(buf);
if (blen >= n-slen)
buf[blen=n-slen-1] = '\0';
memmove(buf+slen, buf, slen+blen+1);
memcpy(buf, s, slen-1);
buf[slen-1] = ' ';
return buf;
}
int remove_substr(char* smain, char* ssub)
{
char *ss, *s = strstr(smain, ssub);
int n = strlen(ssub);
if (s==0)
return 0;
if (islower(s[n]))
s[0] ^= 32;
else {
for (ss=s+n; *ss!=0; s++, ss++)
*s = *ss;
*s = '\0';
}
return 1;
}
void adjust_border(Font* f)
{
int sep = framesep + outersep;
sdigit = stringsize(f, "8");
smaxch = stringsize(f, "MMMg");
smaxch.x = (smaxch.x + 3)/4;
lft_border0 = (1+labdigs)*sdigit.x + framewd + sep;
rt_border = (lft_border0 - sep)/2 + outersep;
bot_border = sdigit.y + framewd + sep;
top_border = smaxch.y + framewd + sep;
lft_border = lft_border0;
}
int is_off_screen(Point p)
{
const Rectangle* r = &(screen->r);
return p.x-r->min.x<lft_border || r->max.x-p.x<rt_border
|| p.y-r->min.y<=top_border || r->max.y-p.y<=bot_border;
}
Cursor	bullseye =
{
{-7, -7},
{
0x1F, 0xF8, 0x3F, 0xFC, 0x7F, 0xFE, 0xFB, 0xDF,
0xF3, 0xCF, 0xE3, 0xC7, 0xFF, 0xFF, 0xFF, 0xFF,
0xFF, 0xFF, 0xFF, 0xFF, 0xE3, 0xC7, 0xF3, 0xCF,
0x7B, 0xDF, 0x7F, 0xFE, 0x3F, 0xFC, 0x1F, 0xF8,
},
{
0x00, 0x00, 0x0F, 0xF0, 0x31, 0x8C, 0x21, 0x84,
0x41, 0x82, 0x41, 0x82, 0x41, 0x82, 0x7F, 0xFE,
0x7F, 0xFE, 0x41, 0x82, 0x41, 0x82, 0x41, 0x82,
0x21, 0x84, 0x31, 0x8C, 0x0F, 0xF0, 0x00, 0x00,
}
};
int get_1click(int but, Mouse* m, Cursor* curs)
{
if (curs)
esetcursor(curs);
while (m->buttons==0)
*m = emouse();
if (curs)
esetcursor(0);
return (m->buttons==Button_bit(but));
}
int get_click_or_kbd(int but, Mouse* m, const char* expected)
{
Event ev;
ulong expbits[4], ty;
expbits[0] = expbits[1] = expbits[2] = expbits[3];
for (; *expected!=0; expected++)
expbits[((*expected)>>5)&3] |= 1 << (*expected&31);
do ty = eread(Emouse|Ekeyboard, &ev);
while ((ty&Emouse) ? ev.mouse.buttons==0
: (ev.kbdc&~127) || !(expbits[(ev.kbdc>>5)&3] & (1<<(ev.kbdc&31))) );
if (ty&Ekeyboard)
return ev.kbdc;
*m = ev.mouse;
return (ev.mouse.buttons==Button_bit(but)) ? -1 : 0;
}
int lift_button(int but, Mouse* m, int tlimit)
{
do {	*m = emouse();
if (m->msec >= tlimit)
return 0;
} while (m->buttons & Button_bit(but));
return 1;
}
void latest_mouse(int but, Mouse* m)
{
int bbit = Button_bit(but);
do {	*m = emouse();
} while ((m->buttons & bbit) && ecanmouse());
}
enum {	DOrange=0xffaa00FF, Dgray=0xbbbbbbFF, DDkgreen=0x009900FF,
DDkred=0xcc0000FF, DViolet=0x990099FF, DDkyellow=0xaaaa00FF,
DLtblue=0xaaaaffFF, DPink=0xffaaaaFF,
};
typedef struct thick_color {
int thick;
Image* clr;
} thick_color;
typedef struct color_ref {
ulong c;
char* nam;
int nam1;
Image* im;
} color_ref;
color_ref clrtab[] = {
DRed,		"Red",		'R', 0,
DPink,		"Pink",		'P', 0,
DDkred,		"Dkred",	'r', 0,
DOrange,	"Orange",	'O', 0,
DYellow,	"Yellow",	'Y', 0,
DDkyellow,	"Dkyellow",	'y', 0,
DGreen,		"Green",	'G', 0,
DDkgreen,	"Dkgreen",	'g', 0,
DCyan,		"Cyan",		'C', 0,
DBlue,		"Blue",		'B', 0,
DLtblue,	"Ltblue",	'b', 0,
DMagenta,	"Magenta",	'M', 0,
DViolet,	"Violet",	'V', 0,
Dgray,		"Gray",		'A', 0,
DBlack,		"Black",	'K', 0,
DWhite,		"White",	'W', 0,
DNofill,	0,		0,   0
};
short nam1_idx[128];
void  init_clrtab(void)
{
int i;
Rectangle r = Rect(0,0,1,1);
memset(&nam1_idx[0], -1, sizeof(nam1_idx));
for (i=0; clrtab[i].c!=DNofill; i++) {
clrtab[i].im = allocimage(display, r, CMAP8, 1, clrtab[i].c);
nam1_idx[clrtab[i].nam1] = i;
}
}
int clrim_id(Image* clr)
{
int i;
for (i=0; clrtab[i].im!=clr; i++)
if (clrtab[i].c==DNofill)
exits("bad image color");
return i;
}
int clr_id(int clr)
{
int i;
for (i=0; clrtab[i].c!=clr; i++)
if (clrtab[i].c==DNofill)
exits("bad color");
return i;
}
#define clr_im(clr)	clrtab[clr_id(clr)].im
#define is_Multi  -2
thick_color* tc_default(thick_color *buf)
{
buf[0].thick = 1;
buf[1].clr = clr_im(DBlack);
buf[1].thick = 0;
return buf;
}
thick_color* parse_color_chars(const char* c0, const char* fin, thick_color *buf)
{
thick_color* tc;
int i, j, n=fin-c0;
const char* c;
for (c=c0; c<fin-1; c++)
if (*c=='T')
n--;
if (buf==0)
tc = (thick_color*) malloc((n+1)*sizeof(thick_color));
else {tc=buf; n=1;}
i = 0;
for (c=c0; c<fin && i<n; c++) {
tc[++i].thick = 0;
if (*c=='T')
if (++c==fin)
return tc_default(tc);
else tc[i].thick=1;
j = (*c&~127) ? -1 : nam1_idx[*c];
if (j < 0)
return tc_default(tc);
tc[i].clr = clrtab[j].im;
}
tc[0].thick = i;
return tc;
}
thick_color* nam2thclr(const char* nam, thick_color *r1, int *idxdest)
{
char *c, *cbest=0, *rp=0;
int i, ibest=-1;
thick_color* tc = 0;
thick_color buf[2];
if (*nam!=0) {
c = strstr(nam, "Multi(");
if (c!=0 && (rp=strchr(c+6,')'))!=0)
{ibest=is_Multi; cbest=c;}
for (i=0; clrtab[i].nam!=0; i++) {
c = strstr(nam,clrtab[i].nam);
if (c!=0 && (ibest==-1 || c<cbest))
{ibest=i; cbest=c;}
}
}
if (ibest==is_Multi) {
tc = parse_color_chars(cbest+6, rp, (idxdest==0 ? 0 : &buf[0]));
ibest = clrim_id(tc[1].clr);
}
if (idxdest!=0)
*idxdest = (ibest<0) ? clr_id(DBlack) : ibest;
r1->clr = (ibest<0) ? clr_im(DBlack) : clrtab[ibest].im;
r1->thick = (tc!=0) ? tc[1].thick : (strstr(nam,"Thick")==0 ? 0 : 1);
return tc;
}
char* nam_with_thclr(char* nam, const thick_color *tc, char* buf, int bufn)
{
thick_color c0;
int clr0i;
nam2thclr(nam, &c0, &clr0i);
char *clr0s;
if (c0.thick==tc->thick && c0.clr==tc->clr)
return nam;
clr0s = clrtab[clr0i].nam;
if (strlen(nam)<bufn) strcpy(buf,nam);
else {strncpy(buf,nam,bufn); buf[bufn-1]='\0';}
if (c0.clr != tc->clr)
remove_substr(buf, clr0s);
if (c0.thick > tc->thick)
while (remove_substr(buf, "Thick"))
;
nam2thclr(nam, &c0, &clr0i);
if (c0.clr != tc->clr)
str_insert(buf, clrtab[clrim_id(tc->clr)].nam, bufn);
if (c0.thick < tc->thick)
str_insert(buf, "Thick", bufn);
return buf;
}
Image* mv_bkgd;
typedef struct fpoint {
double x, y;
} fpoint;
typedef struct frectangle {
fpoint min, max;
} frectangle;
frectangle empty_frect = {1e30, 1e30, -1e30, -1e30};
int fintersects(const frectangle* r1, const frectangle* r2, double slant)
{
double x2min=r2->min.x, x2max=r2->max.x;
if (r1->max.x <= x2min || x2max <= r1->min.x)
return 0;
if (slant >=0)
{x2min*=slant; x2max*=slant;}
else	{double t=x2min*slant; x2min=x2max*slant; x2max=t;}
return r1->max.y > r2->min.y-x2max && r2->max.y-x2min > r1->min.y;
}
int fcontains(const frectangle* r, fpoint p)
{
return r->min.x <=p.x && p.x<= r->max.x && r->min.y <=p.y && p.y<= r->max.y;
}
void grow_bb(frectangle* dest, const frectangle* r)
{
if (r->min.x < dest->min.x) dest->min.x=r->min.x;
if (r->min.y < dest->min.y) dest->min.y=r->min.y;
if (r->max.x > dest->max.x) dest->max.x=r->max.x;
if (r->max.y > dest->max.y) dest->max.y=r->max.y;
}
void slant_frect(frectangle *r, double sl)
{
r->min.y += sl*r->min.x;
r->max.y += sl*r->max.x;
}
fpoint fcenter(const frectangle* r)
{
fpoint c;
c.x = .5*(r->max.x + r->min.x);
c.y = .5*(r->max.y + r->min.y);
return c;
}
typedef struct fpolygon {
fpoint* p;
int n;
frectangle bb;
char* nam;
thick_color c;
thick_color* ct;
struct fpolygon* link;
} fpolygon;
typedef struct fpolygons {
fpolygon* p;
frectangle bb;
frectangle disp;
double slant_ht;
} fpolygons;
fpolygons univ = {
0,
1e30, 1e30, -1e30, -1e30,
0, 0, 0, 0,
2*1e30
};
void free_fp_etc(fpolygon* fp)
{
if (fp->ct != 0)
free(fp->ct);
free(fp->p);
free(fp);
}
void set_default_clrs(fpolygons* fps, fpolygon* fpstop)
{
fpolygon* fp;
for (fp=fps->p; fp!=0 && fp!=fpstop; fp=fp->link)
fp->ct = nam2thclr(fp->nam, &fp->c, 0);
}
void fps_invert(fpolygons* fps)
{
fpolygon *p, *r=0;
for (p=fps->p; p!=0;) {
fpolygon* q = p;
p = p->link;
q->link = r;
r = q;
}
fps->p = r;
}
void fp_remove(fpolygons* fps, fpolygon* fp)
{
fpolygon *q, **p = &fps->p;
while (*p!=fp)
if (*p==0)
return;
else	p = &(*p)->link;
*p = fp->link;
fps->bb = empty_frect;
for (q=fps->p; q!=0; q=q->link)
grow_bb(&fps->bb, &q->bb);
}
typedef struct transform {
double sl;
fpoint o, sc;
} transform;
#define do_transform(d,tr,s)	((d)->x = (tr)->o.x + (tr)->sc.x*(s)->x,  \
(d)->y = (tr)->o.y + (tr)->sc.y*(s)->y    \
+ (tr)->sl*(s)->x)
#define do_untransform(d,tr,s)	((d)->x = (.5+(s)->x-(tr)->o.x)/(tr)->sc.x,    \
(d)->y = (.5+(s)->y-(tr)->sl*(d)->x-(tr)->o.y) \
/(tr)->sc.y)
#define xtransform(tr,xx)	((tr)->o.x + (tr)->sc.x*(xx))
#define ytransform(tr,yy)	((tr)->o.y + (tr)->sc.y*(yy))
#define dxuntransform(tr,xx)	((xx)/(tr)->sc.x)
#define dyuntransform(tr,yy)	((yy)/(tr)->sc.y)
transform cur_trans(void)
{
transform t;
Rectangle d = screen->r;
const frectangle* s = &univ.disp;
double sh = univ.slant_ht;
d.min.x += lft_border;
d.min.y += top_border;
d.max.x -= rt_border;
d.max.y -= bot_border;
t.sc.x = (d.max.x - d.min.x)/(s->max.x - s->min.x);
t.sc.y = -(d.max.y - d.min.y)/fabs(sh);
if (sh > 0) {
t.sl = -t.sc.y*(s->max.y-s->min.y-sh)/(s->max.x - s->min.x);
t.o.y = d.min.y - t.sc.y*s->max.y - t.sl*s->max.x;
} else {
t.sl = t.sc.y*(s->max.y-s->min.y+sh)/(s->max.x - s->min.x);
t.o.y = d.min.y - t.sc.y*s->max.y - t.sl*s->min.x;
}
t.o.x = d.min.x - t.sc.x*s->min.x;
return t;
}
double u_slant_amt(fpolygons *u)
{
double sh=u->slant_ht, dy=u->disp.max.y - u->disp.min.y;
double dx = u->disp.max.x - u->disp.min.x;
return (sh>0) ? (dy-sh)/dx : -(dy+sh)/dx;
}
double set_unslanted_y(fpolygons *u, double *y0, double *y1)
{
double yy1, sl=u_slant_amt(u);
if (u->slant_ht > 0) {
*y0 = u->disp.min.y - sl*u->disp.min.x;
yy1 = *y0 + u->slant_ht;
} else {
yy1 = u->disp.max.y - sl*u->disp.min.x;
*y0 = yy1 + u->slant_ht;
}
if (y1 != 0)
*y1 = yy1;
return sl;
}
void nontrivial_interval(double *lo, double *hi)
{
if (*lo >= *hi) {
double mid = .5*(*lo + *hi);
double tweak = 1e-6 + 1e-6*fabs(mid);
*lo = mid - tweak;
*hi = mid + tweak;
}
}
void init_disp(void)
{
double dw = (univ.bb.max.x - univ.bb.min.x)*underscan;
double dh = (univ.bb.max.y - univ.bb.min.y)*underscan;
univ.disp.min.x = univ.bb.min.x - dw;
univ.disp.min.y = univ.bb.min.y - dh;
univ.disp.max.x = univ.bb.max.x + dw;
univ.disp.max.y = univ.bb.max.y + dh;
nontrivial_interval(&univ.disp.min.x, &univ.disp.max.x);
nontrivial_interval(&univ.disp.min.y, &univ.disp.max.y);
univ.slant_ht = univ.disp.max.y - univ.disp.min.y;
}
void recenter_disp(Point c)
{
transform tr = cur_trans();
fpoint cc, off;
do_untransform(&cc, &tr, &c);
off.x = cc.x - .5*(univ.disp.min.x + univ.disp.max.x);
off.y = cc.y - .5*(univ.disp.min.y + univ.disp.max.y);
univ.disp.min.x += off.x;
univ.disp.min.y += off.y;
univ.disp.max.x += off.x;
univ.disp.max.y += off.y;
}
double untransform_corners(double rminx, double rminy, double rmaxx, double rmaxy,
fpoint *ul, fpoint *lr)
{
fpoint r_ur, r_ul, r_ll, r_lr;
fpoint ur, ll;
transform tr = cur_trans();
double ht;
r_ur.x=rmaxx;  r_ur.y=rminy;
r_ul.x=rminx;  r_ul.y=rminy;
r_ll.x=rminx;  r_ll.y=rmaxy;
r_lr.x=rmaxx;  r_lr.y=rmaxy;
do_untransform(ul, &tr, &r_ul);
do_untransform(lr, &tr, &r_lr);
do_untransform(&ur, &tr, &r_ur);
do_untransform(&ll, &tr, &r_ll);
ht = ur.y - lr->y;
if (ll.x < ul->x)
ul->x = ll.x;
if (ur.y > ul->y)
ul->y = ur.y;
else	ht = -ht;
if (ur.x > lr->x)
lr->x = ur.x;
if (ll.y < lr->y)
lr->y = ll.y;
return ht;
}
void disp_dozoom(double rminx, double rminy, double rmaxx, double rmaxy)
{
fpoint ul, lr;
double sh = untransform_corners(rminx, rminy, rmaxx, rmaxy, &ul, &lr);
if (ul.x==lr.x || ul.y==lr.y)
return;
univ.slant_ht = sh;
univ.disp.min.x = ul.x;
univ.disp.max.y = ul.y;
univ.disp.max.x = lr.x;
univ.disp.min.y = lr.y;
nontrivial_interval(&univ.disp.min.x, &univ.disp.max.x);
nontrivial_interval(&univ.disp.min.y, &univ.disp.max.y);
}
void disp_zoomin(Rectangle r)
{
disp_dozoom(r.min.x, r.min.y, r.max.x, r.max.y);
}
void disp_zoomout(Rectangle r)
{
double qminx, qminy, qmaxx, qmaxy;
double scx, scy;
Rectangle s = screen->r;
if (r.min.x==r.max.x || r.min.y==r.max.y)
return;
s.min.x += lft_border;
s.min.y += top_border;
s.max.x -= rt_border;
s.max.y -= bot_border;
scx = (s.max.x - s.min.x)/(r.max.x - r.min.x);
scy = (s.max.y - s.min.y)/(r.max.y - r.min.y);
qminx = s.min.x + scx*(s.min.x - r.min.x);
qmaxx = s.max.x + scx*(s.max.x - r.max.x);
qminy = s.min.y + scy*(s.min.y - r.min.y);
qmaxy = s.max.y + scy*(s.max.y - r.max.y);
disp_dozoom(qminx, qminy, qmaxx, qmaxy);
}
void expand2(double* a, double* b, double f)
{
double mid = .5*(*a + *b);
*a = mid + f*(*a - mid);
*b = mid + f*(*b - mid);
}
void disp_squareup(void)
{
double dx = univ.disp.max.x - univ.disp.min.x;
double dy = univ.disp.max.y - univ.disp.min.y;
dx /= screen->r.max.x - lft_border - screen->r.min.x - rt_border;
dy /= screen->r.max.y - bot_border - screen->r.min.y - top_border;
if (dx > dy)
expand2(&univ.disp.min.y, &univ.disp.max.y, dx/dy);
else	expand2(&univ.disp.min.x, &univ.disp.max.x, dy/dx);
univ.slant_ht = univ.disp.max.y - univ.disp.min.y;
}
void slant_disp(fpoint p, fpoint q)
{
double yll, ylr, yul, yur;
double sh, dy;
if (p.x == q.x)
return;
sh = univ.slant_ht;
if (sh > 0) {
yll=yul=univ.disp.min.y;  yul+=sh;
ylr=yur=univ.disp.max.y;  ylr-=sh;
} else {
yll=yul=univ.disp.max.y;  yll+=sh;
ylr=yur=univ.disp.min.y;  yur-=sh;
}
dy = (univ.disp.max.x-univ.disp.min.x)*(q.y - p.y)/(q.x - p.x);
dy -= ylr - yll;
if (dy > 0)
{yll-=dy; yur+=dy;}
else	{yul-=dy; ylr+=dy;}
if (ylr > yll) {
univ.disp.min.y = yll;
univ.disp.max.y = yur;
univ.slant_ht = yur - ylr;
} else {
univ.disp.max.y = yul;
univ.disp.min.y = ylr;
univ.slant_ht = ylr - yur;
}
}
void set_fbb(fpolygon* fp)
{
fpoint lo=fp->p[0], hi=fp->p[0];
const fpoint *q, *qtop;
for (qtop=(q=fp->p)+fp->n; ++q<=qtop;) {
if (q->x < lo.x) lo.x=q->x;
if (q->y < lo.y) lo.y=q->y;
if (q->x > hi.x) hi.x=q->x;
if (q->y > hi.y) hi.y=q->y;
}
fp->bb.min = lo;
fp->bb.max = hi;
}
char* mystrdup(char* s)
{
char *r, *t = strrchr(s,'"');
if (t==0) {
t = s + strlen(s);
while (t>s && (t[-1]=='\n' || t[-1]=='\r'))
t--;
}
r = malloc(1+(t-s));
memcpy(r, s, t-s);
r[t-s] = 0;
return r;
}
int is_valid_label(char* lab)
{
char* t;
if (lab[0]=='"')
return (t=strrchr(lab,'"'))!=0 && t!=lab && strspn(t+1," \t\r\n")==strlen(t+1);
return strcspn(lab," \t")==strlen(lab);
}
fpolygon* rd_fpoly(FILE* fin, int *lineno)
{
char buf[4096], junk[2];
fpoint q;
fpolygon* fp;
int allocn;
if (!fgets(buf,4096,fin))
return 0;
(*lineno)++;
if (sscanf(buf,"%lg%lg%1s",&q.x,&q.y,junk) != 2)
return 0;
fp = malloc(sizeof(fpolygon));
allocn = 4;
fp->p = malloc(allocn*sizeof(fpoint));
fp->p[0] = q;
fp->n = 0;
fp->nam = "";
fp->c.thick = 0;
fp->c.clr = clr_im(DBlack);
fp->ct = 0;
while (fgets(buf,4096,fin)) {
(*lineno)++;
if (sscanf(buf,"%lg%lg%1s",&q.x,&q.y,junk) != 2) {
if (!is_valid_label(buf))
{free_fp_etc(fp); return 0;}
fp->nam = (buf[0]=='"') ? buf+1 : buf;
break;
}
if (++(fp->n) == allocn)
fp->p = realloc(fp->p, (allocn<<=1)*sizeof(fpoint));
fp->p[fp->n] = q;
}
fp->nam = mystrdup(fp->nam);
set_fbb(fp);
fp->link = 0;
return fp;
}
int rd_fpolys(FILE* fin, fpolygons* fps)
{
fpolygon *fp, *fp0=fps->p;
int lineno=0, ok_upto=0;
while ((fp=rd_fpoly(fin,&lineno)) != 0) {
ok_upto = lineno;
fp->link = fps->p;
fps->p = fp;
grow_bb(&fps->bb, &fp->bb);
}
set_default_clrs(fps, fp0);
return (ok_upto==lineno) ? 0 : lineno;
}
int doinput(char* fnam)
{
FILE* fin = strcmp(fnam,"-")==0 ? stdin : fopen(fnam, "r");
int errline_or0;
if (fin==0)
return -1;
errline_or0 = rd_fpolys(fin, &univ);
fclose(fin);
return errline_or0;
}
fpolygon* fp_reverse(fpolygon* fp)
{
fpolygon* r = 0;
while (fp!=0) {
fpolygon* q = fp->link;
fp->link = r;
r = fp;
fp = q;
}
return r;
}
void wr_fpoly(FILE* fout, const fpolygon* fp)
{
char buf[256];
int i;
for (i=0; i<=fp->n; i++)
fprintf(fout,"%.12g\t%.12g\n", fp->p[i].x, fp->p[i].y);
fprintf(fout,"\"%s\"\n", nam_with_thclr(fp->nam, &fp->c, buf, 256));
}
void wr_fpolys(FILE* fout, fpolygons* fps)
{
fpolygon* fp;
fps->p = fp_reverse(fps->p);
for (fp=fps->p; fp!=0; fp=fp->link)
wr_fpoly(fout, fp);
fps->p = fp_reverse(fps->p);
}
int dooutput(char* fnam)
{
FILE* fout = fopen(fnam, "w");
if (fout==0)
return 0;
wr_fpolys(fout, &univ);
fclose(fout);
return 1;
}
int do_xory(double x0, double x1, double xlo, double xhi, double* t0, double* t1)
{
*t1 = 1.0;
if (x0<xlo) {
if (x1<xlo) return 0;
*t0 = (xlo-x0)/(x1-x0);
if (x1>xhi) *t1 = (xhi-x0)/(x1-x0);
} else if (x0>xhi) {
if (x1>xhi) return 0;
*t0 = (xhi-x0)/(x1-x0);
if (x1<xlo) *t1 = (xlo-x0)/(x1-x0);
} else {
*t0 = 0.0;
if (x1>xhi) *t1 = (xhi-x0)/(x1-x0);
else if (x1<xlo) *t1 = (xlo-x0)/(x1-x0);
else *t1 = 1.0;
}
return 1;
}
double frac_outside(const fpoint* p, const fpoint* q, const frectangle* r,
double slope)
{
double t0, t1, tt0, tt1;
double px=p->x, qx=q->x;
if (!do_xory(px, qx, r->min.x, r->max.x, &t0, &t1))
return 1;
if (!do_xory(p->y-slope*px, q->y-slope*qx, r->min.y, r->max.y, &tt0, &tt1))
return 1;
if (tt0 > t0)
t0 = tt0;
if (t1<=t0 || tt1<=t0)
return 1;
return t0;
}
double in_length(const fpoint* p0, const fpoint* pn, frectangle r, double slope)
{
const fpoint* p = p0;
double px, py;
do if (++p > pn)
return pn - p0;
while (r.min.x<=(px=p->x) && px<=r.max.x
&& r.min.y<=(py=p->y-slope*px) && py<=r.max.y);
return (p - p0) - frac_outside(p, p-1, &r, slope);
}
double out_length(const fpoint* p0, const fpoint* pn, frectangle r, double slope)
{
const fpoint* p = p0;
double fr;
do {	if (p->x < r.min.x)
do if (++p>pn) return pn-p0;
while (p->x <= r.min.x);
else if (p->x > r.max.x)
do if (++p>pn) return pn-p0;
while (p->x >= r.max.x);
else if (p->y-slope*p->x < r.min.y)
do if (++p>pn) return pn-p0;
while (p->y-slope*p->x <= r.min.y);
else if (p->y-slope*p->x > r.max.y)
do if (++p>pn) return pn-p0;
while (p->y-slope*p->x >= r.max.y);
else return p - p0;
} while ((fr=frac_outside(p-1,p,&r,slope)) == 1);
return (p - p0) + fr-1;
}
#define Nthous  7
#define Len_thous  30
char* thous_nam[Nthous] = {
"one", "thousand", "million", "billion",
"trillion", "quadrillion", "quintillion",
};
typedef struct lab_interval {
double sep;
double unit;
int logunit;
double off;
} lab_interval;
char* abbrev_num(double x, const lab_interval* iv)
{
static char buf[16];
double dx = x - iv->off;
dx = iv->sep * floor(dx/iv->sep + .5);
sprintf(buf,"%g", dx/iv->unit);
return buf;
}
double lead_digits(double n, double r)
{
double rr = pow(10, ceil(log10(r)));
double nn = (n<rr) ? 0.0 : rr*floor(n/rr);
if (n+r-nn >= digs10pow) {
rr /= 10;
nn = (n<rr) ? 0.0 : rr*floor(n/rr);
}
return nn;
}
lab_interval next_larger(double s0, double xlo, double xhi)
{
double nlo, nhi;
lab_interval r;
r.logunit = (int) floor(log10(s0) + LOG2);
r.unit = pow(10, r.logunit);
nlo = xlo/r.unit;
nhi = xhi/r.unit;
if (nhi >= digs10pow)
r.off = r.unit*lead_digits(nlo, nhi-nlo);
else if (nlo <= -digs10pow)
r.off = -r.unit*lead_digits(-nhi, nhi-nlo);
else	r.off = 0;
r.sep = (s0<=r.unit) ? r.unit : (s0<2*r.unit ? 2*r.unit : 5*r.unit);
switch (r.logunit%3) {
case 1:	r.unit*=.1; r.logunit--;
break;
case -1: case 2:
r.unit*=10; r.logunit++;
break;
case -2: r.unit*=100; r.logunit+=2;
}
r.logunit /= 3;
return r;
}
double min_hsep(const transform* tr)
{
double s = (2+labdigs)*sdigit.x;
double ss = (univ.disp.min.x<0) ? s+sdigit.x : s;
return dxuntransform(tr, ss);
}
lab_interval mark_x_axis(const transform* tr)
{
fpoint p = univ.disp.min;
Point q, qtop, qbot, tmp;
double x0=univ.disp.min.x, x1=univ.disp.max.x;
double seps0, nseps, seps;
lab_interval iv = next_larger(min_hsep(tr), x0, x1);
set_unslanted_y(&univ, &p.y, 0);
q.y = ytransform(tr, p.y) + .5;
qtop.y = q.y - tick_len;
qbot.y = q.y + framewd + framesep;
seps0 = ceil(x0/iv.sep);
for (seps=0, nseps=floor(x1/iv.sep)-seps0; seps<=nseps; seps+=1) {
char* num = abbrev_num((p.x=iv.sep*(seps0+seps)), &iv);
Font* f = display->defaultfont;
q.x = qtop.x = qbot.x = xtransform(tr, p.x);
line(screen, qtop, q, Enddisc, Enddisc, 0, axis_color, q);
tmp = stringsize(f, num);
qbot.x -= tmp.x/2;
string(screen, qbot, display->black, qbot, f, num);
}
return iv;
}
lab_interval mark_y_axis(const transform* tr)
{
Font* f = display->defaultfont;
fpoint p = univ.disp.min;
Point q, qrt, qlft;
double y0, y1, seps0, nseps, seps;
lab_interval iv;
set_unslanted_y(&univ, &y0, &y1);
iv = next_larger(dyuntransform(tr,-f->height), y0, y1);
q.x = xtransform(tr, p.x) - .5;
qrt.x = q.x + tick_len;
qlft.x = q.x - (framewd + framesep);
seps0 = ceil(y0/iv.sep);
for (seps=0, nseps=floor(y1/iv.sep)-seps0; seps<=nseps; seps+=1) {
char* num = abbrev_num((p.y=iv.sep*(seps0+seps)), &iv);
Point qq = stringsize(f, num);
q.y = qrt.y = qlft.y = ytransform(tr, p.y);
line(screen, qrt, q, Enddisc, Enddisc, 0, axis_color, q);
qq.x = qlft.x - qq.x;
qq.y = qlft.y - qq.y/2;
string(screen, qq, display->black, qq, f, num);
}
return iv;
}
void lab_iv_info(const lab_interval *iv, double slant, char* buf, int *n)
{
if (iv->off > 0)
(*n) += sprintf(buf+*n,"-%.12g",iv->off);
else if (iv->off < 0)
(*n) += sprintf(buf+*n,"+%.12g",-iv->off);
if (slant>0)
(*n) += sprintf(buf+*n,"-%.6gx", slant);
else if (slant<0)
(*n) += sprintf(buf+*n,"+%.6gx", -slant);
if (abs(iv->logunit) >= Nthous)
(*n) += sprintf(buf+*n," in 1e%d units", 3*iv->logunit);
else if (iv->logunit > 0)
(*n) += sprintf(buf+*n," in %ss", thous_nam[iv->logunit]);
else if (iv->logunit < 0)
(*n) += sprintf(buf+*n," in %sths", thous_nam[-iv->logunit]);
}
void draw_xy_ranges(const lab_interval *xiv, const lab_interval *yiv)
{
Point p;
char buf[2*(19+Len_thous+8)+50];
int bufn = 0;
buf[bufn++] = 'x';
lab_iv_info(xiv, 0, buf, &bufn);
bufn += sprintf(buf+bufn, "; y");
lab_iv_info(yiv, u_slant_amt(&univ), buf, &bufn);
buf[bufn] = '\0';
p = stringsize(display->defaultfont, buf);
top_left = screen->r.min.x + lft_border;
p.x = top_right = screen->r.max.x - rt_border - p.x;
p.y = screen->r.min.y + outersep;
string(screen, p, display->black, p, display->defaultfont, buf);
}
transform draw_frame(void)
{
lab_interval x_iv, y_iv;
transform tr;
Rectangle r = screen->r;
lft_border = (univ.disp.min.y<0) ? lft_border0+sdigit.x : lft_border0;
tr = cur_trans();
r.min.x += lft_border;
r.min.y += top_border;
r.max.x -= rt_border;
r.max.y -= bot_border;
border(screen, r, -framewd, axis_color, r.min);
x_iv = mark_x_axis(&tr);
y_iv = mark_y_axis(&tr);
draw_xy_ranges(&x_iv, &y_iv);
return tr;
}
typedef struct pt_on_fpoly {
fpoint p;
fpolygon* fp;
double t;
} pt_on_fpoly;
static double myx, myy;
#define mydist(p,o,sl,xwt,ywt)	(myx=(p).x-(o).x, myy=(p).y-sl*(p).x-(o).y,	\
xwt*myx*myx + ywt*myy*myy)
double closest_time(const fpoint* p0, const fpoint* ctr, double slant,
double xwt, double ywt)
{
double p00y=p0[0].y-slant*p0[0].x, p01y=p0[1].y-slant*p0[1].x;
double dx=p0[1].x-p0[0].x, dy=p01y-p00y;
double x0=p0[0].x-ctr->x, y0=p00y-ctr->y;
double bot = xwt*dx*dx + ywt*dy*dy;
if (bot==0)
return 0;
return -(xwt*x0*dx + ywt*y0*dy)/bot;
}
int improve_pt(fpoint* p0, double len, const frectangle* r, double slant,
pt_on_fpoly* psel)
{
fpoint ctr = fcenter(r);
double x_wt=2/(r->max.x-r->min.x), y_wt=2/(r->max.y-r->min.y);
double xwt=x_wt*x_wt, ywt=y_wt*y_wt;
double d, dbest = (psel->t <0) ? 1e30 : mydist(psel->p,ctr,slant,xwt,ywt);
double tt, dbest0 = dbest;
fpoint pp;
int ilen = (int) len;
if (len==0 || ilen>0) {
int i;
for (i=(len==0 ? 0 : 1); i<=ilen; i++) {
d = mydist(p0[i], ctr, slant, xwt, ywt);
if (d < dbest)
{psel->p=p0[i]; psel->t=i; dbest=d;}
}
return (dbest < dbest0);
}
tt = closest_time(p0, &ctr, slant, xwt, ywt);
if (tt > len)
tt = len;
pp.x = p0[0].x + tt*(p0[1].x - p0[0].x);
pp.y = p0[0].y + tt*(p0[1].y - p0[0].y);
if (mydist(pp, ctr, slant, xwt, ywt) < dbest) {
psel->p = pp;
psel->t = tt;
return 1;
}
return 0;
}
void select_in_fpoly(fpolygon* fp, const frectangle* r, double slant,
pt_on_fpoly* psel)
{
fpoint *p0=fp->p, *pn=fp->p+fp->n;
double l1, l2;
if (p0==pn)
{improve_pt(p0, 0, r, slant, psel); psel->fp=fp; return;}
while ((l1=out_length(p0,pn,*r,slant)) < pn-p0) {
fpoint p0sav;
int i1 = (int) l1;
p0+=i1; l1-=i1;
p0sav = *p0;
p0[0].x += l1*(p0[1].x - p0[0].x);
p0[0].y += l1*(p0[1].y - p0[0].y);
l2 = in_length(p0, pn, *r, slant);
if (improve_pt(p0, l2, r, slant, psel)) {
if (l1==0 && psel->t!=((int) psel->t)) {
psel->t = 0;
psel->p = *p0;
} else if (psel->t < 1)
psel->t += l1*(1 - psel->t);
psel->t += p0 - fp->p;
psel->fp = fp;
}
*p0 = p0sav;
p0 += (l2>0) ? ((int) ceil(l2)) : 1;
}
}
pt_on_fpoly* select_in_univ(const frectangle* r, double slant)
{
static pt_on_fpoly answ;
fpolygon* fp;
answ.t = -1;
for (fp=univ.p; fp!=0; fp=fp->link)
if (fintersects(r, &fp->bb, slant))
select_in_fpoly(fp, r, slant, &answ);
if (answ.t < 0)
return 0;
return &answ;
}
pt_on_fpoly cur_sel;
pt_on_fpoly prev_sel;
Image* sel_bkg = 0;
void clear_txt(void)
{
Rectangle r;
r.min = screen->r.min;
r.min.x += lft_border;
r.min.y += outersep;
r.max.x = top_left;
r.max.y = r.min.y + smaxch.y;
draw(screen, r, display->white, display->opaque, r.min);
top_left = r.min.x;
}
Rectangle sel_dot_box(const transform* tr)
{
Point ctr;
Rectangle r;
if (tr==0)
ctr.x = ctr.y = Dotrad;
else	do_transform(&ctr, tr, &cur_sel.p);
r.min.x=ctr.x-Dotrad;  r.max.x=ctr.x+Dotrad+1;
r.min.y=ctr.y-Dotrad;  r.max.y=ctr.y+Dotrad+1;
return r;
}
void unselect(const transform* tr)
{
transform tra;
if (sel_bkg==0)
sel_bkg = allocimage(display, sel_dot_box(0), CMAP8, 0, DWhite);
clear_txt();
if (cur_sel.t < 0)
return;
prev_sel = cur_sel;
if (tr==0)
{tra=cur_trans(); tr=&tra;}
draw(screen, sel_dot_box(tr), sel_bkg, display->opaque, ZP);
cur_sel.t = -1;
}
void show_mytext(char* msg)
{
Point tmp, pt = screen->r.min;
int siz;
tmp = stringsize(display->defaultfont, msg);
siz = tmp.x;
pt.x=top_left;  pt.y+=outersep;
if (top_left+siz > top_right) {
Rectangle r;
r.min.y = pt.y;
r.min.x = top_right;
r.max.y = r.min.y + smaxch.y;
r.max.x = top_left+siz;
draw(screen, r, display->white, display->opaque, r.min);
top_right = top_left+siz;
}
string(screen, pt, display->black, ZP, display->defaultfont, msg);
top_left += siz;
}
double rnd(double x, double tol)
{
double t = pow(10, floor(log10(tol)));
return t * floor(x/t + .5);
}
double t_tol(double xtol, double ytol)
{
int t = (int) floor(cur_sel.t);
fpoint* p = cur_sel.fp->p;
double dx, dy;
if (t==cur_sel.t)
return 1;
dx = fabs(p[t+1].x - p[t].x);
dy = fabs(p[t+1].y - p[t].y);
xtol /= (xtol>dx) ? xtol : dx;
ytol /= (ytol>dy) ? ytol : dy;
return (xtol<ytol) ? xtol : ytol;
}
void say_where(const transform* tr)
{
double xtol=dxuntransform(tr,1), ytol=dyuntransform(tr,-1);
char buf[100];
int n, nmax = (top_right - top_left)/smaxch.x;
if (nmax >= 100)
nmax = 100-1;
n = sprintf(buf,"(%.14g,%.14g) at t=%.14g",
rnd(cur_sel.p.x,xtol), rnd(cur_sel.p.y,ytol),
rnd(cur_sel.t, t_tol(xtol,ytol)));
if (cur_sel.fp->nam[0] != 0)
sprintf(buf+n," %.*s", nmax-n-1, cur_sel.fp->nam);
show_mytext(buf);
}
void reselect(const transform* tr)
{
Point pt2, pt3;
fpoint p2;
transform tra;
if (cur_sel.t < 0)
return;
if (tr==0)
{tra=cur_trans(); tr=&tra;}
do_transform(&p2, tr, &cur_sel.p);
if (fabs(p2.x)+fabs(p2.y)>1e8 || (pt2.x=p2.x, pt2.y=p2.y, is_off_screen(pt2)))
{cur_sel.t= -1; return;}
pt3.x=pt2.x-Dotrad;  pt3.y=pt2.y-Dotrad;
draw(sel_bkg, sel_dot_box(0), screen, display->opaque, pt3);
fillellipse(screen, pt2, Dotrad, Dotrad, clr_im(DRed), pt2);
say_where(tr);
}
void do_select(Point pt)
{
transform tr = cur_trans();
fpoint pt1, pt2, ctr;
frectangle r;
double slant;
pt_on_fpoly* psel;
unselect(&tr);
do_untransform(&ctr, &tr, &pt);
pt1.x=pt.x-fuzz;  pt1.y=pt.y+fuzz;
pt2.x=pt.x+fuzz;  pt2.y=pt.y-fuzz;
do_untransform(&r.min, &tr, &pt1);
do_untransform(&r.max, &tr, &pt2);
slant = u_slant_amt(&univ);
slant_frect(&r, -slant);
psel = select_in_univ(&r, slant);
if (psel==0)
return;
if (logfil!=0) {
fprintf(logfil,"%.14g\t%.14g\n", psel->p.x, psel->p.y);
fflush(logfil);
}
cur_sel = *psel;
reselect(&tr);
}
void unshow_mytext(char* msg)
{
Rectangle r;
Point siz = stringsize(display->defaultfont, msg);
top_left -= siz.x;
r.min.y = screen->r.min.y + outersep;
r.min.x = top_left;
r.max.y = r.min.y + siz.y;
r.max.x = r.min.x + siz.x;
draw(screen, r, display->white, display->opaque, r.min);
}
char* prompt_text(char* prompt)
{
static char buf[200];
int n0, n=0, nshown=0;
Rune c;
unselect(0);
show_mytext(prompt);
while (n<200-1-UTFmax && (c=ekbd())!='\n') {
if (c=='\b') {
buf[n] = 0;
if (n > 0)
do n--;
while (n>0 && (buf[n-1]&0xc0)==0x80);
if (n < nshown)
{unshow_mytext(buf+n); nshown=n;}
} else {
n0 = n;
n += runetochar(buf+n, &c);
buf[n] = 0;
if (nshown==n0 && top_right-top_left >= smaxch.x)
{show_mytext(buf+n0); nshown=n;}
}
}
buf[n] = 0;
while (ecanmouse())
emouse();
return buf;
}
void draw_fpts(const fpoint* p0, double n1, const transform* tr, int thick,
Image* clr)
{
int n = (int) n1;
const fpoint* p = p0 + n;
fpoint pp;
Point qq, q;
if (n1 > n) {
pp.x = p[0].x + (n1-n)*(p[1].x - p[0].x);
pp.y = p[0].y + (n1-n)*(p[1].y - p[0].y);
} else	pp = *p--;
do_transform(&qq, tr, &pp);
if (n1==0)
fillellipse(screen, qq, 1+thick, 1+thick, clr, qq);
for (; p>=p0; p--) {
do_transform(&q, tr, p);
if(plotdots)
fillellipse(screen, q, Dotrad, Dotrad, clr, q);
else
line(screen, qq, q, Enddisc, Enddisc, thick, clr, qq);
qq = q;
}
}
void draw_1fpoly(const fpolygon* fp, const transform* tr, Image* clr,
const frectangle *udisp, double slant)
{
fpoint *p0=fp->p, *pn=fp->p+fp->n;
double l1, l2;
if (p0==pn && fcontains(udisp,*p0))
{draw_fpts(p0, 0, tr, fp->c.thick, clr); return;}
while ((l1=out_length(p0,pn,*udisp,slant)) < pn-p0) {
fpoint p0sav;
int i1 = (int) l1;
p0+=i1; l1-=i1;
p0sav = *p0;
p0[0].x += l1*(p0[1].x - p0[0].x);
p0[0].y += l1*(p0[1].y - p0[0].y);
l2 = in_length(p0, pn, *udisp, slant);
draw_fpts(p0, l2, tr, fp->c.thick, clr);
*p0 = p0sav;
p0 += (l2>0) ? ((int) ceil(l2)) : 1;
}
}
double get_clip_data(const fpolygons *u, frectangle *r)
{
double slant = set_unslanted_y(u, &r->min.y, &r->max.y);
r->min.x = u->disp.min.x;
r->max.x = u->disp.max.x;
return slant;
}
void draw_fpoly(const fpolygon* fp, const transform* tr, Image* clr)
{
frectangle r;
double slant = get_clip_data(&univ, &r);
draw_1fpoly(fp, tr, clr, &r, slant);
}
void eresized(int new)
{
transform tr;
fpolygon* fp;
frectangle clipr;
double slant;
if(new && getwindow(display, Refmesg) < 0) {
fprintf(stderr,"can't reattach to window\n");
exits("reshap");
}
draw(screen, screen->r, display->white, display->opaque, screen->r.min);
tr = draw_frame();
slant = get_clip_data(&univ, &clipr);
for (fp=univ.p; fp!=0; fp=fp->link)
if (fintersects(&clipr, &fp->bb, slant))
draw_1fpoly(fp, &tr, fp->c.clr, &clipr, slant);
reselect(0);
if (mv_bkgd!=0 && mv_bkgd->repl==0) {
freeimage(mv_bkgd);
mv_bkgd = display->white;
}
flushimage(display, 1);
}
int draw_palette(int n)
{
int y0 = screen->r.min.y + top_border;
int dy = (screen->r.max.y - bot_border - y0)/n;
Rectangle r;
int i;
r.min.y = y0;
r.min.x = screen->r.max.x - rt_border + framewd;
r.max.y = y0 + dy;
r.max.x = screen->r.max.x;
for (i=0; i<n; i++) {
draw(screen, r, clrtab[i].im, display->opaque, r.min);
r.min.y = r.max.y;
r.max.y += dy;
}
return dy;
}
Image* palette_color(Point pt, int dy, int n)
{
int yy;
if (screen->r.max.x - pt.x > rt_border - framewd)
return 0;
yy = pt.y - (screen->r.min.y + top_border);
if (yy<0 || yy>=n*dy)
return 0;
return clrtab[yy/dy].im;
}
void all_set_clr(fpolygons* fps, Image* clr)
{
fpolygon* p;
for (p=fps->p; p!=0; p=p->link)
p->c.clr = clr;
}
void all_set_scheme(fpolygons* fps, int scheme)
{
fpolygon* p;
for (p=fps->p; p!=0; p=p->link)
if (p->ct!=0 && scheme <= p->ct[0].thick)
p->c = p->ct[scheme];
}
void do_recolor(int but, Mouse* m, int alluniv)
{
int sel, clkk, nclr = clr_id(DWhite);
int dy = draw_palette(nclr);
Image* clr;
clkk = get_click_or_kbd(but, m, "123456789abcdefghijklmnopqrstuvwxyz");
if (clkk < 0) {
clr = palette_color(m->xy, dy, nclr);
if (clr != 0) {
if (alluniv)
all_set_clr(&univ, clr);
else cur_sel.fp->c.clr = clr;
}
eresized(0);
lift_button(but, m, Never);
} else if (clkk > 0) {
sel = ('0'<clkk&&clkk<='9') ? 0 : 10+(clkk-'a')*10;
while (!('0'<=clkk&&clkk<='9'))
clkk = ekbd();
sel += clkk-'0';
if (alluniv)
all_set_scheme(&univ, sel);
else if (sel <= cur_sel.fp->ct[0].thick)
cur_sel.fp->c = cur_sel.fp->ct[sel];
}
eresized(0);
}
void prepare_mv(const fpolygon* fp)
{
Rectangle r = screen->r;
Image* scr0;
int dt = 1 + fp->c.thick;
r.min.x+=lft_border-dt;  r.min.y+=top_border-dt;
r.max.x-=rt_border-dt;   r.max.y-=bot_border-dt;
if (mv_bkgd!=0 && mv_bkgd->repl==0)
freeimage(mv_bkgd);
mv_bkgd = allocimage(display, r, CMAP8, 0, DNofill);
if (mv_bkgd==0)
mv_bkgd = display->white;
else {	transform tr = cur_trans();
draw(mv_bkgd, r, screen, display->opaque, r.min);
draw(mv_bkgd, sel_dot_box(&tr), sel_bkg, display->opaque, ZP);
scr0 = screen;
screen = mv_bkgd;
draw_fpoly(fp, &tr, display->white);
screen = scr0;
}
}
void move_fp(fpolygon* fp, double dx, double dy)
{
fpoint *p, *pn=fp->p+fp->n;
for (p=fp->p; p<=pn; p++) {
(p->x) += dx;
(p->y) += dy;
}
(fp->bb.min.x)+=dx;  (fp->bb.min.y)+=dy;
(fp->bb.max.x)+=dx;  (fp->bb.max.y)+=dy;
}
void rotate_fp(fpolygon* fp, fpoint o, double theta)
{
double s=sin(theta), c=cos(theta);
fpoint *p, *pn=fp->p+fp->n;
for (p=fp->p; p<=pn; p++) {
double x=p->x-o.x, y=p->y-o.y;
(p->x) = o.x + c*x - s*y;
(p->y) = o.y + s*x + c*y;
}
set_fbb(fp);
}
fpoint do_move(int but, Mouse* m)
{
transform tr = cur_trans();
int bbit = Button_bit(but);
fpolygon* fp = cur_sel.fp;
fpoint loc, loc0=cur_sel.p;
double tsav = cur_sel.t;
unselect(&tr);
do {	latest_mouse(but, m);
(fp->c.thick)++;
draw_fpoly(fp, &tr, mv_bkgd);
(fp->c.thick)--;
do_untransform(&loc, &tr, &m->xy);
move_fp(fp, loc.x-cur_sel.p.x, loc.y-cur_sel.p.y);
cur_sel.p = loc;
draw_fpoly(fp, &tr, fp->c.clr);
} while (m->buttons & bbit);
cur_sel.t = tsav;
reselect(&tr);
loc.x -= loc0.x;
loc.y -= loc0.y;
return loc;
}
double dir_angle(const Point* pt, const transform* tr)
{
fpoint p;
double dy, dx;
do_untransform(&p, tr, pt);
dy=p.y-cur_sel.p.y;  dx=p.x-cur_sel.p.x;
return (dx==0 && dy==0) ? 0.0 : atan2(dy, dx);
}
double do_rotate(int but, Mouse* m)
{
transform tr = cur_trans();
int bbit = Button_bit(but);
fpolygon* fp = cur_sel.fp;
double theta0 = dir_angle(&m->xy, &tr);
double th, theta = theta0;
do {	latest_mouse(but, m);
(fp->c.thick)++;
draw_fpoly(fp, &tr, mv_bkgd);
(fp->c.thick)--;
th = dir_angle(&m->xy, &tr);
rotate_fp(fp, cur_sel.p, th-theta);
theta = th;
draw_fpoly(fp, &tr, fp->c.clr);
} while (m->buttons & bbit);
unselect(&tr);
cur_sel = prev_sel;
reselect(&tr);
return theta - theta0;
}
typedef enum e_index {
Erecolor, Ethick, Edelete, Eundo, Erotate, Eoptions,
Emove
} e_index;
char* e_items[Eoptions+1];
Menu e_menu = {e_items, 0, 0};
typedef struct e_action {
e_index typ;
fpolygon* fp;
Image* clr;
double amt;
fpoint pt;
struct e_action* link;
} e_action;
e_action* unact = 0;
e_action* do_undo(e_action*);
e_action* save_act(e_action*,e_index);
void save_mv(fpoint movement)
{
unact = save_act(unact, Emove);
unact->pt = movement;
}
void init_e_menu(void)
{
char* u = "can't undo";
e_items[Erecolor] = "recolor";
e_items[Edelete] = "delete";
e_items[Erotate] = "rotate";
e_items[Eoptions-cantmv] = 0;
e_items[Ethick] = (cur_sel.fp->c.thick >0) ? "thin" : "thick";
if (unact!=0)
switch (unact->typ) {
case Erecolor: u="uncolor"; break;
case Ethick: u=(unact->fp->c.thick==0) ? "unthin" : "unthicken";
break;
case Edelete: u="undelete"; break;
case Emove: u="unmove"; break;
case Erotate: u="unrotate"; break;
}
e_items[Eundo] = u;
}
void do_emenu(int but, Mouse* m)
{
int h;
if (cur_sel.t < 0)
return;
init_e_menu();
h = emenuhit(but, m, &e_menu);
switch(h) {
case Ethick: unact = save_act(unact, h);
cur_sel.fp->c.thick ^= 1;
eresized(0);
break;
case Edelete: unact = save_act(unact, h);
fp_remove(&univ, cur_sel.fp);
unselect(0);
eresized(0);
break;
case Erecolor: unact = save_act(unact, h);
do_recolor(but, m, 0);
break;
case Erotate: unact = save_act(unact, h);
prepare_mv(cur_sel.fp);
if (get_1click(but, m, 0)) {
unact->pt = cur_sel.p;
unact->amt = do_rotate(but, m);
}
break;
case Eundo: unact = do_undo(unact);
break;
}
}
e_action* save_act(e_action* a0, e_index typ)
{
e_action* a = malloc(sizeof(e_action));
a->link = a0;
a->pt.x = a->pt.y = 0.0;
a->amt = cur_sel.fp->c.thick;
a->clr = cur_sel.fp->c.clr;
a->fp = cur_sel.fp;
a->typ = typ;
return a;
}
void do_unmove(e_action* a)
{
double tsav = cur_sel.t;
unselect(0);
move_fp(a->fp, -a->pt.x, -a->pt.y);
if (a->fp == cur_sel.fp) {
cur_sel.p.x -= a->pt.x;
cur_sel.p.y -= a->pt.y;
}
cur_sel.t = tsav;
reselect(0);
}
e_action* do_undo(e_action* a0)
{
e_action* a = a0;
if (a==0)
return 0;
switch(a->typ) {
case Ethick: a->fp->c.thick = a->amt;
eresized(0);
break;
case Erecolor: a->fp->c.clr = a->clr;
eresized(0);
break;
case Edelete:
a->fp->link = univ.p;
univ.p = a->fp;
grow_bb(&univ.bb, &a->fp->bb);
eresized(0);
break;
case Emove:
do_unmove(a);
eresized(0);
break;
case Erotate:
unselect(0);
rotate_fp(a->fp, a->pt, -a->amt);
eresized(0);
break;
}
a0 = a->link;
free(a);
return a0;
}
enum m_index {     Mzoom_in,  Mzoom_out,  Munzoom,  Mslant,    Munslant,
Msquare_up,  Mrecenter,  Mrecolor,  Mrestack,  Mread,
Mwrite,      Mexit};
char* m_items[] = {"zoom in", "zoom out", "unzoom", "slant",   "unslant",
"square up", "recenter", "recolor", "restack", "read",
"write",     "exit", 0};
Menu m_menu = {m_items, 0, 0};
void do_mmenu(int but, Mouse* m)
{
int e, h = emenuhit(but, m, &m_menu);
switch (h) {
case Mzoom_in:
disp_zoomin(egetrect(but,m));
eresized(0);
break;
case Mzoom_out:
disp_zoomout(egetrect(but,m));
eresized(0);
break;
case Msquare_up:
disp_squareup();
eresized(0);
break;
case Munzoom:
init_disp();
eresized(0);
break;
case Mrecenter:
if (get_1click(but, m, &bullseye)) {
recenter_disp(m->xy);
eresized(0);
lift_button(but, m, Never);
}
break;
case Mslant:
if (cur_sel.t>=0 && prev_sel.t>=0) {
slant_disp(prev_sel.p, cur_sel.p);
eresized(0);
}
break;
case Munslant:
univ.slant_ht = univ.disp.max.y - univ.disp.min.y;
eresized(0);
break;
case Mrecolor:
do_recolor(but, m, 1);
break;
case Mrestack:
fps_invert(&univ);
eresized(0);
break;
case Mread:
e = doinput(prompt_text("File:"));
if (e==0)
eresized(0);
else if (e<0)
show_mytext(" - can't read");
else {
char ebuf[80];
snprintf(ebuf, 80, " - error line %d", e);
show_mytext(ebuf);
}
break;
case Mwrite:
if (!dooutput(prompt_text("File:")))
show_mytext(" - can't write");
break;
case Mexit:
exits("");
}
}
void doevent(void)
{
ulong etype;
int mobile;
ulong mvtime;
Event	ev;
etype = eread(Emouse|Ekeyboard, &ev);
if(etype & Emouse) {
if (ev.mouse.buttons & But1) {
do_select(ev.mouse.xy);
mvtime = Never;
mobile = !cantmv && cur_sel.t>=0;
if (mobile) {
mvtime = ev.mouse.msec + Mv_delay;
prepare_mv(cur_sel.fp);
}
if (!lift_button(1, &ev.mouse, mvtime) && mobile)
save_mv(do_move(1, &ev.mouse));
} else if (ev.mouse.buttons & But2)
do_emenu(2, &ev.mouse);
else if (ev.mouse.buttons & But3)
do_mmenu(3, &ev.mouse);
} else if (etype & Ekeyboard) {
if (ev.kbdc=='\n' && cur_sel.t>=0 && logfil!=0) {
fprintf(logfil,"%s\n", cur_sel.fp->nam);
fflush(logfil);
}
}
}
extern char* argv0;
void usage(void)
{
int i;
fprintf(stderr,"Usage %s [options] [infile]\n", argv0);
fprintf(stderr,
"option ::= -l logfile | -m | -p\n"
"\n"
"Read a polygonal line graph in an ASCII format (one x y pair per line, delimited\n"
"by spaces with a label after each polyline), and view it interactively.  Use\n"
"standard input if no infile is specified.\n"
"Option -l specifies a file in which to log the coordinates of each point selected.\n"
"(Clicking a point with button one selects it and displays its coordinates and\n"
"the label of its polylone.)  Option -m allows polylines to be moved and rotated.\n"
"The -p option plots only the vertices of the polygons.\n"
"The polyline labels can use the following color names:"
);
for (i=0; clrtab[i].c!=DNofill; i++)
fprintf(stderr,"%s%8s", (i%8==0 ? "\n" : "  "), clrtab[i].nam);
fputc('\n', stderr);
exits("usage");
}
void main(int argc, char *argv[])
{
int e;
char err[ERRMAX];
ARGBEGIN {
case 'm':
cantmv=0;
break;
case 'l':
logfil = fopen(ARGF(),"w");
break;
case 'p':
plotdots++;
break;
default:
usage();
} ARGEND;
if(initdraw(0, 0, "gview") < 0)
exits("initdraw");
einit(Emouse|Ekeyboard);
do {
e = doinput(*argv ? *argv : "-");
if (e < 0) {
rerrstr(err, sizeof err);
fprintf(stderr, "%s: cannot read %s: %s\n",
argv0, *argv, err);
exits("no valid input file");
} else if (e > 0) {
fprintf(stderr, "%s: %s:%d: bad data syntax\n",
argv0, (*argv ? *argv : "-"), e);
exits("bad syntax in input");
}
} while (*argv && *++argv);
init_disp();
init_clrtab();
set_default_clrs(&univ, 0);
adjust_border(display->defaultfont);
cur_sel.t = prev_sel.t = -1;
eresized(0);
for(;;)
doevent();
}