#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "grap.h"
#include "y.tab.h"
#define	MAXTICK	200
int	ntick	= 0;
double	tickval[MAXTICK];
char	*tickstr[MAXTICK];
int	tside	= 0;
int	tlist	= 0;
int	toffside = 0;
int	goffside = 0;
int	tick_dir = OUT;
double	ticklen	= TICKLEN;
int	autoticks = LEFT|BOT;
int	autodir = 0;
void savetick(double f, char *s)
{
if (ntick >= MAXTICK)
ERROR "too many ticks (%d)", MAXTICK FATAL;
tickval[ntick] = f;
tickstr[ntick] = s;
ntick++;
}
void dflt_tick(double f)
{
if (f >= 0.0)
savetick(f, tostring("%g"));
else
savetick(f, tostring("\\%g"));
}
void tickside(int n)
{
tside |= n;
}
void tickoff(int side)
{
toffside |= side;
}
void gridtickoff(void)
{
goffside = tside;
}
void setlist(void)
{
tlist = 1;
}
void tickdir(int dir, double val, int explicit)
{
tick_dir = dir;
if (explicit)
ticklen = val;
}
void ticks(void)
{
if (toffside)
autoticks &= ~toffside;
if (tlist) {
if (tside & (BOT|TOP))
autoticks &= ~(BOT|TOP);
if (tside & (LEFT|RIGHT))
autoticks &= ~(LEFT|RIGHT);
}
if (tside && !tlist) {
if (tick_dir == IN)
autodir |= tside;
if (tside & (BOT|TOP))
autoticks = (autoticks & ~(BOT|TOP)) | (tside & (BOT|TOP));
if (tside & (LEFT|RIGHT))
autoticks = (autoticks & ~(LEFT|RIGHT)) | (tside & (LEFT|RIGHT));
}
tlist = tside = toffside = goffside = 0;
tick_dir = OUT;
}
double modfloor(double f, double t)
{
t = fabs(t);
return floor(f/t) * t;
}
double modceil(double f, double t)
{
t = fabs(t);
return ceil(f/t) * t;
}
double	xtmin, xtmax;
double	ytmin, ytmax;
double	xquant, xmult;
double	yquant, ymult;
double	lograt = 5;
void do_autoticks(Obj *p)
{
double x, xl, xu, q;
if (p == NULL)
return;
fprintf(tfd, "Autoticks:\t# x %g..%g, y %g..%g",
p->pt.x, p->pt1.x, p->pt.y, p->pt1.y);
fprintf(tfd, ";   xt %g,%g, yt %g,%g, xq,xm = %g,%g, yq,ym = %g,%g\n",
xtmin, xtmax, ytmin, ytmax, xquant, xmult, yquant, ymult);
if ((autoticks & (BOT|TOP)) && p->pt1.x >= p->pt.x) {
q = xquant;
xl = p->pt.x;
xu = p->pt1.x;
if (xl >= xu)
dflt_tick(xl);
else if ((p->log & XFLAG) && xu/xl >= lograt) {
for (x = q; x < xu; x *= 10) {
logtick(x, xl, xu);
if (xu/xl <= 100) {
logtick(2*x, xl, xu);
logtick(5*x, xl, xu);
}
}
} else {
xl = modceil(xtmin - q/100, q);
xu = modfloor(xtmax + q/100, q) + q/2;
for (x = xl; x <= xu; x += q)
dflt_tick(x);
}
tside = autoticks & (BOT|TOP);
ticklist(p, 0);
}
if ((autoticks & (LEFT|RIGHT)) && p->pt1.y >= p->pt.y) {
q = yquant;
xl = p->pt.y;
xu = p->pt1.y;
if (xl >= xu)
dflt_tick(xl);
else if ((p->log & YFLAG) && xu/xl >= lograt) {
for (x = q; x < xu; x *= 10) {
logtick(x, xl, xu);
if (xu/xl <= 100) {
logtick(2*x, xl, xu);
logtick(5*x, xl, xu);
}
}
} else {
xl = modceil(ytmin - q/100, q);
xu = modfloor(ytmax + q/100, q) + q/2;
for (x = xl; x <= xu; x += q)
dflt_tick(x);
}
tside = autoticks & (LEFT|RIGHT);
ticklist(p, 0);
}
}
void logtick(double v, double lb, double ub)
{
float slop = 1.0;
if (slop * lb <= v && ub >= slop * v)
dflt_tick(v);
}
Obj *setauto(void)
{
Obj *p, *q;
if ((q = lookup("lograt",0)) != NULL)
lograt = q->fval;
for (p = objlist; p; p = p->next)
if (p->type == NAME && strcmp(p->name,dflt_coord) == 0)
break;
if (p) {
if ((p->log & XFLAG) && p->pt1.x/p->pt.x >= lograt)
autolog(p, 'x');
else
autoside(p, 'x');
if ((p->log & YFLAG) && p->pt1.y/p->pt.y >= lograt)
autolog(p, 'y');
else
autoside(p, 'y');
}
return p;
}
void autoside(Obj *p, int side)
{
double r, s, d, ub, lb;
if (side == 'x') {
xtmin = lb = p->pt.x;
xtmax = ub = p->pt1.x;
} else {
ytmin = lb = p->pt.y;
ytmax = ub = p->pt1.y;
}
if (ub <= lb)
return;
d = ub - lb;
r = s = 1;
while (d * s < 10)
s *= 10;
d *= s;
while (10 * r < d)
r *= 10;
if (r > d/3)
r /= 2;
else if (r <= d/6)
r *= 2;
if (side == 'x') {
xquant = r / s;
} else {
yquant = r / s;
}
}
void autolog(Obj *p, int side)
{
double r, s, t, ub, lb;
int flg;
if (side == 'x') {
xtmin = lb = p->pt.x;
xtmax = ub = p->pt1.x;
flg = p->coord & XFLAG;
} else {
ytmin = lb = p->pt.y;
ytmax = ub = p->pt1.y;
flg = p->coord & YFLAG;
}
for (s = 1; lb * s < 1; s *= 10)
;
lb *= s;
ub *= s;
for (r = 1; 10 * r < lb; r *= 10)
;
for (t = 1; t < ub; t *= 10)
;
if (side == 'x')
xquant = r / s;
else
yquant = r / s;
if (flg)
return;
if (ub / lb < 100) {
if (lb >= 5 * r)
r *= 5;
else if (lb >= 2 * r)
r *= 2;
if (ub * 5 <= t)
t /= 5;
else if (ub * 2 <= t)
t /= 2;
if (side == 'x') {
xtmin = r / s;
xtmax = t / s;
} else {
ytmin = r / s;
ytmax = t / s;
}
}
}
void iterator(double from, double to, int op, double by, char *fmt)
{
double x;
dprintf("iterate from %g to %g by %g, op = %c, fmt=%s\n",
from, to, by, op, fmt ? fmt : "");
switch (op) {
case '+':
case ' ':
for (x = from; x <= to + (SLOP-1) * by; x += by)
if (fmt)
savetick(x, tostring(fmt));
else
dflt_tick(x);
break;
case '-':
for (x = from; x >= to; x -= by)
if (fmt)
savetick(x, tostring(fmt));
else
dflt_tick(x);
break;
case '*':
for (x = from; x <= SLOP * to; x *= by)
if (fmt)
savetick(x, tostring(fmt));
else
dflt_tick(x);
break;
case '/':
for (x = from; x >= to; x /= by)
if (fmt)
savetick(x, tostring(fmt));
else
dflt_tick(x);
break;
}
if (fmt)
free(fmt);
}
void ticklist(Obj *p, int explicit)
{
if (p == NULL)
return;
fprintf(tfd, "Ticks_%s:\n\tticklen = %g\n", p->name, ticklen);
print_ticks(TICKS, explicit, p, "ticklen", "");
}
void print_ticks(int type, int explicit, Obj *p, char *lenstr, char *descstr)
{
int i, logflag, inside;
char buf[100];
double tv;
for (i = 0; i < ntick; i++)
if (tickstr[i] != NULL)
break;
if (i >= ntick && type == TICKS)
for (i = 0; i < ntick; i++) {
if (tickval[i] >= 0.0)
sprintf(buf, "%g", tickval[i]);
else
sprintf(buf, "\\-%g", -tickval[i]);
tickstr[i] = tostring(buf);
}
else
for (i = 0; i < ntick; i++) {
if (tickstr[i] != NULL) {
sprintf(buf, tickstr[i], tickval[i]);
free(tickstr[i]);
tickstr[i] = tostring(buf);
}
}
logflag = sidelog(p->log, tside);
for (i = 0; i < ntick; i++) {
tv = tickval[i];
halfrange(p, tside, tv);
if (logflag) {
if (tv <= 0.0)
ERROR "can't take log of tick value %g", tv FATAL;
logit(tv);
}
if (type == GRID)
inside = LEFT|RIGHT|TOP|BOT;
else if (explicit)
inside = (tick_dir == IN) ? tside : 0;
else
inside = autodir;
if (tside & BOT)
maketick(type, p->name, BOT, inside, tv, tickstr[i], lenstr, descstr);
if (tside & TOP)
maketick(type, p->name, TOP, inside, tv, tickstr[i], lenstr, descstr);
if (tside & LEFT)
maketick(type, p->name, LEFT, inside, tv, tickstr[i], lenstr, descstr);
if (tside & RIGHT)
maketick(type, p->name, RIGHT, inside, tv, tickstr[i], lenstr, descstr);
if (tickstr[i]) {
free(tickstr[i]);
tickstr[i] = NULL;
}
}
ntick = 0;
}
void maketick(int type, char *name, int side, int inflag, double val, char *lab, char *lenstr, char *descstr)
{
char *sidestr, *td;
fprintf(tfd, "\tline %s ", descstr);
inflag &= side;
switch (side) {
case BOT:
case 0:
td = inflag ? "up" : "down";
fprintf(tfd, "%s %s from (x_%s(%g),0)", td, lenstr, name, val);
break;
case TOP:
td = inflag ? "down" : "up";
fprintf(tfd, "%s %s from (x_%s(%g),frameht)", td, lenstr, name, val);
break;
case LEFT:
td = inflag ? "right" : "left";
fprintf(tfd, "%s %s from (0,y_%s(%g))", td, lenstr, name, val);
break;
case RIGHT:
td = inflag ? "left" : "right";
fprintf(tfd, "%s %s from (framewid,y_%s(%g))", td, lenstr, name, val);
break;
}
fprintf(tfd, "\n");
if (type == GRID && (side & goffside))
return;
sidestr = tick_dir == IN ? "start" : "end";
if (lab != NULL) {
double wid = strlen(lab)/7.5 + (tick_dir == IN ? 0 : 0.1);
switch (side) {
case BOT: case 0:
fprintf(tfd, "\tbox invis \"%s\" ht .25 wid 0 with .n at last line.%s",
lab, sidestr);
break;
case TOP:
fprintf(tfd, "\tbox invis \"%s\" ht .2 wid 0 with .s at last line.%s",
lab, sidestr);
break;
case LEFT:
fprintf(tfd, "\t\"%s \" wid %.2f rjust at last line.%s",
lab, wid, sidestr);
break;
case RIGHT:
fprintf(tfd, "\t\" %s\" wid %.2f ljust at last line.%s",
lab, wid, sidestr);
break;
}
lab_adjust();
fprintf(tfd, "\n");
}
}
Attr	*grid_desc	= 0;
void griddesc(Attr *a)
{
grid_desc = a;
}
void gridlist(Obj *p)
{
char *framestr;
if ((tside & (BOT|TOP)) || tside == 0)
framestr = "frameht";
else
framestr = "framewid";
fprintf(tfd, "Grid_%s:\n", p->name);
tick_dir = IN;
print_ticks(GRID, 0, p, framestr, desc_str(grid_desc));
if (grid_desc) {
freeattr(grid_desc);
grid_desc = 0;
}
}
char *desc_str(Attr *a)
{
static char buf[50], *p;
if (a == NULL)
return p = "";
switch (a->type) {
case DOT:	p = "dotted"; break;
case DASH:	p = "dashed"; break;
case INVIS:	p = "invis"; break;
default:	p = "";
}
if (a->fval != 0.0) {
sprintf(buf, "%s %g", p, a->fval);
return buf;
} else
return p;
}
sidelog(int logflag, int side)
{
if ((logflag & XFLAG) && ((side & (BOT|TOP)) || side == 0))
return 1;
else if ((logflag & YFLAG) && (side & (LEFT|RIGHT)))
return 1;
else
return 0;
}