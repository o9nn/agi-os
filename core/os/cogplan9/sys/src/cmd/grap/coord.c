#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "grap.h"
#include "y.tab.h"
char	*dflt_coord = "gg";
char	*curr_coord = "gg";
int	ncoord	= 0;
Point	xcoord;
Point	ycoord;
int	xcflag	= 0;
int	ycflag	= 0;
int	logcoord = 0;
void coord_x(Point pt)
{
xcoord = pt;
xcflag = 1;
margin = 0;
}
void coord_y(Point pt)
{
ycoord = pt;
ycflag = 1;
margin = 0;
}
void coordlog(int n)
{
logcoord = n;
}
void coord(Obj *p)
{
static char buf[10];
ncoord++;
if (ncoord > 1 && strcmp(p->name, dflt_coord) == 0) {
sprintf(buf, "gg%d", ncoord);
dflt_coord = buf;
p = lookup(dflt_coord, 1);
}
if (xcflag) {
p->coord |= XFLAG;
p->pt.x = min(xcoord.x,xcoord.y);
p->pt1.x = max(xcoord.x,xcoord.y);
if ((logcoord&XFLAG) && p->pt.x <= 0.0)
ERROR "can't have log of x coord %g,%g", p->pt.x, p->pt1.x FATAL;
xcflag = 0;
}
if (ycflag) {
p->coord |= YFLAG;
p->pt.y = min(ycoord.x,ycoord.y);
p->pt1.y = max(ycoord.x,ycoord.y);
if ((logcoord&YFLAG) && p->pt.y <= 0.0)
ERROR "can't have log of y coord %g,%g", p->pt.y, p->pt1.y FATAL;
ycflag = 0;
}
p->log = logcoord;
logcoord = 0;
auto_x = 0;
}
void resetcoord(Obj *p)
{
curr_coord = p->name;
}