#include <stdio.h>
#include <stdlib.h>
#include "grap.h"
#include "y.tab.h"
double	frame_ht;
double	frame_wid;
int	nsides	= 0;
char	*sides[] = {
"\tline from Frame.nw to Frame.ne",
"\tline from Frame.sw to Frame.se",
"\tline from Frame.sw to Frame.nw",
"\tline from Frame.se to Frame.ne"
};
char	*newsides[4] = { 0, 0, 0, 0 };
void frame(void)
{
int i;
fprintf(tfd, "\tframeht = %g\n", frame_ht);
fprintf(tfd, "\tframewid = %g\n", frame_wid);
fprintf(tfd, "Frame:\tbox ht frameht wid framewid with .sw at 0,0 ");
if (nsides == 0)
fprintf(tfd, "\n");
else {
fprintf(tfd, "invis\n");
for (i = 0; i < 4; i++) {
if (newsides[i]) {
fprintf(tfd, "%s\n", newsides[i]);
free(newsides[i]);
newsides[i] = 0;
} else
fprintf(tfd, "%s\n", sides[i]);
}
nsides = 0;
}
}
void frameht(double f)
{
frame_ht = f;
}
void framewid(double f)
{
frame_wid = f;
}
void frameside(int type, Attr *desc)
{
int n;
char buf[100];
nsides++;
n = 0;
switch (type) {
case 0:
frameside(TOP, desc);
frameside(BOT, desc);
frameside(LEFT, desc);
frameside(RIGHT, desc);
return;
case TOP:	n = 0; break;
case BOT:	n = 1; break;
case LEFT:	n = 2; break;
case RIGHT:	n = 3; break;
}
sprintf(buf, "%s %s", sides[n], desc_str(desc));
newsides[n] = tostring(buf);
}