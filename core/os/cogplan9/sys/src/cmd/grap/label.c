#include <stdio.h>
#include <string.h>
#include "grap.h"
#include "y.tab.h"
int pointsize = 10;
int ps_set = 0;
double textht = 1.0/6.0;
double textwid = 1;
double lab_up = 0.0;
double lab_rt = 0.0;
double lab_wid = 0.0;
void labelwid(double amt)
{
lab_wid = amt + .00001;
}
void labelmove(int dir, double amt)
{
switch (dir) {
case UP: lab_up += amt; break;
case DOWN: lab_up -= amt; break;
case LEFT: lab_rt -= amt; break;
case RIGHT: lab_rt += amt; break;
}
}
void label(int label_side, Attr *stringlist)
{
int m;
Attr *ap;
fprintf(tfd, "\ttextht = %g\n", textht);
if (lab_wid != 0.0) {
fprintf(tfd, "\ttextwid = %g\n", lab_wid);
lab_wid = 0;
} else if (label_side == LEFT || label_side == RIGHT) {
textwid = 0;
for (ap = stringlist; ap != NULL; ap = ap->next)
if ((m = strlen(ap->sval)) > textwid)
textwid = m;
textwid /= 15;
fprintf(tfd, "\ttextwid = %g\n", textwid);
}
fprintf(tfd, "Label:\t%s", slprint(stringlist));
freeattr(stringlist);
switch (label_side) {
case BOT:
case 0:
fprintf(tfd, " with .n at Frame.s - (0,2 * textht)");
break;
case LEFT:
fprintf(tfd, " wid textwid with .e at Frame.w - (0.2,0)");
break;
case RIGHT:
fprintf(tfd, " wid textwid with .w at Frame.e + (0.2,0)");
break;
case TOP:
fprintf(tfd, " with .s at Frame.n + (0,2 * textht)");
break;
}
lab_adjust();
fprintf(tfd, "\n");
label_side = BOT;
}
void lab_adjust(void)
{
if (lab_up != 0.0 || lab_rt != 0.0)
fprintf(tfd, " + (%g,%g)", lab_rt, lab_up);
}
char *sizeit(Attr *ap)
{
int n;
static char buf[1000];
if (!ap->op) {
if (ps_set) {
sprintf(buf, "\\s%d%s\\s0", pointsize, ap->sval);
return buf;
} else
return ap->sval;
} else if (!ps_set) {
n = (int) ap->fval;
switch (ap->op) {
case ' ':
sprintf(buf, "\\s%d%s\\s0", n, ap->sval);
break;
case '+':
sprintf(buf, "\\s+%d%s\\s-%d", n, ap->sval, n);
break;
case '-':
sprintf(buf, "\\s-%d%s\\s+%d", n, ap->sval, n);
break;
case '*':
case '/':
return ap->sval;
}
return buf;
} else {
n = (int) ap->fval;
switch (ap->op) {
case ' ':
sprintf(buf, "\\s%d%s\\s0", n, ap->sval);
break;
case '+':
sprintf(buf, "\\s%d%s\\s0", pointsize+n, ap->sval);
break;
case '-':
sprintf(buf, "\\s%d%s\\s0", pointsize-n, ap->sval);
break;
case '*':
case '/':
return ap->sval;
}
return buf;
}
}