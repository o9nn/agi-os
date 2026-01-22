#include <stdio.h>
#include <stdlib.h>
#include "grap.h"
#include "y.tab.h"
typedef struct {
Obj	*var;
double	to;
double	by;
int	op;
char	*str;
} For;
#define	MAXFOR	10
For	forstk[MAXFOR];
For	*forp = forstk;
void forloop(Obj *var, double from, double to, int op, double by, char *str)
{
fprintf(tfd, "# for %s from %g to %g by %c %g \n",
var->name, from, to, op, by);
if (++forp >= forstk+MAXFOR)
ERROR "for loop nested too deep" FATAL;
forp->var = var;
forp->to = to;
forp->op = op;
forp->by = by;
forp->str = str;
setvar(var, from);
nextfor();
unput('\n');
}
void nextfor(void)
{
if (forp->var->fval > SLOP * forp->to) {
free(forp->str);
if (--forp < forstk)
ERROR "forstk popped too far" FATAL;
} else {
pushsrc(String, "\nEndfor\n");
pushsrc(String, forp->str);
}
}
void endfor(void)
{
switch (forp->op) {
case '+':
case ' ':
forp->var->fval += forp->by;
break;
case '-':
forp->var->fval -= forp->by;
break;
case '*':
forp->var->fval *= forp->by;
break;
case '/':
forp->var->fval /= forp->by;
break;
}
nextfor();
}
char *ifstat(double expr, char *thenpart, char *elsepart)
{
dprintf("if %g then <%s> else <%s>\n", expr, thenpart, elsepart? elsepart : "");
if (expr) {
unput('\n');
pushsrc(Free, thenpart);
pushsrc(String, thenpart);
unput('\n');
if (elsepart)
free(elsepart);
return thenpart;
} else {
free(thenpart);
if (elsepart) {
unput('\n');
pushsrc(Free, elsepart);
pushsrc(String, elsepart);
unput('\n');
}
return elsepart;
}
}