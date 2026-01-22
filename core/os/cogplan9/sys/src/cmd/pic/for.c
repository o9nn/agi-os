#include <stdio.h>
#include <stdlib.h>
#include "pic.h"
#include "y.tab.h"
#define	SLOP	1.001
typedef struct {
char	*var;
double	to;
double	by;
int	op;
char	*str;
} For;
For	forstk[10];
For	*forp = forstk;
void	setfval(char *, double);
void	nextfor(void);
void forloop(char *var, double from, double to, int op,
double by, char *str)
{
dprintf("# for %s from %g to %g by %c %g \n",
var, from, to, op, by);
if (++forp >= forstk+10)
ERROR "for loop nested too deep" FATAL;
forp->var = var;
forp->to = to;
forp->op = op;
forp->by = by;
forp->str = str;
setfval(var, from);
nextfor();
unput('\n');
}
void nextfor(void)
{
if (getfval(forp->var) > SLOP * forp->to) {
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
struct symtab *p = lookup(forp->var);
switch (forp->op) {
case '+':
case ' ':
p->s_val.f += forp->by;
break;
case '-':
p->s_val.f -= forp->by;
break;
case '*':
p->s_val.f *= forp->by;
break;
case '/':
p->s_val.f /= forp->by;
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