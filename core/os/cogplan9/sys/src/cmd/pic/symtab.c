#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "pic.h"
#include "y.tab.h"
YYSTYPE getvar(char *s)
{
struct symtab *p;
static YYSTYPE bug;
p = lookup(s);
if (p == NULL) {
if (islower(s[0]))
ERROR "no such variable as %s", s WARNING;
else
ERROR "no such place as %s", s WARNING;
return(bug);
}
return(p->s_val);
}
double getfval(char *s)
{
YYSTYPE y;
y = getvar(s);
return y.f;
}
void setfval(char *s, double f)
{
struct symtab *p;
if ((p = lookup(s)) != NULL)
p->s_val.f = f;
}
struct symtab *makevar(char *s, int t, YYSTYPE v)
{
struct symtab *p;
for (p = stack[nstack].p_symtab; p != NULL; p = p->s_next)
if (strcmp(s, p->s_name) == 0)
break;
if (p == NULL) {
p = (struct symtab *) malloc(sizeof(struct symtab));
if (p == NULL)
ERROR "out of symtab space with %s", s FATAL;
p->s_next = stack[nstack].p_symtab;
stack[nstack].p_symtab = p;
}
p->s_name = s;
p->s_type = t;
p->s_val = v;
return(p);
}
struct symtab *lookup(char *s)
{
int i;
struct symtab *p;
for (i = nstack; i >= 0; i--)
for (p = stack[i].p_symtab; p != NULL; p = p->s_next)
if (strcmp(s, p->s_name) == 0)
return(p);
return(NULL);
}
void freesymtab(struct symtab *p)
{
struct symtab *q;
for ( ; p != NULL; p = q) {
q = p->s_next;
free(p->s_name);
free((char *)p);
}
}
void freedef(char *s)
{
struct symtab *p, *q, *op;
for (p = op = q = stack[nstack].p_symtab; p != NULL; p = p->s_next) {
if (strcmp(s, p->s_name) == 0) {
if (p->s_type != DEFNAME)
break;
if (p == op)
stack[nstack].p_symtab = p->s_next;
else
q->s_next = p->s_next;
free(p->s_name);
free(p->s_val.p);
free((char *)p);
return;
}
q = p;
}
}