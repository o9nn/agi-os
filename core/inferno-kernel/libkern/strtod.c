#include <lib9.h>
static int
strtodf(void *vp)
{
return *(*((char**)vp))++;
}
double
strtod(char *s, char **end)
{
double d;
char *ss;
int c;
ss = s;
d = charstod(strtodf, &s);
if(end){
*end = --s;
while(s > ss){
c = *--s;
if(c!='-' && c!='+' && c!='e' && c!='E')
break;
(*end)--;
}
}
return d;
}