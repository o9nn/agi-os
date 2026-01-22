#include <u.h>
#include <libc.h>
int	strcomment = '#';
int
strparse(char *p, int arsize, char **arv)
{
int arc = 0;
while(p){
while(*p == ' ' || *p == '\t')
p++;
if(*p == 0 || *p == strcomment)
break;
if(arc >= arsize-1)
break;
arv[arc++] = p;
while(*p && *p != ' ' && *p != '\t')
p++;
if(*p == 0)
break;
*p++ = 0;
}
arv[arc] = 0;
return arc;
}