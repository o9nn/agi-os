#include <u.h>
#include <libc.h>
#define SEP(x)	((x)=='/' || (x) == 0)
char*
cleanname(char *name)
{
char *p, *q, *dotdot;
int rooted;
rooted = name[0] == '/';
p = q = dotdot = name+rooted;
while(*p) {
if(p[0] == '/')
p++;
else if(p[0] == '.' && SEP(p[1]))
p += 1;
else if(p[0] == '.' && p[1] == '.' && SEP(p[2])) {
p += 2;
if(q > dotdot) {
while(--q > dotdot && *q != '/')
;
} else if(!rooted) {
if(q != name)
*q++ = '/';
*q++ = '.';
*q++ = '.';
dotdot = q;
}
} else {
if(q != name+rooted)
*q++ = '/';
while((*q = *p) != '/' && *q != 0)
p++, q++;
}
}
if(q == name)
*q++ = '.';
*q = '\0';
return name;
}