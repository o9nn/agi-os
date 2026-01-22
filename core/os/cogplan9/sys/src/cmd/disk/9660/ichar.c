#include <u.h>
#include <libc.h>
#include <bio.h>
#include <libsec.h>
#include <ctype.h>
#include "iso9660.h"
char*
isostring(uchar *buf, int len)
{
char *p, *q;
p = emalloc(len+1);
memmove(p, buf, len);
p[len] = '\0';
while(len > 0 && p[len-1] == ' ')
p[--len] = '\0';
for(q=p; *q; q++)
*q = tolower(*q);
q = atom(p);
free(p);
return q;
}
int
isisofrog(char c)
{
if(c >= '0' && c <= '9')
return 0;
if(c >= 'a' && c <= 'z')
return 0;
if(c == '_')
return 0;
return 1;
}
int
isbadiso9660(char *s)
{
char *p, *q;
int i;
if((p = strchr(s, '.')) != nil) {
if(p-s > 8)
return 1;
for(q=s; q<p; q++)
if(isisofrog(*q))
return 1;
if(strlen(p+1) > 3)
return 1;
for(q=p+1; *q; q++)
if(isisofrog(*q))
return 1;
} else {
if(strlen(s) > 8)
return 1;
for(q=s; *q; q++)
if(isisofrog(*q))
return 1;
if(strlen(s) == 7 && (s[0] == 'D' || s[0] == 'F')) {
for(i=1; i<7; i++)
if(s[i] < '0' || s[i] > '9')
break;
if(i == 7)
return 1;
}
}
return 0;
}
int
isocmp(const void *va, const void *vb)
{
int i;
char s1[32], s2[32], *b1, *b2, *e1, *e2;
const Direc *a, *b;
a = va;
b = vb;
strecpy(s1, s1+sizeof s1, a->confname);
b1 = s1;
strecpy(s2, s2+sizeof s2, b->confname);
b2 = s2;
if((e1 = strchr(b1, '.')) != nil)
*e1++ = '\0';
else
e1 = "";
if((e2 = strchr(b2, '.')) != nil)
*e2++ = '\0';
else
e2 = "";
if((i = strcmp(b1, b2)) != 0)
return i;
return strcmp(e1, e2);
}
static char*
mkisostring(char *isobuf, int n, char *s)
{
char *p, *q, *eq;
eq = isobuf+n;
for(p=s, q=isobuf; *p && q < eq; p++)
if('a' <= *p && *p <= 'z')
*q++ = *p+'A'-'a';
else
*q++ = *p;
while(q < eq)
*q++ = ' ';
return isobuf;
}
void
Cputisopvd(Cdimg *cd, Cdinfo info)
{
char buf[130];
Cputc(cd, 1);
Cputs(cd, "CD001", 5);
Cputc(cd, 1);
Cputc(cd, 0);
assert(~info.flags & (CDplan9|CDrockridge));
strcpy(buf, "");
if(info.flags & CDplan9)
strcat(buf, "plan 9 ");
if(info.flags & CDrockridge)
strcat(buf, "rrip ");
if(info.flags & CDbootable)
strcat(buf, "boot ");
if(info.flags & CDconform)
strcat(buf, "iso9660");
else
strcat(buf, "utf8");
struprcpy(buf, buf);
Cputs(cd, buf, 32);
Cputs(cd, mkisostring(buf, 32, info.volumename), 32);
Crepeat(cd, 0, 8);
Cputn(cd, 0, 4);
Crepeat(cd, 0, 32);
Cputn(cd, 1, 2);
Cputn(cd, 1, 2);
Cputn(cd, Blocksize, 2);
Cputn(cd, 0, 4);
Cputnl(cd, 0, 4);
Cputnl(cd, 0, 4);
Cputnm(cd, 0, 4);
Cputnm(cd, 0, 4);
Cputisodir(cd, nil, DTroot, 1, Cwoffset(cd));
Cputs(cd, mkisostring(buf, 128, info.volumeset), 128);
Cputs(cd, mkisostring(buf, 128, info.publisher), 128);
Cputs(cd, mkisostring(buf, 128, info.preparer), 128);
Cputs(cd, mkisostring(buf, 128, info.application), 128);
Cputs(cd, "", 37);
Cputs(cd, "", 37);
Cputs(cd, "", 37);
Cputdate1(cd, now);
Cputdate1(cd, now);
Cputdate1(cd, 0);
Cputdate1(cd, 0);
Cputc(cd, 1);
Cpadblock(cd);
}