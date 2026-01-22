#include <lib9.h>
#include "regexp.h"
extern	void
regsub(char *sp,
char *dp,
Resub *mp,
int ms)
{
char *ssp;
int i;
while(*sp != '\0'){
if(*sp == '\\'){
switch(*++sp){
case '0':
case '1':
case '2':
case '3':
case '4':
case '5':
case '6':
case '7':
case '8':
case '9':
i = *sp-'0';
if(mp[i].s.sp != 0 && mp!=0 && ms>i)
for(ssp = mp[i].s.sp;
ssp < mp[i].e.ep;
ssp++)
*dp++ = *ssp;
break;
case '\\':
*dp++ = '\\';
break;
case '\0':
sp--;
break;
default:
*dp++ = *sp;
break;
}
}else if(*sp == '&'){
if(mp[0].s.sp != 0 && mp!=0 && ms>0)
if(mp[0].s.sp != 0)
for(ssp = mp[0].s.sp;
ssp < mp[0].e.ep; ssp++)
*dp++ = *ssp;
}else
*dp++ = *sp;
sp++;
}
*dp = '\0';
}