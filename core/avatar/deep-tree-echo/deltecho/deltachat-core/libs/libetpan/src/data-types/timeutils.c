#include "timeutils.h"
#ifndef WRONG
#define WRONG (-1)
#endif
static int tmcomp(struct tm * atmp, struct tm * btmp)
{
register int result;
if ((result = (atmp->tm_year - btmp->tm_year)) == 0 &&
(result = (atmp->tm_mon - btmp->tm_mon)) == 0 &&
(result = (atmp->tm_mday - btmp->tm_mday)) == 0 &&
(result = (atmp->tm_hour - btmp->tm_hour)) == 0 &&
(result = (atmp->tm_min - btmp->tm_min)) == 0)
result = atmp->tm_sec - btmp->tm_sec;
return result;
}
time_t mail_mkgmtime(struct tm * tmp)
{
register int dir;
register int bits;
register int saved_seconds;
time_t t;
struct tm yourtm, *mytm;
yourtm = *tmp;
saved_seconds = yourtm.tm_sec;
yourtm.tm_sec = 0;
for (bits = 0, t = 1; t > 0; ++bits, t <<= 1)
;
t = (t < 0) ? 0 : ((time_t) 1 << bits);
if(bits > 40) bits = 40;
for ( ; ; ) {
mytm = gmtime(&t);
if(!mytm) return WRONG;
dir = tmcomp(mytm, &yourtm);
if (dir != 0) {
if (bits-- < 0)
return WRONG;
if (bits < 0)
--t;
else if (dir > 0)
t -= (time_t) 1 << bits;
else
t += (time_t) 1 << bits;
continue;
}
break;
}
t += saved_seconds;
return t;
}