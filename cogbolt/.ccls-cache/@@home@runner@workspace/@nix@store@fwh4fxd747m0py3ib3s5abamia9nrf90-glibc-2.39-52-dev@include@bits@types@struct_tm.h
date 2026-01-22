#ifndef __struct_tm_defined
#define __struct_tm_defined 1
#include <bits/types.h>
struct tm
{
int tm_sec;
int tm_min;
int tm_hour;
int tm_mday;
int tm_mon;
int tm_year;
int tm_wday;
int tm_yday;
int tm_isdst;
# ifdef	__USE_MISC
long int tm_gmtoff;
const char *tm_zone;
# else
long int __tm_gmtoff;
const char *__tm_zone;
# endif
};
#endif