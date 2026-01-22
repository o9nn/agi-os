#include "stdio_.h"
#include "string_.h"
#include "dos_.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gp.h"
const char *
gp_strerror(int errnum)
{
return strerror(errnum);
}
void
gp_get_realtime(long *pdt)
{
union REGS osdate, ostime;
long idate;
static const int mstart[12] =
{0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334};
osdate.h.ah = 0x2a;
intdos(&osdate, &osdate);
#define da_year rshort.cx
#define da_mon h.dh
#define da_day h.dl
ostime.h.ah = 0x2c;
intdos(&ostime, &ostime);
#define ti_hour h.ch
#define ti_min h.cl
#define ti_sec h.dh
#define ti_hund h.dl
idate = (long)osdate.da_year * 365 +
(
((osdate.da_year + 1979) / 4 - 1979 / 4) +
(1979 / 100 - (osdate.da_year + 1979) / 100) +
((osdate.da_year + 1979) / 400 - 1979 / 400) +
mstart[osdate.da_mon - 1] +
osdate.da_day - 1);
idate += (2 < osdate.da_mon
&& (osdate.da_year % 4 == 0
&& ((osdate.da_year + 1980) % 100 != 0
|| (osdate.da_year + 1980) % 400 == 0)));
pdt[0] =
((idate * 24 + ostime.ti_hour) * 60 + ostime.ti_min) * 60 +
ostime.ti_sec;
pdt[1] = ostime.ti_hund * 10000000;
}
void
gp_get_usertime(long *pdt)
{
gp_get_realtime(pdt);
}
int
gp_file_is_console(FILE * f)
{
union REGS regs;
#ifdef __DLL__
if (f == NULL)
return 1;
#else
if (f == NULL)
return 0;
#endif
regs.h.ah = 0x44;
regs.h.al = 0;
regs.rshort.bx = fileno(f);
intdos(&regs, &regs);
return ((regs.h.dl & 0x80) != 0 && (regs.h.dl & 3) != 0);
}
const char *
gp_getenv_display(void)
{
return NULL;
}
const char gp_scratch_file_name_prefix[] = "_temp_";
const char gp_null_file_name[] = "nul";
const char gp_current_directory_name[] = ".";