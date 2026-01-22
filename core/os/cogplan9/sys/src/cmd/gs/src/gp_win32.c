#include "malloc_.h"
#include "stdio_.h"
#include "string_.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gserror.h"
#include "gserrors.h"
#include "gp.h"
#include "windows_.h"
const char *
gp_strerror(int errnum)
{
return strerror(errnum);
}
void
gp_get_realtime(long *pdt)
{
SYSTEMTIME st;
long idate;
static const int mstart[12] = {
0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334
};
GetSystemTime(&st);
idate = (st.wYear - 1980) * 365 +
((st.wYear - 1) / 4 - 1979 / 4) +
(1979 / 100 - (st.wYear - 1) / 100) +
((st.wYear - 1) / 400 - 1979 / 400) +
mstart[st.wMonth - 1] +
st.wDay - 1;
idate += (2 < st.wMonth
&& (st.wYear % 4 == 0
&& (st.wYear % 100 != 0 || st.wYear % 400 == 0)));
pdt[0] = ((idate * 24 + st.wHour) * 60 + st.wMinute) * 60 + st.wSecond;
pdt[1] = st.wMilliseconds * 1000000;
}
void
gp_get_usertime(long *pdt)
{
gp_get_realtime(pdt);
}
int
gp_file_is_console(FILE * f)
{
#ifdef __DLL__
if (f == NULL)
return 1;
#else
if (f == NULL)
return 0;
#endif
if (fileno(f) <= 2)
return 1;
return 0;
}
const char *
gp_getenv_display(void)
{
return NULL;
}
const char gp_scratch_file_name_prefix[] = "_temp_";
const char gp_null_file_name[] = "nul";
const char gp_current_directory_name[] = ".";