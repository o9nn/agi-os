#include "cryptlib.h"
#include <openssl/rand.h>
#include "rand_lcl.h"
#ifdef OPENSSL_SYS_OS2
# define INCL_DOSPROCESS
# define INCL_DOSPROFILE
# define INCL_DOSMISC
# define INCL_DOSMODULEMGR
# include <os2.h>
# define CMD_KI_RDCNT (0x63)
typedef struct _CPUUTIL {
ULONG ulTimeLow;
ULONG ulTimeHigh;
ULONG ulIdleLow;
ULONG ulIdleHigh;
ULONG ulBusyLow;
ULONG ulBusyHigh;
ULONG ulIntrLow;
ULONG ulIntrHigh;
} CPUUTIL;
# ifndef __KLIBC__
APIRET APIENTRY(*DosPerfSysCall) (ULONG ulCommand, ULONG ulParm1,
ULONG ulParm2, ULONG ulParm3) = NULL;
APIRET APIENTRY(*DosQuerySysState) (ULONG func, ULONG arg1, ULONG pid,
ULONG _res_, PVOID buf, ULONG bufsz) =
NULL;
# endif
HMODULE hDoscalls = 0;
int RAND_poll(void)
{
char failed_module[20];
QWORD qwTime;
ULONG SysVars[QSV_FOREGROUND_PROCESS];
if (hDoscalls == 0) {
ULONG rc =
DosLoadModule(failed_module, sizeof(failed_module), "DOSCALLS",
&hDoscalls);
# ifndef __KLIBC__
if (rc == 0) {
rc = DosQueryProcAddr(hDoscalls, 976, NULL,
(PFN *) & DosPerfSysCall);
if (rc)
DosPerfSysCall = NULL;
rc = DosQueryProcAddr(hDoscalls, 368, NULL,
(PFN *) & DosQuerySysState);
if (rc)
DosQuerySysState = NULL;
}
# endif
}
DosTmrQueryTime(&qwTime);
RAND_add(&qwTime, sizeof(qwTime), 2);
DosQuerySysInfo(1, QSV_FOREGROUND_PROCESS, SysVars, sizeof(SysVars));
RAND_add(SysVars, sizeof(SysVars), 4);
if (DosPerfSysCall) {
CPUUTIL util;
if (DosPerfSysCall(CMD_KI_RDCNT, (ULONG) & util, 0, 0) == 0) {
RAND_add(&util, sizeof(util), 10);
} else {
# ifndef __KLIBC__
DosPerfSysCall = NULL;
# endif
}
}
if (DosQuerySysState) {
char *buffer = OPENSSL_malloc(256 * 1024);
if (!buffer)
return 0;
if (DosQuerySysState(0x1F, 0, 0, 0, buffer, 256 * 1024) == 0) {
RAND_add(buffer, 256 * 1024, **(ULONG **) buffer);
}
OPENSSL_free(buffer);
return 1;
}
return 0;
}
#endif