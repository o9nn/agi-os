#include "windows_.h"
#include "iapi.h"
#include "gp_mswin.h"
GSDLLEXPORT BOOL WINAPI
DllEntryPoint(HINSTANCE hInst, DWORD fdwReason, LPVOID lpReserved)
{
DWORD version = GetVersion();
if (((HIWORD(version) & 0x8000) != 0) && ((HIWORD(version) & 0x4000) == 0))
is_win32s = TRUE;
phInstance = hInst;
return TRUE;
}
GSDLLEXPORT BOOL WINAPI
DllMain(HINSTANCE hInst, DWORD fdwReason, LPVOID lpReserved)
{
return DllEntryPoint(hInst, fdwReason, lpReserved);
}