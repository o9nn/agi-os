#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gscdefs.h"
int gp_getenv_registry(HKEY hkeyroot, const char *key, const char *name,
char *ptr, int *plen);
int
gp_getenv(const char *name, char *ptr, int *plen)
{
const char *str = getenv(name);
if (str) {
int len = strlen(str);
if (len < *plen) {
strcpy(ptr, str);
*plen = len + 1;
return 0;
}
*plen = len + 1;
return -1;
}
#ifdef __WIN32__
{
DWORD version = GetVersion();
if (!(((HIWORD(version) & 0x8000) != 0)
&& ((HIWORD(version) & 0x4000) == 0))) {
int code;
char key[256];
char dotversion[16];
sprintf(dotversion, "%d.%02d", (int)(gs_revision / 100),
(int)(gs_revision % 100));
sprintf(key, "Software\\%s\\%s", gs_productfamily, dotversion);
code = gp_getenv_registry(HKEY_CURRENT_USER, key, name, ptr, plen);
if ( code <= 0 )
return code;
code = gp_getenv_registry(HKEY_LOCAL_MACHINE, key, name, ptr, plen);
if ( code <= 0 )
return code;
}
}
#endif
if (*plen > 0)
*ptr = 0;
*plen = 1;
return 1;
}
int
gp_getenv_registry(HKEY hkeyroot, const char *key, const char *name,
char *ptr, int *plen)
{
HKEY hkey;
DWORD cbData, keytype;
BYTE b;
LONG rc;
BYTE *bptr = (BYTE *)ptr;
if (RegOpenKeyEx(hkeyroot, key, 0, KEY_READ, &hkey)
== ERROR_SUCCESS) {
keytype = REG_SZ;
cbData = *plen;
if (bptr == (char *)NULL)
bptr = &b;
rc = RegQueryValueEx(hkey, (char *)name, 0, &keytype, bptr, &cbData);
RegCloseKey(hkey);
if (rc == ERROR_SUCCESS) {
*plen = cbData;
return 0;
} else if (rc == ERROR_MORE_DATA) {
*plen = cbData;
return -1;
}
}
return 1;
}