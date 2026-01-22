#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gscdefs.h"
int
win_registry_key(char *buf, int len)
{
const char *software = "Software";
if (strlen(software) + 1 + strlen(gs_productfamily) >= len)
return -1;
strcpy(buf, software);
strcat(buf, "\\");
strcat(buf, gs_productfamily);
return 0;
}
int
win_get_reg_value(const char *name, char *ptr, int *plen)
{
HKEY hkey;
DWORD cbData, keytype;
BYTE b;
LONG rc;
BYTE *bptr = (BYTE *)ptr;
char key[256];
win_registry_key(key, sizeof(key));
if (RegOpenKeyEx(HKEY_CURRENT_USER, key, 0, KEY_READ, &hkey)
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
int
win_set_reg_value(const char *name, const char *value)
{
HKEY hkey;
LONG rc;
char key[256];
DWORD dwDisposition;
win_registry_key(key, sizeof(key));
rc = RegOpenKeyEx(HKEY_CURRENT_USER, key, 0, KEY_WRITE, &hkey);
if (rc != ERROR_SUCCESS)
rc = RegCreateKeyEx(HKEY_CURRENT_USER, key, 0, "", 0,
KEY_ALL_ACCESS, NULL, &hkey, &dwDisposition);
if (rc == ERROR_SUCCESS) {
rc = RegSetValueEx(hkey, name, 0, REG_SZ,
(CONST BYTE *)value, strlen(value)+1);
RegCloseKey(hkey);
}
return rc == ERROR_SUCCESS ? 0 : -1;
}