#include "stdio_.h"
#include "string_.h"
#include "gsmemory.h"
#include "gstypes.h"
#include "gp.h"
extern char *getenv(const char *);
int
gp_getenv(const char *key, char *ptr, int *plen)
{
const char *str = getenv(key);
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
if (*plen > 0)
*ptr = 0;
*plen = 1;
return 1;
}