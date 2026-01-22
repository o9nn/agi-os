#include <stdlib.h>
#include <string.h>
#include "antiword.h"
static char *szMessage =
"Memory allocation failed, unable to continue";
#if defined(__dos) && !defined(__DJGPP__)
static char *szDosMessage =
"DOS can't allocate this kind of memory, unable to continue";
#endif
void *
xmalloc(size_t tSize)
{
void	*pvTmp;
TRACE_MSG("xmalloc");
if (tSize == 0) {
tSize = 1;
}
pvTmp = malloc(tSize);
if (pvTmp == NULL) {
DBG_MSG("xmalloc returned NULL");
DBG_DEC(tSize);
werr(1, szMessage);
}
return pvTmp;
}
void *
xcalloc(size_t tNmemb, size_t tSize)
{
void	*pvTmp;
TRACE_MSG("xcalloc");
#if defined(__dos) && !defined(__DJGPP__)
if ((ULONG)tNmemb * (ULONG)tSize > 0xffffUL) {
DBG_DEC((ULONG)tNmemb * (ULONG)tSize);
werr(1, szDosMessage);
}
#endif
if (tNmemb == 0 || tSize == 0) {
tNmemb = 1;
tSize = 1;
}
pvTmp = calloc(tNmemb, tSize);
if (pvTmp == NULL) {
DBG_MSG("xcalloc returned NULL");
werr(1, szMessage);
}
return pvTmp;
}
void *
xrealloc(void *pvArg, size_t tSize)
{
void	*pvTmp;
TRACE_MSG("xrealloc");
pvTmp = realloc(pvArg, tSize);
if (pvTmp == NULL) {
DBG_MSG("realloc returned NULL");
werr(1, szMessage);
}
return pvTmp;
}
char *
xstrdup(const char *szArg)
{
char	*szTmp;
TRACE_MSG("xstrdup");
szTmp = xmalloc(strlen(szArg) + 1);
strcpy(szTmp, szArg);
return szTmp;
}
void *
xfree(void *pvArg)
{
TRACE_MSG("xfree");
if (pvArg != NULL) {
free(pvArg);
}
return NULL;
}