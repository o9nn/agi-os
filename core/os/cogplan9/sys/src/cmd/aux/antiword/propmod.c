#include <stdio.h>
#include <string.h>
#include "antiword.h"
#if defined(DEBUG)
#define ELEMENTS_TO_ADD 3
#else
#define ELEMENTS_TO_ADD 30
#endif
static UCHAR **ppAnchor = NULL;
static size_t tNextFree = 0;
static size_t tMaxElements = 0;
void
vDestroyPropModList(void)
{
size_t tIndex;
DBG_MSG("vDestroyPropModList");
for (tIndex = 0; tIndex < tNextFree; tIndex++) {
ppAnchor[tIndex] = xfree(ppAnchor[tIndex]);
}
ppAnchor = xfree(ppAnchor);
tNextFree = 0;
tMaxElements = 0;
}
void
vAdd2PropModList(const UCHAR *aucPropMod)
{
size_t tSize, tLen;
fail(aucPropMod == NULL);
NO_DBG_MSG("vAdd2PropModList");
if (tNextFree >= tMaxElements) {
tMaxElements += ELEMENTS_TO_ADD;
tSize = tMaxElements * sizeof(UCHAR **);
ppAnchor = xrealloc(ppAnchor, tSize);
}
NO_DBG_DEC(tNextFree);
tLen = 2 + (size_t)usGetWord(0, aucPropMod);
NO_DBG_HEX(tLen);
NO_DBG_PRINT_BLOCK(pucPropMod, tLen);
ppAnchor[tNextFree] = xmalloc(tLen);
memcpy(ppAnchor[tNextFree], aucPropMod, tLen);
tNextFree++;
}
const UCHAR *
aucReadPropModListItem(USHORT usPropMod)
{
static UCHAR aucBuffer[4];
size_t tIndex;
if (usPropMod == IGNORE_PROPMOD) {
return NULL;
}
if (!odd(usPropMod)) {
aucBuffer[0] = 2;
aucBuffer[1] = 0;
aucBuffer[2] = (UCHAR)((usPropMod & 0x00fe) >> 1);
aucBuffer[3] = (UCHAR)((usPropMod & 0xff00) >> 8);
return aucBuffer;
}
if (ppAnchor == NULL) {
return NULL;
}
tIndex = (size_t)(usPropMod >> 1);
if (tIndex >= tNextFree) {
DBG_HEX(usPropMod);
DBG_DEC(tIndex);
DBG_DEC(tNextFree);
return NULL;
}
return ppAnchor[tIndex];
}