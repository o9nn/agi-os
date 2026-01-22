#include "antiword.h"
#define SIZE_RATIO	(BIG_BLOCK_SIZE/SMALL_BLOCK_SIZE)
static ULONG	*aulSmallBlockList = NULL;
static size_t	tSmallBlockListLen = 0;
void
vDestroySmallBlockList(void)
{
DBG_MSG("vDestroySmallBlockList");
aulSmallBlockList = xfree(aulSmallBlockList);
tSmallBlockListLen = 0;
}
BOOL
bCreateSmallBlockList(ULONG ulStartblock, const ULONG *aulBBD, size_t tBBDLen)
{
ULONG	ulTmp;
size_t	tSize;
int	iIndex;
fail(aulSmallBlockList != NULL);
fail(tSmallBlockListLen != 0);
fail(ulStartblock > MAX_BLOCKNUMBER && ulStartblock != END_OF_CHAIN);
fail(aulBBD == NULL);
fail(tBBDLen == 0);
for (tSmallBlockListLen = 0, ulTmp = ulStartblock;
tSmallBlockListLen < tBBDLen && ulTmp != END_OF_CHAIN;
tSmallBlockListLen++, ulTmp = aulBBD[ulTmp]) {
if (ulTmp >= (ULONG)tBBDLen) {
DBG_DEC(ulTmp);
DBG_DEC(tBBDLen);
werr(1, "The Big Block Depot is damaged");
}
}
DBG_DEC(tSmallBlockListLen);
if (tSmallBlockListLen == 0) {
fail(ulStartblock != END_OF_CHAIN);
aulSmallBlockList = NULL;
return TRUE;
}
tSize = tSmallBlockListLen * sizeof(ULONG);
aulSmallBlockList = xmalloc(tSize);
for (iIndex = 0, ulTmp = ulStartblock;
iIndex < (int)tBBDLen && ulTmp != END_OF_CHAIN;
iIndex++, ulTmp = aulBBD[ulTmp]) {
if (ulTmp >= (ULONG)tBBDLen) {
DBG_DEC(ulTmp);
DBG_DEC(tBBDLen);
werr(1, "The Big Block Depot is damaged");
}
aulSmallBlockList[iIndex] = ulTmp;
NO_DBG_DEC(aulSmallBlockList[iIndex]);
}
return TRUE;
}
ULONG
ulDepotOffset(ULONG ulIndex, size_t tBlockSize)
{
ULONG	ulTmp;
size_t	tTmp;
fail(ulIndex >= ULONG_MAX / BIG_BLOCK_SIZE);
switch (tBlockSize) {
case BIG_BLOCK_SIZE:
return (ulIndex + 1) * BIG_BLOCK_SIZE;
case SMALL_BLOCK_SIZE:
tTmp = (size_t)(ulIndex / SIZE_RATIO);
ulTmp = ulIndex % SIZE_RATIO;
if (aulSmallBlockList == NULL ||
tTmp >= tSmallBlockListLen) {
DBG_HEX(aulSmallBlockList);
DBG_DEC(tSmallBlockListLen);
DBG_DEC(tTmp);
return 0;
}
return ((aulSmallBlockList[tTmp] + 1) * SIZE_RATIO +
ulTmp) * SMALL_BLOCK_SIZE;
default:
DBG_DEC(tBlockSize);
DBG_FIXME();
return 0;
}
}