#include <stdlib.h>
#include <errno.h>
#include "antiword.h"
#if defined(__riscos)
#define EIO 42
#endif
typedef struct data_mem_tag {
data_block_type tInfo;
struct data_mem_tag *pNext;
} data_mem_type;
static data_mem_type *pAnchor = NULL;
static data_mem_type *pBlockLast = NULL;
static data_mem_type *pBlockCurrent = NULL;
static ULONG ulBlockOffset = 0;
static size_t tByteNext = 0;
static UCHAR aucBlock[BIG_BLOCK_SIZE];
void
vDestroyDataBlockList(void)
{
data_mem_type *pCurr, *pNext;
DBG_MSG("vDestroyDataBlockList");
pCurr = pAnchor;
while (pCurr != NULL) {
pNext = pCurr->pNext;
pCurr = xfree(pCurr);
pCurr = pNext;
}
pAnchor = NULL;
pBlockLast = NULL;
pBlockCurrent = NULL;
ulBlockOffset = 0;
tByteNext = 0;
}
BOOL
bAdd2DataBlockList(const data_block_type *pDataBlock)
{
data_mem_type *pListMember;
fail(pDataBlock == NULL);
fail(pDataBlock->ulFileOffset == FC_INVALID);
fail(pDataBlock->ulDataPos == CP_INVALID);
fail(pDataBlock->ulLength == 0);
NO_DBG_MSG("bAdd2DataBlockList");
NO_DBG_HEX(pDataBlock->ulFileOffset);
NO_DBG_HEX(pDataBlock->ulDataPos);
NO_DBG_HEX(pDataBlock->ulLength);
if (pDataBlock->ulFileOffset == FC_INVALID ||
pDataBlock->ulDataPos == CP_INVALID ||
pDataBlock->ulLength == 0) {
werr(0, "Software (datablock) error");
return FALSE;
}
if (pBlockLast != NULL &&
pBlockLast->tInfo.ulFileOffset +
pBlockLast->tInfo.ulLength == pDataBlock->ulFileOffset &&
pBlockLast->tInfo.ulDataPos +
pBlockLast->tInfo.ulLength == pDataBlock->ulDataPos) {
pBlockLast->tInfo.ulLength += pDataBlock->ulLength;
return TRUE;
}
pListMember = xmalloc(sizeof(data_mem_type));
pListMember->tInfo = *pDataBlock;
pListMember->pNext = NULL;
if (pAnchor == NULL) {
pAnchor = pListMember;
} else {
fail(pBlockLast == NULL);
pBlockLast->pNext = pListMember;
}
pBlockLast = pListMember;
return TRUE;
}
ULONG
ulGetDataOffset(FILE *pFile)
{
return pBlockCurrent->tInfo.ulFileOffset + ulBlockOffset + tByteNext;
}
BOOL
bSetDataOffset(FILE *pFile, ULONG ulFileOffset)
{
data_mem_type *pCurr;
size_t tReadLen;
DBG_HEX(ulFileOffset);
for (pCurr = pAnchor; pCurr != NULL; pCurr = pCurr->pNext) {
if (ulFileOffset < pCurr->tInfo.ulFileOffset ||
ulFileOffset >= pCurr->tInfo.ulFileOffset +
pCurr->tInfo.ulLength) {
continue;
}
tReadLen = (size_t)(pCurr->tInfo.ulFileOffset +
pCurr->tInfo.ulLength -
ulFileOffset);
if (tReadLen > sizeof(aucBlock)) {
tReadLen = sizeof(aucBlock);
}
if (!bReadBytes(aucBlock, tReadLen, ulFileOffset, pFile)) {
return FALSE;
}
pBlockCurrent = pCurr;
ulBlockOffset = ulFileOffset - pCurr->tInfo.ulFileOffset;
tByteNext = 0;
return TRUE;
}
return FALSE;
}
int
iNextByte(FILE *pFile)
{
ULONG ulReadOff;
size_t tReadLen;
fail(pBlockCurrent == NULL);
if (tByteNext >= sizeof(aucBlock) ||
ulBlockOffset + tByteNext >= pBlockCurrent->tInfo.ulLength) {
if (ulBlockOffset + sizeof(aucBlock) <
pBlockCurrent->tInfo.ulLength) {
ulBlockOffset += sizeof(aucBlock);
} else {
pBlockCurrent = pBlockCurrent->pNext;
ulBlockOffset = 0;
}
if (pBlockCurrent == NULL) {
errno = EIO;
return EOF;
}
tReadLen = (size_t)
(pBlockCurrent->tInfo.ulLength - ulBlockOffset);
if (tReadLen > sizeof(aucBlock)) {
tReadLen = sizeof(aucBlock);
}
ulReadOff = pBlockCurrent->tInfo.ulFileOffset + ulBlockOffset;
if (!bReadBytes(aucBlock, tReadLen, ulReadOff, pFile)) {
errno = EIO;
return EOF;
}
tByteNext = 0;
}
return (int)aucBlock[tByteNext++];
}
USHORT
usNextWord(FILE *pFile)
{
USHORT usLSB, usMSB;
usLSB = (USHORT)iNextByte(pFile);
if (usLSB == (USHORT)EOF) {
errno = EIO;
return (USHORT)EOF;
}
usMSB = (USHORT)iNextByte(pFile);
if (usMSB == (USHORT)EOF) {
DBG_MSG("usNextWord: Unexpected EOF");
errno = EIO;
return (USHORT)EOF;
}
return (usMSB << 8) | usLSB;
}
ULONG
ulNextLong(FILE *pFile)
{
ULONG ulLSW, ulMSW;
ulLSW = (ULONG)usNextWord(pFile);
if (ulLSW == (ULONG)EOF) {
errno = EIO;
return (ULONG)EOF;
}
ulMSW = (ULONG)usNextWord(pFile);
if (ulMSW == (ULONG)EOF) {
DBG_MSG("ulNextLong: Unexpected EOF");
errno = EIO;
return (ULONG)EOF;
}
return (ulMSW << 16) | ulLSW;
}
USHORT
usNextWordBE(FILE *pFile)
{
USHORT usLSB, usMSB;
usMSB = (USHORT)iNextByte(pFile);
if (usMSB == (USHORT)EOF) {
errno = EIO;
return (USHORT)EOF;
}
usLSB = (USHORT)iNextByte(pFile);
if (usLSB == (USHORT)EOF) {
DBG_MSG("usNextWordBE: Unexpected EOF");
errno = EIO;
return (USHORT)EOF;
}
return (usMSB << 8) | usLSB;
}
ULONG
ulNextLongBE(FILE *pFile)
{
ULONG ulLSW, ulMSW;
ulMSW = (ULONG)usNextWordBE(pFile);
if (ulMSW == (ULONG)EOF) {
errno = EIO;
return (ULONG)EOF;
}
ulLSW = (ULONG)usNextWordBE(pFile);
if (ulLSW == (ULONG)EOF) {
DBG_MSG("ulNextLongBE: Unexpected EOF");
errno = EIO;
return (ULONG)EOF;
}
return (ulMSW << 16) | ulLSW;
}
size_t
tSkipBytes(FILE *pFile, size_t tToSkip)
{
size_t tToGo, tMaxMove, tMove;
fail(pFile == NULL);
fail(pBlockCurrent == NULL);
tToGo = tToSkip;
while (tToGo != 0) {
tMaxMove = min(sizeof(aucBlock) - tByteNext,
(size_t)(pBlockCurrent->tInfo.ulLength -
ulBlockOffset - tByteNext));
tMove = min(tMaxMove, tToGo);
tByteNext += tMove;
tToGo -= tMove;
if (tToGo != 0) {
if (iNextByte(pFile) == EOF) {
return tToSkip - tToGo;
}
tToGo--;
}
}
return tToSkip;
}
ULONG
ulDataPos2FileOffset(ULONG ulDataPos)
{
data_mem_type *pCurr;
fail(ulDataPos == CP_INVALID);
for (pCurr = pAnchor; pCurr != NULL; pCurr = pCurr->pNext) {
if (ulDataPos < pCurr->tInfo.ulDataPos ||
ulDataPos >= pCurr->tInfo.ulDataPos +
pCurr->tInfo.ulLength) {
continue;
}
return pCurr->tInfo.ulFileOffset +
ulDataPos -
pCurr->tInfo.ulDataPos;
}
DBG_HEX_C(ulDataPos != 0, ulDataPos);
return FC_INVALID;
}