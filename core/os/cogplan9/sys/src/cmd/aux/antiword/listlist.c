#include "antiword.h"
typedef struct list_desc_tag {
list_block_type tInfo;
ULONG ulListID;
USHORT usIstd;
UCHAR ucListLevel;
struct list_desc_tag *pNext;
} list_desc_type;
typedef struct list_value_tag {
USHORT usValue;
USHORT usListIndex;
UCHAR ucListLevel;
struct list_value_tag *pNext;
} list_value_type;
static ULONG *aulLfoList = NULL;
static USHORT usLfoLen = 0;
static list_desc_type *pAnchor = NULL;
static list_desc_type *pBlockLast = NULL;
static list_value_type *pValues = NULL;
static int iOldListSeqNumber = 0;
static USHORT usOldListValue = 0;
void
vDestroyListInfoList(void)
{
list_desc_type *pCurr, *pNext;
list_value_type *pValueCurr, *pValueNext;
DBG_MSG("vDestroyListInfoList");
usLfoLen = 0;
aulLfoList = xfree(aulLfoList);
pCurr = pAnchor;
while (pCurr != NULL) {
pNext = pCurr->pNext;
pCurr = xfree(pCurr);
pCurr = pNext;
}
pAnchor = NULL;
pBlockLast = NULL;
pValueCurr = pValues;
while (pValueCurr != NULL) {
pValueNext = pValueCurr->pNext;
pValueCurr = xfree(pValueCurr);
pValueCurr = pValueNext;
}
pValues = NULL;
iOldListSeqNumber = 0;
usOldListValue = 0;
}
void
vBuildLfoList(const UCHAR *aucBuffer, size_t tBufLen)
{
size_t tRecords;
int iIndex;
fail(aucBuffer == NULL);
if (tBufLen < 4) {
return;
}
tRecords = (size_t)ulGetLong(0, aucBuffer);
NO_DBG_DEC(tRecords);
if (4 + 16 * tRecords > tBufLen || tRecords >= 0x7fff) {
DBG_DEC(tRecords);
DBG_DEC(4 + 16 * tRecords);
DBG_DEC(tBufLen);
return;
}
aulLfoList = xcalloc(tRecords, sizeof(ULONG));
for (iIndex = 0; iIndex < (int)tRecords; iIndex++) {
aulLfoList[iIndex] = ulGetLong(4 + 16 * iIndex, aucBuffer);
NO_DBG_HEX(aulLfoList[iIndex]);
}
usLfoLen = (USHORT)tRecords;
}
void
vAdd2ListInfoList(ULONG ulListID, USHORT usIstd, UCHAR ucListLevel,
const list_block_type *pListBlock)
{
list_desc_type *pListMember;
fail(pListBlock == NULL);
NO_DBG_HEX(ulListID);
NO_DBG_DEC(usIstd);
NO_DBG_DEC(ucListLevel);
NO_DBG_DEC(pListBlock->ulStartAt);
NO_DBG_DEC(pListBlock->bNoRestart);
NO_DBG_DEC(pListBlock->sLeftIndent);
NO_DBG_HEX(pListBlock->ucNFC);
NO_DBG_HEX(pListBlock->usListChar);
pListMember = xmalloc(sizeof(list_desc_type));
pListMember->tInfo = *pListBlock;
pListMember->ulListID = ulListID;
pListMember->usIstd = usIstd;
pListMember->ucListLevel = ucListLevel;
pListMember->pNext = NULL;
if (pListMember->tInfo.ulStartAt > 0xffff) {
DBG_DEC(pListMember->tInfo.ulStartAt);
pListMember->tInfo.ulStartAt = 1;
}
if (pAnchor == NULL) {
pAnchor = pListMember;
} else {
fail(pBlockLast == NULL);
pBlockLast->pNext = pListMember;
}
pBlockLast = pListMember;
}
const list_block_type *
pGetListInfo(USHORT usListIndex, UCHAR ucListLevel)
{
list_desc_type *pCurr;
list_block_type *pNearMatch;
ULONG ulListID;
if (usListIndex == 0) {
return NULL;
}
if (usListIndex - 1 >= usLfoLen || ucListLevel > 8) {
DBG_DEC(usListIndex);
DBG_DEC(ucListLevel);
return NULL;
}
fail(aulLfoList == NULL);
ulListID = aulLfoList[usListIndex - 1];
NO_DBG_HEX(ulListID);
pNearMatch = NULL;
for (pCurr = pAnchor; pCurr != NULL; pCurr = pCurr->pNext) {
if (pCurr->ulListID != ulListID) {
continue;
}
if (pCurr->ucListLevel == ucListLevel) {
return &pCurr->tInfo;
}
if (pCurr->ucListLevel == 0) {
pNearMatch = &pCurr->tInfo;
}
}
return pNearMatch;
}
const list_block_type *
pGetListInfoByIstd(USHORT usIstd)
{
list_desc_type *pCurr;
if (usIstd == ISTD_INVALID || usIstd == STI_NIL || usIstd == STI_USER) {
return NULL;
}
for (pCurr = pAnchor; pCurr != NULL; pCurr = pCurr->pNext) {
if (pCurr->usIstd == usIstd) {
return &pCurr->tInfo;
}
}
return NULL;
}
static void
vRestartListValues(USHORT usListIndex, UCHAR ucListLevel)
{
list_value_type *pPrev, *pCurr, *pNext;
int iCounter;
iCounter = 0;
pPrev = NULL;
pCurr = pValues;
while (pCurr != NULL) {
if (pCurr->usListIndex != usListIndex ||
pCurr->ucListLevel <= ucListLevel) {
pPrev = pCurr;
pCurr = pCurr->pNext;
continue;
}
pNext = pCurr->pNext;
if (pPrev == NULL) {
pValues = pNext;
} else {
pPrev->pNext = pNext;
}
DBG_DEC(pCurr->usListIndex);
DBG_DEC(pCurr->ucListLevel);
pCurr = xfree(pCurr);
pCurr = pNext;
iCounter++;
}
DBG_DEC_C(iCounter > 0, iCounter);
}
USHORT
usGetListValue(int iListSeqNumber, int iWordVersion,
const style_block_type *pStyle)
{
list_value_type *pCurr;
USHORT usValue;
fail(iListSeqNumber < 0);
fail(iListSeqNumber < iOldListSeqNumber);
fail(iWordVersion < 0);
fail(pStyle == NULL);
if (iListSeqNumber <= 0) {
return 0;
}
if (iWordVersion < 8) {
if (iListSeqNumber == iOldListSeqNumber ||
(iListSeqNumber == iOldListSeqNumber + 1 &&
eGetNumType(pStyle->ucNumLevel) == level_type_sequence)) {
if (!pStyle->bNumPause) {
usOldListValue++;
}
} else {
usOldListValue = pStyle->usStartAt;
}
iOldListSeqNumber = iListSeqNumber;
return usOldListValue;
}
if (pStyle->usListIndex == 0 ||
pStyle->usListIndex - 1 >= usLfoLen ||
pStyle->ucListLevel > 8) {
return 0;
}
for (pCurr = pValues; pCurr != NULL; pCurr = pCurr->pNext) {
if (pCurr->usListIndex == pStyle->usListIndex &&
pCurr->ucListLevel == pStyle->ucListLevel) {
pCurr->usValue++;
usValue = pCurr->usValue;
if (!pStyle->bNoRestart) {
vRestartListValues(pStyle->usListIndex,
pStyle->ucListLevel);
}
return usValue;
}
}
pCurr = xmalloc(sizeof(list_value_type));
pCurr->usValue = pStyle->usStartAt;
pCurr->usListIndex = pStyle->usListIndex;
pCurr->ucListLevel = pStyle->ucListLevel;
pCurr->pNext = pValues;
pValues = pCurr;
usValue = pCurr->usValue;
if (!pStyle->bNoRestart) {
vRestartListValues(pStyle->usListIndex, pStyle->ucListLevel);
}
return usValue;
}