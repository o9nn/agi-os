#include <stdlib.h>
#include <string.h>
#include "antiword.h"
typedef struct row_desc_tag {
row_block_type tInfo;
struct row_desc_tag *pNext;
} row_desc_type;
static row_desc_type *pAnchor = NULL;
static row_desc_type *pRowLast = NULL;
static row_desc_type *pRowCurrent = NULL;
void
vDestroyRowInfoList(void)
{
row_desc_type *pCurr, *pNext;
DBG_MSG("vDestroyRowInfoList");
pCurr = pAnchor;
while (pCurr != NULL) {
pNext = pCurr->pNext;
pCurr = xfree(pCurr);
pCurr = pNext;
}
pAnchor = NULL;
pRowLast = NULL;
pRowCurrent = NULL;
}
void
vAdd2RowInfoList(const row_block_type *pRowBlock)
{
row_desc_type *pListMember;
short *psTmp;
int iIndex;
fail(pRowBlock == NULL);
if (pRowBlock->ulFileOffsetStart == FC_INVALID ||
pRowBlock->ulFileOffsetEnd == FC_INVALID ||
pRowBlock->ulFileOffsetStart == pRowBlock->ulFileOffsetEnd) {
DBG_HEX_C(pRowBlock->ulFileOffsetStart != FC_INVALID,
pRowBlock->ulFileOffsetStart);
DBG_HEX_C(pRowBlock->ulFileOffsetEnd != FC_INVALID,
pRowBlock->ulFileOffsetEnd);
return;
}
NO_DBG_HEX(pRowBlock->ulFileOffsetStart);
NO_DBG_HEX(pRowBlock->ulFileOffsetEnd);
NO_DBG_DEC(pRowBlock->ucNumberOfColumns);
pListMember = xmalloc(sizeof(row_desc_type));
pListMember->tInfo = *pRowBlock;
pListMember->pNext = NULL;
for (iIndex = 0, psTmp = pListMember->tInfo.asColumnWidth;
iIndex < (int)pListMember->tInfo.ucNumberOfColumns;
iIndex++, psTmp++) {
if (*psTmp < 0) {
*psTmp = 0;
DBG_MSG("The column width was negative");
}
}
if (pAnchor == NULL) {
pAnchor = pListMember;
pRowCurrent = pListMember;
} else {
fail(pRowLast == NULL);
pRowLast->pNext = pListMember;
}
pRowLast = pListMember;
}
const row_block_type *
pGetNextRowInfoListItem(void)
{
const row_block_type *pItem;
if (pRowCurrent == NULL) {
return NULL;
}
pItem = &pRowCurrent->tInfo;
pRowCurrent = pRowCurrent->pNext;
return pItem;
}