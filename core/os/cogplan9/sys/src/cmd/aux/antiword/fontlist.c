#include <stdlib.h>
#include <stddef.h>
#include "antiword.h"
typedef struct font_desc_tag {
font_block_type tInfo;
struct font_desc_tag *pNext;
} font_mem_type;
static font_mem_type *pAnchor = NULL;
static font_mem_type *pFontLast = NULL;
void
vDestroyFontInfoList(void)
{
font_mem_type *pCurr, *pNext;
DBG_MSG("vDestroyFontInfoList");
pCurr = pAnchor;
while (pCurr != NULL) {
pNext = pCurr->pNext;
pCurr = xfree(pCurr);
pCurr = pNext;
}
pAnchor = NULL;
pFontLast = NULL;
}
void
vCorrectFontValues(font_block_type *pFontBlock)
{
UINT uiRealSize;
USHORT usRealStyle;
uiRealSize = pFontBlock->usFontSize;
usRealStyle = pFontBlock->usFontStyle;
if (bIsSmallCapitals(pFontBlock->usFontStyle)) {
uiRealSize = (uiRealSize * 4 + 2) / 5;
usRealStyle &= ~FONT_SMALL_CAPITALS;
usRealStyle |= FONT_CAPITALS;
}
if (bIsSuperscript(pFontBlock->usFontStyle) ||
bIsSubscript(pFontBlock->usFontStyle)) {
uiRealSize = (uiRealSize * 2 + 1) / 3;
}
if (uiRealSize < MIN_FONT_SIZE) {
DBG_DEC(uiRealSize);
uiRealSize = MIN_FONT_SIZE;
} else if (uiRealSize > MAX_FONT_SIZE) {
DBG_DEC(uiRealSize);
uiRealSize = MAX_FONT_SIZE;
}
pFontBlock->usFontSize = (USHORT)uiRealSize;
if (pFontBlock->ucFontColor == 8) {
pFontBlock->ucFontColor = 16;
}
pFontBlock->usFontStyle = usRealStyle;
}
void
vAdd2FontInfoList(const font_block_type *pFontBlock)
{
font_mem_type *pListMember;
fail(pFontBlock == NULL);
NO_DBG_MSG("bAdd2FontInfoList");
if (pFontBlock->ulFileOffset == FC_INVALID) {
return;
}
NO_DBG_HEX(pFontBlock->ulFileOffset);
NO_DBG_DEC_C(pFontBlock->ucFontNumber != 0,
pFontBlock->ucFontNumber);
NO_DBG_DEC_C(pFontBlock->usFontSize != DEFAULT_FONT_SIZE,
pFontBlock->usFontSize);
NO_DBG_DEC_C(pFontBlock->ucFontColor != 0,
pFontBlock->ucFontColor);
NO_DBG_HEX_C(pFontBlock->usFontStyle != 0x00,
pFontBlock->usFontStyle);
if (pFontLast != NULL &&
pFontLast->tInfo.ulFileOffset == pFontBlock->ulFileOffset) {
fail(pFontLast->pNext != NULL);
pFontLast->tInfo = *pFontBlock;
return;
}
pListMember = xmalloc(sizeof(font_mem_type));
pListMember->tInfo = *pFontBlock;
pListMember->pNext = NULL;
vCorrectFontValues(&pListMember->tInfo);
if (pAnchor == NULL) {
pAnchor = pListMember;
} else {
fail(pFontLast == NULL);
pFontLast->pNext = pListMember;
}
pFontLast = pListMember;
}
const font_block_type *
pGetNextFontInfoListItem(const font_block_type *pCurr)
{
const font_mem_type *pRecord;
size_t tOffset;
if (pCurr == NULL) {
if (pAnchor == NULL) {
return NULL;
}
return &pAnchor->tInfo;
}
tOffset = offsetof(font_mem_type, tInfo);
pRecord = (font_mem_type *)(void *)((char *)pCurr - tOffset);
fail(pCurr != &pRecord->tInfo);
if (pRecord->pNext == NULL) {
return NULL;
}
return &pRecord->pNext->tInfo;
}