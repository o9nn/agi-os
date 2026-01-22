#include <stdlib.h>
#include <stddef.h>
#include <ctype.h>
#include "antiword.h"
typedef struct style_mem_tag {
style_block_type	tInfo;
ULONG			ulSequenceNumber;
struct style_mem_tag	*pNext;
} style_mem_type;
static style_mem_type	*pAnchor = NULL;
static style_mem_type	*pStyleLast = NULL;
static conversion_type	eConversionType = conversion_unknown;
static encoding_type	eEncoding = encoding_neutral;
static const style_mem_type	*pMidPtr = NULL;
static BOOL		bMoveMidPtr = FALSE;
static BOOL		bInSequence = TRUE;
void
vDestroyStyleInfoList(void)
{
style_mem_type	*pCurr, *pNext;
DBG_MSG("vDestroyStyleInfoList");
pCurr = pAnchor;
while (pCurr != NULL) {
pNext = pCurr->pNext;
pCurr = xfree(pCurr);
pCurr = pNext;
}
pAnchor = NULL;
pStyleLast = NULL;
pMidPtr = NULL;
bMoveMidPtr = FALSE;
bInSequence = TRUE;
}
static void
vConvertListCharacter(UCHAR ucNFC, USHORT usListChar, char *szListChar)
{
options_type	tOptions;
size_t	tLen;
fail(szListChar == NULL);
fail(szListChar[0] != '\0');
if (usListChar < 0x80 && isprint((int)usListChar)) {
DBG_CHR_C(isalnum((int)usListChar), usListChar);
szListChar[0] = (char)usListChar;
szListChar[1] = '\0';
return;
}
if (ucNFC != LIST_SPECIAL &&
ucNFC != LIST_SPECIAL2 &&
ucNFC != LIST_BULLETS) {
szListChar[0] = '.';
szListChar[1] = '\0';
return;
}
if (eConversionType == conversion_unknown ||
eEncoding == encoding_neutral) {
vGetOptions(&tOptions);
eConversionType = tOptions.eConversionType;
eEncoding = tOptions.eEncoding;
}
switch (usListChar) {
case 0x0000: case 0x00b7: case 0x00fe: case  0xf021: case 0xf043:
case 0xf06c: case 0xf093: case 0xf0b7:
usListChar = 0x2022;
break;
case 0x0096: case 0xf02d:
usListChar = 0x2013;
break;
case 0x00a8:
usListChar = 0x2666;
break;
case 0x00de:
usListChar = 0x21d2;
break;
case 0x00e0: case 0xf074:
usListChar = 0x25ca;
break;
case 0x00e1:
usListChar = 0x2329;
break;
case 0xf020:
usListChar = 0x0020;
break;
case 0xf041:
usListChar = 0x270c;
break;
case 0xf066:
usListChar = 0x03d5;
break;
case 0xf06e:
usListChar = 0x25a0;
break;
case 0xf06f: case 0xf070: case 0xf0a8:
usListChar = 0x25a1;
break;
case 0xf071:
usListChar = 0x2751;
break;
case 0xf075: case 0xf077:
usListChar = 0x25c6;
break;
case 0xf076:
usListChar = 0x2756;
break;
case 0xf0a7:
usListChar = 0x25aa;
break;
case 0xf0d8:
usListChar = 0x27a2;
break;
case 0xf0e5:
usListChar = 0x2199;
break;
case 0xf0f0:
usListChar = 0x21e8;
break;
case 0xf0fc:
usListChar = 0x2713;
break;
default:
if ((usListChar >= 0xe000 && usListChar < 0xf900) ||
(usListChar < 0x80 && !isprint((int)usListChar))) {
DBG_HEX(usListChar);
DBG_FIXME();
if (ucNFC == LIST_SPECIAL || ucNFC == LIST_SPECIAL2) {
usListChar = 0x2190;
} else {
usListChar = 0x2022;
}
}
break;
}
if (eEncoding == encoding_utf_8) {
tLen = tUcs2Utf8(usListChar, szListChar, 4);
szListChar[tLen] = '\0';
} else {
switch (usListChar) {
case 0x03d5: case 0x25a1: case 0x25c6: case 0x25ca:
case 0x2751:
szListChar[0] = 'o';
break;
case 0x2013: case 0x2500:
szListChar[0] = '-';
break;
case 0x2190: case 0x2199: case 0x2329:
szListChar[0] = '<';
break;
case 0x21d2:
szListChar[0] = '=';
break;
case 0x21e8: case 0x27a2:
szListChar[0] = '>';
break;
case 0x25a0: case 0x25aa:
szListChar[0] = '.';
break;
case 0x2666:
szListChar[0] = OUR_DIAMOND;
break;
case 0x270c:
szListChar[0] = 'x';
break;
case 0x2713:
szListChar[0] = 'V';
break;
case 0x2756:
szListChar[0] = '*';
break;
case 0x2022:
default:
vGetBulletValue(eConversionType, eEncoding,
szListChar, 2);
break;
}
tLen = 1;
}
szListChar[tLen] = '\0';
}
level_type_enum
eGetNumType(UCHAR ucNumLevel)
{
switch (ucNumLevel) {
case  1: case  2: case  3: case  4: case  5:
case  6: case  7: case  8: case  9:
return level_type_outline;
case 10:
return level_type_numbering;
case 11:
return level_type_sequence;
case 12:
return level_type_pause;
default:
return level_type_none;
}
}
void
vCorrectStyleValues(style_block_type *pStyleBlock)
{
if (pStyleBlock->usBeforeIndent > 0x7fff) {
pStyleBlock->usBeforeIndent = 0;
} else if (pStyleBlock->usBeforeIndent > 2160) {
DBG_DEC(pStyleBlock->usBeforeIndent);
pStyleBlock->usBeforeIndent = 2160;
}
if (pStyleBlock->usIstd >= 1 &&
pStyleBlock->usIstd <= 9 &&
pStyleBlock->usBeforeIndent < HEADING_GAP) {
NO_DBG_DEC(pStyleBlock->usBeforeIndent);
pStyleBlock->usBeforeIndent = HEADING_GAP;
}
if (pStyleBlock->usAfterIndent > 0x7fff) {
pStyleBlock->usAfterIndent = 0;
} else if (pStyleBlock->usAfterIndent > 2160) {
DBG_DEC(pStyleBlock->usAfterIndent);
pStyleBlock->usAfterIndent = 2160;
}
if (pStyleBlock->usIstd >= 1 &&
pStyleBlock->usIstd <= 9 &&
pStyleBlock->usAfterIndent < HEADING_GAP) {
NO_DBG_DEC(pStyleBlock->usAfterIndent);
pStyleBlock->usAfterIndent = HEADING_GAP;
}
if (pStyleBlock->sLeftIndent < 0) {
pStyleBlock->sLeftIndent = 0;
}
if (pStyleBlock->sRightIndent > 0) {
pStyleBlock->sRightIndent = 0;
}
vConvertListCharacter(pStyleBlock->ucNFC,
pStyleBlock->usListChar,
pStyleBlock->szListChar);
}
void
vAdd2StyleInfoList(const style_block_type *pStyleBlock)
{
style_mem_type	*pListMember;
fail(pStyleBlock == NULL);
NO_DBG_MSG("bAdd2StyleInfoList");
if (pStyleBlock->ulFileOffset == FC_INVALID) {
NO_DBG_DEC(pStyleBlock->usIstd);
return;
}
NO_DBG_HEX(pStyleBlock->ulFileOffset);
NO_DBG_DEC_C(pStyleBlock->sLeftIndent != 0,
pStyleBlock->sLeftIndent);
NO_DBG_DEC_C(pStyleBlock->sRightIndent != 0,
pStyleBlock->sRightIndent);
NO_DBG_DEC_C(pStyleBlock->bNumPause, pStyleBlock->bNumPause);
NO_DBG_DEC_C(pStyleBlock->usIstd != 0, pStyleBlock->usIstd);
NO_DBG_DEC_C(pStyleBlock->usStartAt != 1, pStyleBlock->usStartAt);
NO_DBG_DEC_C(pStyleBlock->usAfterIndent != 0,
pStyleBlock->usAfterIndent);
NO_DBG_DEC_C(pStyleBlock->ucAlignment != 0, pStyleBlock->ucAlignment);
NO_DBG_DEC(pStyleBlock->ucNFC);
NO_DBG_HEX(pStyleBlock->usListChar);
if (pStyleLast != NULL &&
pStyleLast->tInfo.ulFileOffset == pStyleBlock->ulFileOffset) {
fail(pStyleLast->pNext != NULL);
pStyleLast->tInfo = *pStyleBlock;
vCorrectStyleValues(&pStyleLast->tInfo);
return;
}
pListMember = xmalloc(sizeof(style_mem_type));
pListMember->tInfo = *pStyleBlock;
pListMember->pNext = NULL;
pListMember->ulSequenceNumber =
ulGetSeqNumber(pListMember->tInfo.ulFileOffset);
vCorrectStyleValues(&pListMember->tInfo);
if (pAnchor == NULL) {
pAnchor = pListMember;
pMidPtr = pAnchor;
bMoveMidPtr = FALSE;
bInSequence = TRUE;
} else {
fail(pStyleLast == NULL);
pStyleLast->pNext = pListMember;
if (bMoveMidPtr) {
pMidPtr = pMidPtr->pNext;
bMoveMidPtr = FALSE;
} else {
bMoveMidPtr = TRUE;
}
if (bInSequence) {
bInSequence = pListMember->ulSequenceNumber >
pStyleLast->ulSequenceNumber;
}
}
pStyleLast = pListMember;
}
const style_block_type *
pGetNextStyleInfoListItem(const style_block_type *pCurr)
{
const style_mem_type	*pRecord;
size_t	tOffset;
if (pCurr == NULL) {
if (pAnchor == NULL) {
return NULL;
}
return &pAnchor->tInfo;
}
tOffset = offsetof(style_mem_type, tInfo);
pRecord = (style_mem_type *)(void *)((char *)pCurr - tOffset);
fail(pCurr != &pRecord->tInfo);
if (pRecord->pNext == NULL) {
return NULL;
}
return &pRecord->pNext->tInfo;
}
const style_block_type *
pGetNextTextStyle(const style_block_type *pCurr)
{
const style_block_type	*pRecord;
pRecord = pCurr;
do {
pRecord = pGetNextStyleInfoListItem(pRecord);
} while (pRecord != NULL &&
(pRecord->eListID == hdrftr_list ||
pRecord->eListID == macro_list ||
pRecord->eListID == annotation_list));
return pRecord;
}
USHORT
usGetIstd(ULONG ulFileOffset)
{
const style_mem_type	*pCurr, *pBest, *pStart;
ULONG	ulSeq, ulBest;
ulSeq = ulGetSeqNumber(ulFileOffset);
if (ulSeq == FC_INVALID) {
return ISTD_NORMAL;
}
NO_DBG_HEX(ulFileOffset);
NO_DBG_DEC(ulSeq);
if (bInSequence &&
pMidPtr != NULL &&
ulSeq > pMidPtr->ulSequenceNumber) {
pStart = pMidPtr;
} else {
pStart = pAnchor;
}
pBest = NULL;
ulBest = 0;
for (pCurr = pStart; pCurr != NULL; pCurr = pCurr->pNext) {
if (pCurr->ulSequenceNumber != FC_INVALID &&
(pBest == NULL || pCurr->ulSequenceNumber > ulBest) &&
pCurr->ulSequenceNumber <= ulSeq) {
pBest = pCurr;
ulBest = pCurr->ulSequenceNumber;
}
if (bInSequence && pCurr->ulSequenceNumber > ulSeq) {
break;
}
}
NO_DBG_DEC(ulBest);
if (pBest == NULL) {
return ISTD_NORMAL;
}
NO_DBG_DEC(pBest->tInfo.usIstd);
return pBest->tInfo.usIstd;
}
BOOL
bStyleImpliesList(const style_block_type *pStyle, int iWordVersion)
{
fail(pStyle == NULL);
fail(iWordVersion < 0);
if (pStyle->usIstd >= 1 && pStyle->usIstd <= 9) {
return FALSE;
}
if (iWordVersion < 8) {
return pStyle->ucNumLevel != 0;
}
return pStyle->usListIndex != 0;
}