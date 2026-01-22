#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#if defined(__riscos)
#include "DeskLib:Hourglass.h"
#include "drawfile.h"
#endif
#include "antiword.h"
#define INITIAL_SIZE 40
#define EXTENTION_SIZE 20
#define OUTPUT_LINE() \
do {\
vAlign2Window(pDiag, pAnchor, lWidthMax, ucAlignment);\
TRACE_MSG("after vAlign2Window");\
pAnchor = pStartNewOutput(pAnchor, NULL);\
pOutput = pAnchor;\
} while(0)
#define RESET_LINE() \
do {\
pAnchor = pStartNewOutput(pAnchor, NULL);\
pOutput = pAnchor;\
} while(0)
#if defined(__riscos)
static ULONG ulDocumentLength;
static ULONG ulCharCounter;
static int iCurrPct, iPrevPct;
#endif
static int iWordVersion = -1;
static BOOL bOldMacFile = FALSE;
static const section_block_type *pSection = NULL;
static const section_block_type *pSectionNext = NULL;
static options_type tOptions;
static const row_block_type *pRowInfo = NULL;
static BOOL bStartRow = FALSE;
static BOOL bEndRowNorm = FALSE;
static BOOL bEndRowFast = FALSE;
static BOOL bIsTableRow = FALSE;
static USHORT usIstdNext = ISTD_NORMAL;
static const style_block_type *pStyleInfo = NULL;
static style_block_type tStyleNext;
static BOOL bStartStyle = FALSE;
static BOOL bStartStyleNext = FALSE;
static const font_block_type *pFontInfo = NULL;
static font_block_type tFontNext;
static BOOL bStartFont = FALSE;
static BOOL bStartFontNext = FALSE;
static ULONG ulFileOffsetImage = FC_INVALID;
static void
vUpdateCounters(void)
{
#if defined(__riscos)
ulCharCounter++;
iCurrPct = (int)((ulCharCounter * 100) / ulDocumentLength);
if (iCurrPct != iPrevPct) {
Hourglass_Percentage(iCurrPct);
iPrevPct = iCurrPct;
}
#endif
}
BOOL
bOutputContainsText(const output_type *pAnchor)
{
const output_type *pCurr;
size_t tIndex;
fail(pAnchor == NULL);
for (pCurr = pAnchor; pCurr != NULL; pCurr = pCurr->pNext) {
fail(pCurr->lStringWidth < 0);
for (tIndex = 0; tIndex < pCurr->tNextFree; tIndex++) {
if (isspace((int)(UCHAR)pCurr->szStorage[tIndex])) {
continue;
}
#if defined(DEBUG)
if (pCurr->szStorage[tIndex] == FILLER_CHAR) {
continue;
}
#endif
return TRUE;
}
}
return FALSE;
}
static long
lTotalStringWidth(const output_type *pAnchor)
{
const output_type *pCurr;
long lTotal;
lTotal = 0;
for (pCurr = pAnchor; pCurr != NULL; pCurr = pCurr->pNext) {
DBG_DEC_C(pCurr->lStringWidth < 0, pCurr->lStringWidth);
fail(pCurr->lStringWidth < 0);
lTotal += pCurr->lStringWidth;
}
return lTotal;
}
static void
vStoreByte(UCHAR ucChar, output_type *pOutput)
{
fail(pOutput == NULL);
if (ucChar == 0) {
pOutput->szStorage[pOutput->tNextFree] = '\0';
return;
}
while (pOutput->tNextFree + 2 > pOutput->tStorageSize) {
pOutput->tStorageSize += EXTENTION_SIZE;
pOutput->szStorage = xrealloc(pOutput->szStorage,
pOutput->tStorageSize);
}
pOutput->szStorage[pOutput->tNextFree] = (char)ucChar;
pOutput->szStorage[pOutput->tNextFree + 1] = '\0';
pOutput->tNextFree++;
}
static void
vStoreChar(ULONG ulChar, BOOL bChangeAllowed, output_type *pOutput)
{
char szResult[4];
size_t tIndex, tLen;
fail(pOutput == NULL);
if (tOptions.eEncoding == encoding_utf_8 && bChangeAllowed) {
DBG_HEX_C(ulChar > 0xffff, ulChar);
fail(ulChar > 0xffff);
tLen = tUcs2Utf8(ulChar, szResult, sizeof(szResult));
for (tIndex = 0; tIndex < tLen; tIndex++) {
vStoreByte((UCHAR)szResult[tIndex], pOutput);
}
} else {
DBG_HEX_C(ulChar > 0xff, ulChar);
fail(ulChar > 0xff);
vStoreByte((UCHAR)ulChar, pOutput);
tLen = 1;
}
pOutput->lStringWidth += lComputeStringWidth(
pOutput->szStorage + pOutput->tNextFree - tLen,
tLen,
pOutput->tFontRef,
pOutput->usFontSize);
}
static void
vStoreCharacter(ULONG ulChar, output_type *pOutput)
{
vStoreChar(ulChar, TRUE, pOutput);
}
static void
vStoreString(const char *szString, size_t tStringLength, output_type *pOutput)
{
size_t tIndex;
fail(szString == NULL || pOutput == NULL);
for (tIndex = 0; tIndex < tStringLength; tIndex++) {
vStoreCharacter((ULONG)(UCHAR)szString[tIndex], pOutput);
}
}
static void
vStoreNumberAsDecimal(UINT uiNumber, output_type *pOutput)
{
size_t tLen;
char szString[3 * sizeof(UINT) + 1];
fail(uiNumber == 0);
fail(pOutput == NULL);
tLen = (size_t)sprintf(szString, "%u", uiNumber);
vStoreString(szString, tLen, pOutput);
}
static void
vStoreNumberAsRoman(UINT uiNumber, output_type *pOutput)
{
size_t tLen;
char szString[15];
fail(uiNumber == 0);
fail(pOutput == NULL);
tLen = tNumber2Roman(uiNumber, FALSE, szString);
vStoreString(szString, tLen, pOutput);
}
static void
vStoreStyle(diagram_type *pDiag, output_type *pOutput,
const style_block_type *pStyle)
{
size_t tLen;
char szString[120];
fail(pDiag == NULL);
fail(pOutput == NULL);
fail(pStyle == NULL);
if (tOptions.eConversionType == conversion_xml) {
vSetHeaders(pDiag, pStyle->usIstd);
} else {
tLen = tStyle2Window(szString, sizeof(szString),
pStyle, pSection);
vStoreString(szString, tLen, pOutput);
}
}
static void
vPutIndentation(diagram_type *pDiag, output_type *pOutput,
BOOL bNoMarks, BOOL bFirstLine,
UINT uiListNumber, UCHAR ucNFC, const char *szListChar,
long lLeftIndentation, long lLeftIndentation1)
{
long lWidth;
size_t tIndex, tNextFree;
char szLine[30];
fail(pDiag == NULL);
fail(pOutput == NULL);
fail(szListChar == NULL);
fail(lLeftIndentation < 0);
if (tOptions.eConversionType == conversion_xml) {
return;
}
if (bNoMarks) {
if (bFirstLine) {
lLeftIndentation += lLeftIndentation1;
}
if (lLeftIndentation < 0) {
lLeftIndentation = 0;
}
vSetLeftIndentation(pDiag, lLeftIndentation);
return;
}
if (lLeftIndentation <= 0) {
DBG_HEX_C(ucNFC != 0x00, ucNFC);
vSetLeftIndentation(pDiag, 0);
return;
}
#if defined(DEBUG)
if (tOptions.eEncoding == encoding_utf_8) {
fail(strlen(szListChar) > 3);
} else {
DBG_HEX_C(iscntrl((int)szListChar[0]), szListChar[0]);
fail(iscntrl((int)szListChar[0]));
fail(szListChar[1] != '\0');
}
#endif
switch (ucNFC) {
case LIST_ARABIC_NUM:
case LIST_NUMBER_TXT:
tNextFree = (size_t)sprintf(szLine, "%u", uiListNumber);
break;
case LIST_UPPER_ROMAN:
case LIST_LOWER_ROMAN:
tNextFree = tNumber2Roman(uiListNumber,
ucNFC == LIST_UPPER_ROMAN, szLine);
break;
case LIST_UPPER_ALPHA:
case LIST_LOWER_ALPHA:
tNextFree = tNumber2Alpha(uiListNumber,
ucNFC == LIST_UPPER_ALPHA, szLine);
break;
case LIST_ORDINAL_NUM:
case LIST_ORDINAL_TXT:
if (uiListNumber % 10 == 1 && uiListNumber != 11) {
tNextFree =
(size_t)sprintf(szLine, "%ust", uiListNumber);
} else if (uiListNumber % 10 == 2 && uiListNumber != 12) {
tNextFree =
(size_t)sprintf(szLine, "%und", uiListNumber);
} else if (uiListNumber % 10 == 3 && uiListNumber != 13) {
tNextFree =
(size_t)sprintf(szLine, "%urd", uiListNumber);
} else {
tNextFree =
(size_t)sprintf(szLine, "%uth", uiListNumber);
}
break;
case LIST_OUTLINE_NUM:
tNextFree = (size_t)sprintf(szLine, "%02u", uiListNumber);
break;
case LIST_SPECIAL:
case LIST_SPECIAL2:
case LIST_BULLETS:
tNextFree = 0;
break;
default:
DBG_HEX(ucNFC);
DBG_FIXME();
tNextFree = (size_t)sprintf(szLine, "%u", uiListNumber);
break;
}
tNextFree += (size_t)sprintf(szLine + tNextFree, "%.3s", szListChar);
szLine[tNextFree++] = ' ';
szLine[tNextFree] = '\0';
lWidth = lComputeStringWidth(szLine, tNextFree,
pOutput->tFontRef, pOutput->usFontSize);
lLeftIndentation -= lWidth;
if (lLeftIndentation < 0) {
lLeftIndentation = 0;
}
vSetLeftIndentation(pDiag, lLeftIndentation);
for (tIndex = 0; tIndex < tNextFree; tIndex++) {
vStoreChar((ULONG)(UCHAR)szLine[tIndex], FALSE, pOutput);
}
}
static void
vPutSeparatorLine(output_type *pOutput)
{
long lCharWidth;
int iCounter, iChars;
char szOne[2];
fail(pOutput == NULL);
szOne[0] = OUR_EM_DASH;
szOne[1] = '\0';
lCharWidth = lComputeStringWidth(szOne, 1,
pOutput->tFontRef, pOutput->usFontSize);
NO_DBG_DEC(lCharWidth);
iChars = (int)((144000 + lCharWidth / 2) / lCharWidth);
NO_DBG_DEC(iChars);
for (iCounter = 0; iCounter < iChars; iCounter++) {
vStoreCharacter((ULONG)(UCHAR)OUR_EM_DASH, pOutput);
}
}
static output_type *
pStartNextOutput(output_type *pCurrent)
{
output_type *pNew;
TRACE_MSG("pStartNextOutput");
if (pCurrent->tNextFree == 0) {
fail(pCurrent->szStorage[0] != '\0');
fail(pCurrent->lStringWidth != 0);
return pCurrent;
}
pNew = xmalloc(sizeof(*pNew));
pCurrent->pNext = pNew;
pNew->tStorageSize = INITIAL_SIZE;
pNew->szStorage = xmalloc(pNew->tStorageSize);
pNew->szStorage[0] = '\0';
pNew->tNextFree = 0;
pNew->lStringWidth = 0;
pNew->ucFontColor = FONT_COLOR_DEFAULT;
pNew->usFontStyle = FONT_REGULAR;
pNew->tFontRef = (drawfile_fontref)0;
pNew->usFontSize = DEFAULT_FONT_SIZE;
pNew->pPrev = pCurrent;
pNew->pNext = NULL;
return pNew;
}
static output_type *
pStartNewOutput(output_type *pAnchor, output_type *pLeftOver)
{
output_type *pCurr, *pNext;
USHORT usFontStyle, usFontSize;
drawfile_fontref tFontRef;
UCHAR ucFontColor;
TRACE_MSG("pStartNewOutput");
ucFontColor = FONT_COLOR_DEFAULT;
usFontStyle = FONT_REGULAR;
tFontRef = (drawfile_fontref)0;
usFontSize = DEFAULT_FONT_SIZE;
pCurr = pAnchor;
while (pCurr != NULL) {
TRACE_MSG("Free the old output space");
pNext = pCurr->pNext;
pCurr->szStorage = xfree(pCurr->szStorage);
if (pCurr->pNext == NULL) {
ucFontColor = pCurr->ucFontColor;
usFontStyle = pCurr->usFontStyle;
tFontRef = pCurr->tFontRef;
usFontSize = pCurr->usFontSize;
}
pCurr = xfree(pCurr);
pCurr = pNext;
}
if (pLeftOver == NULL) {
TRACE_MSG("Create new output space");
pLeftOver = xmalloc(sizeof(*pLeftOver));
pLeftOver->tStorageSize = INITIAL_SIZE;
NO_DBG_DEC(pLeftOver->tStorageSize);
TRACE_MSG("before 2nd xmalloc");
pLeftOver->szStorage = xmalloc(pLeftOver->tStorageSize);
TRACE_MSG("after 2nd xmalloc");
pLeftOver->szStorage[0] = '\0';
pLeftOver->tNextFree = 0;
pLeftOver->lStringWidth = 0;
pLeftOver->ucFontColor = ucFontColor;
pLeftOver->usFontStyle = usFontStyle;
pLeftOver->tFontRef = tFontRef;
pLeftOver->usFontSize = usFontSize;
pLeftOver->pPrev = NULL;
pLeftOver->pNext = NULL;
}
fail(!bCheckDoubleLinkedList(pLeftOver));
return pLeftOver;
}
static ULONG
ulGetChar(FILE *pFile, list_id_enum eListID)
{
const font_block_type *pCurr;
ULONG ulChar, ulFileOffset, ulCharPos;
row_info_enum eRowInfo;
USHORT usChar, usPropMod;
BOOL bSkip;
fail(pFile == NULL);
pCurr = pFontInfo;
bSkip = FALSE;
for (;;) {
usChar = usNextChar(pFile, eListID,
&ulFileOffset, &ulCharPos, &usPropMod);
if (usChar == (USHORT)EOF) {
return (ULONG)EOF;
}
vUpdateCounters();
eRowInfo = ePropMod2RowInfo(usPropMod, iWordVersion);
if (!bStartRow) {
#if 0
bStartRow = eRowInfo == found_a_cell ||
(pRowInfo != NULL &&
ulFileOffset == pRowInfo->ulFileOffsetStart &&
eRowInfo != found_not_a_cell);
#else
bStartRow = pRowInfo != NULL &&
ulFileOffset == pRowInfo->ulFileOffsetStart;
#endif
NO_DBG_HEX_C(bStartRow, pRowInfo->ulFileOffsetStart);
}
if (!bEndRowNorm) {
#if 0
bEndRow = eRowInfo == found_end_of_row ||
(pRowInfo != NULL &&
ulFileOffset == pRowInfo->ulFileOffsetEnd &&
eRowInfo != found_not_end_of_row);
#else
bEndRowNorm = pRowInfo != NULL &&
ulFileOffset == pRowInfo->ulFileOffsetEnd;
#endif
NO_DBG_HEX_C(bEndRowNorm, pRowInfo->ulFileOffsetEnd);
}
if (!bEndRowFast) {
bEndRowFast = eRowInfo == found_end_of_row;
NO_DBG_HEX_C(bEndRowFast, pRowInfo->ulFileOffsetEnd);
}
if (!bStartStyle) {
bStartStyle = pStyleInfo != NULL &&
ulFileOffset == pStyleInfo->ulFileOffset;
NO_DBG_HEX_C(bStartStyle, ulFileOffset);
}
if (pCurr != NULL && ulFileOffset == pCurr->ulFileOffset) {
bStartFont = TRUE;
NO_DBG_HEX(ulFileOffset);
pFontInfo = pCurr;
pCurr = pGetNextFontInfoListItem(pCurr);
}
if (usChar == START_EMBEDDED) {
bSkip = TRUE;
continue;
}
if (usChar == END_IGNORE || usChar == END_EMBEDDED) {
bSkip = FALSE;
continue;
}
if (bSkip) {
continue;
}
ulChar = ulTranslateCharacters(usChar,
ulFileOffset,
iWordVersion,
tOptions.eConversionType,
tOptions.eEncoding,
bOldMacFile);
if (ulChar == IGNORE_CHARACTER) {
continue;
}
if (ulChar == PICTURE) {
ulFileOffsetImage = ulGetPictInfoListItem(ulFileOffset);
} else {
ulFileOffsetImage = FC_INVALID;
}
if (ulChar == PAR_END) {
vFillStyleFromStylesheet(usIstdNext, &tStyleNext);
vCorrectStyleValues(&tStyleNext);
bStartStyleNext = TRUE;
vFillFontFromStylesheet(usIstdNext, &tFontNext);
vCorrectFontValues(&tFontNext);
bStartFontNext = TRUE;
}
if (ulChar == PAGE_BREAK) {
pSectionNext = pGetSectionInfo(pSection, ulCharPos);
}
return ulChar;
}
}
static long
lGetWidthMax(int iParagraphBreak)
{
fail(iParagraphBreak < 0);
if (iParagraphBreak == 0) {
return LONG_MAX;
}
if (iParagraphBreak < MIN_SCREEN_WIDTH) {
return lChar2MilliPoints(MIN_SCREEN_WIDTH);
}
if (iParagraphBreak > MAX_SCREEN_WIDTH) {
return lChar2MilliPoints(MAX_SCREEN_WIDTH);
}
return lChar2MilliPoints(iParagraphBreak);
}
BOOL
bWordDecryptor(FILE *pFile, long lFilesize, diagram_type *pDiag)
{
imagedata_type tImage;
const style_block_type *pStyleTmp;
const font_block_type *pFontTmp;
const char *szListChar;
output_type *pAnchor, *pOutput, *pLeftOver;
ULONG ulChar;
long lBeforeIndentation, lAfterIndentation;
long lLeftIndentation, lLeftIndentation1, lRightIndentation;
long lWidthCurr, lWidthMax, lDefaultTabWidth, lHalfSpaceWidth, lTmp;
list_id_enum eListID;
image_info_enum eRes;
UINT uiFootnoteNumber, uiEndnoteNumber, uiTmp;
int iListSeqNumber;
BOOL bWasTableRow, bTableFontClosed, bWasEndOfParagraph;
BOOL bInList, bWasInList, bNoMarks, bFirstLine;
BOOL bAllCapitals, bHiddenText, bMarkDelText, bSuccess;
USHORT usListNumber;
USHORT usFontStyle, usFontStyleMinimal, usFontSize, usTmp;
UCHAR ucFontNumber, ucFontColor;
UCHAR ucNFC, ucAlignment;
fail(pFile == NULL || lFilesize <= 0 || pDiag == NULL);
TRACE_MSG("bWordDecryptor");
iWordVersion = iInitDocument(pFile, lFilesize);
if (iWordVersion < 0) {
DBG_DEC(iWordVersion);
return FALSE;
}
vGetOptions(&tOptions);
bOldMacFile = bIsOldMacFile();
vPrepareHdrFtrText(pFile);
vPrepareFootnoteText(pFile);
vPrologue2(pDiag, iWordVersion);
#if defined(__riscos)
ulCharCounter = 0;
iCurrPct = 0;
iPrevPct = -1;
ulDocumentLength = ulGetDocumentLength();
#endif
pSection = pGetSectionInfo(NULL, 0);
pSectionNext = pSection;
lDefaultTabWidth = lGetDefaultTabWidth();
DBG_DEC_C(lDefaultTabWidth != 36000, lDefaultTabWidth);
pRowInfo = pGetNextRowInfoListItem();
DBG_HEX_C(pRowInfo != NULL, pRowInfo->ulFileOffsetStart);
DBG_HEX_C(pRowInfo != NULL, pRowInfo->ulFileOffsetEnd);
DBG_MSG_C(pRowInfo == NULL, "No rows at all");
bStartRow = FALSE;
bEndRowNorm = FALSE;
bEndRowFast = FALSE;
bIsTableRow = FALSE;
bWasTableRow = FALSE;
vResetStyles();
pStyleInfo = pGetNextTextStyle(NULL);
bStartStyle = FALSE;
bInList = FALSE;
bWasInList = FALSE;
iListSeqNumber = 0;
usIstdNext = ISTD_NORMAL;
pAnchor = NULL;
pFontInfo = pGetNextFontInfoListItem(NULL);
DBG_HEX_C(pFontInfo != NULL, pFontInfo->ulFileOffset);
DBG_MSG_C(pFontInfo == NULL, "No fonts at all");
bStartFont = FALSE;
ucFontNumber = 0;
usFontStyleMinimal = FONT_REGULAR;
usFontStyle = FONT_REGULAR;
usFontSize = DEFAULT_FONT_SIZE;
ucFontColor = FONT_COLOR_DEFAULT;
pAnchor = pStartNewOutput(pAnchor, NULL);
pOutput = pAnchor;
pOutput->ucFontColor = ucFontColor;
pOutput->usFontStyle = usFontStyle;
pOutput->tFontRef = tOpenFont(ucFontNumber, usFontStyle, usFontSize);
pOutput->usFontSize = usFontSize;
bTableFontClosed = TRUE;
lBeforeIndentation = 0;
lAfterIndentation = 0;
lLeftIndentation = 0;
lLeftIndentation1 = 0;
lRightIndentation = 0;
bWasEndOfParagraph = TRUE;
bNoMarks = TRUE;
bFirstLine = TRUE;
ucNFC = LIST_BULLETS;
if (pStyleInfo != NULL) {
szListChar = pStyleInfo->szListChar;
pStyleTmp = pStyleInfo;
} else {
if (tStyleNext.szListChar[0] == '\0') {
vGetBulletValue(tOptions.eConversionType,
tOptions.eEncoding, tStyleNext.szListChar, 4);
}
szListChar = tStyleNext.szListChar;
pStyleTmp = &tStyleNext;
}
usListNumber = 0;
ucAlignment = ALIGNMENT_LEFT;
bAllCapitals = FALSE;
bHiddenText = FALSE;
bMarkDelText = FALSE;
lWidthMax = lGetWidthMax(tOptions.iParagraphBreak);
NO_DBG_DEC(lWidthMax);
Hourglass_On();
uiFootnoteNumber = 0;
uiEndnoteNumber = 0;
eListID = text_list;
for(;;) {
ulChar = ulGetChar(pFile, eListID);
if (ulChar == (ULONG)EOF) {
if (bOutputContainsText(pAnchor)) {
OUTPUT_LINE();
} else {
RESET_LINE();
}
switch (eListID) {
case text_list:
if (tOptions.eConversionType !=
conversion_xml) {
eListID = footnote_list;
if (uiFootnoteNumber != 0) {
vPutSeparatorLine(pAnchor);
OUTPUT_LINE();
uiFootnoteNumber = 0;
}
break;
}
case footnote_list:
eListID = endnote_list;
if (uiEndnoteNumber != 0) {
vPutSeparatorLine(pAnchor);
OUTPUT_LINE();
uiEndnoteNumber = 0;
}
break;
case endnote_list:
eListID = textbox_list;
if (bExistsTextBox()) {
vPutSeparatorLine(pAnchor);
OUTPUT_LINE();
}
break;
case textbox_list:
eListID = hdrtextbox_list;
if (bExistsHdrTextBox()) {
vPutSeparatorLine(pAnchor);
OUTPUT_LINE();
}
break;
case hdrtextbox_list:
default:
eListID = end_of_lists;
break;
}
if (eListID == end_of_lists) {
break;
}
continue;
}
if (ulChar == UNKNOWN_NOTE_CHAR) {
switch (eListID) {
case footnote_list:
ulChar = FOOTNOTE_CHAR;
break;
case endnote_list:
ulChar = ENDNOTE_CHAR;
break;
default:
break;
}
}
if (bStartRow) {
if (bOutputContainsText(pAnchor)) {
OUTPUT_LINE();
} else {
RESET_LINE();
}
fail(pAnchor != pOutput);
if (bTableFontClosed) {
vCloseFont();
uiTmp = ((UINT)usFontSize * 5 + 3) / 6;
if (uiTmp < MIN_TABLEFONT_SIZE) {
uiTmp = MIN_TABLEFONT_SIZE;
} else if (uiTmp > MAX_TABLEFONT_SIZE) {
uiTmp = MAX_TABLEFONT_SIZE;
}
pOutput->usFontSize = (USHORT)uiTmp;
pOutput->tFontRef =
tOpenTableFont(pOutput->usFontSize);
pOutput->usFontStyle = FONT_REGULAR;
pOutput->ucFontColor = FONT_COLOR_BLACK;
bTableFontClosed = FALSE;
}
bIsTableRow = TRUE;
bStartRow = FALSE;
}
if (bWasTableRow &&
!bIsTableRow &&
ulChar != PAR_END &&
ulChar != HARD_RETURN &&
ulChar != PAGE_BREAK &&
ulChar != COLUMN_FEED) {
OUTPUT_LINE();
vEndOfParagraph(pDiag,
pOutput->tFontRef,
pOutput->usFontSize,
(long)pOutput->usFontSize * 600);
}
switch (ulChar) {
case PAGE_BREAK:
case COLUMN_FEED:
if (bIsTableRow) {
break;
}
if (bOutputContainsText(pAnchor)) {
OUTPUT_LINE();
} else {
RESET_LINE();
}
if (ulChar == PAGE_BREAK) {
vEndOfPage(pDiag, lAfterIndentation,
pSection != pSectionNext);
} else {
vEndOfParagraph(pDiag,
pOutput->tFontRef,
pOutput->usFontSize,
lAfterIndentation);
}
break;
default:
break;
}
if (bStartFont || (bStartFontNext && ulChar != PAR_END)) {
if (bStartFont) {
fail(pFontInfo == NULL);
pFontTmp = pFontInfo;
} else {
pFontTmp = &tFontNext;
}
bAllCapitals = bIsCapitals(pFontTmp->usFontStyle);
bHiddenText = bIsHidden(pFontTmp->usFontStyle);
bMarkDelText = bIsMarkDel(pFontTmp->usFontStyle);
usTmp = pFontTmp->usFontStyle &
(FONT_BOLD|FONT_ITALIC|FONT_UNDERLINE|
FONT_STRIKE|FONT_MARKDEL|
FONT_SUPERSCRIPT|FONT_SUBSCRIPT);
if (!bIsTableRow &&
(usFontSize != pFontTmp->usFontSize ||
ucFontNumber != pFontTmp->ucFontNumber ||
usFontStyleMinimal != usTmp ||
ucFontColor != pFontTmp->ucFontColor)) {
pOutput = pStartNextOutput(pOutput);
vCloseFont();
pOutput->ucFontColor = pFontTmp->ucFontColor;
pOutput->usFontStyle = pFontTmp->usFontStyle;
pOutput->usFontSize = pFontTmp->usFontSize;
pOutput->tFontRef = tOpenFont(
pFontTmp->ucFontNumber,
pFontTmp->usFontStyle,
pFontTmp->usFontSize);
fail(!bCheckDoubleLinkedList(pAnchor));
}
ucFontNumber = pFontTmp->ucFontNumber;
usFontSize = pFontTmp->usFontSize;
ucFontColor = pFontTmp->ucFontColor;
usFontStyle = pFontTmp->usFontStyle;
usFontStyleMinimal = usTmp;
if (bStartFont) {
pFontInfo = pGetNextFontInfoListItem(pFontInfo);
NO_DBG_HEX_C(pFontInfo != NULL,
pFontInfo->ulFileOffset);
DBG_MSG_C(pFontInfo == NULL, "No more fonts");
}
bStartFont = FALSE;
bStartFontNext = FALSE;
}
if (bStartStyle || (bStartStyleNext && ulChar != PAR_END)) {
bFirstLine = TRUE;
if (bStartStyle) {
fail(pStyleInfo == NULL);
pStyleTmp = pStyleInfo;
} else {
pStyleTmp = &tStyleNext;
}
if (!bIsTableRow) {
vStoreStyle(pDiag, pOutput, pStyleTmp);
}
usIstdNext = pStyleTmp->usIstdNext;
lBeforeIndentation =
lTwips2MilliPoints(pStyleTmp->usBeforeIndent);
lAfterIndentation =
lTwips2MilliPoints(pStyleTmp->usAfterIndent);
lLeftIndentation =
lTwips2MilliPoints(pStyleTmp->sLeftIndent);
lLeftIndentation1 =
lTwips2MilliPoints(pStyleTmp->sLeftIndent1);
lRightIndentation =
lTwips2MilliPoints(pStyleTmp->sRightIndent);
bInList = bStyleImpliesList(pStyleTmp, iWordVersion);
bNoMarks = !bInList || pStyleTmp->bNumPause;
ucNFC = pStyleTmp->ucNFC;
szListChar = pStyleTmp->szListChar;
ucAlignment = pStyleTmp->ucAlignment;
if (bInList && !bWasInList) {
iListSeqNumber++;
vStartOfList(pDiag, ucNFC,
bWasTableRow && !bIsTableRow);
}
if (!bInList && bWasInList) {
vEndOfList(pDiag);
}
bWasInList = bInList;
if (bStartStyle) {
pStyleInfo = pGetNextTextStyle(pStyleInfo);
NO_DBG_HEX_C(pStyleInfo != NULL,
pStyleInfo->ulFileOffset);
DBG_MSG_C(pStyleInfo == NULL,
"No more styles");
}
bStartStyle = FALSE;
bStartStyleNext = FALSE;
}
if (bWasEndOfParagraph) {
vStartOfParagraph1(pDiag, lBeforeIndentation);
}
if (!bIsTableRow &&
lTotalStringWidth(pAnchor) == 0) {
if (!bNoMarks) {
usListNumber = usGetListValue(iListSeqNumber,
iWordVersion,
pStyleTmp);
}
if (bInList && bFirstLine) {
vStartOfListItem(pDiag, bNoMarks);
}
vPutIndentation(pDiag, pAnchor, bNoMarks, bFirstLine,
usListNumber, ucNFC, szListChar,
lLeftIndentation, lLeftIndentation1);
bFirstLine = FALSE;
bNoMarks = TRUE;
}
if (bWasEndOfParagraph) {
vStartOfParagraph2(pDiag);
bWasEndOfParagraph = FALSE;
}
switch (ulChar) {
case PICTURE:
(void)memset(&tImage, 0, sizeof(tImage));
eRes = eExamineImage(pFile, ulFileOffsetImage, &tImage);
switch (eRes) {
case image_no_information:
bSuccess = FALSE;
break;
case image_minimal_information:
case image_full_information:
#if 0
if (bOutputContainsText(pAnchor)) {
OUTPUT_LINE();
} else {
RESET_LINE();
}
#endif
bSuccess = bTranslateImage(pDiag, pFile,
eRes == image_minimal_information,
ulFileOffsetImage, &tImage);
break;
default:
DBG_DEC(eRes);
bSuccess = FALSE;
break;
}
if (!bSuccess) {
vStoreString("[pic]", 5, pOutput);
}
break;
case FOOTNOTE_CHAR:
uiFootnoteNumber++;
if (tOptions.eConversionType == conversion_xml) {
vStoreCharacter((ULONG)FOOTNOTE_OR_ENDNOTE,
pOutput);
break;
}
vStoreCharacter((ULONG)'[', pOutput);
vStoreNumberAsDecimal(uiFootnoteNumber, pOutput);
vStoreCharacter((ULONG)']', pOutput);
break;
case ENDNOTE_CHAR:
uiEndnoteNumber++;
vStoreCharacter((ULONG)'[', pOutput);
vStoreNumberAsRoman(uiEndnoteNumber, pOutput);
vStoreCharacter((ULONG)']', pOutput);
break;
case UNKNOWN_NOTE_CHAR:
vStoreString("[?]", 3, pOutput);
break;
case PAR_END:
if (bIsTableRow) {
vStoreCharacter((ULONG)'\n', pOutput);
break;
}
if (bOutputContainsText(pAnchor)) {
OUTPUT_LINE();
} else {
vMove2NextLine(pDiag,
pOutput->tFontRef, pOutput->usFontSize);
RESET_LINE();
}
vEndOfParagraph(pDiag,
pOutput->tFontRef,
pOutput->usFontSize,
lAfterIndentation);
bWasEndOfParagraph = TRUE;
break;
case HARD_RETURN:
if (bIsTableRow) {
vStoreCharacter((ULONG)'\n', pOutput);
break;
}
if (bOutputContainsText(pAnchor)) {
OUTPUT_LINE();
} else {
vMove2NextLine(pDiag,
pOutput->tFontRef, pOutput->usFontSize);
RESET_LINE();
}
break;
case PAGE_BREAK:
case COLUMN_FEED:
pSection = pSectionNext;
break;
case TABLE_SEPARATOR:
if (bIsTableRow) {
vStoreCharacter(ulChar, pOutput);
break;
}
vStoreCharacter((ULONG)' ', pOutput);
vStoreCharacter((ULONG)TABLE_SEPARATOR_CHAR, pOutput);
break;
case TAB:
if (bIsTableRow ||
tOptions.eConversionType == conversion_xml) {
vStoreCharacter((ULONG)' ', pOutput);
break;
}
if (tOptions.iParagraphBreak == 0 &&
(tOptions.eConversionType == conversion_text ||
tOptions.eConversionType == conversion_fmt_text)) {
vStoreCharacter(TAB, pOutput);
break;
}
lHalfSpaceWidth = (lComputeSpaceWidth(
pOutput->tFontRef,
pOutput->usFontSize) + 1) / 2;
lTmp = lTotalStringWidth(pAnchor);
lTmp += lDrawUnits2MilliPoints(pDiag->lXleft);
lTmp /= lDefaultTabWidth;
do {
vStoreCharacter((ULONG)FILLER_CHAR, pOutput);
lWidthCurr = lTotalStringWidth(pAnchor);
lWidthCurr +=
lDrawUnits2MilliPoints(pDiag->lXleft);
} while (lTmp == lWidthCurr / lDefaultTabWidth &&
lWidthCurr < lWidthMax + lRightIndentation);
break;
default:
if (bHiddenText && tOptions.bHideHiddenText) {
continue;
}
if (bMarkDelText && tOptions.bRemoveRemovedText) {
continue;
}
if (ulChar == UNICODE_ELLIPSIS &&
tOptions.eEncoding != encoding_utf_8) {
vStoreString("...", 3, pOutput);
} else {
if (bAllCapitals) {
ulChar = ulToUpper(ulChar);
}
vStoreCharacter(ulChar, pOutput);
}
break;
}
if (bWasTableRow && !bIsTableRow) {
vEndOfTable(pDiag);
NO_DBG_MSG("End of table font");
vCloseFont();
bTableFontClosed = TRUE;
pOutput->ucFontColor = ucFontColor;
pOutput->usFontStyle = usFontStyle;
pOutput->usFontSize = usFontSize;
pOutput->tFontRef = tOpenFont(
ucFontNumber, usFontStyle, usFontSize);
}
bWasTableRow = bIsTableRow;
if (bIsTableRow) {
fail(pAnchor != pOutput);
if (!bEndRowNorm && !bEndRowFast) {
continue;
}
if (bEndRowNorm) {
fail(pRowInfo == NULL);
vTableRow2Window(pDiag, pAnchor, pRowInfo,
tOptions.eConversionType,
tOptions.iParagraphBreak);
} else {
fail(!bEndRowFast);
}
pAnchor = pStartNewOutput(pAnchor, NULL);
pOutput = pAnchor;
if (bEndRowNorm) {
pRowInfo = pGetNextRowInfoListItem();
}
bIsTableRow = FALSE;
bEndRowNorm = FALSE;
bEndRowFast = FALSE;
NO_DBG_HEX_C(pRowInfo != NULL,
pRowInfo->ulFileOffsetStart);
NO_DBG_HEX_C(pRowInfo != NULL,
pRowInfo->ulFileOffsetEnd);
continue;
}
lWidthCurr = lTotalStringWidth(pAnchor);
lWidthCurr += lDrawUnits2MilliPoints(pDiag->lXleft);
if (lWidthCurr < lWidthMax + lRightIndentation) {
continue;
}
pLeftOver = pSplitList(pAnchor);
vJustify2Window(pDiag, pAnchor,
lWidthMax, lRightIndentation, ucAlignment);
pAnchor = pStartNewOutput(pAnchor, pLeftOver);
for (pOutput = pAnchor;
pOutput->pNext != NULL;
pOutput = pOutput->pNext)
;
fail(pOutput == NULL);
if (lTotalStringWidth(pAnchor) > 0) {
vSetLeftIndentation(pDiag, lLeftIndentation);
}
}
pAnchor = pStartNewOutput(pAnchor, NULL);
pAnchor->szStorage = xfree(pAnchor->szStorage);
pAnchor = xfree(pAnchor);
vCloseFont();
vFreeDocument();
Hourglass_Off();
return TRUE;
}
static long
lLastStringWidth(const output_type *pAnchor)
{
const output_type *pCurr, *pStart;
pStart = NULL;
for (pCurr = pAnchor; pCurr != NULL; pCurr = pCurr->pNext) {
if (pCurr->tNextFree == 1 &&
(pCurr->szStorage[0] == PAR_END ||
pCurr->szStorage[0] == HARD_RETURN)) {
pStart = pCurr->pNext;
}
}
if (pStart == NULL) {
pStart = pAnchor;
}
return lTotalStringWidth(pStart);
}
output_type *
pHdrFtrDecryptor(FILE *pFile, ULONG ulCharPosStart, ULONG ulCharPosNext)
{
output_type *pAnchor, *pOutput, *pLeftOver;
ULONG ulChar, ulFileOffset, ulCharPos;
long lWidthCurr, lWidthMax;
long lRightIndentation;
USHORT usChar;
UCHAR ucAlignment;
BOOL bSkip;
fail(iWordVersion < 0);
fail(tOptions.eConversionType == conversion_unknown);
fail(tOptions.eEncoding == 0);
if (ulCharPosStart == ulCharPosNext) {
return NULL;
}
lRightIndentation = 0;
ucAlignment = ALIGNMENT_LEFT;
bSkip = FALSE;
lWidthMax = lGetWidthMax(tOptions.iParagraphBreak);
pAnchor = pStartNewOutput(NULL, NULL);
pOutput = pAnchor;
pOutput->tFontRef = tOpenFont(0, FONT_REGULAR, DEFAULT_FONT_SIZE);
usChar = usToHdrFtrPosition(pFile, ulCharPosStart);
ulCharPos = ulCharPosStart;
ulFileOffset = ulCharPos2FileOffset(ulCharPos);
while (usChar != (USHORT)EOF && ulCharPos != ulCharPosNext) {
if (usChar == START_EMBEDDED) {
bSkip = TRUE;
} else if (usChar == END_IGNORE || usChar == END_EMBEDDED) {
bSkip = FALSE;
}
if (bSkip || usChar == END_IGNORE || usChar == END_EMBEDDED) {
ulChar = IGNORE_CHARACTER;
} else {
ulChar = ulTranslateCharacters(usChar,
ulFileOffset,
iWordVersion,
tOptions.eConversionType,
tOptions.eEncoding,
bOldMacFile);
}
if (ulChar != IGNORE_CHARACTER) {
switch (ulChar) {
case PICTURE:
vStoreString("[pic]", 5, pOutput);
break;
case PAR_END:
case HARD_RETURN:
case PAGE_BREAK:
case COLUMN_FEED:
pOutput = pStartNextOutput(pOutput);
vCloseFont();
pOutput->tFontRef = tOpenFont(0,
FONT_REGULAR, DEFAULT_FONT_SIZE);
if (ulChar == HARD_RETURN) {
vStoreCharacter(HARD_RETURN, pOutput);
} else {
vStoreCharacter(PAR_END, pOutput);
}
pOutput = pStartNextOutput(pOutput);
vCloseFont();
pOutput->tFontRef = tOpenFont(0,
FONT_REGULAR, DEFAULT_FONT_SIZE);
fail(!bCheckDoubleLinkedList(pAnchor));
break;
case TABLE_SEPARATOR:
vStoreCharacter((ULONG)' ', pOutput);
vStoreCharacter((ULONG)TABLE_SEPARATOR_CHAR,
pOutput);
break;
case TAB:
vStoreCharacter((ULONG)FILLER_CHAR, pOutput);
break;
default:
vStoreCharacter(ulChar, pOutput);
break;
}
}
lWidthCurr = lLastStringWidth(pAnchor);
if (lWidthCurr >= lWidthMax + lRightIndentation) {
pLeftOver = pSplitList(pAnchor);
for (pOutput = pAnchor;
pOutput->pNext != NULL;
pOutput = pOutput->pNext)
;
fail(pOutput == NULL);
pOutput = pStartNextOutput(pOutput);
vStoreCharacter(HARD_RETURN, pOutput);
pOutput->pNext = pLeftOver;
if (pLeftOver != NULL) {
pLeftOver->pPrev = pOutput;
}
fail(!bCheckDoubleLinkedList(pAnchor));
for (pOutput = pAnchor;
pOutput->pNext != NULL;
pOutput = pOutput->pNext)
;
fail(pOutput == NULL);
}
usChar = usNextChar(pFile, hdrftr_list,
&ulFileOffset, &ulCharPos, NULL);
}
vCloseFont();
if (bOutputContainsText(pAnchor)) {
return pAnchor;
}
pAnchor = pStartNewOutput(pAnchor, NULL);
pAnchor->szStorage = xfree(pAnchor->szStorage);
pAnchor = xfree(pAnchor);
return NULL;
}
char *
szFootnoteDecryptor(FILE *pFile, ULONG ulCharPosStart, ULONG ulCharPosNext)
{
char *szText;
ULONG ulChar, ulFileOffset, ulCharPos;
USHORT usChar;
size_t tLen, tIndex, tNextFree, tStorageSize;
char szResult[6];
BOOL bSkip;
fail(iWordVersion < 0);
fail(tOptions.eConversionType == conversion_unknown);
fail(tOptions.eEncoding == 0);
if (ulCharPosStart == ulCharPosNext) {
return NULL;
}
if (tOptions.eConversionType != conversion_xml) {
return NULL;
}
bSkip = FALSE;
tStorageSize = INITIAL_SIZE;
szText = xmalloc(tStorageSize);
tNextFree = 0;
szText[tNextFree] = '\0';
usChar = usToFootnotePosition(pFile, ulCharPosStart);
ulCharPos = ulCharPosStart;
ulFileOffset = ulCharPos2FileOffset(ulCharPos);
while (usChar != (USHORT)EOF && ulCharPos != ulCharPosNext &&
(usChar == FOOTNOTE_OR_ENDNOTE ||
usChar == PAR_END ||
usChar == TAB ||
usChar == (USHORT)' ')) {
usChar = usNextChar(pFile, footnote_list,
&ulFileOffset, &ulCharPos, NULL);
}
while (usChar != (USHORT)EOF && ulCharPos != ulCharPosNext) {
if (usChar == START_EMBEDDED) {
bSkip = TRUE;
} else if (usChar == END_IGNORE || usChar == END_EMBEDDED) {
bSkip = FALSE;
}
if (bSkip ||
usChar == END_IGNORE ||
usChar == END_EMBEDDED ||
usChar == FOOTNOTE_OR_ENDNOTE) {
ulChar = IGNORE_CHARACTER;
} else {
ulChar = ulTranslateCharacters(usChar,
ulFileOffset,
iWordVersion,
tOptions.eConversionType,
tOptions.eEncoding,
bOldMacFile);
}
if (ulChar == PICTURE) {
tLen = 5;
strcpy(szResult, "[pic]");
} else if (ulChar == IGNORE_CHARACTER) {
tLen = 0;
szResult[0] = '\0';
} else {
switch (ulChar) {
case PAR_END:
case HARD_RETURN:
case PAGE_BREAK:
case COLUMN_FEED:
ulChar = (ULONG)PAR_END;
break;
case TAB:
ulChar = (ULONG)' ';
break;
default:
break;
}
tLen = tUcs2Utf8(ulChar, szResult, sizeof(szResult));
}
if (tNextFree + tLen + 1 > tStorageSize) {
tStorageSize += EXTENTION_SIZE;
szText = xrealloc(szText, tStorageSize);
}
for (tIndex = 0; tIndex < tLen; tIndex++) {
szText[tNextFree++] = szResult[tIndex];
}
szText[tNextFree] = '\0';
usChar = usNextChar(pFile, footnote_list,
&ulFileOffset, &ulCharPos, NULL);
}
while (tNextFree != 0 && szText[tNextFree - 1] == ' ') {
szText[tNextFree - 1] = '\0';
tNextFree--;
}
if (tNextFree == 0) {
szText = xfree(szText);
return NULL;
}
return szText;
}