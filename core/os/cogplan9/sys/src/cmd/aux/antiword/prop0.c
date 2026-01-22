#include <string.h>
#include <time.h>
#include "antiword.h"
static time_t
tConvertDosDate(const char *szDosDate)
{
struct tm tTime;
const char *pcTmp;
time_t tResult;
memset(&tTime, 0, sizeof(tTime));
pcTmp = szDosDate;
if (!isdigit(*pcTmp)) {
return (time_t)-1;
}
tTime.tm_mon = (int)(*pcTmp - '0');
pcTmp++;
if (isdigit(*pcTmp)) {
tTime.tm_mon *= 10;
tTime.tm_mon += (int)(*pcTmp - '0');
pcTmp++;
}
if (isalnum(*pcTmp)) {
return (time_t)-1;
}
pcTmp++;
if (!isdigit(*pcTmp)) {
return (time_t)-1;
}
tTime.tm_mday = (int)(*pcTmp - '0');
pcTmp++;
if (isdigit(*pcTmp)) {
tTime.tm_mday *= 10;
tTime.tm_mday += (int)(*pcTmp - '0');
pcTmp++;
}
if (isalnum(*pcTmp)) {
return (time_t)-1;
}
pcTmp++;
if (!isdigit(*pcTmp)) {
return (time_t)-1;
}
tTime.tm_year = (int)(*pcTmp - '0');
pcTmp++;
if (isdigit(*pcTmp)) {
tTime.tm_year *= 10;
tTime.tm_year += (int)(*pcTmp - '0');
pcTmp++;
}
if (tTime.tm_mon == 0 || tTime.tm_mday == 0 || tTime.tm_mday > 31) {
return (time_t)-1;
}
tTime.tm_mon--;
if (tTime.tm_year < 80) {
tTime.tm_year += 100;
}
tTime.tm_isdst = -1;
tResult = mktime(&tTime);
NO_DBG_MSG(ctime(&tResult));
return tResult;
}
void
vGet0DopInfo(FILE *pFile, const UCHAR *aucHeader)
{
document_block_type tDocument;
UCHAR *aucBuffer;
ULONG ulBeginSumdInfo, ulBeginNextBlock;
size_t tLen;
USHORT usOffset;
tDocument.ucHdrFtrSpecification = 0;
tDocument.usDefaultTabWidth = usGetWord(0x70, aucHeader);
tDocument.tCreateDate = (time_t)-1;
tDocument.tRevisedDate = (time_t)-1;
ulBeginSumdInfo = 128 * (ULONG)usGetWord(0x1c, aucHeader);
DBG_HEX(ulBeginSumdInfo);
ulBeginNextBlock = 128 * (ULONG)usGetWord(0x6a, aucHeader);
DBG_HEX(ulBeginNextBlock);
if (ulBeginSumdInfo < ulBeginNextBlock && ulBeginNextBlock != 0) {
tLen = (size_t)(ulBeginNextBlock - ulBeginSumdInfo);
aucBuffer = xmalloc(tLen);
if (bReadBytes(aucBuffer, tLen, ulBeginSumdInfo, pFile)) {
usOffset = usGetWord(12, aucBuffer);
if (aucBuffer[usOffset] != 0) {
NO_DBG_STRN(aucBuffer + usOffset, 8);
tDocument.tRevisedDate =
tConvertDosDate((char *)aucBuffer + usOffset);
}
usOffset = usGetWord(14, aucBuffer);
if (aucBuffer[usOffset] != 0) {
NO_DBG_STRN(aucBuffer + usOffset, 8);
tDocument.tCreateDate =
tConvertDosDate((char *)aucBuffer + usOffset);
}
}
aucBuffer = xfree(aucBuffer);
}
vCreateDocumentInfoList(&tDocument);
}
static void
vGet0SectionInfo(const UCHAR *aucGrpprl, size_t tBytes,
section_block_type *pSection)
{
USHORT usCcol;
UCHAR ucTmp;
fail(aucGrpprl == NULL || pSection == NULL);
if (tBytes < 2) {
return;
}
ucTmp = ucGetByte(1, aucGrpprl);
DBG_HEX(ucTmp);
ucTmp &= 0x07;
DBG_HEX(ucTmp);
pSection->bNewPage = ucTmp != 0 && ucTmp != 1;
if (tBytes < 18) {
return;
}
usCcol = (USHORT)ucGetByte(17, aucGrpprl);
DBG_DEC(usCcol);
}
void
vGet0SepInfo(FILE *pFile, const UCHAR *aucHeader)
{
section_block_type tSection;
UCHAR *aucBuffer;
ULONG ulBeginOfText, ulTextOffset, ulBeginSectInfo;
ULONG ulCharPos, ulSectPage, ulBeginNextBlock;
size_t tSectInfoLen, tIndex, tSections, tBytes;
UCHAR aucTmp[2], aucFpage[35];
fail(pFile == NULL || aucHeader == NULL);
ulBeginOfText = 128;
NO_DBG_HEX(ulBeginOfText);
ulBeginSectInfo = 128 * (ULONG)usGetWord(0x18, aucHeader);
DBG_HEX(ulBeginSectInfo);
ulBeginNextBlock = 128 * (ULONG)usGetWord(0x1a, aucHeader);
DBG_HEX(ulBeginNextBlock);
if (ulBeginSectInfo == ulBeginNextBlock) {
return;
}
if (!bReadBytes(aucTmp, 2, ulBeginSectInfo, pFile)) {
return;
}
tSections = (size_t)usGetWord(0, aucTmp);
NO_DBG_DEC(tSections);
tSectInfoLen = 10 * tSections;
NO_DBG_DEC(tSectInfoLen);
aucBuffer = xmalloc(tSectInfoLen);
if (!bReadBytes(aucBuffer, tSectInfoLen, ulBeginSectInfo + 4, pFile)) {
aucBuffer = xfree(aucBuffer);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tSectInfoLen);
for (tIndex = 0; tIndex < tSections; tIndex++) {
ulTextOffset = ulGetLong(10 * tIndex, aucBuffer);
NO_DBG_HEX(ulTextOffset);
ulCharPos = ulBeginOfText + ulTextOffset;
NO_DBG_HEX(ulTextOffset);
ulSectPage = ulGetLong(10 * tIndex + 6, aucBuffer);
NO_DBG_HEX(ulSectPage);
if (ulSectPage == FC_INVALID ||
ulSectPage < 128 ||
ulSectPage >= ulBeginSectInfo) {
DBG_HEX_C(ulSectPage != FC_INVALID, ulSectPage);
vDefault2SectionInfoList(ulCharPos);
continue;
}
if (!bReadBytes(aucTmp, 1, ulSectPage, pFile)) {
continue;
}
tBytes = 1 + (size_t)ucGetByte(0, aucTmp);
NO_DBG_DEC(tBytes);
if (tBytes > sizeof(aucFpage)) {
DBG_DEC(tBytes);
tBytes = sizeof(aucFpage);
}
if (!bReadBytes(aucFpage, tBytes, ulSectPage, pFile)) {
continue;
}
NO_DBG_PRINT_BLOCK(aucFpage, tBytes);
vGetDefaultSection(&tSection);
vGet0SectionInfo(aucFpage + 1, tBytes - 1, &tSection);
vAdd2SectionInfoList(&tSection, ulCharPos);
}
aucBuffer = xfree(aucBuffer);
}
static void
vGet0StyleInfo(int iFodo, const UCHAR *aucGrpprl, style_block_type *pStyle)
{
int iBytes;
UCHAR ucTmp;
fail(iFodo <= 0 || aucGrpprl == NULL || pStyle == NULL);
pStyle->usIstdNext = ISTD_NORMAL;
iBytes = (int)ucGetByte(iFodo, aucGrpprl);
if (iBytes < 1) {
return;
}
ucTmp = ucGetByte(iFodo + 1, aucGrpprl);
if ((ucTmp & BIT(0)) != 0) {
ucTmp >>= 1;
if (ucTmp >= 88 && ucTmp <= 94) {
pStyle->usIstd = ucTmp - 87;
pStyle->ucNumLevel = 1;
}
}
if (iBytes < 2) {
return;
}
ucTmp = ucGetByte(iFodo + 2, aucGrpprl);
pStyle->ucAlignment = ucTmp & 0x02;
if (iBytes < 3) {
return;
}
ucTmp = ucGetByte(iFodo + 3, aucGrpprl);
ucTmp &= 0x7f;
if (ucTmp >= 88 && ucTmp <= 94) {
pStyle->usIstd = ucTmp - 87;
pStyle->ucNumLevel = 1;
}
if (iBytes < 6) {
return;
}
pStyle->sRightIndent = (short)usGetWord(iFodo + 5, aucGrpprl);
NO_DBG_DEC(pStyle->sRightIndent);
if (iBytes < 8) {
return;
}
pStyle->sLeftIndent = (short)usGetWord(iFodo + 7, aucGrpprl);
NO_DBG_DEC(pStyle->sLeftIndent);
if (iBytes < 10) {
return;
}
pStyle->sLeftIndent1 = (short)usGetWord(iFodo + 9, aucGrpprl);
NO_DBG_DEC(pStyle->sLeftIndent1);
if (iBytes < 14) {
return;
}
pStyle->usBeforeIndent = usGetWord(iFodo + 13, aucGrpprl);
NO_DBG_DEC(pStyle->usBeforeIndent);
if (iBytes < 16) {
return;
}
pStyle->usAfterIndent = usGetWord(iFodo + 15, aucGrpprl);
NO_DBG_DEC(pStyle->usAfterIndent);
}
void
vGet0PapInfo(FILE *pFile, const UCHAR *aucHeader)
{
style_block_type tStyle;
ULONG ulBeginParfInfo, ulCharPos, ulCharPosNext;
int iIndex, iRun, iFodo;
UCHAR aucFpage[128];
fail(pFile == NULL || aucHeader == NULL);
ulBeginParfInfo = 128 * (ULONG)usGetWord(0x12, aucHeader);
NO_DBG_HEX(ulBeginParfInfo);
do {
if (!bReadBytes(aucFpage, 128, ulBeginParfInfo, pFile)) {
return;
}
NO_DBG_PRINT_BLOCK(aucFpage, 128);
ulCharPosNext = ulGetLong(0, aucFpage);
iRun = (int)ucGetByte(0x7f, aucFpage);
NO_DBG_DEC(iRun);
for (iIndex = 0; iIndex < iRun; iIndex++) {
iFodo = (int)usGetWord(6 * iIndex + 8, aucFpage);
if (iFodo <= 0 || iFodo > 0x79) {
DBG_DEC_C(iFodo != (int)0xffff, iFodo);
continue;
}
vFillStyleFromStylesheet(0, &tStyle);
vGet0StyleInfo(iFodo, aucFpage + 4, &tStyle);
ulCharPos = ulCharPosNext;
ulCharPosNext = ulGetLong(6 * iIndex + 4, aucFpage);
tStyle.ulFileOffset = ulCharPos;
vAdd2StyleInfoList(&tStyle);
}
ulBeginParfInfo += 128;
} while (ulCharPosNext == ulBeginParfInfo);
}
static void
vGet0FontInfo(int iFodo, const UCHAR *aucGrpprl, font_block_type *pFont)
{
int iBytes;
UCHAR ucTmp;
fail(iFodo <= 0 || aucGrpprl == NULL || pFont == NULL);
iBytes = (int)ucGetByte(iFodo, aucGrpprl);
if (iBytes < 2) {
return;
}
ucTmp = ucGetByte(iFodo + 2, aucGrpprl);
if ((ucTmp & BIT(0)) != 0) {
pFont->usFontStyle |= FONT_BOLD;
}
if ((ucTmp & BIT(1)) != 0) {
pFont->usFontStyle |= FONT_ITALIC;
}
pFont->ucFontNumber = ucTmp >> 2;
NO_DBG_DEC(pFont->ucFontNumber);
if (iBytes < 3) {
return;
}
pFont->usFontSize = (USHORT)ucGetByte(iFodo + 3, aucGrpprl);
NO_DBG_DEC(pFont->usFontSize);
if (iBytes < 4) {
return;
}
ucTmp = ucGetByte(iFodo + 4, aucGrpprl);
if ((ucTmp & BIT(0)) != 0 || (ucTmp & BIT(2)) != 0) {
pFont->usFontStyle |= FONT_UNDERLINE;
}
if ((ucTmp & BIT(1)) != 0) {
pFont->usFontStyle |= FONT_STRIKE;
}
if ((ucTmp & BIT(4)) != 0) {
pFont->usFontStyle |= FONT_CAPITALS;
}
if ((ucTmp & BIT(5)) != 0) {
pFont->usFontStyle |= FONT_SMALL_CAPITALS;
}
if ((ucTmp & BIT(7)) != 0) {
pFont->usFontStyle |= FONT_HIDDEN;
}
DBG_HEX(pFont->usFontStyle);
if (iBytes < 6) {
return;
}
ucTmp = ucGetByte(iFodo + 6, aucGrpprl);
if (ucTmp != 0) {
if (ucTmp < 128) {
pFont->usFontStyle |= FONT_SUPERSCRIPT;
DBG_MSG("Superscript");
} else {
pFont->usFontStyle |= FONT_SUBSCRIPT;
DBG_MSG("Subscript");
}
}
if (iBytes < 7) {
return;
}
ucTmp = ucGetByte(iFodo + 7, aucGrpprl);
switch (ucTmp & 0x07) {
case 0: pFont->ucFontColor = FONT_COLOR_BLACK; break;
case 1: pFont->ucFontColor = FONT_COLOR_RED; break;
case 2: pFont->ucFontColor = FONT_COLOR_GREEN; break;
case 3: pFont->ucFontColor = FONT_COLOR_BLUE; break;
case 4: pFont->ucFontColor = FONT_COLOR_CYAN; break;
case 5: pFont->ucFontColor = FONT_COLOR_MAGENTA; break;
case 6: pFont->ucFontColor = FONT_COLOR_YELLOW; break;
case 7: pFont->ucFontColor = FONT_COLOR_WHITE; break;
default:pFont->ucFontColor = FONT_COLOR_BLACK; break;
}
NO_DBG_DEC(pFont->ucFontColor);
}
void
vGet0ChrInfo(FILE *pFile, const UCHAR *aucHeader)
{
font_block_type tFont;
ULONG ulBeginCharInfo, ulCharPos, ulCharPosNext;
int iIndex, iRun, iFodo;
UCHAR aucFpage[128];
fail(pFile == NULL || aucHeader == NULL);
ulBeginCharInfo = ulGetLong(0x0e, aucHeader);
NO_DBG_HEX(ulBeginCharInfo);
ulBeginCharInfo = ROUND128(ulBeginCharInfo);
NO_DBG_HEX(ulBeginCharInfo);
do {
if (!bReadBytes(aucFpage, 128, ulBeginCharInfo, pFile)) {
return;
}
NO_DBG_PRINT_BLOCK(aucFpage, 128);
ulCharPosNext = ulGetLong(0, aucFpage);
iRun = (int)ucGetByte(0x7f, aucFpage);
NO_DBG_DEC(iRun);
for (iIndex = 0; iIndex < iRun; iIndex++) {
iFodo = (int)usGetWord(6 * iIndex + 8, aucFpage);
if (iFodo <= 0 || iFodo > 0x79) {
DBG_DEC_C(iFodo != (int)0xffff, iFodo);
continue;
}
vFillFontFromStylesheet(0, &tFont);
vGet0FontInfo(iFodo, aucFpage + 4, &tFont);
ulCharPos = ulCharPosNext;
ulCharPosNext = ulGetLong(6 * iIndex + 4, aucFpage);
tFont.ulFileOffset = ulCharPos;
vAdd2FontInfoList(&tFont);
}
ulBeginCharInfo += 128;
} while (ulCharPosNext == ulBeginCharInfo);
}