#include <ctype.h>
#include <string.h>
#include "antiword.h"
#define FONT_LINE_LENGTH	81
#define PITCH_UNKNOWN		0
#define PITCH_FIXED		1
#define PITCH_VARIABLE		2
#define FAMILY_UNKNOWN		0
#define FAMILY_ROMAN		1
#define FAMILY_SWISS		2
#define FAMILY_MODERN		3
#define FAMILY_SCRIPT		4
#define FAMILY_DECORATIVE	5
static size_t		tFontTableRecords = 0;
static font_table_type	*pFontTable = NULL;
int
iGetFontByNumber(UCHAR ucWordFontNumber, USHORT usFontStyle)
{
int	iIndex;
for (iIndex = 0; iIndex < (int)tFontTableRecords; iIndex++) {
if (ucWordFontNumber == pFontTable[iIndex].ucWordFontNumber &&
usFontStyle == pFontTable[iIndex].usFontStyle &&
pFontTable[iIndex].szOurFontname[0] != '\0') {
return iIndex;
}
}
DBG_DEC(ucWordFontNumber);
DBG_HEX(usFontStyle);
return -1;
}
const char *
szGetOurFontname(int iIndex)
{
if (iIndex < 0 || iIndex >= (int)tFontTableRecords) {
return NULL;
}
return pFontTable[iIndex].szOurFontname;
}
int
iFontname2Fontnumber(const char *szOurFontname, USHORT usFontStyle)
{
int	iIndex;
for (iIndex = 0; iIndex < (int)tFontTableRecords; iIndex++) {
if (pFontTable[iIndex].usFontStyle == usFontStyle &&
STREQ(pFontTable[iIndex].szOurFontname, szOurFontname)) {
return (int)pFontTable[iIndex].ucWordFontNumber;
}
}
return -1;
}
static const char *
szGetDefaultFont(UCHAR ucFFN, int iEmphasis)
{
UCHAR	ucPrq, ucFf;
fail(iEmphasis < 0 || iEmphasis > 3);
ucPrq = ucFFN & 0x03;
ucFf = (ucFFN & 0x70) >> 4;
NO_DBG_DEC(ucPrq);
NO_DBG_DEC(ucFf);
if (ucPrq == PITCH_FIXED) {
switch (iEmphasis) {
case 1: return FONT_MONOSPACED_BOLD;
case 2: return FONT_MONOSPACED_ITALIC;
case 3: return FONT_MONOSPACED_BOLDITALIC;
default: return FONT_MONOSPACED_PLAIN;
}
} else if (ucFf == FAMILY_ROMAN) {
switch (iEmphasis) {
case 1: return FONT_SERIF_BOLD;
case 2: return FONT_SERIF_ITALIC;
case 3: return FONT_SERIF_BOLDITALIC;
default: return FONT_SERIF_PLAIN;
}
} else if (ucFf == FAMILY_SWISS) {
switch (iEmphasis) {
case 1: return FONT_SANS_SERIF_BOLD;
case 2: return FONT_SANS_SERIF_ITALIC;
case 3: return FONT_SANS_SERIF_BOLDITALIC;
default: return FONT_SANS_SERIF_PLAIN;
}
} else {
switch (iEmphasis) {
case 1: return FONT_SERIF_BOLD;
case 2: return FONT_SERIF_ITALIC;
case 3: return FONT_SERIF_BOLDITALIC;
default: return FONT_SERIF_PLAIN;
}
}
}
static BOOL
bFontEqual(const UCHAR *aucWord, const char *szTable, int iBytesPerChar)
{
const UCHAR	*pucTmp;
const char	*pcTmp;
fail(aucWord == NULL || szTable == NULL);
fail(iBytesPerChar != 1 && iBytesPerChar != 2);
for (pucTmp = aucWord, pcTmp = szTable;
*pucTmp != 0;
pucTmp += iBytesPerChar, pcTmp++) {
if (ulToUpper((ULONG)*pucTmp) !=
ulToUpper((ULONG)(UCHAR)*pcTmp)) {
return FALSE;
}
}
return *pcTmp == '\0';
}
static void
vFontname2Table(const UCHAR *aucFont, const UCHAR *aucAltFont,
int iBytesPerChar, int iEmphasis, UCHAR ucFFN,
const char *szWordFont, const char *szOurFont,
font_table_type *pFontTableRecord)
{
BOOL	bMatchFound;
fail(aucFont == NULL || aucFont[0] == 0);
fail(aucAltFont != NULL && aucAltFont[0] == 0);
fail(iBytesPerChar != 1 && iBytesPerChar != 2);
fail(iEmphasis < 0 || iEmphasis > 3);
fail(szWordFont == NULL || szWordFont[0] == '\0');
fail(szOurFont == NULL || szOurFont[0] == '\0');
fail(pFontTableRecord == NULL);
bMatchFound = bFontEqual(aucFont, szWordFont, iBytesPerChar);
if (!bMatchFound && aucAltFont != NULL) {
bMatchFound = bFontEqual(aucAltFont, szWordFont, iBytesPerChar);
}
if (!bMatchFound &&
pFontTableRecord->szWordFontname[0] == '\0' &&
szWordFont[0] == '*' &&
szWordFont[1] == '\0') {
szOurFont = szGetDefaultFont(ucFFN, iEmphasis);
bMatchFound = TRUE;
}
if (bMatchFound) {
switch (iBytesPerChar) {
case 1:
(void)strncpy(pFontTableRecord->szWordFontname,
(const char *)aucFont,
sizeof(pFontTableRecord->szWordFontname) - 1);
break;
case 2:
(void)unincpy(pFontTableRecord->szWordFontname,
aucFont,
sizeof(pFontTableRecord->szWordFontname) - 1);
break;
default:
DBG_FIXME();
pFontTableRecord->szWordFontname[0] = '\0';
break;
}
pFontTableRecord->szWordFontname[
sizeof(pFontTableRecord->szWordFontname) - 1] = '\0';
(void)strncpy(pFontTableRecord->szOurFontname, szOurFont,
sizeof(pFontTableRecord->szOurFontname) - 1);
pFontTableRecord->szOurFontname[
sizeof(pFontTableRecord->szOurFontname) - 1] = '\0';
NO_DBG_MSG(pFontTableRecord->szWordFontname);
NO_DBG_MSG(pFontTableRecord->szOurFontname);
pFontTableRecord->ucFFN = ucFFN;
pFontTableRecord->ucEmphasis = (UCHAR)iEmphasis;
}
}
static void
vCreateFontTable(void)
{
font_table_type	*pTmp;
int	iNbr;
if (tFontTableRecords == 0) {
pFontTable = xfree(pFontTable);
return;
}
pFontTable = xcalloc(tFontTableRecords, sizeof(*pFontTable));
for (iNbr = 0, pTmp = pFontTable;
pTmp < pFontTable + tFontTableRecords;
iNbr++, pTmp++) {
pTmp->ucWordFontNumber = (UCHAR)(iNbr / 4);
switch (iNbr % 4) {
case 0:
pTmp->usFontStyle = FONT_REGULAR;
break;
case 1:
pTmp->usFontStyle = FONT_BOLD;
break;
case 2:
pTmp->usFontStyle = FONT_ITALIC;
break;
case 3:
pTmp->usFontStyle = FONT_BOLD|FONT_ITALIC;
break;
default:
DBG_DEC(iNbr);
break;
}
}
}
static void
vMinimizeFontTable(void)
{
font_block_type		tFontNext;
const style_block_type	*pStyle;
const font_block_type	*pFont;
font_table_type		*pTmp;
int	iUnUsed;
BOOL	bMustAddTableFont;
NO_DBG_MSG("vMinimizeFontTable");
if (tFontTableRecords == 0) {
pFontTable = xfree(pFontTable);
return;
}
bMustAddTableFont = TRUE;
#if 0
DBG_MSG("Before");
DBG_DEC(tFontTableRecords);
for (pTmp = pFontTable;
pTmp < pFontTable + tFontTableRecords;
pTmp++) {
DBG_DEC(pTmp->ucWordFontNumber);
DBG_HEX(pTmp->usFontStyle);
DBG_MSG(pTmp->szWordFontname);
DBG_MSG(pTmp->szOurFontname);
}
#endif
pFontTable[0].ucInUse = 1;
pFont = NULL;
while((pFont = pGetNextFontInfoListItem(pFont)) != NULL) {
pTmp = pFontTable + 4 * (int)pFont->ucFontNumber;
if (bIsBold(pFont->usFontStyle)) {
pTmp++;
}
if (bIsItalic(pFont->usFontStyle)) {
pTmp += 2;
}
if (pTmp >= pFontTable + tFontTableRecords) {
continue;
}
if (STREQ(pTmp->szOurFontname, TABLE_FONT)) {
bMustAddTableFont = FALSE;
}
pTmp->ucInUse = 1;
}
pStyle = NULL;
while((pStyle = pGetNextStyleInfoListItem(pStyle)) != NULL) {
vFillFontFromStylesheet(pStyle->usIstdNext, &tFontNext);
vCorrectFontValues(&tFontNext);
pTmp = pFontTable + 4 * (int)tFontNext.ucFontNumber;
if (bIsBold(tFontNext.usFontStyle)) {
pTmp++;
}
if (bIsItalic(tFontNext.usFontStyle)) {
pTmp += 2;
}
if (pTmp >= pFontTable + tFontTableRecords) {
continue;
}
if (STREQ(pTmp->szOurFontname, TABLE_FONT)) {
bMustAddTableFont = FALSE;
}
pTmp->ucInUse = 1;
}
iUnUsed = 0;
for (pTmp = pFontTable;
pTmp < pFontTable + tFontTableRecords;
pTmp++) {
if (pTmp->ucInUse == 0) {
iUnUsed++;
continue;
}
if (iUnUsed > 0) {
fail(pTmp - iUnUsed <= pFontTable);
*(pTmp - iUnUsed) = *pTmp;
}
}
fail(iUnUsed < 0);
fail(tFontTableRecords <= (size_t)iUnUsed);
tFontTableRecords -= (size_t)iUnUsed;
if (bMustAddTableFont) {
pTmp = pFontTable + tFontTableRecords;
fail(pTmp <= pFontTable);
pTmp->ucWordFontNumber = (pTmp - 1)->ucWordFontNumber + 1;
pTmp->usFontStyle = FONT_REGULAR;
pTmp->ucInUse = 1;
strcpy(pTmp->szWordFontname, "Extra Table Font");
strcpy(pTmp->szOurFontname, TABLE_FONT);
tFontTableRecords++;
iUnUsed--;
}
if (iUnUsed > 0) {
pFontTable = xrealloc(pFontTable,
tFontTableRecords * sizeof(*pFontTable));
}
#if defined(DEBUG)
DBG_MSG("After");
DBG_DEC(tFontTableRecords);
for (pTmp = pFontTable;
pTmp < pFontTable + tFontTableRecords;
pTmp++) {
DBG_DEC(pTmp->ucWordFontNumber);
DBG_HEX(pTmp->usFontStyle);
DBG_MSG(pTmp->szWordFontname);
DBG_MSG(pTmp->szOurFontname);
}
#endif
}
static BOOL
bReadFontFile(FILE *pFontTableFile, char *szWordFont,
int *piItalic, int *piBold, char *szOurFont, int *piSpecial)
{
char	*pcTmp;
int	iFields;
char	szLine[FONT_LINE_LENGTH];
fail(szWordFont == NULL || szOurFont == NULL);
fail(piItalic == NULL || piBold == NULL || piSpecial == NULL);
while (fgets(szLine, (int)sizeof(szLine), pFontTableFile) != NULL) {
if (szLine[0] == '#' ||
szLine[0] == '\n' ||
szLine[0] == '\r') {
continue;
}
iFields = sscanf(szLine, "%[^,],%d,%d,%1s%[^,],%d",
szWordFont, piItalic, piBold,
&szOurFont[0], &szOurFont[1], piSpecial);
if (iFields != 6) {
pcTmp = strchr(szLine, '\r');
if (pcTmp != NULL) {
*pcTmp = '\0';
}
pcTmp = strchr(szLine, '\n');
if (pcTmp != NULL) {
*pcTmp = '\0';
}
DBG_DEC(iFields);
werr(0, "Syntax error in: '%s'", szLine);
continue;
}
if (strlen(szWordFont) >=
sizeof(pFontTable[0].szWordFontname)) {
werr(0, "Word fontname too long: '%s'", szWordFont);
continue;
}
if (strlen(szOurFont) >=
sizeof(pFontTable[0].szOurFontname)) {
werr(0, "Local fontname too long: '%s'", szOurFont);
continue;
}
return TRUE;
}
return FALSE;
}
void
vCreate0FontTable(void)
{
FILE	*pFontTableFile;
font_table_type	*pTmp;
UCHAR	*aucFont;
int	iBold, iItalic, iSpecial, iEmphasis, iFtc;
UCHAR	ucPrq, ucFf, ucFFN;
char	szWordFont[FONT_LINE_LENGTH], szOurFont[FONT_LINE_LENGTH];
tFontTableRecords = 0;
pFontTable = xfree(pFontTable);
pFontTableFile = pOpenFontTableFile();
if (pFontTableFile == NULL) {
return;
}
tFontTableRecords = 64;
tFontTableRecords *= 4;
tFontTableRecords++;
vCreateFontTable();
iItalic = 0;
iBold = 0;
iSpecial = 0;
while (bReadFontFile(pFontTableFile, szWordFont,
&iItalic, &iBold, szOurFont, &iSpecial)) {
iEmphasis = 0;
if (iBold != 0) {
iEmphasis++;
}
if (iItalic != 0) {
iEmphasis += 2;
}
for (iFtc = 0, pTmp = pFontTable + iEmphasis;
pTmp < pFontTable + tFontTableRecords;
iFtc++, pTmp += 4) {
if (iFtc >= 16 && iFtc <= 55) {
ucPrq = PITCH_VARIABLE;
ucFf = FAMILY_ROMAN;
aucFont = (UCHAR *)"Times";
} else {
ucPrq = PITCH_FIXED;
ucFf = FAMILY_MODERN;
aucFont = (UCHAR *)"Courier";
}
ucFFN = (ucFf << 4) | ucPrq;
vFontname2Table(aucFont, NULL, 1, iEmphasis,
ucFFN, szWordFont, szOurFont, pTmp);
}
}
(void)fclose(pFontTableFile);
vMinimizeFontTable();
}
void
vCreate2FontTable(FILE *pFile, int iWordVersion, const UCHAR *aucHeader)
{
FILE	*pFontTableFile;
font_table_type	*pTmp;
UCHAR	*aucFont;
UCHAR	*aucBuffer;
ULONG	ulBeginFontInfo;
size_t	tFontInfoLen;
int	iPos, iOff, iRecLen;
int	iBold, iItalic, iSpecial, iEmphasis;
UCHAR	ucFFN;
char	szWordFont[FONT_LINE_LENGTH], szOurFont[FONT_LINE_LENGTH];
fail(pFile == NULL || aucHeader == NULL);
fail(iWordVersion != 1 && iWordVersion != 2);
tFontTableRecords = 0;
pFontTable = xfree(pFontTable);
pFontTableFile = pOpenFontTableFile();
if (pFontTableFile == NULL) {
return;
}
ulBeginFontInfo = ulGetLong(0xb2, aucHeader);
DBG_HEX(ulBeginFontInfo);
tFontInfoLen = (size_t)usGetWord(0xb6, aucHeader);
DBG_DEC(tFontInfoLen);
if (ulBeginFontInfo > (ULONG)LONG_MAX || tFontInfoLen == 0) {
DBG_HEX_C(tFontInfoLen != 0, ulBeginFontInfo);
(void)fclose(pFontTableFile);
return;
}
aucBuffer = xmalloc(tFontInfoLen);
if (!bReadBytes(aucBuffer, tFontInfoLen, ulBeginFontInfo, pFile)) {
aucBuffer = xfree(aucBuffer);
(void)fclose(pFontTableFile);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tFontInfoLen);
DBG_DEC(usGetWord(0, aucBuffer));
if (iWordVersion == 1) {
fail(tFontInfoLen < 2);
tFontTableRecords = 3;
iOff = 2;
} else {
fail(tFontInfoLen < 6);
tFontTableRecords = 0;
iOff = 3;
}
iPos = 2;
while (iPos + iOff < (int)tFontInfoLen) {
iRecLen = (int)ucGetByte(iPos, aucBuffer);
NO_DBG_DEC(iRecLen);
NO_DBG_MSG(aucBuffer + iPos + iOff);
iPos += iRecLen + 1;
tFontTableRecords++;
}
tFontTableRecords *= 4;
tFontTableRecords++;
vCreateFontTable();
if (iWordVersion == 1) {
fail(tFontTableRecords < 13);
vFontname2Table((UCHAR *)"Tms Rmn", NULL, 1, 0,
(UCHAR)((FAMILY_ROMAN << 4) | PITCH_VARIABLE),
"*", "Times-Roman", pFontTable + 0);
vFontname2Table((UCHAR *)"Tms Rmn", NULL, 1, 1,
(UCHAR)((FAMILY_ROMAN << 4) | PITCH_VARIABLE),
"*", "Times-Bold", pFontTable + 1);
vFontname2Table((UCHAR *)"Tms Rmn", NULL, 1, 2,
(UCHAR)((FAMILY_ROMAN << 4) | PITCH_VARIABLE),
"*", "Times-Italic", pFontTable + 2);
vFontname2Table((UCHAR *)"Tms Rmn", NULL, 1, 3,
(UCHAR)((FAMILY_ROMAN << 4) | PITCH_VARIABLE),
"*", "Times-BoldItalic", pFontTable + 3);
vFontname2Table((UCHAR *)"Symbol", NULL, 1, 0,
(UCHAR)((FAMILY_ROMAN << 4) | PITCH_VARIABLE),
"*", "Times-Roman", pFontTable + 4);
vFontname2Table((UCHAR *)"Symbol", NULL, 1, 1,
(UCHAR)((FAMILY_ROMAN << 4) | PITCH_VARIABLE),
"*", "Times-Bold", pFontTable + 5);
vFontname2Table((UCHAR *)"Symbol", NULL, 1, 2,
(UCHAR)((FAMILY_ROMAN << 4) | PITCH_VARIABLE),
"*", "Times-Italic", pFontTable + 6);
vFontname2Table((UCHAR *)"Symbol", NULL, 1, 3,
(UCHAR)((FAMILY_ROMAN << 4) | PITCH_VARIABLE),
"*", "Times-BoldItalic", pFontTable + 7);
vFontname2Table((UCHAR *)"Helv", NULL, 1, 0,
(UCHAR)((FAMILY_SWISS << 4) | PITCH_VARIABLE),
"*", "Helvetica", pFontTable + 8);
vFontname2Table((UCHAR *)"Helv", NULL, 1, 1,
(UCHAR)((FAMILY_SWISS << 4) | PITCH_VARIABLE),
"*", "Helvetica-Bold", pFontTable + 9);
vFontname2Table((UCHAR *)"Helv", NULL, 1, 2,
(UCHAR)((FAMILY_SWISS << 4) | PITCH_VARIABLE),
"*", "Helvetica-Oblique", pFontTable + 10);
vFontname2Table((UCHAR *)"Helv", NULL, 1, 3,
(UCHAR)((FAMILY_SWISS << 4) | PITCH_VARIABLE),
"*", "Helvetica-BoldOblique", pFontTable + 11);
}
iItalic = 0;
iBold = 0;
iSpecial = 0;
while (bReadFontFile(pFontTableFile, szWordFont,
&iItalic, &iBold, szOurFont, &iSpecial)) {
iEmphasis = 0;
if (iBold != 0) {
iEmphasis++;
}
if (iItalic != 0) {
iEmphasis += 2;
}
pTmp = pFontTable + iEmphasis;
iPos = 2;
while (iPos + iOff < (int)tFontInfoLen) {
iRecLen = (int)ucGetByte(iPos, aucBuffer);
ucFFN = ucGetByte(iPos + 1, aucBuffer);
aucFont = aucBuffer + iPos + iOff;
vFontname2Table(aucFont, NULL, 1, iEmphasis,
ucFFN, szWordFont, szOurFont, pTmp);
pTmp += 4;
iPos += iRecLen + 1;
}
}
(void)fclose(pFontTableFile);
aucBuffer = xfree(aucBuffer);
vMinimizeFontTable();
}
void
vCreate6FontTable(FILE *pFile, ULONG ulStartBlock,
const ULONG *aulBBD, size_t tBBDLen,
const UCHAR *aucHeader)
{
FILE	*pFontTableFile;
font_table_type	*pTmp;
UCHAR	*aucFont, *aucAltFont;
UCHAR	*aucBuffer;
ULONG	ulBeginFontInfo;
size_t	tFontInfoLen;
int	iPos, iRecLen, iOffsetAltName;
int	iBold, iItalic, iSpecial, iEmphasis;
UCHAR	ucFFN;
char	szWordFont[FONT_LINE_LENGTH], szOurFont[FONT_LINE_LENGTH];
fail(pFile == NULL || aucHeader == NULL);
fail(ulStartBlock > MAX_BLOCKNUMBER && ulStartBlock != END_OF_CHAIN);
fail(aulBBD == NULL);
tFontTableRecords = 0;
pFontTable = xfree(pFontTable);
pFontTableFile = pOpenFontTableFile();
if (pFontTableFile == NULL) {
return;
}
ulBeginFontInfo = ulGetLong(0xd0, aucHeader);
DBG_HEX(ulBeginFontInfo);
tFontInfoLen = (size_t)ulGetLong(0xd4, aucHeader);
DBG_DEC(tFontInfoLen);
fail(tFontInfoLen < 9);
aucBuffer = xmalloc(tFontInfoLen);
if (!bReadBuffer(pFile, ulStartBlock,
aulBBD, tBBDLen, BIG_BLOCK_SIZE,
aucBuffer, ulBeginFontInfo, tFontInfoLen)) {
aucBuffer = xfree(aucBuffer);
(void)fclose(pFontTableFile);
return;
}
DBG_DEC(usGetWord(0, aucBuffer));
tFontTableRecords = 0;
iPos = 2;
while (iPos + 6 < (int)tFontInfoLen) {
iRecLen = (int)ucGetByte(iPos, aucBuffer);
NO_DBG_DEC(iRecLen);
iOffsetAltName = (int)ucGetByte(iPos + 5, aucBuffer);
NO_DBG_MSG(aucBuffer + iPos + 6);
NO_DBG_MSG_C(iOffsetAltName > 0,
aucBuffer + iPos + 6 + iOffsetAltName);
iPos += iRecLen + 1;
tFontTableRecords++;
}
tFontTableRecords *= 4;
tFontTableRecords++;
vCreateFontTable();
iItalic = 0;
iBold = 0;
iSpecial = 0;
while (bReadFontFile(pFontTableFile, szWordFont,
&iItalic, &iBold, szOurFont, &iSpecial)) {
iEmphasis = 0;
if (iBold != 0) {
iEmphasis++;
}
if (iItalic != 0) {
iEmphasis += 2;
}
pTmp = pFontTable + iEmphasis;
iPos = 2;
while (iPos + 6 < (int)tFontInfoLen) {
iRecLen = (int)ucGetByte(iPos, aucBuffer);
ucFFN = ucGetByte(iPos + 1, aucBuffer);
aucFont = aucBuffer + iPos + 6;
iOffsetAltName = (int)ucGetByte(iPos + 5, aucBuffer);
if (iOffsetAltName <= 0) {
aucAltFont = NULL;
} else {
aucAltFont = aucFont + iOffsetAltName;
NO_DBG_MSG(aucFont);
NO_DBG_MSG(aucAltFont);
}
vFontname2Table(aucFont, aucAltFont, 1, iEmphasis,
ucFFN, szWordFont, szOurFont, pTmp);
pTmp += 4;
iPos += iRecLen + 1;
}
}
(void)fclose(pFontTableFile);
aucBuffer = xfree(aucBuffer);
vMinimizeFontTable();
}
void
vCreate8FontTable(FILE *pFile, const pps_info_type *pPPS,
const ULONG *aulBBD, size_t tBBDLen,
const ULONG *aulSBD, size_t tSBDLen,
const UCHAR *aucHeader)
{
FILE	*pFontTableFile;
font_table_type	*pTmp;
const ULONG	*aulBlockDepot;
UCHAR	*aucFont, *aucAltFont;
UCHAR	*aucBuffer;
ULONG	ulBeginFontInfo;
size_t	tFontInfoLen, tBlockDepotLen, tBlockSize;
int	iPos, iRecLen, iOffsetAltName;
int	iBold, iItalic, iSpecial, iEmphasis;
UCHAR	ucFFN;
char	szWordFont[FONT_LINE_LENGTH], szOurFont[FONT_LINE_LENGTH];
fail(pFile == NULL || pPPS == NULL || aucHeader == NULL);
fail(aulBBD == NULL || aulSBD == NULL);
tFontTableRecords = 0;
pFontTable = xfree(pFontTable);
pFontTableFile = pOpenFontTableFile();
if (pFontTableFile == NULL) {
return;
}
ulBeginFontInfo = ulGetLong(0x112, aucHeader);
DBG_HEX(ulBeginFontInfo);
tFontInfoLen = (size_t)ulGetLong(0x116, aucHeader);
DBG_DEC(tFontInfoLen);
fail(tFontInfoLen < 46);
DBG_DEC(pPPS->tTable.ulSB);
DBG_HEX(pPPS->tTable.ulSize);
if (pPPS->tTable.ulSize == 0) {
DBG_MSG("No fontname table");
(void)fclose(pFontTableFile);
return;
}
if (pPPS->tTable.ulSize < MIN_SIZE_FOR_BBD_USE) {
aulBlockDepot = aulSBD;
tBlockDepotLen = tSBDLen;
tBlockSize = SMALL_BLOCK_SIZE;
} else {
aulBlockDepot = aulBBD;
tBlockDepotLen = tBBDLen;
tBlockSize = BIG_BLOCK_SIZE;
}
aucBuffer = xmalloc(tFontInfoLen);
if (!bReadBuffer(pFile, pPPS->tTable.ulSB,
aulBlockDepot, tBlockDepotLen, tBlockSize,
aucBuffer, ulBeginFontInfo, tFontInfoLen)) {
aucBuffer = xfree(aucBuffer);
(void)fclose(pFontTableFile);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tFontInfoLen);
tFontTableRecords = (size_t)usGetWord(0, aucBuffer);
tFontTableRecords *= 4;
tFontTableRecords++;
vCreateFontTable();
iItalic = 0;
iBold = 0;
iSpecial = 0;
while (bReadFontFile(pFontTableFile, szWordFont,
&iItalic, &iBold, szOurFont, &iSpecial)) {
iEmphasis = 0;
if (iBold != 0) {
iEmphasis++;
}
if (iItalic != 0) {
iEmphasis += 2;
}
pTmp = pFontTable + iEmphasis;
iPos = 4;
while (iPos + 40 < (int)tFontInfoLen) {
iRecLen = (int)ucGetByte(iPos, aucBuffer);
ucFFN = ucGetByte(iPos + 1, aucBuffer);
aucFont = aucBuffer + iPos + 40;
iOffsetAltName = (int)unilen(aucFont);
if (iPos + 40 + iOffsetAltName + 4 >= iRecLen) {
aucAltFont = NULL;
} else {
aucAltFont = aucFont + iOffsetAltName + 2;
NO_DBG_UNICODE(aucFont);
NO_DBG_UNICODE(aucAltFont);
}
vFontname2Table(aucFont, aucAltFont, 2, iEmphasis,
ucFFN, szWordFont, szOurFont, pTmp);
pTmp += 4;
iPos += iRecLen + 1;
}
}
(void)fclose(pFontTableFile);
aucBuffer = xfree(aucBuffer);
vMinimizeFontTable();
}
void
vDestroyFontTable(void)
{
DBG_MSG("vDestroyFontTable");
tFontTableRecords = 0;
pFontTable = xfree(pFontTable);
}
const font_table_type *
pGetNextFontTableRecord(const font_table_type *pRecordCurr)
{
size_t	tIndexCurr;
if (pRecordCurr == NULL) {
return &pFontTable[0];
}
if (pRecordCurr < pFontTable ||
pRecordCurr >= pFontTable + tFontTableRecords) {
DBG_HEX(pRecordCurr);
DBG_HEX(pFontTable);
return NULL;
}
tIndexCurr = (size_t)(pRecordCurr - pFontTable);
if (tIndexCurr + 1 < tFontTableRecords) {
return &pFontTable[tIndexCurr + 1];
}
return NULL;
}
size_t
tGetFontTableLength(void)
{
return tFontTableRecords;
}
#if !defined(__riscos)
static void
vCorrect4PDF(void)
{
font_table_type	*pTmp;
const char	*szOurFont;
for (pTmp = pFontTable; pTmp < pFontTable + tFontTableRecords; pTmp++) {
if (STRCEQ(pTmp->szOurFontname, FONT_MONOSPACED_PLAIN) ||
STRCEQ(pTmp->szOurFontname, FONT_MONOSPACED_BOLD) ||
STRCEQ(pTmp->szOurFontname, FONT_MONOSPACED_ITALIC) ||
STRCEQ(pTmp->szOurFontname, FONT_MONOSPACED_BOLDITALIC) ||
STRCEQ(pTmp->szOurFontname, FONT_SERIF_PLAIN) ||
STRCEQ(pTmp->szOurFontname, FONT_SERIF_BOLD) ||
STRCEQ(pTmp->szOurFontname, FONT_SERIF_ITALIC) ||
STRCEQ(pTmp->szOurFontname, FONT_SERIF_BOLDITALIC) ||
STRCEQ(pTmp->szOurFontname, FONT_SANS_SERIF_PLAIN) ||
STRCEQ(pTmp->szOurFontname, FONT_SANS_SERIF_BOLD) ||
STRCEQ(pTmp->szOurFontname, FONT_SANS_SERIF_ITALIC) ||
STRCEQ(pTmp->szOurFontname, FONT_SANS_SERIF_BOLDITALIC)) {
continue;
}
szOurFont =
szGetDefaultFont(pTmp->ucFFN, (int)pTmp->ucEmphasis);
(void)strncpy(pTmp->szOurFontname, szOurFont,
sizeof(pTmp->szOurFontname) - 1);
pTmp->szOurFontname[sizeof(pTmp->szOurFontname) - 1] = '\0';
}
}
static void
vCorrect4CyrPS(void)
{
font_table_type	*pTmp;
const char	*szOurFont;
UCHAR	ucFFN;
ucFFN = (FAMILY_UNKNOWN << 4) | PITCH_FIXED;
for (pTmp = pFontTable; pTmp < pFontTable + tFontTableRecords; pTmp++) {
szOurFont = szGetDefaultFont(ucFFN, (int)pTmp->ucEmphasis);
(void)strncpy(pTmp->szOurFontname, szOurFont,
sizeof(pTmp->szOurFontname) - 1);
pTmp->szOurFontname[sizeof(pTmp->szOurFontname) - 1] = '\0';
}
}
#endif
void
vCorrectFontTable(conversion_type eConversionType, encoding_type eEncoding)
{
#if !defined(__riscos)
if (eConversionType == conversion_pdf) {
vCorrect4PDF();
}
if (eConversionType == conversion_ps &&
eEncoding == encoding_cyrillic) {
vCorrect4CyrPS();
}
#endif
}
long
lComputeSpaceWidth(drawfile_fontref tFontRef, USHORT usFontSize)
{
char	szSpace[] = " ";
fail(usFontSize < MIN_FONT_SIZE || usFontSize > MAX_FONT_SIZE);
return lComputeStringWidth(szSpace, 1, tFontRef, usFontSize);
}