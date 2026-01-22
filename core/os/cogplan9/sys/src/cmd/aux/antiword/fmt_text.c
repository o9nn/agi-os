#include <string.h>
#include "antiword.h"
static encoding_type	eEncoding = encoding_neutral;
static long		lYtopCurr = 0;
static UCHAR		ucNbsp = 0;
void
vPrologueFMT(diagram_type *pDiag, const options_type *pOptions)
{
fail(pDiag == NULL);
fail(pOptions == NULL);
eEncoding = pOptions->eEncoding;
pDiag->lXleft = 0;
pDiag->lYtop = 0;
lYtopCurr = 0;
}
static void
vPrintFMT(FILE *pFile,
const char *szString, size_t tStringLength, USHORT usFontstyle)
{
const UCHAR	*pucByte, *pucStart, *pucLast, *pucNonSpace;
fail(szString == NULL);
if (szString == NULL || szString[0] == '\0' || tStringLength == 0) {
return;
}
if (eEncoding == encoding_utf_8) {
fprintf(pFile, "%.*s", (int)tStringLength, szString);
return;
}
if (ucNbsp == 0) {
ucNbsp = ucGetNbspCharacter();
DBG_HEX_C(ucNbsp != 0xa0, ucNbsp);
}
pucStart = (UCHAR *)szString;
pucLast = pucStart + tStringLength - 1;
pucNonSpace = pucLast;
while ((*pucNonSpace == (UCHAR)' ' || *pucNonSpace == ucNbsp) &&
pucNonSpace > pucStart) {
pucNonSpace--;
}
pucByte = pucStart;
while ((*pucByte == (UCHAR)' ' || *pucByte == ucNbsp) &&
pucByte <= pucLast) {
(void)putc(' ', pFile);
pucByte++;
}
if (pucByte > pucLast) {
return;
}
if (bIsBold(usFontstyle)) {
(void)putc('*', pFile);
}
if (bIsItalic(usFontstyle)) {
(void)putc('/', pFile);
}
if (bIsUnderline(usFontstyle)) {
(void)putc('_', pFile);
}
while (pucByte <= pucNonSpace) {
if (*pucByte == ucNbsp) {
(void)putc(' ', pFile);
} else {
(void)putc((char)*pucByte, pFile);
}
pucByte++;
}
if (bIsUnderline(usFontstyle)) {
(void)putc('_', pFile);
}
if (bIsItalic(usFontstyle)) {
(void)putc('/', pFile);
}
if (bIsBold(usFontstyle)) {
(void)putc('*', pFile);
}
while (pucByte <= pucLast) {
(void)putc(' ', pFile);
pucByte++;
}
}
static void
vMoveTo(diagram_type *pDiag)
{
int	iCount, iNbr;
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
if (pDiag->lYtop != lYtopCurr) {
iNbr = iDrawUnits2Char(pDiag->lXleft);
for (iCount = 0; iCount < iNbr; iCount++) {
(void)putc(FILLER_CHAR, pDiag->pOutFile);
}
lYtopCurr = pDiag->lYtop;
}
}
void
vSubstringFMT(diagram_type *pDiag,
const char *szString, size_t tStringLength, long lStringWidth,
USHORT usFontstyle)
{
fail(pDiag == NULL || szString == NULL);
fail(pDiag->pOutFile == NULL);
fail(pDiag->lXleft < 0);
fail(tStringLength != strlen(szString));
if (szString[0] == '\0' || tStringLength == 0) {
return;
}
vMoveTo(pDiag);
vPrintFMT(pDiag->pOutFile, szString, tStringLength, usFontstyle);
pDiag->lXleft += lStringWidth;
}