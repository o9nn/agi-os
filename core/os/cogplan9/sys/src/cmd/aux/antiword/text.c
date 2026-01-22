#include <string.h>
#include "antiword.h"
static encoding_type	eEncoding = encoding_neutral;
static long		lYtopCurr = 0;
static UCHAR		ucNbsp = 0;
void
vPrologueTXT(diagram_type *pDiag, const options_type *pOptions)
{
fail(pDiag == NULL);
fail(pOptions == NULL);
eEncoding = pOptions->eEncoding;
pDiag->lXleft = 0;
pDiag->lYtop = 0;
lYtopCurr = 0;
}
void
vEpilogueTXT(FILE *pOutFile)
{
fail(pOutFile == NULL);
fprintf(pOutFile, "\n");
}
static void
vPrintTXT(FILE *pFile, const char *szString, size_t tStringLength)
{
const UCHAR	*ucBytes;
size_t		tCount;
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
ucBytes = (UCHAR *)szString;
for (tCount = 0; tCount < tStringLength ; tCount++) {
if (ucBytes[tCount] == ucNbsp) {
(void)putc(' ', pFile);
} else {
(void)putc(szString[tCount], pFile);
}
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
vMove2NextLineTXT(diagram_type *pDiag)
{
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
pDiag->lYtop++;
(void)fprintf(pDiag->pOutFile, "\n");
}
void
vSubstringTXT(diagram_type *pDiag,
const char *szString, size_t tStringLength, long lStringWidth)
{
fail(pDiag == NULL || szString == NULL);
fail(pDiag->pOutFile == NULL);
fail(pDiag->lXleft < 0);
fail(tStringLength != strlen(szString));
if (szString[0] == '\0' || tStringLength == 0) {
return;
}
vMoveTo(pDiag);
vPrintTXT(pDiag->pOutFile, szString, tStringLength);
pDiag->lXleft += lStringWidth;
}
void
vStartOfParagraphTXT(diagram_type *pDiag, long lBeforeIndentation)
{
fail(pDiag == NULL);
fail(lBeforeIndentation < 0);
if (lBeforeIndentation >= lTwips2MilliPoints(HEADING_GAP)) {
vMove2NextLineTXT(pDiag);
}
}
void
vEndOfParagraphTXT(diagram_type *pDiag, long lAfterIndentation)
{
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
fail(lAfterIndentation < 0);
if (pDiag->lXleft > 0) {
vMove2NextLineTXT(pDiag);
}
if (lAfterIndentation >= lTwips2MilliPoints(HEADING_GAP)) {
vMove2NextLineTXT(pDiag);
}
}
void
vEndOfPageTXT(diagram_type *pDiag, long lAfterIndentation)
{
vEndOfParagraphTXT(pDiag, lAfterIndentation);
}