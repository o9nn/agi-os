#include "antiword.h"
static conversion_type	eConversionType = conversion_unknown;
static encoding_type	eEncoding = encoding_neutral;
static void
vPrologue1(diagram_type *pDiag, const char *szTask, const char *szFilename)
{
options_type	tOptions;
fail(pDiag == NULL);
fail(szTask == NULL || szTask[0] == '\0');
vGetOptions(&tOptions);
eConversionType = tOptions.eConversionType;
eEncoding = tOptions.eEncoding;
switch (eConversionType) {
case conversion_text:
vPrologueTXT(pDiag, &tOptions);
break;
case conversion_fmt_text:
vPrologueFMT(pDiag, &tOptions);
break;
case conversion_ps:
vProloguePS(pDiag, szTask, szFilename, &tOptions);
break;
case conversion_xml:
vPrologueXML(pDiag, &tOptions);
break;
case conversion_pdf:
vProloguePDF(pDiag, szTask, &tOptions);
break;
default:
DBG_DEC(eConversionType);
break;
}
}
static void
vEpilogue(diagram_type *pDiag)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
vEpilogueTXT(pDiag->pOutFile);
break;
case conversion_ps:
vEpiloguePS(pDiag);
break;
case conversion_xml:
vEpilogueXML(pDiag);
break;
case conversion_pdf:
vEpiloguePDF(pDiag);
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vImagePrologue(diagram_type *pDiag, const imagedata_type *pImg)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
break;
case conversion_ps:
vImageProloguePS(pDiag, pImg);
break;
case conversion_xml:
break;
case conversion_pdf:
vImageProloguePDF(pDiag, pImg);
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vImageEpilogue(diagram_type *pDiag)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
break;
case conversion_ps:
vImageEpiloguePS(pDiag);
break;
case conversion_xml:
break;
case conversion_pdf:
vImageEpiloguePDF(pDiag);
break;
default:
DBG_DEC(eConversionType);
break;
}
}
BOOL
bAddDummyImage(diagram_type *pDiag, const imagedata_type *pImg)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
return FALSE;
case conversion_ps:
return bAddDummyImagePS(pDiag, pImg);
case conversion_xml:
return FALSE;
case conversion_pdf:
return bAddDummyImagePDF(pDiag, pImg);
default:
DBG_DEC(eConversionType);
return FALSE;
}
}
diagram_type *
pCreateDiagram(const char *szTask, const char *szFilename)
{
diagram_type	*pDiag;
fail(szTask == NULL || szTask[0] == '\0');
DBG_MSG("pCreateDiagram");
pDiag = xmalloc(sizeof(diagram_type));
pDiag->pOutFile = stdout;
vPrologue1(pDiag, szTask, szFilename);
return pDiag;
}
void
vDestroyDiagram(diagram_type *pDiag)
{
DBG_MSG("vDestroyDiagram");
fail(pDiag == NULL);
if (pDiag == NULL) {
return;
}
vEpilogue(pDiag);
pDiag = xfree(pDiag);
}
void
vPrologue2(diagram_type *pDiag, int iWordVersion)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
break;
case conversion_ps:
vAddFontsPS(pDiag);
break;
case conversion_xml:
vCreateBookIntro(pDiag, iWordVersion);
break;
case conversion_pdf:
vCreateInfoDictionary(pDiag, iWordVersion);
vAddFontsPDF(pDiag);
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vMove2NextLine(diagram_type *pDiag, drawfile_fontref tFontRef,
USHORT usFontSize)
{
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
fail(usFontSize < MIN_FONT_SIZE || usFontSize > MAX_FONT_SIZE);
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
vMove2NextLineTXT(pDiag);
break;
case conversion_ps:
vMove2NextLinePS(pDiag, usFontSize);
break;
case conversion_xml:
vMove2NextLineXML(pDiag);
break;
case conversion_pdf:
vMove2NextLinePDF(pDiag, usFontSize);
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vSubstring2Diagram(diagram_type *pDiag,
char *szString, size_t tStringLength, long lStringWidth,
UCHAR ucFontColor, USHORT usFontstyle, drawfile_fontref tFontRef,
USHORT usFontSize, USHORT usMaxFontSize)
{
switch (eConversionType) {
case conversion_text:
vSubstringTXT(pDiag, szString, tStringLength, lStringWidth);
break;
case conversion_fmt_text:
vSubstringFMT(pDiag, szString, tStringLength, lStringWidth,
usFontstyle);
break;
case conversion_ps:
vSubstringPS(pDiag, szString, tStringLength, lStringWidth,
ucFontColor, usFontstyle, tFontRef,
usFontSize, usMaxFontSize);
break;
case conversion_xml:
vSubstringXML(pDiag, szString, tStringLength, lStringWidth,
usFontstyle);
break;
case conversion_pdf:
vSubstringPDF(pDiag, szString, tStringLength, lStringWidth,
ucFontColor, usFontstyle, tFontRef,
usFontSize, usMaxFontSize);
break;
default:
DBG_DEC(eConversionType);
break;
}
pDiag->lXleft += lStringWidth;
}
void
vStartOfParagraph1(diagram_type *pDiag, long lBeforeIndentation)
{
fail(pDiag == NULL);
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
vStartOfParagraphTXT(pDiag, lBeforeIndentation);
break;
case conversion_ps:
vStartOfParagraphPS(pDiag, lBeforeIndentation);
break;
case conversion_xml:
break;
case conversion_pdf:
vStartOfParagraphPDF(pDiag, lBeforeIndentation);
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vStartOfParagraph2(diagram_type *pDiag)
{
fail(pDiag == NULL);
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
break;
case conversion_ps:
break;
case conversion_xml:
vStartOfParagraphXML(pDiag, 1);
break;
case conversion_pdf:
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vEndOfParagraph(diagram_type *pDiag,
drawfile_fontref tFontRef, USHORT usFontSize, long lAfterIndentation)
{
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
fail(usFontSize < MIN_FONT_SIZE || usFontSize > MAX_FONT_SIZE);
fail(lAfterIndentation < 0);
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
vEndOfParagraphTXT(pDiag, lAfterIndentation);
break;
case conversion_ps:
vEndOfParagraphPS(pDiag, usFontSize, lAfterIndentation);
break;
case conversion_xml:
vEndOfParagraphXML(pDiag, 1);
break;
case conversion_pdf:
vEndOfParagraphPDF(pDiag, usFontSize, lAfterIndentation);
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vEndOfPage(diagram_type *pDiag, long lAfterIndentation, BOOL bNewSection)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
vEndOfPageTXT(pDiag, lAfterIndentation);
break;
case conversion_ps:
vEndOfPagePS(pDiag, bNewSection);
break;
case conversion_xml:
vEndOfPageXML(pDiag);
break;
case conversion_pdf:
vEndOfPagePDF(pDiag, bNewSection);
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vSetHeaders(diagram_type *pDiag, USHORT usIstd)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
break;
case conversion_ps:
break;
case conversion_xml:
vSetHeadersXML(pDiag, usIstd);
break;
case conversion_pdf:
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vStartOfList(diagram_type *pDiag, UCHAR ucNFC, BOOL bIsEndOfTable)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
break;
case conversion_ps:
break;
case conversion_xml:
vStartOfListXML(pDiag, ucNFC, bIsEndOfTable);
break;
case conversion_pdf:
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vEndOfList(diagram_type *pDiag)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
break;
case conversion_ps:
break;
case conversion_xml:
vEndOfListXML(pDiag);
break;
case conversion_pdf:
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vStartOfListItem(diagram_type *pDiag, BOOL bNoMarks)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
break;
case conversion_ps:
break;
case conversion_xml:
vStartOfListItemXML(pDiag, bNoMarks);
break;
case conversion_pdf:
break;
default:
DBG_DEC(eConversionType);
break;
}
}
void
vEndOfTable(diagram_type *pDiag)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
break;
case conversion_ps:
break;
case conversion_xml:
vEndOfTableXML(pDiag);
break;
case conversion_pdf:
break;
default:
DBG_DEC(eConversionType);
break;
}
}
BOOL
bAddTableRow(diagram_type *pDiag, char **aszColTxt,
int iNbrOfColumns, const short *asColumnWidth, UCHAR ucBorderInfo)
{
switch (eConversionType) {
case conversion_text:
case conversion_fmt_text:
break;
case conversion_ps:
break;
case conversion_xml:
vAddTableRowXML(pDiag, aszColTxt,
iNbrOfColumns, asColumnWidth,
ucBorderInfo);
return TRUE;
case conversion_pdf:
break;
default:
DBG_DEC(eConversionType);
break;
}
return FALSE;
}