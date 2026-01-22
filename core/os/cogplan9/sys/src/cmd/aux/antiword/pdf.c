#include <stdarg.h>
#include <string.h>
#include "version.h"
#include "antiword.h"
#define INITIAL_LOCATION_SIZE 20
#define INITIAL_PAGEOBJECT_SIZE 5
#if defined(DEBUG)
#define EXTENSION_ARRAY_SIZE 10
#else
#define EXTENSION_ARRAY_SIZE 30
#endif
static encoding_type eEncoding = encoding_neutral;
static const char *szProducer = NULL;
static long lPageHeight = LONG_MAX;
static long lPageWidth = LONG_MAX;
static long lFooterHeight = 0;
static BOOL bInFtrSpace = FALSE;
static drawfile_fontref tFontRefCurr = (drawfile_fontref)-1;
static USHORT usFontSizeCurr = 0;
static int iFontColorCurr = -1;
static long lYtopCurr = -1;
static int iImageCount = 0;
static int iSectionIndex = 0;
static BOOL bFirstInSection = TRUE;
static long lFilePosition = 0;
static long *alLocation = NULL;
static size_t tLocations = 0;
static int iMaxLocationNumber = 0;
static long lStreamStart = -1;
static int *aiPageObject = NULL;
static int iPageCount = 0;
static size_t tMaxPageObjects = 0;
static int iObjectNumberCurr = 17;
static void vMoveTo(diagram_type *, long);
static const struct {
const char *szPDFname;
const char *szPSname;
} atFontname[] = {
{ "Courier", FONT_MONOSPACED_PLAIN },
{ "Courier-Bold", FONT_MONOSPACED_BOLD },
{ "Courier-Oblique", FONT_MONOSPACED_ITALIC },
{ "Courier-BoldOblique", FONT_MONOSPACED_BOLDITALIC },
{ "Helvetica", FONT_SANS_SERIF_PLAIN },
{ "Helvetica-Bold", FONT_SANS_SERIF_BOLD },
{ "Helvetica-Oblique", FONT_SANS_SERIF_ITALIC },
{ "Helvetica-BoldOblique", FONT_SANS_SERIF_BOLDITALIC },
{ "Times-Roman", FONT_SERIF_PLAIN },
{ "Times-Bold", FONT_SERIF_BOLD },
{ "Times-Italic", FONT_SERIF_ITALIC },
{ "Times-BoldItalic", FONT_SERIF_BOLDITALIC },
};
static const char *iso_8859_1[] = {
"128 /Euro",
"140 /ellipsis /trademark /perthousand /bullet",
"    /quoteleft /quoteright /guilsinglleft /guilsinglright",
"    /quotedblleft /quotedblright /quotedblbase /endash /emdash",
"    /minus /OE /oe /dagger /daggerdbl /fi /fl",
"160 /space /exclamdown /cent /sterling /currency",
"    /yen /brokenbar /section /dieresis /copyright",
"    /ordfeminine /guillemotleft /logicalnot /hyphen /registered",
"    /macron /degree /plusminus /twosuperior /threesuperior",
"    /acute /mu /paragraph /periodcentered /cedilla",
"    /onesuperior /ordmasculine /guillemotright /onequarter",
"    /onehalf /threequarters /questiondown /Agrave /Aacute",
"    /Acircumflex /Atilde /Adieresis /Aring /AE /Ccedilla",
"    /Egrave /Eacute /Ecircumflex /Edieresis /Igrave /Iacute",
"    /Icircumflex /Idieresis /Eth /Ntilde /Ograve /Oacute",
"    /Ocircumflex /Otilde /Odieresis /multiply /Oslash",
"    /Ugrave /Uacute /Ucircumflex /Udieresis /Yacute /Thorn",
"    /germandbls /agrave /aacute /acircumflex /atilde",
"    /adieresis /aring /ae /ccedilla /egrave /eacute",
"    /ecircumflex /edieresis /igrave /iacute /icircumflex",
"    /idieresis /eth /ntilde /ograve /oacute /ocircumflex",
"    /otilde /odieresis /divide /oslash /ugrave /uacute",
"    /ucircumflex /udieresis /yacute /thorn /ydieresis",
};
static const char *iso_8859_2[] = {
"160 /space /Aogonek /breve /Lslash /currency /Lcaron",
"    /Sacute /section /dieresis /Scaron /Scommaaccent",
"    /Tcaron /Zacute /hyphen /Zcaron /Zdotaccent /degree",
"    /aogonek /ogonek /lslash /acute /lcaron /sacute",
"    /caron /cedilla /scaron /scommaaccent /tcaron",
"    /zacute /hungarumlaut /zcaron /zdotaccent /Racute",
"    /Aacute /Acircumflex /Abreve /Adieresis /Lacute",
"    /Cacute /Ccedilla /Ccaron /Eacute /Eogonek",
"    /Edieresis /Ecaron /Iacute /Icircumflex /Dcaron",
"    /.notdef /Nacute /Ncaron /Oacute /Ocircumflex",
"    /Ohungarumlaut /Odieresis /multiply /Rcaron /Uring",
"    /Uacute /Uhungarumlaut /Udieresis /Yacute /Tcommaaccent",
"    /germandbls /racute /aacute /acircumflex /abreve",
"    /adieresis /lacute /cacute /ccedilla /ccaron /eacute",
"    /eogonek /edieresis /ecaron /iacute /icircumflex",
"    /dcaron /.notdef /nacute /ncaron /oacute /ocircumflex",
"    /ohungarumlaut /odieresis /divide /rcaron /uring",
"    /uacute /uhungarumlaut /udieresis /yacute /tcommaaccent",
"    /dotaccent",
};
static size_t
tGetFontIndex(drawfile_fontref tFontRef)
{
const char *szFontname;
size_t tIndex;
szFontname = szGetFontname(tFontRef);
fail(szFontname == NULL);
if (szFontname == NULL) {
return 0;
}
for (tIndex = 0; tIndex < elementsof(atFontname); tIndex++) {
if (STRCEQ(atFontname[tIndex].szPSname, szFontname)) {
return tIndex;
}
}
DBG_DEC(tFontRef);
DBG_MSG(szFontname);
return 0;
}
static void
vSetLocation(int iLocationNumber)
{
fail(iLocationNumber <= 0);
if ((size_t)iLocationNumber >= tLocations) {
tLocations += EXTENSION_ARRAY_SIZE;
alLocation = xrealloc(alLocation, tLocations * sizeof(long));
memset(alLocation + tLocations - EXTENSION_ARRAY_SIZE,
0,
EXTENSION_ARRAY_SIZE * sizeof(long));
DBG_DEC(tLocations);
}
if (iLocationNumber > iMaxLocationNumber) {
iMaxLocationNumber = iLocationNumber;
}
DBG_DEC_C((size_t)iLocationNumber >= tLocations, iLocationNumber);
DBG_DEC_C((size_t)iLocationNumber >= tLocations, tLocations);
fail((size_t)iLocationNumber >= tLocations);
alLocation[iLocationNumber] = lFilePosition;
}
static void
vFillNextPageObject(void)
{
iPageCount++;
if ((size_t)iPageCount >= tMaxPageObjects) {
tMaxPageObjects += EXTENSION_ARRAY_SIZE;
aiPageObject = xrealloc(aiPageObject,
tMaxPageObjects * sizeof(int));
DBG_DEC(tMaxPageObjects);
}
aiPageObject[iPageCount] = iObjectNumberCurr;
}
static void
vFPprintf(FILE *pOutFile, const char *szFormat, ...)
{
va_list tArg;
va_start(tArg, szFormat);
lFilePosition += vfprintf(pOutFile, szFormat, tArg);
va_end(tArg);
}
void
vCreateInfoDictionary(diagram_type *pDiag, int iWordVersion)
{
FILE *pOutFile;
const char *szTitle, *szAuthor, *szSubject, *szCreator;
const char *szCreationDate, *szModDate;
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
fail(iWordVersion < 0);
fail(szProducer == NULL || szProducer[0] == '\0');
szTitle = szGetTitle();
szAuthor = szGetAuthor();
szSubject = szGetSubject();
szCreationDate = szGetCreationDate();
szModDate = szGetModDate();
switch (iWordVersion) {
case 0: szCreator = "Word for DOS"; break;
case 1: szCreator = "WinWord 1.x"; break;
case 2: szCreator = "WinWord 2.0"; break;
case 4: szCreator = "MacWord 4"; break;
case 5: szCreator = "MacWord 5"; break;
case 6: szCreator = "Word 6"; break;
case 7: szCreator = "Word 7/95"; break;
case 8: szCreator = "Word 97 or later"; break;
default: szCreator = NULL; break;
}
pOutFile = pDiag->pOutFile;
vSetLocation(2);
vFPprintf(pOutFile, "2 0 obj\n");
vFPprintf(pOutFile, "<<\n");
if (szTitle != NULL && szTitle[0] != '\0') {
vFPprintf(pOutFile, "/Title (%s)\n", szTitle);
}
if (szAuthor != NULL && szAuthor[0] != '\0') {
vFPprintf(pOutFile, "/Author (%s)\n", szAuthor);
}
if (szSubject != NULL && szSubject[0] != '\0') {
vFPprintf(pOutFile, "/Subject (%s)\n", szSubject);
}
if (szCreator != NULL && szCreator[0] != '\0') {
vFPprintf(pOutFile, "/Creator (%s)\n", szCreator);
}
vFPprintf(pOutFile, "/Producer (%s %s)\n", szProducer, VERSIONSTRING);
if (szCreationDate != NULL && szCreationDate[0] != '\0') {
vFPprintf(pOutFile, "/CreationDate (%s)\n", szCreationDate);
}
if (szModDate != NULL && szModDate[0] != '\0') {
vFPprintf(pOutFile, "/ModDate (%s)\n", szModDate);
}
vFPprintf(pOutFile, ">>\n");
vFPprintf(pOutFile, "endobj\n");
}
static void
vAddHdrFtr(diagram_type *pDiag, const hdrftr_block_type *pHdrFtrInfo)
{
output_type *pStart, *pPrev, *pNext;
fail(pDiag == NULL);
fail(pHdrFtrInfo == NULL);
vStartOfParagraphPDF(pDiag, 0);
pStart = pHdrFtrInfo->pText;
while (pStart != NULL) {
pNext = pStart;
while (pNext != NULL &&
(pNext->tNextFree != 1 ||
(pNext->szStorage[0] != PAR_END &&
pNext->szStorage[0] != HARD_RETURN))) {
pNext = pNext->pNext;
}
if (pNext == NULL) {
if (bOutputContainsText(pStart)) {
vAlign2Window(pDiag, pStart,
lChar2MilliPoints(DEFAULT_SCREEN_WIDTH),
ALIGNMENT_LEFT);
} else {
vMove2NextLinePDF(pDiag, pStart->usFontSize);
}
break;
}
fail(pNext->tNextFree != 1);
fail(pNext->szStorage[0] != PAR_END &&
pNext->szStorage[0] != HARD_RETURN);
if (pStart != pNext) {
pPrev = pNext->pPrev;
fail(pPrev->pNext != pNext);
pPrev->pNext = NULL;
if (bOutputContainsText(pStart)) {
vAlign2Window(pDiag, pStart,
lChar2MilliPoints(DEFAULT_SCREEN_WIDTH),
ALIGNMENT_LEFT);
} else {
vMove2NextLinePDF(pDiag, pStart->usFontSize);
}
pPrev->pNext = pNext;
}
if (pNext->szStorage[0] == PAR_END) {
vEndOfParagraphPDF(pDiag, pNext->usFontSize,
(long)pNext->usFontSize * 200);
}
pStart = pNext->pNext;
}
}
static void
vAddHeader(diagram_type *pDiag)
{
const hdrftr_block_type *pHdrInfo;
const hdrftr_block_type *pFtrInfo;
fail(pDiag == NULL);
NO_DBG_MSG("vAddHeader");
pHdrInfo = pGetHdrFtrInfo(iSectionIndex, TRUE,
odd(iPageCount), bFirstInSection);
pFtrInfo = pGetHdrFtrInfo(iSectionIndex, FALSE,
odd(iPageCount), bFirstInSection);
lFooterHeight = pFtrInfo == NULL ? 0 : pFtrInfo->lHeight;
fail(lFooterHeight < 0);
if (pHdrInfo == NULL ||
pHdrInfo->pText == NULL ||
pHdrInfo->lHeight <= 0) {
fail(pHdrInfo != NULL && pHdrInfo->lHeight < 0);
fail(pHdrInfo != NULL &&
pHdrInfo->pText != NULL &&
pHdrInfo->lHeight == 0);
return;
}
vAddHdrFtr(pDiag, pHdrInfo);
DBG_DEC_C(pHdrInfo->lHeight !=
lPageHeight - PS_TOP_MARGIN - pDiag->lYtop,
pHdrInfo->lHeight);
DBG_DEC_C(pHdrInfo->lHeight !=
lPageHeight - PS_TOP_MARGIN - pDiag->lYtop,
lPageHeight - PS_TOP_MARGIN - pDiag->lYtop);
}
static void
vAddFooter(diagram_type *pDiag)
{
const hdrftr_block_type *pFtrInfo;
fail(pDiag == NULL);
NO_DBG_MSG("vAddFooter");
pFtrInfo = pGetHdrFtrInfo(iSectionIndex, FALSE,
odd(iPageCount), bFirstInSection);
bFirstInSection = FALSE;
if (pFtrInfo == NULL ||
pFtrInfo->pText == NULL ||
pFtrInfo->lHeight <= 0) {
fail(pFtrInfo != NULL && pFtrInfo->lHeight < 0);
fail(pFtrInfo != NULL &&
pFtrInfo->pText != NULL &&
pFtrInfo->lHeight == 0);
return;
}
bInFtrSpace = TRUE;
DBG_DEC_C(pFtrInfo->lHeight != lFooterHeight, pFtrInfo->lHeight);
DBG_DEC_C(pFtrInfo->lHeight != lFooterHeight, lFooterHeight);
DBG_DEC_C(pDiag->lYtop < lFooterHeight + PS_BOTTOM_MARGIN,
pDiag->lYtop);
DBG_DEC_C(pDiag->lYtop < lFooterHeight + PS_BOTTOM_MARGIN,
lFooterHeight + PS_BOTTOM_MARGIN);
if (pDiag->lYtop > lFooterHeight + PS_BOTTOM_MARGIN) {
pDiag->lYtop = lFooterHeight + PS_BOTTOM_MARGIN;
vMoveTo(pDiag, 0);
} else if (pDiag->lYtop < lFooterHeight + PS_BOTTOM_MARGIN / 2) {
DBG_FIXME();
pDiag->lYtop = lFooterHeight + PS_BOTTOM_MARGIN;
vMoveTo(pDiag, 0);
}
DBG_FLT_C(pDiag->lYtop < lFooterHeight + PS_BOTTOM_MARGIN,
dDrawUnits2Points(lFooterHeight + PS_BOTTOM_MARGIN - pDiag->lYtop));
vAddHdrFtr(pDiag, pFtrInfo);
bInFtrSpace = FALSE;
}
static void
vEndPageObject(FILE *pOutFile)
{
long lStreamEnd;
if (lStreamStart < 0) {
return;
}
vFPprintf(pOutFile, "ET\n");
lStreamEnd = lFilePosition;
vFPprintf(pOutFile, "endstream\n");
vFPprintf(pOutFile, "endobj\n");
iObjectNumberCurr++;
vSetLocation(iObjectNumberCurr);
vFPprintf(pOutFile, "%d 0 obj\n", iObjectNumberCurr);
vFPprintf(pOutFile, "%lu\n", lStreamEnd - lStreamStart);
vFPprintf(pOutFile, "endobj\n");
}
static void
vMove2NextPage(diagram_type *pDiag, BOOL bNewSection)
{
FILE *pOutFile;
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
pOutFile = pDiag->pOutFile;
vAddFooter(pDiag);
vEndPageObject(pOutFile);
if (bNewSection) {
iSectionIndex++;
bFirstInSection = TRUE;
}
iObjectNumberCurr++;
vSetLocation(iObjectNumberCurr);
vFillNextPageObject();
vFPprintf(pOutFile, "%d 0 obj\n", iObjectNumberCurr);
vFPprintf(pOutFile, "<<\n");
vFPprintf(pOutFile, "/Type /Page\n");
vFPprintf(pOutFile, "/Parent 3 0 R\n");
vFPprintf(pOutFile, "/Resources 17 0 R\n");
vFPprintf(pOutFile, "/Contents %d 0 R\n", iObjectNumberCurr + 1);
vFPprintf(pOutFile, ">>\n");
vFPprintf(pOutFile, "endobj\n");
iObjectNumberCurr++;
vSetLocation(iObjectNumberCurr);
vFPprintf(pOutFile, "%d 0 obj\n", iObjectNumberCurr);
vFPprintf(pOutFile, "<<\n");
vFPprintf(pOutFile, "/Length %d 0 R\n", iObjectNumberCurr + 1);
vFPprintf(pOutFile, ">>\n");
vFPprintf(pOutFile, "stream\n");
lStreamStart = lFilePosition;
vFPprintf(pOutFile, "BT\n");
pDiag->lYtop = lPageHeight - PS_TOP_MARGIN;
tFontRefCurr = (drawfile_fontref)-1;
usFontSizeCurr = 0;
iFontColorCurr = -1;
lYtopCurr = -1;
vAddHeader(pDiag);
}
static void
vMoveTo(diagram_type *pDiag, long lLastVerticalMovement)
{
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
if (pDiag->lYtop <= lFooterHeight + PS_BOTTOM_MARGIN && !bInFtrSpace) {
vMove2NextPage(pDiag, FALSE);
pDiag->lYtop -= lLastVerticalMovement;
}
fail(pDiag->lYtop < lFooterHeight + PS_BOTTOM_MARGIN && !bInFtrSpace);
DBG_DEC_C(pDiag->lYtop < PS_BOTTOM_MARGIN, pDiag->lYtop);
fail(pDiag->lYtop < PS_BOTTOM_MARGIN / 3);
if (pDiag->lYtop != lYtopCurr) {
vFPprintf(pDiag->pOutFile, "1 0 0 1 %.2f %.2f Tm\n",
dDrawUnits2Points(pDiag->lXleft + PS_LEFT_MARGIN),
dDrawUnits2Points(pDiag->lYtop));
lYtopCurr = pDiag->lYtop;
}
}
void
vProloguePDF(diagram_type *pDiag,
const char *szTask, const options_type *pOptions)
{
FILE *pOutFile;
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
fail(pOptions == NULL);
pOutFile = pDiag->pOutFile;
eEncoding = pOptions->eEncoding;
tLocations = INITIAL_LOCATION_SIZE;
alLocation = xcalloc(tLocations, sizeof(long));
tMaxPageObjects = INITIAL_PAGEOBJECT_SIZE;
aiPageObject = xcalloc(tMaxPageObjects, sizeof(int));
if (pOptions->iPageHeight == INT_MAX) {
lPageHeight = LONG_MAX;
} else {
lPageHeight = lPoints2DrawUnits(pOptions->iPageHeight);
}
DBG_DEC(lPageHeight);
if (pOptions->iPageWidth == INT_MAX) {
lPageWidth = LONG_MAX;
} else {
lPageWidth = lPoints2DrawUnits(pOptions->iPageWidth);
}
DBG_DEC(lPageWidth);
lFooterHeight = 0;
bInFtrSpace = FALSE;
tFontRefCurr = (drawfile_fontref)-1;
usFontSizeCurr = 0;
iFontColorCurr = -1;
lYtopCurr = -1;
iPageCount = 0;
iImageCount = 0;
iSectionIndex = 0;
bFirstInSection = TRUE;
lFilePosition = 0;
iMaxLocationNumber = 0;
lStreamStart = -1;
iObjectNumberCurr = 17;
pDiag->lXleft = 0;
pDiag->lYtop = 0;
szProducer = szTask;
vFPprintf(pOutFile, "%%PDF-1.3\n");
vFPprintf(pOutFile, "%%%c%c%c%c\n", 0xe2, 0xe3, 0xcf, 0xd3);
vSetLocation(1);
vFPprintf(pOutFile, "1 0 obj\n");
vFPprintf(pOutFile, "<<\n");
vFPprintf(pOutFile, "/Type /Catalog\n");
vFPprintf(pOutFile, "/Pages 3 0 R\n");
vFPprintf(pOutFile, ">>\n");
vFPprintf(pOutFile, "endobj\n");
}
void
vEpiloguePDF(diagram_type *pDiag)
{
FILE *pOutFile;
long lXref;
int iIndex;
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
pOutFile = pDiag->pOutFile;
vAddFooter(pDiag);
vEndPageObject(pOutFile);
vSetLocation(3);
vFPprintf(pOutFile, "3 0 obj\n");
vFPprintf(pOutFile, "<<\n");
vFPprintf(pOutFile, "/Type /Pages\n");
vFPprintf(pOutFile, "/Count %d\n", iPageCount);
vFPprintf(pOutFile, "/MediaBox [ 0 0 %.0f %.0f ]\n",
dDrawUnits2Points(lPageWidth),
dDrawUnits2Points(lPageHeight));
vFPprintf(pOutFile, "/Kids [ ");
for (iIndex = 1; iIndex <= iPageCount; iIndex++) {
vFPprintf(pOutFile, "\t%d 0 R\n", aiPageObject[iIndex]);
}
vFPprintf(pOutFile, "]\n");
vFPprintf(pOutFile, ">>\n");
vFPprintf(pOutFile, "endobj\n");
lXref = lFilePosition;
vFPprintf(pOutFile, "xref\n");
vFPprintf(pOutFile, "0 %d\n", iMaxLocationNumber + 1);
vFPprintf(pOutFile, "0000000000 65535 f \n");
for (iIndex = 1; iIndex <= iMaxLocationNumber; iIndex++) {
vFPprintf(pOutFile, "%.10ld 00000 n \n", alLocation[iIndex]);
}
vFPprintf(pOutFile, "trailer\n");
vFPprintf(pOutFile, "<<\n");
vFPprintf(pOutFile, "/Size %d\n", iMaxLocationNumber + 1);
vFPprintf(pOutFile, "/Root 1 0 R\n");
vFPprintf(pOutFile, "/Info 2 0 R\n");
vFPprintf(pOutFile, ">>\n");
vFPprintf(pOutFile, "startxref\n");
vFPprintf(pOutFile, "%ld\n", lXref);
vFPprintf(pOutFile, "%%%%EOF\n");
szProducer = NULL;
aiPageObject = xfree(aiPageObject);
alLocation = xfree(alLocation);
}
static void
vPrintPalette(FILE *pOutFile, const imagedata_type *pImg)
{
int iIndex;
fail(pOutFile == NULL);
fail(pImg == NULL);
fail(pImg->iColorsUsed < 2);
fail(pImg->iColorsUsed > 256);
vFPprintf(pOutFile, "\t/ColorSpace [ /Indexed\n");
vFPprintf(pOutFile, "\t/Device%s %d\n",
pImg->bColorImage ? "RGB" : "Gray", pImg->iColorsUsed - 1);
vFPprintf(pOutFile, "<");
for (iIndex = 0; iIndex < pImg->iColorsUsed; iIndex++) {
vFPprintf(pOutFile, "%02x",
(unsigned int)pImg->aucPalette[iIndex][0]);
if (pImg->bColorImage) {
vFPprintf(pOutFile, "%02x%02x",
(unsigned int)pImg->aucPalette[iIndex][1],
(unsigned int)pImg->aucPalette[iIndex][2]);
}
if (iIndex % 8 == 7) {
vFPprintf(pOutFile, "\n");
} else {
vFPprintf(pOutFile, " ");
}
}
vFPprintf(pOutFile, "> ]\n");
}
void
vImageProloguePDF(diagram_type *pDiag, const imagedata_type *pImg)
{
FILE *pOutFile;
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
fail(pImg == NULL);
if (pImg->iVerSizeScaled <= 0 || pImg->iHorSizeScaled <= 0) {
return;
}
iImageCount++;
DBG_DEC_C(pDiag->lXleft != 0, pDiag->lXleft);
pDiag->lYtop -= lPoints2DrawUnits(pImg->iVerSizeScaled);
vMoveTo(pDiag, lPoints2DrawUnits(pImg->iVerSizeScaled));
pOutFile = pDiag->pOutFile;
vFPprintf(pOutFile, "ET\n");
vFPprintf(pOutFile, "q %% Image %03d\n", iImageCount);
if (pImg->eImageType == imagetype_is_dib) {
vFPprintf(pOutFile, "%d 0 0 %d %.2f %.2f cm\n",
pImg->iHorSizeScaled, -pImg->iVerSizeScaled,
dDrawUnits2Points(pDiag->lXleft + PS_LEFT_MARGIN),
dDrawUnits2Points(pDiag->lYtop) + pImg->iVerSizeScaled);
} else {
vFPprintf(pOutFile, "%d 0 0 %d %.2f %.2f cm\n",
pImg->iHorSizeScaled, pImg->iVerSizeScaled,
dDrawUnits2Points(pDiag->lXleft + PS_LEFT_MARGIN),
dDrawUnits2Points(pDiag->lYtop));
}
vFPprintf(pOutFile, "BI\n");
vFPprintf(pOutFile, "\t/Width %d\n", pImg->iWidth);
vFPprintf(pOutFile, "\t/Height %d\n", pImg->iHeight);
switch (pImg->eImageType) {
case imagetype_is_jpeg:
switch (pImg->iComponents) {
case 1:
vFPprintf(pOutFile, "\t/ColorSpace /DeviceGray\n");
break;
case 3:
vFPprintf(pOutFile, "\t/ColorSpace /DeviceRGB\n");
break;
case 4:
vFPprintf(pOutFile, "\t/ColorSpace /DeviceCMYK\n");
if (pImg->bAdobe) {
vFPprintf(pOutFile,
"\t/Decode [1 0 1 0 1 0 1 0]\n");
}
break;
default:
DBG_DEC(pImg->iComponents);
break;
}
vFPprintf(pOutFile, "\t/BitsPerComponent 8\n");
vFPprintf(pOutFile,
"\t/Filter [ /ASCII85Decode /DCTDecode ]\n");
break;
case imagetype_is_png:
if (pImg->iComponents == 3 || pImg->iComponents == 4) {
vFPprintf(pOutFile, "\t/ColorSpace /DeviceRGB\n");
vFPprintf(pOutFile, "\t/BitsPerComponent 8\n");
} else if (pImg->iColorsUsed > 0) {
vPrintPalette(pOutFile, pImg);
fail(pImg->uiBitsPerComponent > 8);
vFPprintf(pOutFile, "\t/BitsPerComponent %u\n",
pImg->uiBitsPerComponent);
} else {
vFPprintf(pOutFile, "\t/ColorSpace /DeviceGray\n");
vFPprintf(pOutFile, "\t/BitsPerComponent 8\n");
}
vFPprintf(pOutFile,
"\t/Filter [ /ASCII85Decode /FlateDecode ]\n");
vFPprintf(pOutFile, "\t/DecodeParms [ null <<\n");
vFPprintf(pOutFile, "\t\t/Predictor 10\n");
vFPprintf(pOutFile, "\t\t/Colors %d\n", pImg->iComponents);
vFPprintf(pOutFile, "\t\t/BitsPerComponent %u\n",
pImg->uiBitsPerComponent);
vFPprintf(pOutFile, "\t\t/Columns %d\n", pImg->iWidth);
vFPprintf(pOutFile, "\t\t>> ]\n");
break;
case imagetype_is_dib:
if (pImg->uiBitsPerComponent <= 8) {
vPrintPalette(pOutFile, pImg);
} else {
vFPprintf(pOutFile, "\t/ColorSpace /DeviceRGB\n");
}
vFPprintf(pOutFile, "\t/BitsPerComponent 8\n");
vFPprintf(pOutFile, "\t/Filter /ASCII85Decode\n");
break;
default:
vFPprintf(pOutFile, "\t/ColorSpace /Device%s\n",
pImg->bColorImage ? "RGB" : "Gray");
vFPprintf(pOutFile, "\t/BitsPerComponent 8\n");
vFPprintf(pOutFile, "\t/Filter /ASCIIHexDecode\n");
break;
}
vFPprintf(pOutFile, "ID\n");
}
void
vImageEpiloguePDF(diagram_type *pDiag)
{
FILE *pOutFile;
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
pOutFile = pDiag->pOutFile;
lFilePosition = ftell(pOutFile);
vFPprintf(pOutFile, "EI\n");
vFPprintf(pOutFile, "Q\n");
vFPprintf(pOutFile, "BT\n");
pDiag->lXleft = 0;
}
BOOL
bAddDummyImagePDF(diagram_type *pDiag, const imagedata_type *pImg)
{
FILE *pOutFile;
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
fail(pImg == NULL);
if (pImg->iVerSizeScaled <= 0 || pImg->iHorSizeScaled <= 0) {
return FALSE;
}
iImageCount++;
DBG_DEC_C(pDiag->lXleft != 0, pDiag->lXleft);
pDiag->lYtop -= lPoints2DrawUnits(pImg->iVerSizeScaled);
vMoveTo(pDiag, lPoints2DrawUnits(pImg->iVerSizeScaled));
pOutFile = pDiag->pOutFile;
vFPprintf(pOutFile, "ET\n");
vFPprintf(pOutFile, "q %% Image %03d\n", iImageCount);
vFPprintf(pOutFile, "\t1.0 w\n");
vFPprintf(pOutFile, "\t0.3 G\n");
vFPprintf(pOutFile, "\t%.2f %.2f %d %d re\n",
dDrawUnits2Points(pDiag->lXleft + PS_LEFT_MARGIN),
dDrawUnits2Points(pDiag->lYtop),
pImg->iHorSizeScaled,
pImg->iVerSizeScaled);
vFPprintf(pOutFile, "\tS\n");
vFPprintf(pOutFile, "Q\n");
vFPprintf(pOutFile, "BT\n");
pDiag->lXleft = 0;
return TRUE;
}
void
vAddFontsPDF(diagram_type *pDiag)
{
FILE *pOutFile;
size_t tIndex;
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
pOutFile = pDiag->pOutFile;
vSetLocation(4);
vFPprintf(pOutFile, "4 0 obj\n");
vFPprintf(pOutFile, "<<\n");
vFPprintf(pOutFile, "/Type /Encoding\n");
vFPprintf(pOutFile, "/BaseEncoding /StandardEncoding\n");
vFPprintf(pOutFile, "/Differences [\n");
switch (eEncoding) {
case encoding_latin_1:
for (tIndex = 0;
tIndex < elementsof(iso_8859_1);
tIndex++) {
vFPprintf(pOutFile, "%s\n", iso_8859_1[tIndex]);
}
break;
case encoding_latin_2:
for (tIndex = 0;
tIndex < elementsof(iso_8859_2);
tIndex++) {
vFPprintf(pOutFile, "%s\n", iso_8859_2[tIndex]);
}
break;
case encoding_cyrillic:
werr(1,
"The combination PDF and Cyrillic is not supported");
break;
case encoding_utf_8:
werr(1,
"The combination PDF and UTF-8 is not supported");
break;
default:
DBG_DEC(eEncoding);
break;
}
vFPprintf(pOutFile, "]\n");
vFPprintf(pOutFile, ">>\n");
vFPprintf(pOutFile, "endobj\n");
for (tIndex = 0; tIndex < 12; tIndex++) {
vSetLocation(5 + tIndex);
vFPprintf(pOutFile, "%u 0 obj\n", 5 + tIndex);
vFPprintf(pOutFile, "<<\n");
vFPprintf(pOutFile, "/Type /Font\n");
vFPprintf(pOutFile, "/Subtype /Type1\n");
vFPprintf(pOutFile, "/Name /F%u\n", 1 + tIndex);
vFPprintf(pOutFile, "/BaseFont /%s\n",
atFontname[tIndex].szPDFname);
vFPprintf(pOutFile, "/Encoding 4 0 R\n");
vFPprintf(pOutFile, ">>\n");
vFPprintf(pOutFile, "endobj\n");
}
vSetLocation(17);
vFPprintf(pOutFile, "17 0 obj\n");
vFPprintf(pOutFile, "<<\n");
vFPprintf(pOutFile, "/ProcSet [ /PDF /Text ]\n");
vFPprintf(pOutFile, "/Font <<\n");
for (tIndex = 0; tIndex < 12; tIndex++) {
vFPprintf(pOutFile, "\t/F%u %u 0 R\n", 1 + tIndex, 5 + tIndex);
}
vFPprintf(pOutFile, "\t>>\n");
vFPprintf(pOutFile, ">>\n");
vFPprintf(pOutFile, "endobj\n");
vAddHeader(pDiag);
}
static void
vPrintPDF(FILE *pFile, const char *szString, size_t tStringLength,
USHORT usFontstyle)
{
const UCHAR *aucBytes;
double dMove;
size_t tCount;
fail(szString == NULL);
if (szString == NULL || szString[0] == '\0' || tStringLength == 0) {
return;
}
DBG_DEC_C(usFontSizeCurr < MIN_FONT_SIZE, usFontSizeCurr);
dMove = 0.0;
if (bIsSuperscript(usFontstyle) && usFontSizeCurr != 0) {
dMove = (double)((usFontSizeCurr + 1) / 2) * 0.375;
vFPprintf(pFile, "%.2f Ts\n", dMove);
}
if (bIsSubscript(usFontstyle) && usFontSizeCurr != 0) {
dMove = (double)usFontSizeCurr * 0.125;
vFPprintf(pFile, "%.2f Ts\n", -dMove);
}
aucBytes = (UCHAR *)szString;
vFPprintf(pFile, "(");
for (tCount = 0; tCount < tStringLength ; tCount++) {
switch (aucBytes[tCount]) {
case '(':
case ')':
case '\\':
vFPprintf(pFile, "\\%c", szString[tCount]);
break;
default:
if (aucBytes[tCount] < 0x20 ||
aucBytes[tCount] == 0x7f ||
(aucBytes[tCount] >= 0x81 &&
aucBytes[tCount] < 0x8c)) {
DBG_HEX(aucBytes[tCount]);
vFPprintf(pFile, " ");
} else if (aucBytes[tCount] >= 0x80) {
vFPprintf(pFile, "\\%03o",
(UINT)aucBytes[tCount]);
} else {
vFPprintf(pFile, "%c", szString[tCount]);
}
break;
}
}
vFPprintf(pFile, ") Tj\n");
if (dMove != 0.0) {
vFPprintf(pFile, "0 Ts\n");
}
}
static void
vSetColor(FILE *pFile, UCHAR ucFontColor)
{
ULONG ulTmp, ulRed, ulGreen, ulBlue;
ulTmp = ulColor2Color(ucFontColor);
ulRed = (ulTmp & 0x0000ff00) >> 8;
ulGreen = (ulTmp & 0x00ff0000) >> 16;
ulBlue = (ulTmp & 0xff000000) >> 24;
vFPprintf(pFile, "%.3f %.3f %.3f rg\n",
ulRed / 255.0, ulGreen / 255.0, ulBlue / 255.0);
}
void
vMove2NextLinePDF(diagram_type *pDiag, USHORT usFontSize)
{
fail(pDiag == NULL);
fail(usFontSize < MIN_FONT_SIZE || usFontSize > MAX_FONT_SIZE);
pDiag->lYtop -= lComputeLeading(usFontSize);
}
void
vSubstringPDF(diagram_type *pDiag,
char *szString, size_t tStringLength, long lStringWidth,
UCHAR ucFontColor, USHORT usFontstyle, drawfile_fontref tFontRef,
USHORT usFontSize, USHORT usMaxFontSize)
{
size_t tFontIndex;
fail(pDiag == NULL || szString == NULL);
fail(pDiag->pOutFile == NULL);
fail(pDiag->lXleft < 0);
fail(tStringLength != strlen(szString));
fail(usFontSize < MIN_FONT_SIZE || usFontSize > MAX_FONT_SIZE);
fail(usMaxFontSize < MIN_FONT_SIZE || usMaxFontSize > MAX_FONT_SIZE);
fail(usFontSize > usMaxFontSize);
if (szString[0] == '\0' || tStringLength == 0) {
return;
}
vMoveTo(pDiag, lComputeLeading(usMaxFontSize));
if (tFontRef != tFontRefCurr || usFontSize != usFontSizeCurr) {
tFontIndex = tGetFontIndex(tFontRef);
vFPprintf(pDiag->pOutFile, "/F%u %.1f Tf\n",
1 + tFontIndex, (double)usFontSize / 2.0);
tFontRefCurr = tFontRef;
usFontSizeCurr = usFontSize;
}
if ((int)ucFontColor != iFontColorCurr) {
vSetColor(pDiag->pOutFile, ucFontColor);
iFontColorCurr = (int)ucFontColor;
}
vPrintPDF(pDiag->pOutFile, szString, tStringLength, usFontstyle);
pDiag->lXleft += lStringWidth;
}
void
vStartOfParagraphPDF(diagram_type *pDiag, long lBeforeIndentation)
{
fail(pDiag == NULL);
fail(lBeforeIndentation < 0);
pDiag->lXleft = 0;
pDiag->lYtop -= lMilliPoints2DrawUnits(lBeforeIndentation);
}
void
vEndOfParagraphPDF(diagram_type *pDiag,
USHORT usFontSize, long lAfterIndentation)
{
fail(pDiag == NULL);
fail(pDiag->pOutFile == NULL);
fail(usFontSize < MIN_FONT_SIZE || usFontSize > MAX_FONT_SIZE);
fail(lAfterIndentation < 0);
if (pDiag->lXleft > 0) {
vMove2NextLinePDF(pDiag, usFontSize);
}
pDiag->lXleft = 0;
pDiag->lYtop -= lMilliPoints2DrawUnits(lAfterIndentation);
}
void
vEndOfPagePDF(diagram_type *pDiag, BOOL bNewSection)
{
vMove2NextPage(pDiag, bNewSection);
}