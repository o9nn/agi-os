#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "antiword.h"
#include "fontinfo.h"
static BOOL bUsePlainText = TRUE;
static encoding_type eEncoding = encoding_neutral;
FILE *
pOpenFontTableFile(void)
{
FILE *pFile;
const char *szHome, *szAntiword, *szGlobalFile;
char szEnvironmentFile[PATH_MAX+1];
char szLocalFile[PATH_MAX+1];
szEnvironmentFile[0] = '\0';
szLocalFile[0] = '\0';
szAntiword = szGetAntiwordDirectory();
if (szAntiword != NULL && szAntiword[0] != '\0') {
if (strlen(szAntiword) +
sizeof(FILE_SEPARATOR FONTNAMES_FILE) >=
sizeof(szEnvironmentFile)) {
werr(0,
"The name of your ANTIWORDHOME directory is too long");
return NULL;
}
sprintf(szEnvironmentFile, "%s%s",
szAntiword,
FILE_SEPARATOR FONTNAMES_FILE);
DBG_MSG(szEnvironmentFile);
pFile = fopen(szEnvironmentFile, "r");
if (pFile != NULL) {
return pFile;
}
}
szHome = szGetHomeDirectory();
if (strlen(szHome) +
sizeof(FILE_SEPARATOR ANTIWORD_DIR FILE_SEPARATOR FONTNAMES_FILE) >=
sizeof(szLocalFile)) {
werr(0, "The name of your HOME directory is too long");
return NULL;
}
sprintf(szLocalFile, "%s%s",
szHome,
FILE_SEPARATOR ANTIWORD_DIR FILE_SEPARATOR FONTNAMES_FILE);
DBG_MSG(szLocalFile);
pFile = fopen(szLocalFile, "r");
if (pFile != NULL) {
return pFile;
}
szGlobalFile = GLOBAL_ANTIWORD_DIR FILE_SEPARATOR FONTNAMES_FILE;
DBG_MSG(szGlobalFile);
pFile = fopen(szGlobalFile, "r");
if (pFile != NULL) {
return pFile;
}
if (szEnvironmentFile[0] != '\0') {
werr(0, "I can not open your fontnames file.\n"
"Neither '%s' nor\n"
"'%s' nor\n"
"'%s' can be opened for reading.",
szEnvironmentFile, szLocalFile, szGlobalFile);
} else {
werr(0, "I can not open your fontnames file.\n"
"Neither '%s' nor\n"
"'%s' can be opened for reading.",
szLocalFile, szGlobalFile);
}
return NULL;
}
void
vCloseFont(void)
{
NO_DBG_MSG("vCloseFont");
eEncoding = encoding_neutral;
bUsePlainText = TRUE;
}
drawfile_fontref
tOpenFont(UCHAR ucWordFontNumber, USHORT usFontStyle, USHORT usWordFontSize)
{
options_type tOptions;
const char *szOurFontname;
size_t tIndex;
int iFontnumber;
NO_DBG_MSG("tOpenFont");
NO_DBG_DEC(ucWordFontNumber);
NO_DBG_HEX(usFontStyle);
NO_DBG_DEC(usWordFontSize);
usFontStyle &= FONT_BOLD|FONT_ITALIC;
NO_DBG_HEX(usFontStyle);
vGetOptions(&tOptions);
eEncoding = tOptions.eEncoding;
bUsePlainText = tOptions.eConversionType != conversion_draw &&
tOptions.eConversionType != conversion_ps &&
tOptions.eConversionType != conversion_pdf;
if (bUsePlainText) {
return (drawfile_fontref)0;
}
iFontnumber = iGetFontByNumber(ucWordFontNumber, usFontStyle);
szOurFontname = szGetOurFontname(iFontnumber);
if (szOurFontname == NULL || szOurFontname[0] == '\0') {
DBG_DEC(iFontnumber);
return (drawfile_fontref)0;
}
NO_DBG_MSG(szOurFontname);
for (tIndex = 0; tIndex < elementsof(szFontnames); tIndex++) {
if (STREQ(szFontnames[tIndex], szOurFontname)) {
NO_DBG_DEC(tIndex);
return (drawfile_fontref)tIndex;
}
}
return (drawfile_fontref)0;
}
drawfile_fontref
tOpenTableFont(USHORT usWordFontSize)
{
options_type tOptions;
int iWordFontnumber;
NO_DBG_MSG("tOpenTableFont");
vGetOptions(&tOptions);
eEncoding = tOptions.eEncoding;
bUsePlainText = tOptions.eConversionType != conversion_draw &&
tOptions.eConversionType != conversion_ps &&
tOptions.eConversionType != conversion_pdf;
if (bUsePlainText) {
return (drawfile_fontref)0;
}
iWordFontnumber = iFontname2Fontnumber(TABLE_FONT, FONT_REGULAR);
if (iWordFontnumber < 0 || iWordFontnumber > (int)UCHAR_MAX) {
DBG_DEC(iWordFontnumber);
return (drawfile_fontref)0;
}
return tOpenFont((UCHAR)iWordFontnumber, FONT_REGULAR, usWordFontSize);
}
const char *
szGetFontname(drawfile_fontref tFontRef)
{
fail((size_t)(UCHAR)tFontRef >= elementsof(szFontnames));
return szFontnames[(int)(UCHAR)tFontRef];
}
long
lComputeStringWidth(const char *szString, size_t tStringLength,
drawfile_fontref tFontRef, USHORT usFontSize)
{
USHORT *ausCharWidths;
UCHAR *pucChar;
long lRelWidth;
size_t tIndex;
int iFontRef;
fail(szString == NULL);
fail(usFontSize < MIN_FONT_SIZE || usFontSize > MAX_FONT_SIZE);
if (szString[0] == '\0' || tStringLength == 0) {
return 0;
}
if (eEncoding == encoding_utf_8) {
fail(!bUsePlainText);
return lChar2MilliPoints(
utf8_strwidth(szString, tStringLength));
}
if (bUsePlainText) {
return lChar2MilliPoints(tStringLength);
}
if (eEncoding == encoding_cyrillic) {
return (tStringLength * 600L * (long)usFontSize + 1) / 2;
}
DBG_DEC_C(eEncoding != encoding_latin_1 &&
eEncoding != encoding_latin_2, eEncoding);
fail(eEncoding != encoding_latin_1 &&
eEncoding != encoding_latin_2);
iFontRef = (int)(UCHAR)tFontRef;
if (eEncoding == encoding_latin_2) {
ausCharWidths = ausCharacterWidths2[iFontRef];
} else {
ausCharWidths = ausCharacterWidths1[iFontRef];
}
lRelWidth = 0;
for (tIndex = 0, pucChar = (UCHAR *)szString;
tIndex < tStringLength;
tIndex++, pucChar++) {
lRelWidth += (long)ausCharWidths[(int)*pucChar];
}
return (lRelWidth * (long)usFontSize + 1) / 2;
}
size_t
tCountColumns(const char *szString, size_t tLength)
{
fail(szString == NULL);
if (eEncoding != encoding_utf_8) {
return tLength;
}
return (size_t)utf8_strwidth(szString, tLength);
}
size_t
tGetCharacterLength(const char *szString)
{
fail(szString == NULL);
if (eEncoding != encoding_utf_8) {
return 1;
}
return (size_t)utf8_chrlength(szString);
}