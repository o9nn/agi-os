#include <stdio.h>
#include <stdlib.h>
#if defined(__dos)
#include <fcntl.h>
#include <io.h>
#endif
#if defined(__CYGWIN__) || defined(__CYGMING__)
# ifdef X_LOCALE
# include <X11/Xlocale.h>
# else
# include <locale.h>
# endif
#else
#include <locale.h>
#endif
#if defined(N_PLAT_NLM)
#if !defined(_VA_LIST)
#include "NW-only/nw_os.h"
#endif
#include "getopt.h"
#endif
#include "version.h"
#include "antiword.h"
static const char *szTask = NULL;
static void
vUsage(void)
{
fprintf(stderr, "\tName: %s\n", szTask);
fprintf(stderr, "\tPurpose: "PURPOSESTRING"\n");
fprintf(stderr, "\tAuthor: "AUTHORSTRING"\n");
fprintf(stderr, "\tVersion: "VERSIONSTRING);
#if defined(__dos)
fprintf(stderr, VERSIONSTRING2);
#endif
fprintf(stderr, "\n");
fprintf(stderr, "\tStatus: "STATUSSTRING"\n");
fprintf(stderr,
"\tUsage: %s [switches] wordfile1 [wordfile2 ...]\n", szTask);
fprintf(stderr,
"\tSwitches: [-f|-t|-a papersize|-p papersize|-x dtd]"
"[-m mapping][-w #][-i #][-Ls]\n");
fprintf(stderr, "\t\t-f formatted text output\n");
fprintf(stderr, "\t\t-t text output (default)\n");
fprintf(stderr, "\t\t-a <paper size name> Adobe PDF output\n");
fprintf(stderr, "\t\t-p <paper size name> PostScript output\n");
fprintf(stderr, "\t\t   paper size like: a4, letter or legal\n");
fprintf(stderr, "\t\t-x <dtd> XML output\n");
fprintf(stderr, "\t\t   like: db (DocBook)\n");
fprintf(stderr, "\t\t-m <mapping> character mapping file\n");
fprintf(stderr, "\t\t-w <width> in characters of text output\n");
fprintf(stderr, "\t\t-i <level> image level (PostScript only)\n");
fprintf(stderr, "\t\t-L use landscape mode (PostScript only)\n");
fprintf(stderr, "\t\t-r Show removed text\n");
fprintf(stderr, "\t\t-s Show hidden (by Word) text\n");
}
static FILE *
pStdin2TmpFile(long *lFilesize)
{
FILE *pTmpFile;
size_t tSize;
BOOL bFailure;
UCHAR aucBytes[BUFSIZ];
DBG_MSG("pStdin2TmpFile");
fail(lFilesize == NULL);
pTmpFile = tmpfile();
if (pTmpFile == NULL) {
return NULL;
}
#if defined(__dos)
setmode(fileno(stdin), O_BINARY);
#endif
*lFilesize = 0;
bFailure = TRUE;
for (;;) {
tSize = fread(aucBytes, 1, sizeof(aucBytes), stdin);
if (tSize == 0) {
bFailure = feof(stdin) == 0;
break;
}
if (fwrite(aucBytes, 1, tSize, pTmpFile) != tSize) {
bFailure = TRUE;
break;
}
*lFilesize += (long)tSize;
}
#if defined(__dos)
setmode(fileno(stdin), O_TEXT);
#endif
if (bFailure) {
*lFilesize = 0;
(void)fclose(pTmpFile);
return NULL;
}
rewind(pTmpFile);
return pTmpFile;
}
static BOOL
bProcessFile(const char *szFilename)
{
FILE *pFile;
diagram_type *pDiag;
long lFilesize;
int iWordVersion;
BOOL bResult;
fail(szFilename == NULL || szFilename[0] == '\0');
DBG_MSG(szFilename);
if (szFilename[0] == '-' && szFilename[1] == '\0') {
pFile = pStdin2TmpFile(&lFilesize);
if (pFile == NULL) {
werr(0, "I can't save the standard input to a file");
return FALSE;
}
} else {
pFile = fopen(szFilename, "rb");
if (pFile == NULL) {
werr(0, "I can't open '%s' for reading", szFilename);
return FALSE;
}
lFilesize = lGetFilesize(szFilename);
if (lFilesize < 0) {
(void)fclose(pFile);
werr(0, "I can't get the size of '%s'", szFilename);
return FALSE;
}
}
iWordVersion = iGuessVersionNumber(pFile, lFilesize);
if (iWordVersion < 0 || iWordVersion == 3) {
if (bIsRtfFile(pFile)) {
werr(0, "%s is not a Word Document."
" It is probably a Rich Text Format file",
szFilename);
} if (bIsWordPerfectFile(pFile)) {
werr(0, "%s is not a Word Document."
" It is probably a Word Perfect file",
szFilename);
} else {
#if defined(__dos)
werr(0, "%s is not a Word Document or the filename"
" is not in the 8+3 format.", szFilename);
#else
werr(0, "%s is not a Word Document.", szFilename);
#endif
}
(void)fclose(pFile);
return FALSE;
}
rewind(pFile);
pDiag = pCreateDiagram(szTask, szFilename);
if (pDiag == NULL) {
(void)fclose(pFile);
return FALSE;
}
bResult = bWordDecryptor(pFile, lFilesize, pDiag);
vDestroyDiagram(pDiag);
(void)fclose(pFile);
return bResult;
}
int
main(int argc, char **argv)
{
options_type tOptions;
const char *szWordfile;
int iFirst, iIndex, iGoodCount;
BOOL bUsage, bMultiple, bUseTXT, bUseXML;
if (argc <= 0) {
return EXIT_FAILURE;
}
szTask = szBasename(argv[0]);
if (argc <= 1) {
iFirst = 1;
bUsage = TRUE;
} else {
iFirst = iReadOptions(argc, argv);
bUsage = iFirst <= 0;
}
if (bUsage) {
vUsage();
return iFirst < 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}
#if defined(N_PLAT_NLM) && !defined(_VA_LIST)
nwinit();
#endif
vGetOptions(&tOptions);
#if !defined(__dos)
if (is_locale_utf8()) {
#if defined(__STDC_ISO_10646__)
if (tOptions.eEncoding == encoding_utf_8) {
if (setlocale(LC_CTYPE, "") == NULL) {
werr(1, "Can't set the UTF-8 locale! "
"Check LANG, LC_CTYPE, LC_ALL.");
}
DBG_MSG("The UTF-8 locale has been set");
} else {
(void)setlocale(LC_CTYPE, "C");
}
#endif
} else {
if (setlocale(LC_CTYPE, "") == NULL) {
werr(0, "Can't set the locale! Will use defaults");
(void)setlocale(LC_CTYPE, "C");
}
DBG_MSG("The locale has been set");
}
#endif
bMultiple = argc - iFirst > 1;
bUseTXT = tOptions.eConversionType == conversion_text ||
tOptions.eConversionType == conversion_fmt_text;
bUseXML = tOptions.eConversionType == conversion_xml;
iGoodCount = 0;
#if defined(__dos)
if (tOptions.eConversionType == conversion_pdf) {
setmode(fileno(stdout), O_BINARY);
}
#endif
if (bUseXML) {
fprintf(stdout,
"<?xml version=\"1.0\" encoding=\"utf-8\"?>\n"
"<!DOCTYPE %s PUBLIC \"-
"\t\"http:
bMultiple ? "set" : "book");
if (bMultiple) {
fprintf(stdout, "<set>\n");
}
}
for (iIndex = iFirst; iIndex < argc; iIndex++) {
if (bMultiple && bUseTXT) {
szWordfile = szBasename(argv[iIndex]);
fprintf(stdout, "::::::::::::::\n");
fprintf(stdout, "%s\n", szWordfile);
fprintf(stdout, "::::::::::::::\n");
}
if (bProcessFile(argv[iIndex])) {
iGoodCount++;
}
}
if (bMultiple && bUseXML) {
fprintf(stdout, "</set>\n");
}
DBG_DEC(iGoodCount);
return iGoodCount <= 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}