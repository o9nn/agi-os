#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <time.h>
#if defined(__riscos)
#include "DeskLib:SWI.h"
#else
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#endif
#if !defined(S_ISREG)
#define S_ISREG(x)	(((x) & S_IFMT) == S_IFREG)
#endif
#include "antiword.h"
#if defined(__vms)
#include <unixlib.h>
#endif
#if !defined(__riscos)
const char *
szGetHomeDirectory(void)
{
const char	*szHome;
#if defined(__vms)
szHome = decc$translate_vms(getenv("HOME"));
#elif defined(__Plan9__)
szHome = getenv("home");
#else
szHome = getenv("HOME");
#endif
if (szHome == NULL || szHome[0] == '\0') {
#if defined(N_PLAT_NLM)
szHome = "SYS:";
#elif defined(__dos)
szHome = "C:";
#else
werr(0, "I can't find the name of your HOME directory");
szHome = "";
#endif
}
return szHome;
}
const char *
szGetAntiwordDirectory(void)
{
#if defined(__vms)
return decc$translate_vms(getenv("ANTIWORDHOME"));
#else
return getenv("ANTIWORDHOME");
#endif
}
#endif
long
lGetFilesize(const char *szFilename)
{
#if defined(__riscos)
os_error	*e;
int	iType, iSize;
e = SWI(2, 5, SWI_OS_File | XOS_Bit,
17, szFilename,
&iType, NULL, NULL, NULL, &iSize);
if (e != NULL) {
werr(0, "Get Filesize error %d: %s",
e->errnum, e->errmess);
return -1;
}
if (iType != 1) {
return -1;
}
return (long)iSize;
#else
struct stat	tBuffer;
errno = 0;
if (stat(szFilename, &tBuffer) != 0) {
werr(0, "Get Filesize error %d", errno);
return -1;
}
if (!S_ISREG(tBuffer.st_mode)) {
return -1;
}
return (long)tBuffer.st_size;
#endif
}
#if defined(DEBUG)
void
vPrintBlock(const char	*szFile, int iLine,
const UCHAR *aucBlock, size_t tLength)
{
int i, j;
fail(szFile == NULL || iLine < 0 || aucBlock == NULL);
fprintf(stderr, "%s[%3d]:\n", szFile, iLine);
for (i = 0; i < 32; i++) {
if (16 * i >= (int)tLength) {
return;
}
fprintf(stderr, "%03x: ", (unsigned int)(16 * i));
for (j = 0; j < 16; j++) {
if (16 * i + j < (int)tLength) {
fprintf(stderr, "%02x ",
(unsigned int)aucBlock[16 * i + j]);
}
}
fprintf(stderr, "\n");
}
}
void
vPrintUnicode(const char *szFile, int iLine, const UCHAR *aucUni, size_t tLen)
{
char	*szASCII;
fail(tLen % 2 != 0);
tLen /= 2;
szASCII = xmalloc(tLen + 1);
(void)unincpy(szASCII, aucUni, tLen);
szASCII[tLen] = '\0';
(void)fprintf(stderr, "%s[%3d]: %.*s\n",
szFile, iLine, (int)tLen, szASCII);
szASCII = xfree(szASCII);
}
BOOL
bCheckDoubleLinkedList(output_type *pAnchor)
{
output_type	*pCurr, *pLast;
int		iInList;
pLast = pAnchor;
iInList = 0;
for (pCurr = pAnchor; pCurr != NULL; pCurr = pCurr->pNext) {
pLast = pCurr;
iInList++;
}
NO_DBG_DEC(iInList);
for (pCurr = pLast; pCurr != NULL; pCurr = pCurr->pPrev) {
pLast = pCurr;
iInList--;
}
DBG_DEC_C(iInList != 0, iInList);
return pAnchor == pLast && iInList == 0;
}
#endif
BOOL
bReadBytes(UCHAR *aucBytes, size_t tMemb, ULONG ulOffset, FILE *pFile)
{
fail(aucBytes == NULL || pFile == NULL || ulOffset > (ULONG)LONG_MAX);
if (ulOffset > (ULONG)LONG_MAX) {
return FALSE;
}
if (fseek(pFile, (long)ulOffset, SEEK_SET) != 0) {
return FALSE;
}
if (fread(aucBytes, sizeof(UCHAR), tMemb, pFile) != tMemb) {
return FALSE;
}
return TRUE;
}
BOOL
bReadBuffer(FILE *pFile, ULONG ulStartBlock,
const ULONG *aulBlockDepot, size_t tBlockDepotLen, size_t tBlockSize,
UCHAR *aucBuffer, ULONG ulOffset, size_t tToRead)
{
ULONG	ulBegin, ulIndex;
size_t	tLen;
fail(pFile == NULL);
fail(ulStartBlock > MAX_BLOCKNUMBER && ulStartBlock != END_OF_CHAIN);
fail(aulBlockDepot == NULL);
fail(tBlockSize != BIG_BLOCK_SIZE && tBlockSize != SMALL_BLOCK_SIZE);
fail(aucBuffer == NULL);
fail(tToRead == 0);
for (ulIndex = ulStartBlock;
ulIndex != END_OF_CHAIN && tToRead != 0;
ulIndex = aulBlockDepot[ulIndex]) {
if (ulIndex >= (ULONG)tBlockDepotLen) {
DBG_DEC(ulIndex);
DBG_DEC(tBlockDepotLen);
if (tBlockSize >= BIG_BLOCK_SIZE) {
werr(1, "The Big Block Depot is damaged");
} else {
werr(1, "The Small Block Depot is damaged");
}
}
if (ulOffset >= (ULONG)tBlockSize) {
ulOffset -= tBlockSize;
continue;
}
ulBegin = ulDepotOffset(ulIndex, tBlockSize) + ulOffset;
tLen = min(tBlockSize - (size_t)ulOffset, tToRead);
ulOffset = 0;
if (!bReadBytes(aucBuffer, tLen, ulBegin, pFile)) {
werr(0, "Read big block 0x%lx not possible", ulBegin);
return FALSE;
}
aucBuffer += tLen;
tToRead -= tLen;
}
DBG_DEC_C(tToRead != 0, tToRead);
return tToRead == 0;
}
ULONG
ulColor2Color(UCHAR ucFontColor)
{
static const ULONG	aulColorTable[] = {
0x00000000UL,
0x00000000UL,
0xff000000UL,
0xffff0000UL,
0x00ff0000UL,
0xff00ff00UL,
0x0000ff00UL,
0x00ffff00UL,
0xffffff00UL,
0x80000000UL,
0x80800000UL,
0x00800000UL,
0x80008000UL,
0x00008000UL,
0x00808000UL,
0x80808000UL,
0xc0c0c000UL,
};
if ((size_t)ucFontColor >= elementsof(aulColorTable)) {
return aulColorTable[0];
}
return aulColorTable[(int)ucFontColor];
}
static int
iFindSplit(const char *szString, size_t tStringLen)
{
size_t	tSplit;
if (tStringLen == 0) {
return -1;
}
tSplit = tStringLen - 1;
while (tSplit >= 1) {
if (szString[tSplit] == ' ' ||
(szString[tSplit] == '-' && szString[tSplit - 1] != ' ')) {
return (int)tSplit;
}
tSplit--;
}
return -1;
}
output_type *
pSplitList(output_type *pAnchor)
{
output_type	*pCurr, *pLeftOver;
int		iIndex;
fail(pAnchor == NULL);
for (pCurr = pAnchor; pCurr->pNext != NULL; pCurr = pCurr->pNext)
;
iIndex = -1;
for (; pCurr != NULL; pCurr = pCurr->pPrev) {
iIndex = iFindSplit(pCurr->szStorage, pCurr->tNextFree);
if (iIndex >= 0) {
break;
}
}
if (pCurr == NULL || iIndex < 0) {
return NULL;
}
NO_DBG_MSG("pLeftOver");
pLeftOver = xmalloc(sizeof(*pLeftOver));
fail(pCurr->tNextFree < (size_t)iIndex);
pLeftOver->tStorageSize = pCurr->tNextFree - (size_t)iIndex;
pLeftOver->szStorage = xmalloc(pLeftOver->tStorageSize);
pLeftOver->tNextFree = pCurr->tNextFree - (size_t)iIndex - 1;
(void)strncpy(pLeftOver->szStorage,
pCurr->szStorage + iIndex + 1, pLeftOver->tNextFree);
pLeftOver->szStorage[pLeftOver->tNextFree] = '\0';
NO_DBG_MSG(pLeftOver->szStorage);
pLeftOver->ucFontColor = pCurr->ucFontColor;
pLeftOver->usFontStyle = pCurr->usFontStyle;
pLeftOver->tFontRef = pCurr->tFontRef;
pLeftOver->usFontSize = pCurr->usFontSize;
pLeftOver->lStringWidth = lComputeStringWidth(
pLeftOver->szStorage,
pLeftOver->tNextFree,
pLeftOver->tFontRef,
pLeftOver->usFontSize);
pLeftOver->pPrev = NULL;
pLeftOver->pNext = pCurr->pNext;
if (pLeftOver->pNext != NULL) {
pLeftOver->pNext->pPrev = pLeftOver;
}
fail(!bCheckDoubleLinkedList(pLeftOver));
NO_DBG_MSG("pAnchor");
NO_DBG_HEX(pCurr->szStorage[iIndex]);
while (iIndex >= 0 && isspace((int)(UCHAR)pCurr->szStorage[iIndex])) {
iIndex--;
}
pCurr->tNextFree = (size_t)iIndex + 1;
pCurr->szStorage[pCurr->tNextFree] = '\0';
NO_DBG_MSG(pCurr->szStorage);
pCurr->lStringWidth = lComputeStringWidth(
pCurr->szStorage,
pCurr->tNextFree,
pCurr->tFontRef,
pCurr->usFontSize);
pCurr->pNext = NULL;
fail(!bCheckDoubleLinkedList(pAnchor));
return pLeftOver;
}
size_t
tNumber2Roman(UINT uiNumber, BOOL bUpperCase, char *szOutput)
{
char	*outp, *p, *q;
UINT	uiNextVal, uiValue;
fail(szOutput == NULL);
uiNumber %= 4000;
if (uiNumber == 0) {
szOutput[0] = '\0';
return 0;
}
outp = szOutput;
p = bUpperCase ? "M\2D\5C\2L\5X\2V\5I" : "m\2d\5c\2l\5x\2v\5i";
uiValue = 1000;
for (;;) {
while (uiNumber >= uiValue) {
*outp++ = *p;
uiNumber -= uiValue;
}
if (uiNumber == 0) {
*outp = '\0';
fail(outp < szOutput);
return (size_t)(outp - szOutput);
}
q = p + 1;
uiNextVal = uiValue / (UINT)(UCHAR)*q;
if ((int)*q == 2) {
uiNextVal /= (UINT)(UCHAR)*(q += 2);
}
if (uiNumber + uiNextVal >= uiValue) {
*outp++ = *++q;
uiNumber += uiNextVal;
} else {
p++;
uiValue /= (UINT)(UCHAR)(*p++);
}
}
}
size_t
tNumber2Alpha(UINT uiNumber, BOOL bUpperCase, char *szOutput)
{
char	*outp;
UINT	uiTmp;
fail(szOutput == NULL);
if (uiNumber == 0) {
szOutput[0] = '\0';
return 0;
}
outp = szOutput;
uiTmp = (UINT)(bUpperCase ? 'A': 'a');
if (uiNumber <= 26) {
uiNumber -= 1;
*outp++ = (char)(uiTmp + uiNumber);
} else if (uiNumber <= 26U + 26U*26U) {
uiNumber -= 26 + 1;
*outp++ = (char)(uiTmp + uiNumber / 26);
*outp++ = (char)(uiTmp + uiNumber % 26);
} else if (uiNumber <= 26U + 26U*26U + 26U*26U*26U) {
uiNumber -= 26 + 26*26 + 1;
*outp++ = (char)(uiTmp + uiNumber / (26*26));
*outp++ = (char)(uiTmp + uiNumber / 26 % 26);
*outp++ = (char)(uiTmp + uiNumber % 26);
}
*outp = '\0';
fail(outp < szOutput);
return (size_t)(outp - szOutput);
}
char *
unincpy(char *s1, const UCHAR *s2, size_t n)
{
char	*pcDest;
ULONG	ulChar;
size_t	tLen;
USHORT	usUni;
for (pcDest = s1, tLen = 0; tLen < n; pcDest++, tLen++) {
usUni = usGetWord(tLen * 2, s2);
if (usUni == 0) {
break;
}
ulChar = ulTranslateCharacters(usUni, 0, 8,
conversion_unknown, encoding_neutral, FALSE);
if (ulChar == IGNORE_CHARACTER) {
ulChar = (ULONG)'?';
}
*pcDest = (char)ulChar;
}
for (; tLen < n; tLen++) {
*pcDest++ = '\0';
}
return s1;
}
size_t
unilen(const UCHAR *s)
{
size_t	tLen;
USHORT	usUni;
tLen = 0;
for (;;) {
usUni = usGetWord(tLen, s);
if (usUni == 0) {
return tLen;
}
tLen += 2;
}
}
const char *
szBasename(const char *szFilename)
{
const char	*szTmp;
fail(szFilename == NULL);
if (szFilename == NULL || szFilename[0] == '\0') {
return "null";
}
szTmp = strrchr(szFilename, FILE_SEPARATOR[0]);
if (szTmp == NULL) {
return szFilename;
}
return ++szTmp;
}
long
lComputeLeading(USHORT usFontSize)
{
long	lLeading;
lLeading = (long)usFontSize * 500L;
if (usFontSize < 18) {
lLeading *= 112;
} else if (usFontSize < 28) {
lLeading *= 124;
} else if (usFontSize < 48) {
lLeading *= 104;
} else {
lLeading *= 100;
}
lLeading = lMilliPoints2DrawUnits(lLeading);
lLeading += 50;
lLeading /= 100;
return lLeading;
}
size_t
tUcs2Utf8(ULONG ulChar, char *szResult, size_t tMaxResultLen)
{
if (szResult == NULL || tMaxResultLen == 0) {
return 0;
}
if (ulChar < 0x80 && tMaxResultLen >= 2) {
szResult[0] = (char)ulChar;
szResult[1] = '\0';
return 1;
}
if (ulChar < 0x800 && tMaxResultLen >= 3) {
szResult[0] = (char)(0xc0 | ulChar >> 6);
szResult[1] = (char)(0x80 | (ulChar & 0x3f));
szResult[2] = '\0';
return 2;
}
if (ulChar < 0x10000 && tMaxResultLen >= 4) {
szResult[0] = (char)(0xe0 | ulChar >> 12);
szResult[1] = (char)(0x80 | (ulChar >> 6 & 0x3f));
szResult[2] = (char)(0x80 | (ulChar & 0x3f));
szResult[3] = '\0';
return 3;
}
if (ulChar < 0x200000 && tMaxResultLen >= 5) {
szResult[0] = (char)(0xf0 | ulChar >> 18);
szResult[1] = (char)(0x80 | (ulChar >> 12 & 0x3f));
szResult[2] = (char)(0x80 | (ulChar >> 6 & 0x3f));
szResult[3] = (char)(0x80 | (ulChar & 0x3f));
szResult[4] = '\0';
return 4;
}
szResult[0] = '\0';
return 0;
}
void
vGetBulletValue(conversion_type eConversionType, encoding_type eEncoding,
char *szResult, size_t tMaxResultLen)
{
fail(szResult == NULL);
fail(tMaxResultLen < 2);
if (eEncoding == encoding_utf_8) {
(void)tUcs2Utf8(UNICODE_BULLET, szResult, tMaxResultLen);
} else {
szResult[0] = (char)ucGetBulletCharacter(eConversionType,
eEncoding);
szResult[1] = '\0';
}
}
BOOL
bAllZero(const UCHAR *aucBytes, size_t tLength)
{
size_t	tIndex;
if (aucBytes == NULL || tLength == 0) {
return TRUE;
}
for (tIndex = 0; tIndex < tLength; tIndex++) {
if (aucBytes[tIndex] != 0) {
return FALSE;
}
}
return TRUE;
}
#if !defined(__riscos)
static BOOL
bGetCodesetFromLocale(char *szCodeset, size_t tMaxCodesetLength, BOOL *pbEuro)
{
#if !defined(__dos)
const char	*szLocale;
const char	*pcTmp;
size_t		tIndex;
char		szModifier[6];
#endif
if (pbEuro != NULL) {
*pbEuro = FALSE;
}
if (szCodeset == NULL || tMaxCodesetLength == 0) {
return FALSE;
}
#if defined(__dos)
if (tMaxCodesetLength < 2 + sizeof(int) * 3 + 1) {
DBG_DEC(tMaxCodesetLength);
DBG_DEC(2 + sizeof(int) * 3 + 1);
return FALSE;
}
sprintf(szCodeset, "cp%d", iGetCodepage());
DBG_MSG(szCodeset);
#else
szLocale = getenv("LC_ALL");
if (szLocale == NULL || szLocale[0] == '\0') {
szLocale = getenv("LC_CTYPE");
if (szLocale == NULL || szLocale[0] == '\0') {
szLocale = getenv("LANG");
}
}
if (szLocale == NULL || szLocale[0] == '\0') {
return FALSE;
}
DBG_MSG(szLocale);
pcTmp = strchr(szLocale, '.');
if (pcTmp == NULL) {
szCodeset[0] = '\0';
} else {
pcTmp++;
for (tIndex = 0; tIndex < tMaxCodesetLength; tIndex++) {
if (*pcTmp == '@' || *pcTmp == '+' ||
*pcTmp == ',' || *pcTmp == '_' ||
*pcTmp == '\0') {
szCodeset[tIndex] = '\0';
break;
}
szCodeset[tIndex] = *pcTmp;
pcTmp++;
}
szCodeset[tMaxCodesetLength - 1] = '\0';
}
if (pbEuro == NULL) {
return TRUE;
}
pcTmp = strchr(szLocale, '@');
if (pcTmp != NULL) {
pcTmp++;
for (tIndex = 0; tIndex < sizeof(szModifier); tIndex++) {
if (*pcTmp == '+' || *pcTmp == ',' ||
*pcTmp == '_' || *pcTmp == '\0') {
szModifier[tIndex] = '\0';
break;
}
szModifier[tIndex] = *pcTmp;
pcTmp++;
}
szModifier[sizeof(szModifier) - 1] = '\0';
*pbEuro = STRCEQ(szModifier, "Euro");
}
#endif
return TRUE;
}
BOOL
bGetNormalizedCodeset(char *szCodeset, size_t tMaxCodesetLength, BOOL *pbEuro)
{
BOOL	bOnlyDigits;
const char	*pcSrc;
char	*pcDest;
char	*szTmp, *szCodesetNorm;
if (pbEuro != NULL) {
*pbEuro = FALSE;
}
if (szCodeset == NULL || tMaxCodesetLength < 4) {
return FALSE;
}
szTmp = xmalloc(tMaxCodesetLength - 3);
if (!bGetCodesetFromLocale(szTmp, tMaxCodesetLength - 3, pbEuro)) {
szTmp = xfree(szTmp);
return FALSE;
}
szCodesetNorm = xmalloc(tMaxCodesetLength - 3);
bOnlyDigits = TRUE;
pcDest = szCodesetNorm;
for (pcSrc = szTmp; *pcSrc != '\0'; pcSrc++) {
if (isalnum(*pcSrc)) {
*pcDest = tolower(*pcSrc);
if (!isdigit(*pcDest)) {
bOnlyDigits = FALSE;
}
pcDest++;
}
}
*pcDest = '\0';
DBG_MSG(szCodesetNorm);
if (bOnlyDigits && szCodesetNorm[0] != '\0') {
fail(strlen(szCodesetNorm) + 3 >= tMaxCodesetLength);
sprintf(szCodeset, "iso%s", szCodesetNorm);
} else {
fail(strlen(szCodesetNorm) >= tMaxCodesetLength);
strncpy(szCodeset, szCodesetNorm, pcDest - szCodesetNorm + 1);
szCodeset[tMaxCodesetLength - 1] = '\0';
}
DBG_MSG(szCodeset);
szCodesetNorm = xfree(szCodesetNorm);
szTmp = xfree(szTmp);
return TRUE;
}
const char *
szGetDefaultMappingFile(void)
{
static const struct {
const char	*szCodeset;
const char	*szMappingFile;
} atMappingFile[] = {
{ "iso88591",	MAPPING_FILE_8859_1 },
{ "iso88592",	MAPPING_FILE_8859_2 },
{ "iso88593",	"8859-3.txt" },
{ "iso88594",	"8859-4.txt" },
{ "iso88595",	"8859-5.txt" },
{ "iso88596",	MAPPING_FILE_8859_5 },
{ "iso88597",	"8859-7.txt" },
{ "iso88598",	"8859-8.txt" },
{ "iso88599",	"8859-9.txt" },
{ "iso885910",	"8859-10.txt" },
{ "iso885913",	"8859-13.txt" },
{ "iso885914",	"8859-14.txt" },
{ "iso885915",	MAPPING_FILE_8859_15 },
{ "iso885916",	"8859-16.txt" },
{ "koi8r",	MAPPING_FILE_KOI8_R },
{ "koi8u",	MAPPING_FILE_KOI8_U },
{ "utf8",	MAPPING_FILE_UTF_8 },
{ "cp437",	MAPPING_FILE_CP437 },
{ "cp850",	"cp850.txt" },
{ "cp852",	MAPPING_FILE_CP852 },
{ "cp862",	"cp862.txt" },
{ "cp864",	"cp864.txt" },
{ "cp866",	MAPPING_FILE_CP866 },
{ "cp1250",	MAPPING_FILE_CP1250 },
{ "cp1251",	MAPPING_FILE_CP1251 },
{ "cp1252",	"cp1252.txt" },
};
size_t	tIndex;
BOOL	bEuro;
char	szCodeset[20];
szCodeset[0] = '\0';
bEuro = FALSE;
if (!bGetNormalizedCodeset(szCodeset, sizeof(szCodeset), &bEuro)) {
return MAPPING_FILE_8859_1;
}
if (szCodeset[0] == '\0') {
if (bEuro) {
return MAPPING_FILE_8859_15;
} else {
return MAPPING_FILE_8859_1;
}
}
for (tIndex = 0; tIndex < elementsof(atMappingFile); tIndex++) {
if (STREQ(atMappingFile[tIndex].szCodeset, szCodeset)) {
return atMappingFile[tIndex].szMappingFile;
}
}
#if defined(__dos)
return MAPPING_FILE_CP437;
#else
return MAPPING_FILE_8859_1;
#endif
}
#endif
time_t
tConvertDTTM(ULONG ulDTTM)
{
struct tm	tTime;
time_t		tResult;
if (ulDTTM == 0) {
return (time_t)-1;
}
memset(&tTime, 0, sizeof(tTime));
tTime.tm_min = (int)(ulDTTM & 0x0000003f);
tTime.tm_hour = (int)((ulDTTM & 0x000007c0) >> 6);
tTime.tm_mday = (int)((ulDTTM & 0x0000f800) >> 11);
tTime.tm_mon = (int)((ulDTTM & 0x000f0000) >> 16);
tTime.tm_year = (int)((ulDTTM & 0x1ff00000) >> 20);
tTime.tm_isdst = -1;
tTime.tm_mon--;
tResult = mktime(&tTime);
NO_DBG_MSG(ctime(&tResult));
return tResult;
}