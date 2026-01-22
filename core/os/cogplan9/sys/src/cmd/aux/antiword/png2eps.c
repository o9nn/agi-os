#include <stdio.h>
#include <ctype.h>
#include "antiword.h"
#if defined(DEBUG)
static int	iPicCounter = 0;
#endif
static size_t
tSkipToData(FILE *pFile, size_t tMaxBytes, size_t *ptSkipped)
{
ULONG	ulName, ulTmp;
size_t	tDataLength, tToSkip;
int	iCounter;
fail(pFile == NULL);
fail(ptSkipped == NULL);
while (*ptSkipped + 8 < tMaxBytes) {
tDataLength = (size_t)ulNextLongBE(pFile);
DBG_DEC(tDataLength);
*ptSkipped += 4;
ulName = 0x00;
for (iCounter = 0; iCounter < 4; iCounter++) {
ulTmp = (ULONG)iNextByte(pFile);
if (!isalpha((int)ulTmp)) {
DBG_HEX(ulTmp);
return (size_t)-1;
}
ulName <<= 8;
ulName |= ulTmp;
}
DBG_HEX(ulName);
*ptSkipped += 4;
if (ulName == PNG_CN_IEND) {
break;
}
if (ulName == PNG_CN_IDAT) {
return tDataLength;
}
tToSkip = tDataLength + 4;
if (tToSkip >= tMaxBytes - *ptSkipped) {
DBG_DEC(tToSkip);
DBG_DEC(tMaxBytes - *ptSkipped);
return (size_t)-1;
}
(void)tSkipBytes(pFile, tToSkip);
*ptSkipped += tToSkip;
}
return (size_t)-1;
}
static size_t
tFindFirstPixelData(FILE *pFile, size_t tMaxBytes, size_t *ptSkipped)
{
fail(pFile == NULL);
fail(tMaxBytes == 0);
fail(ptSkipped == NULL);
if (tMaxBytes < 8) {
DBG_DEC(tMaxBytes);
return (size_t)-1;
}
(void)tSkipBytes(pFile, 8);
*ptSkipped = 8;
return tSkipToData(pFile, tMaxBytes, ptSkipped);
}
static size_t
tFindNextPixelData(FILE *pFile, size_t tMaxBytes, size_t *ptSkipped)
{
fail(pFile == NULL);
fail(tMaxBytes == 0);
fail(ptSkipped == NULL);
if (tMaxBytes < 4) {
DBG_DEC(tMaxBytes);
return (size_t)-1;
}
(void)tSkipBytes(pFile, 4);
*ptSkipped = 4;
return tSkipToData(pFile, tMaxBytes, ptSkipped);
}
#if defined(DEBUG)
static void
vCopy2File(FILE *pFile, ULONG ulFileOffset, size_t tPictureLen)
{
FILE	*pOutFile;
size_t	tIndex;
int	iTmp;
char	szFilename[30];
if (!bSetDataOffset(pFile, ulFileOffset)) {
return;
}
sprintf(szFilename, "/tmp/pic/pic%04d.png", ++iPicCounter);
pOutFile = fopen(szFilename, "wb");
if (pOutFile == NULL) {
return;
}
for (tIndex = 0; tIndex < tPictureLen; tIndex++) {
iTmp = iNextByte(pFile);
if (putc(iTmp, pOutFile) == EOF) {
break;
}
}
(void)fclose(pOutFile);
}
#endif
BOOL
bTranslatePNG(diagram_type *pDiag, FILE *pFile,
ULONG ulFileOffset, size_t tPictureLen, const imagedata_type *pImg)
{
size_t	tMaxBytes, tDataLength, tSkipped;
#if defined(DEBUG)
vCopy2File(pFile, ulFileOffset, tPictureLen);
#endif
if (!bSetDataOffset(pFile, ulFileOffset)) {
return FALSE;
}
tMaxBytes = tPictureLen;
tDataLength = tFindFirstPixelData(pFile, tMaxBytes, &tSkipped);
if (tDataLength == (size_t)-1) {
return FALSE;
}
vImagePrologue(pDiag, pImg);
do {
tMaxBytes -= tSkipped;
vASCII85EncodeArray(pFile, pDiag->pOutFile, tDataLength);
tMaxBytes -= tDataLength;
tDataLength = tFindNextPixelData(pFile, tMaxBytes, &tSkipped);
} while (tDataLength != (size_t)-1);
vASCII85EncodeByte(pDiag->pOutFile, EOF);
vImageEpilogue(pDiag);
return TRUE;
}