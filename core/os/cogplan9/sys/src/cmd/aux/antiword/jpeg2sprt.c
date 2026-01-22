#include <stdio.h>
#include "antiword.h"
#if 0
static int iPicCounter = 0;
#endif
#if 0
static void
vCopy2File(UCHAR *pucJpeg, size_t tJpegSize)
{
FILE	*pOutFile;
size_t	tIndex;
char	szFilename[30];
sprintf(szFilename, "<Wimp$ScrapDir>.jpeg%04d", ++iPicCounter);
pOutFile = fopen(szFilename, "wb");
if (pOutFile == NULL) {
return;
}
DBG_MSG(szFilename);
for (tIndex = 0; tIndex < tJpegSize; tIndex++) {
if (putc(pucJpeg[tIndex], pOutFile) == EOF) {
break;
}
}
(void)fclose(pOutFile);
vSetFiletype(szFilename, FILETYPE_JPEG);
}
#endif
BOOL
bSave2Draw(diagram_type *pDiag, FILE *pFile,
size_t tJpegSize, const imagedata_type *pImg)
{
UCHAR	*pucJpeg, *pucTmp;
size_t	tLen;
int	iByte;
pucJpeg = xmalloc(tJpegSize);
for (pucTmp = pucJpeg, tLen = 0; tLen < tJpegSize; pucTmp++, tLen++) {
iByte = iNextByte(pFile);
if (iByte == EOF) {
return FALSE;
}
*pucTmp = (UCHAR)iByte;
}
#if 0
vCopy2File(pucJpeg, tJpegSize);
#endif
vImage2Diagram(pDiag, pImg, pucJpeg, tJpegSize);
xfree(pucJpeg);
return TRUE;
}
BOOL
bTranslateJPEG(diagram_type *pDiag, FILE *pFile,
ULONG ulFileOffset, size_t tPictureLen, const imagedata_type *pImg)
{
if (!bSetDataOffset(pFile, ulFileOffset)) {
return FALSE;
}
if (iGetRiscOsVersion() >= 360) {
return bSave2Draw(pDiag, pFile, tPictureLen, pImg);
}
return bAddDummyImage(pDiag, pImg);
}