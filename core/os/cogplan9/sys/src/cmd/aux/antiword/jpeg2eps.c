#include <stdio.h>
#include "antiword.h"
#if defined(DEBUG)
static int	iPicCounter = 0;
#endif
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
sprintf(szFilename, "/tmp/pic/pic%04d.jpg", ++iPicCounter);
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
bTranslateJPEG(diagram_type *pDiag, FILE *pFile,
ULONG ulFileOffset, size_t tPictureLen, const imagedata_type *pImg)
{
#if defined(DEBUG)
vCopy2File(pFile, ulFileOffset, tPictureLen);
#endif
if (!bSetDataOffset(pFile, ulFileOffset)) {
return FALSE;
}
vImagePrologue(pDiag, pImg);
vASCII85EncodeFile(pFile, pDiag->pOutFile, tPictureLen);
vImageEpilogue(pDiag);
return TRUE;
}