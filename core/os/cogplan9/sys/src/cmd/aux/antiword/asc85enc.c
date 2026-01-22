#include <stdio.h>
#include "antiword.h"
static const ULONG	aulPower85[5] = {
1UL, 85UL, 85UL * 85, 85UL * 85 * 85, 85UL * 85 * 85 * 85,
};
static int	iOutBytes = 0;
static char	cCharPrev = '\0';
static void
vOutputByte(ULONG ulChar, FILE *pOutFile)
{
if (iOutBytes == 1 && cCharPrev == '%' && ulChar == (ULONG)'%') {
if (putc('\n', pOutFile) != EOF) {
iOutBytes = 0;
}
}
if (putc((int)ulChar, pOutFile) == EOF) {
return;
}
iOutBytes++;
if (iOutBytes > 63) {
if (putc('\n', pOutFile) != EOF) {
iOutBytes = 0;
}
}
cCharPrev = (char)ulChar;
}
void
vASCII85EncodeByte(FILE *pOutFile, int iByte)
{
static ULONG	ulBuffer[4] = { 0, 0, 0, 0 };
static int	iInBuffer = 0;
ULONG	ulValue, ulTmp;
int	iIndex;
fail(pOutFile == NULL);
fail(iInBuffer < 0);
fail(iInBuffer > 3);
if (iByte == EOF) {
if (iInBuffer > 0 && iInBuffer < 4) {
ulValue = 0;
for (iIndex = iInBuffer - 1; iIndex >= 0; iIndex--) {
ulValue |=
ulBuffer[iIndex] << (8 * (3 - iIndex));
}
for (iIndex = 4; iIndex >= 4 - iInBuffer; iIndex--) {
ulTmp = ulValue / aulPower85[iIndex];
vOutputByte(ulTmp + '!', pOutFile);
ulValue -= ulTmp * aulPower85[iIndex];
}
}
(void)putc('~', pOutFile);
(void)putc('>', pOutFile);
(void)putc('\n', pOutFile);
iInBuffer = 0;
iOutBytes = 0;
cCharPrev = '\0';
return;
}
ulBuffer[iInBuffer] = (ULONG)iByte & 0xff;
iInBuffer++;
if (iInBuffer >= 4) {
ulValue = (ulBuffer[0] << 24) | (ulBuffer[1] << 16) |
(ulBuffer[2] << 8) | ulBuffer[3];
if (ulValue == 0) {
vOutputByte((ULONG)'z', pOutFile);
} else {
for (iIndex = 4; iIndex >= 0; iIndex--) {
ulTmp = ulValue / aulPower85[iIndex];
vOutputByte(ulTmp + '!', pOutFile);
ulValue -= ulTmp * aulPower85[iIndex];
}
}
iInBuffer = 0;
}
}
void
vASCII85EncodeArray(FILE *pInFile, FILE *pOutFile, size_t tLength)
{
size_t	tCount;
int	iByte;
fail(pInFile == NULL);
fail(pOutFile == NULL);
DBG_DEC(tLength);
for (tCount = 0; tCount < tLength; tCount++) {
iByte = iNextByte(pInFile);
if (iByte == EOF) {
break;
}
vASCII85EncodeByte(pOutFile, iByte);
}
}
void
vASCII85EncodeFile(FILE *pInFile, FILE *pOutFile, size_t tLength)
{
fail(pInFile == NULL);
fail(pOutFile == NULL);
fail(tLength == 0);
vASCII85EncodeArray(pInFile, pOutFile, tLength);
vASCII85EncodeByte(pOutFile, EOF);
}