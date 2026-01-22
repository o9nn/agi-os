#include "antiword.h"
typedef struct footnote_local_tag {
footnote_block_type tInfo;
ULONG ulCharPosStart;
ULONG ulCharPosNext;
BOOL bUseful;
} footnote_local_type;
static ULONG *aulFootnoteList = NULL;
static size_t tFootnoteListLength = 0;
static ULONG *aulEndnoteList = NULL;
static size_t tEndnoteListLength = 0;
static footnote_local_type *pFootnoteText = NULL;
static size_t tFootnoteTextLength = 0;
void
vDestroyNotesInfoLists(void)
{
footnote_local_type *pRecord;
size_t tFootnote;
TRACE_MSG("vDestroyNotesInfoLists");
aulEndnoteList = xfree(aulEndnoteList);
aulFootnoteList = xfree(aulFootnoteList);
tEndnoteListLength = 0;
tFootnoteListLength = 0;
for (tFootnote = 0; tFootnote < tFootnoteTextLength; tFootnote++) {
pRecord = pFootnoteText + tFootnote;
pRecord->tInfo.szText = xfree(pRecord->tInfo.szText);
}
pFootnoteText = xfree(pFootnoteText);
tFootnoteTextLength = 0;
}
static void
vGet0FootnotesInfoAndText(FILE *pFile, const UCHAR *aucHeader)
{
footnote_local_type *pCurr;
UCHAR *aucBuffer;
ULONG ulFileOffset, ulBeginOfText, ulOffset, ulBeginFootnoteInfo;
ULONG ulCharPos, ulBeginNextBlock;
size_t tFootnotes, tFootnoteInfoLen;
size_t tIndex;
UCHAR aucTmp[2];
TRACE_MSG("vGet0FootnotesInfoAndText");
fail(pFile == NULL || aucHeader == NULL);
ulBeginOfText = 128;
NO_DBG_HEX(ulBeginOfText);
ulBeginFootnoteInfo = 128 * (ULONG)usGetWord(0x14, aucHeader);
DBG_HEX(ulBeginFootnoteInfo);
ulBeginNextBlock = 128 * (ULONG)usGetWord(0x16, aucHeader);
DBG_HEX(ulBeginNextBlock);
if (ulBeginFootnoteInfo == ulBeginNextBlock) {
DBG_MSG("No Footnotes in this document");
return;
}
if (!bReadBytes(aucTmp, 2, ulBeginFootnoteInfo, pFile)) {
return;
}
tFootnotes = (size_t)usGetWord(0, aucTmp);
if (tFootnotes < 2) {
DBG_MSG("No Footnotes in this document (2)");
}
DBG_DEC(tFootnotes);
tFootnoteInfoLen = 8 * tFootnotes;
aucBuffer = xmalloc(tFootnoteInfoLen);
if (!bReadBytes(aucBuffer,
tFootnoteInfoLen, ulBeginFootnoteInfo + 4, pFile)) {
aucBuffer = xfree(aucBuffer);
return;
}
DBG_PRINT_BLOCK(aucBuffer, tFootnoteInfoLen);
fail(tFootnoteListLength != 0);
tFootnoteListLength = tFootnotes - 1;
fail(tFootnoteListLength == 0);
fail(aulFootnoteList != NULL);
aulFootnoteList = xcalloc(tFootnoteListLength, sizeof(ULONG));
for (tIndex = 0; tIndex < tFootnoteListLength; tIndex++) {
ulOffset = ulGetLong(tIndex * 8, aucBuffer);
DBG_HEX(ulOffset);
ulFileOffset = ulCharPos2FileOffset(ulBeginOfText + ulOffset);
DBG_HEX(ulFileOffset);
aulFootnoteList[tIndex] = ulFileOffset;
}
fail(tFootnoteTextLength != 0);
tFootnoteTextLength = tFootnotes - 1;
fail(tFootnoteTextLength == 0);
fail(pFootnoteText != NULL);
pFootnoteText = xcalloc(tFootnoteTextLength,
sizeof(footnote_local_type));
for (tIndex = 0; tIndex < tFootnoteTextLength; tIndex++) {
pCurr = pFootnoteText + tIndex;
pCurr->tInfo.szText = NULL;
ulOffset = ulGetLong(tIndex * 8 + 4, aucBuffer);
DBG_HEX(ulOffset);
ulCharPos = ulBeginOfText + ulOffset;
DBG_HEX(ulCharPos);
DBG_HEX(ulCharPos2FileOffset(ulCharPos));
pCurr->ulCharPosStart = ulCharPos;
ulOffset = ulGetLong((tIndex + 1) * 8 + 4, aucBuffer);
DBG_HEX(ulOffset);
ulCharPos = ulBeginOfText + ulOffset;
DBG_HEX(ulCharPos);
DBG_HEX(ulCharPos2FileOffset(ulCharPos));
pCurr->ulCharPosNext = ulCharPos;
pCurr->bUseful = pCurr->ulCharPosStart != pCurr->ulCharPosNext;
}
aucBuffer = xfree(aucBuffer);
}
static void
vGet0NotesInfo(FILE *pFile, const UCHAR *aucHeader)
{
TRACE_MSG("vGet0NotesInfo");
vGet0FootnotesInfoAndText(pFile, aucHeader);
}
static void
vGet2FootnotesInfo(FILE *pFile, const UCHAR *aucHeader)
{
UCHAR *aucBuffer;
ULONG ulFileOffset, ulBeginOfText, ulOffset, ulBeginFootnoteInfo;
size_t tFootnoteInfoLen;
size_t tIndex;
TRACE_MSG("vGet2FootnotesInfo");
fail(pFile == NULL || aucHeader == NULL);
ulBeginOfText = ulGetLong(0x18, aucHeader);
NO_DBG_HEX(ulBeginOfText);
ulBeginFootnoteInfo = ulGetLong(0x64, aucHeader);
NO_DBG_HEX(ulBeginFootnoteInfo);
tFootnoteInfoLen = (size_t)usGetWord(0x68, aucHeader);
NO_DBG_DEC(tFootnoteInfoLen);
if (tFootnoteInfoLen < 10) {
DBG_MSG("No Footnotes in this document");
return;
}
aucBuffer = xmalloc(tFootnoteInfoLen);
if (!bReadBytes(aucBuffer,
tFootnoteInfoLen, ulBeginFootnoteInfo, pFile)) {
aucBuffer = xfree(aucBuffer);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tFootnoteInfoLen);
fail(tFootnoteListLength != 0);
tFootnoteListLength = (tFootnoteInfoLen - 4) / 6;
fail(tFootnoteListLength == 0);
fail(aulFootnoteList != NULL);
aulFootnoteList = xcalloc(tFootnoteListLength, sizeof(ULONG));
for (tIndex = 0; tIndex < tFootnoteListLength; tIndex++) {
ulOffset = ulGetLong(tIndex * 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulFileOffset = ulCharPos2FileOffset(ulBeginOfText + ulOffset);
NO_DBG_HEX(ulFileOffset);
aulFootnoteList[tIndex] = ulFileOffset;
}
aucBuffer = xfree(aucBuffer);
}
static void
vGet2FootnotesText(FILE *pFile, const UCHAR *aucHeader)
{
footnote_local_type *pCurr;
UCHAR *aucBuffer;
ULONG ulCharPos, ulBeginOfFootnotes, ulOffset, ulBeginFootnoteText;
size_t tFootnoteTextLen;
size_t tIndex;
TRACE_MSG("vGet2FootnotesText");
fail(pFile == NULL || aucHeader == NULL);
ulBeginOfFootnotes = ulGetLong(0x18, aucHeader);
ulBeginOfFootnotes += ulGetLong(0x34, aucHeader);
NO_DBG_HEX(ulBeginOfFootnotes);
ulBeginFootnoteText = ulGetLong(0x6a, aucHeader);
NO_DBG_HEX(ulBeginFootnoteText);
tFootnoteTextLen =
(size_t)usGetWord(0x6e, aucHeader);
NO_DBG_DEC(tFootnoteTextLen);
if (tFootnoteTextLen < 12) {
DBG_MSG("No Footnote text in this document");
return;
}
aucBuffer = xmalloc(tFootnoteTextLen);
if (!bReadBytes(aucBuffer,
tFootnoteTextLen, ulBeginFootnoteText, pFile)) {
aucBuffer = xfree(aucBuffer);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tFootnoteTextLen);
fail(tFootnoteTextLength != 0);
tFootnoteTextLength = tFootnoteTextLen / 4 - 2;
fail(tFootnoteTextLength == 0);
fail(pFootnoteText != NULL);
pFootnoteText = xcalloc(tFootnoteTextLength,
sizeof(footnote_local_type));
for (tIndex = 0; tIndex < tFootnoteTextLength; tIndex++) {
pCurr = pFootnoteText + tIndex;
pCurr->tInfo.szText = NULL;
ulOffset = ulGetLong(tIndex * 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulCharPos = ulBeginOfFootnotes + ulOffset;
NO_DBG_HEX(ulCharPos);
NO_DBG_HEX(ulCharPos2FileOffset(ulCharPos));
pCurr->ulCharPosStart = ulCharPos;
ulOffset = ulGetLong(tIndex * 4 + 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulCharPos = ulBeginOfFootnotes + ulOffset;
NO_DBG_HEX(ulCharPos);
NO_DBG_HEX(ulCharPos2FileOffset(ulCharPos));
pCurr->ulCharPosNext = ulCharPos;
pCurr->bUseful = pCurr->ulCharPosStart != pCurr->ulCharPosNext;
}
aucBuffer = xfree(aucBuffer);
}
static void
vGet2NotesInfo(FILE *pFile, const UCHAR *aucHeader)
{
TRACE_MSG("vGet2NotesInfo");
vGet2FootnotesInfo(pFile, aucHeader);
vGet2FootnotesText(pFile, aucHeader);
}
static void
vGet6FootnotesInfo(FILE *pFile, ULONG ulStartBlock,
const ULONG *aulBBD, size_t tBBDLen,
const UCHAR *aucHeader)
{
UCHAR *aucBuffer;
ULONG ulFileOffset, ulBeginOfText, ulOffset, ulBeginFootnoteInfo;
size_t tFootnoteInfoLen;
size_t tIndex;
TRACE_MSG("vGet6FootnotesInfo");
fail(pFile == NULL || aucHeader == NULL);
fail(ulStartBlock > MAX_BLOCKNUMBER && ulStartBlock != END_OF_CHAIN);
fail(aulBBD == NULL);
ulBeginOfText = ulGetLong(0x18, aucHeader);
NO_DBG_HEX(ulBeginOfText);
ulBeginFootnoteInfo = ulGetLong(0x68, aucHeader);
NO_DBG_HEX(ulBeginFootnoteInfo);
tFootnoteInfoLen =
(size_t)ulGetLong(0x6c, aucHeader);
NO_DBG_DEC(tFootnoteInfoLen);
if (tFootnoteInfoLen < 10) {
DBG_MSG("No Footnotes in this document");
return;
}
aucBuffer = xmalloc(tFootnoteInfoLen);
if (!bReadBuffer(pFile, ulStartBlock,
aulBBD, tBBDLen, BIG_BLOCK_SIZE,
aucBuffer, ulBeginFootnoteInfo, tFootnoteInfoLen)) {
aucBuffer = xfree(aucBuffer);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tFootnoteInfoLen);
fail(tFootnoteListLength != 0);
tFootnoteListLength = (tFootnoteInfoLen - 4) / 6;
fail(tFootnoteListLength == 0);
fail(aulFootnoteList != NULL);
aulFootnoteList = xcalloc(tFootnoteListLength, sizeof(ULONG));
for (tIndex = 0; tIndex < tFootnoteListLength; tIndex++) {
ulOffset = ulGetLong(tIndex * 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulFileOffset = ulCharPos2FileOffset(ulBeginOfText + ulOffset);
NO_DBG_HEX(ulFileOffset);
aulFootnoteList[tIndex] = ulFileOffset;
}
aucBuffer = xfree(aucBuffer);
}
static void
vGet6FootnotesText(FILE *pFile, ULONG ulStartBlock,
const ULONG *aulBBD, size_t tBBDLen,
const UCHAR *aucHeader)
{
footnote_local_type *pCurr;
UCHAR *aucBuffer;
ULONG ulCharPos, ulBeginOfFootnotes, ulOffset, ulBeginFootnoteText;
size_t tFootnoteTextLen;
size_t tIndex;
TRACE_MSG("vGet6FootnotesText");
fail(pFile == NULL || aucHeader == NULL);
fail(ulStartBlock > MAX_BLOCKNUMBER && ulStartBlock != END_OF_CHAIN);
fail(aulBBD == NULL);
ulBeginOfFootnotes = ulGetLong(0x18, aucHeader);
ulBeginOfFootnotes += ulGetLong(0x34, aucHeader);
NO_DBG_HEX(ulBeginOfFootnotes);
ulBeginFootnoteText = ulGetLong(0x70, aucHeader);
NO_DBG_HEX(ulBeginFootnoteText);
tFootnoteTextLen =
(size_t)ulGetLong(0x74, aucHeader);
NO_DBG_DEC(tFootnoteTextLen);
if (tFootnoteTextLen < 12) {
DBG_MSG("No Footnote text in this document");
return;
}
aucBuffer = xmalloc(tFootnoteTextLen);
if (!bReadBuffer(pFile, ulStartBlock,
aulBBD, tBBDLen, BIG_BLOCK_SIZE,
aucBuffer, ulBeginFootnoteText, tFootnoteTextLen)) {
aucBuffer = xfree(aucBuffer);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tFootnoteTextLen);
fail(tFootnoteTextLength != 0);
tFootnoteTextLength = tFootnoteTextLen / 4 - 2;
fail(tFootnoteTextLength == 0);
fail(pFootnoteText != NULL);
pFootnoteText = xcalloc(tFootnoteTextLength,
sizeof(footnote_local_type));
for (tIndex = 0; tIndex < tFootnoteTextLength; tIndex++) {
pCurr = pFootnoteText + tIndex;
pCurr->tInfo.szText = NULL;
ulOffset = ulGetLong(tIndex * 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulCharPos = ulBeginOfFootnotes + ulOffset;
NO_DBG_HEX(ulCharPos);
NO_DBG_HEX(ulCharPos2FileOffset(ulCharPos));
pCurr->ulCharPosStart = ulCharPos;
ulOffset = ulGetLong(tIndex * 4 + 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulCharPos = ulBeginOfFootnotes + ulOffset;
NO_DBG_HEX(ulCharPos);
NO_DBG_HEX(ulCharPos2FileOffset(ulCharPos));
pCurr->ulCharPosNext = ulCharPos;
pCurr->bUseful = pCurr->ulCharPosStart != pCurr->ulCharPosNext;
}
aucBuffer = xfree(aucBuffer);
}
static void
vGet6EndnotesInfo(FILE *pFile, ULONG ulStartBlock,
const ULONG *aulBBD, size_t tBBDLen,
const UCHAR *aucHeader)
{
UCHAR *aucBuffer;
ULONG ulFileOffset, ulBeginOfText, ulOffset, ulBeginEndnoteInfo;
size_t tEndnoteInfoLen;
size_t tIndex;
TRACE_MSG("vGet6EndnotesInfo");
fail(pFile == NULL || aucHeader == NULL);
fail(ulStartBlock > MAX_BLOCKNUMBER && ulStartBlock != END_OF_CHAIN);
fail(aulBBD == NULL);
ulBeginOfText = ulGetLong(0x18, aucHeader);
NO_DBG_HEX(ulBeginOfText);
ulBeginEndnoteInfo = ulGetLong(0x1d2, aucHeader);
NO_DBG_HEX(ulBeginEndnoteInfo);
tEndnoteInfoLen =
(size_t)ulGetLong(0x1d6, aucHeader);
NO_DBG_DEC(tEndnoteInfoLen);
if (tEndnoteInfoLen < 10) {
DBG_MSG("No Endnotes in this document");
return;
}
aucBuffer = xmalloc(tEndnoteInfoLen);
if (!bReadBuffer(pFile, ulStartBlock,
aulBBD, tBBDLen, BIG_BLOCK_SIZE,
aucBuffer, ulBeginEndnoteInfo, tEndnoteInfoLen)) {
aucBuffer = xfree(aucBuffer);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tEndnoteInfoLen);
fail(tEndnoteListLength != 0);
tEndnoteListLength = (tEndnoteInfoLen - 4) / 6;
fail(tEndnoteListLength == 0);
fail(aulEndnoteList != NULL);
aulEndnoteList = xcalloc(tEndnoteListLength, sizeof(ULONG));
for (tIndex = 0; tIndex < tEndnoteListLength; tIndex++) {
ulOffset = ulGetLong(tIndex * 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulFileOffset = ulCharPos2FileOffset(ulBeginOfText + ulOffset);
NO_DBG_HEX(ulFileOffset);
aulEndnoteList[tIndex] = ulFileOffset;
}
aucBuffer = xfree(aucBuffer);
}
static void
vGet6NotesInfo(FILE *pFile, ULONG ulStartBlock,
const ULONG *aulBBD, size_t tBBDLen,
const UCHAR *aucHeader)
{
TRACE_MSG("vGet6NotesInfo");
vGet6FootnotesInfo(pFile, ulStartBlock,
aulBBD, tBBDLen, aucHeader);
vGet6FootnotesText(pFile, ulStartBlock,
aulBBD, tBBDLen, aucHeader);
vGet6EndnotesInfo(pFile, ulStartBlock,
aulBBD, tBBDLen, aucHeader);
}
static void
vGet8FootnotesInfo(FILE *pFile, const pps_info_type *pPPS,
const ULONG *aulBBD, size_t tBBDLen,
const ULONG *aulSBD, size_t tSBDLen,
const UCHAR *aucHeader)
{
const ULONG *aulBlockDepot;
UCHAR *aucBuffer;
ULONG ulFileOffset, ulBeginOfText, ulOffset, ulBeginFootnoteInfo;
size_t tFootnoteInfoLen, tBlockDepotLen, tBlockSize;
size_t tIndex;
TRACE_MSG("vGet8FootnotesInfo");
ulBeginOfText = ulGetLong(0x18, aucHeader);
NO_DBG_HEX(ulBeginOfText);
ulBeginFootnoteInfo = ulGetLong(0xaa, aucHeader);
NO_DBG_HEX(ulBeginFootnoteInfo);
tFootnoteInfoLen =
(size_t)ulGetLong(0xae, aucHeader);
NO_DBG_DEC(tFootnoteInfoLen);
if (tFootnoteInfoLen < 10) {
DBG_MSG("No Footnotes in this document");
return;
}
NO_DBG_DEC(pPPS->tTable.ulSB);
NO_DBG_HEX(pPPS->tTable.ulSize);
if (pPPS->tTable.ulSize == 0) {
DBG_MSG("No footnotes information");
return;
}
if (pPPS->tTable.ulSize < MIN_SIZE_FOR_BBD_USE) {
aulBlockDepot = aulSBD;
tBlockDepotLen = tSBDLen;
tBlockSize = SMALL_BLOCK_SIZE;
} else {
aulBlockDepot = aulBBD;
tBlockDepotLen = tBBDLen;
tBlockSize = BIG_BLOCK_SIZE;
}
aucBuffer = xmalloc(tFootnoteInfoLen);
if (!bReadBuffer(pFile, pPPS->tTable.ulSB,
aulBlockDepot, tBlockDepotLen, tBlockSize,
aucBuffer, ulBeginFootnoteInfo, tFootnoteInfoLen)) {
aucBuffer = xfree(aucBuffer);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tFootnoteInfoLen);
fail(tFootnoteListLength != 0);
tFootnoteListLength = (tFootnoteInfoLen - 4) / 6;
fail(tFootnoteListLength == 0);
fail(aulFootnoteList != NULL);
aulFootnoteList = xcalloc(tFootnoteListLength, sizeof(ULONG));
for (tIndex = 0; tIndex < tFootnoteListLength; tIndex++) {
ulOffset = ulGetLong(tIndex * 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulFileOffset = ulCharPos2FileOffset(ulBeginOfText + ulOffset);
NO_DBG_HEX(ulFileOffset);
aulFootnoteList[tIndex] = ulFileOffset;
}
aucBuffer = xfree(aucBuffer);
}
static void
vGet8FootnotesText(FILE *pFile, const pps_info_type *pPPS,
const ULONG *aulBBD, size_t tBBDLen,
const ULONG *aulSBD, size_t tSBDLen,
const UCHAR *aucHeader)
{
footnote_local_type *pCurr;
const ULONG *aulBlockDepot;
UCHAR *aucBuffer;
ULONG ulCharPos, ulBeginOfFootnotes, ulOffset, ulBeginFootnoteText;
size_t tFootnoteTextLen, tBlockDepotLen, tBlockSize;
size_t tIndex;
TRACE_MSG("vGet8FootnotesText");
ulBeginOfFootnotes = ulGetLong(0x18, aucHeader);
ulBeginOfFootnotes += ulGetLong(0x4c, aucHeader);
NO_DBG_HEX(ulBeginOfFootnotes);
ulBeginFootnoteText = ulGetLong(0xb2, aucHeader);
NO_DBG_HEX(ulBeginFootnoteText);
tFootnoteTextLen =
(size_t)ulGetLong(0xb6, aucHeader);
NO_DBG_DEC(tFootnoteTextLen);
if (tFootnoteTextLen < 12) {
DBG_MSG("No Footnote text in this document");
return;
}
NO_DBG_DEC(pPPS->tTable.ulSB);
NO_DBG_HEX(pPPS->tTable.ulSize);
if (pPPS->tTable.ulSize == 0) {
DBG_MSG("No footnote text information");
return;
}
if (pPPS->tTable.ulSize < MIN_SIZE_FOR_BBD_USE) {
aulBlockDepot = aulSBD;
tBlockDepotLen = tSBDLen;
tBlockSize = SMALL_BLOCK_SIZE;
} else {
aulBlockDepot = aulBBD;
tBlockDepotLen = tBBDLen;
tBlockSize = BIG_BLOCK_SIZE;
}
aucBuffer = xmalloc(tFootnoteTextLen);
if (!bReadBuffer(pFile, pPPS->tTable.ulSB,
aulBlockDepot, tBlockDepotLen, tBlockSize,
aucBuffer, ulBeginFootnoteText, tFootnoteTextLen)) {
aucBuffer = xfree(aucBuffer);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tFootnoteTextLen);
fail(tFootnoteTextLength != 0);
tFootnoteTextLength = tFootnoteTextLen / 4 - 2;
fail(tFootnoteTextLength == 0);
fail(pFootnoteText != NULL);
pFootnoteText = xcalloc(tFootnoteTextLength,
sizeof(footnote_local_type));
for (tIndex = 0; tIndex < tFootnoteTextLength; tIndex++) {
pCurr = pFootnoteText + tIndex;
pCurr->tInfo.szText = NULL;
ulOffset = ulGetLong(tIndex * 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulCharPos = ulBeginOfFootnotes + ulOffset;
NO_DBG_HEX(ulCharPos);
NO_DBG_HEX(ulCharPos2FileOffset(ulCharPos));
pCurr->ulCharPosStart = ulCharPos;
ulOffset = ulGetLong(tIndex * 4 + 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulCharPos = ulBeginOfFootnotes + ulOffset;
NO_DBG_HEX(ulCharPos);
NO_DBG_HEX(ulCharPos2FileOffset(ulCharPos));
pCurr->ulCharPosNext = ulCharPos;
pCurr->bUseful = pCurr->ulCharPosStart != pCurr->ulCharPosNext;
}
aucBuffer = xfree(aucBuffer);
}
static void
vGet8EndnotesInfo(FILE *pFile, const pps_info_type *pPPS,
const ULONG *aulBBD, size_t tBBDLen,
const ULONG *aulSBD, size_t tSBDLen,
const UCHAR *aucHeader)
{
const ULONG *aulBlockDepot;
UCHAR *aucBuffer;
ULONG ulFileOffset, ulBeginOfText, ulOffset, ulBeginEndnoteInfo;
size_t tEndnoteInfoLen, tBlockDepotLen, tBlockSize;
size_t tIndex;
TRACE_MSG("vGet8EndnotesInfo");
ulBeginOfText = ulGetLong(0x18, aucHeader);
NO_DBG_HEX(ulBeginOfText);
ulBeginEndnoteInfo = ulGetLong(0x20a, aucHeader);
NO_DBG_HEX(ulBeginEndnoteInfo);
tEndnoteInfoLen = (size_t)ulGetLong(0x20e, aucHeader);
NO_DBG_DEC(tEndnoteInfoLen);
if (tEndnoteInfoLen < 10) {
DBG_MSG("No endnotes in this document");
return;
}
NO_DBG_DEC(pPPS->tTable.ulSB);
NO_DBG_HEX(pPPS->tTable.ulSize);
if (pPPS->tTable.ulSize == 0) {
DBG_MSG("No endnotes information");
return;
}
if (pPPS->tTable.ulSize < MIN_SIZE_FOR_BBD_USE) {
aulBlockDepot = aulSBD;
tBlockDepotLen = tSBDLen;
tBlockSize = SMALL_BLOCK_SIZE;
} else {
aulBlockDepot = aulBBD;
tBlockDepotLen = tBBDLen;
tBlockSize = BIG_BLOCK_SIZE;
}
aucBuffer = xmalloc(tEndnoteInfoLen);
if (!bReadBuffer(pFile, pPPS->tTable.ulSB,
aulBlockDepot, tBlockDepotLen, tBlockSize,
aucBuffer, ulBeginEndnoteInfo, tEndnoteInfoLen)) {
aucBuffer = xfree(aucBuffer);
return;
}
NO_DBG_PRINT_BLOCK(aucBuffer, tEndnoteInfoLen);
fail(tEndnoteListLength != 0);
tEndnoteListLength = (tEndnoteInfoLen - 4) / 6;
fail(tEndnoteListLength == 0);
fail(aulEndnoteList != NULL);
aulEndnoteList = xcalloc(tEndnoteListLength, sizeof(ULONG));
for (tIndex = 0; tIndex < tEndnoteListLength; tIndex++) {
ulOffset = ulGetLong(tIndex * 4, aucBuffer);
NO_DBG_HEX(ulOffset);
ulFileOffset = ulCharPos2FileOffset(ulBeginOfText + ulOffset);
NO_DBG_HEX(ulFileOffset);
aulEndnoteList[tIndex] = ulFileOffset;
}
aucBuffer = xfree(aucBuffer);
}
static void
vGet8NotesInfo(FILE *pFile, const pps_info_type *pPPS,
const ULONG *aulBBD, size_t tBBDLen,
const ULONG *aulSBD, size_t tSBDLen,
const UCHAR *aucHeader)
{
TRACE_MSG("vGet8NotesInfo");
vGet8FootnotesInfo(pFile, pPPS,
aulBBD, tBBDLen, aulSBD, tSBDLen, aucHeader);
vGet8FootnotesText(pFile, pPPS,
aulBBD, tBBDLen, aulSBD, tSBDLen, aucHeader);
vGet8EndnotesInfo(pFile, pPPS,
aulBBD, tBBDLen, aulSBD, tSBDLen, aucHeader);
}
void
vGetNotesInfo(FILE *pFile, const pps_info_type *pPPS,
const ULONG *aulBBD, size_t tBBDLen,
const ULONG *aulSBD, size_t tSBDLen,
const UCHAR *aucHeader, int iWordVersion)
{
TRACE_MSG("vGetNotesInfo");
fail(pFile == NULL);
fail(pPPS == NULL && iWordVersion >= 6);
fail(aulBBD == NULL && tBBDLen != 0);
fail(aulSBD == NULL && tSBDLen != 0);
fail(aucHeader == NULL);
switch (iWordVersion) {
case 0:
vGet0NotesInfo(pFile, aucHeader);
break;
case 1:
case 2:
vGet2NotesInfo(pFile, aucHeader);
break;
case 4:
case 5:
break;
case 6:
case 7:
vGet6NotesInfo(pFile, pPPS->tWordDocument.ulSB,
aulBBD, tBBDLen, aucHeader);
break;
case 8:
vGet8NotesInfo(pFile, pPPS,
aulBBD, tBBDLen, aulSBD, tSBDLen, aucHeader);
break;
default:
werr(0, "Sorry, no notes information");
break;
}
}
void
vPrepareFootnoteText(FILE *pFile)
{
footnote_local_type *pCurr;
size_t tFootnote;
fail(pFile == NULL);
fail(pFootnoteText == NULL && tFootnoteTextLength != 0);
if (pFootnoteText == NULL || tFootnoteTextLength == 0) {
return;
}
for (tFootnote = 0; tFootnote < tFootnoteTextLength; tFootnote++) {
pCurr = pFootnoteText + tFootnote;
pCurr->bUseful = pCurr->ulCharPosStart != pCurr->ulCharPosNext;
if (pCurr->bUseful) {
pCurr->tInfo.szText = szFootnoteDecryptor(pFile,
pCurr->ulCharPosStart,
pCurr->ulCharPosNext);
} else {
pCurr->tInfo.szText = NULL;
}
}
}
const char *
szGetFootnootText(UINT uiFootnoteIndex)
{
if ((size_t)uiFootnoteIndex >= tFootnoteTextLength) {
return NULL;
}
return pFootnoteText[uiFootnoteIndex].tInfo.szText;
}
notetype_enum
eGetNotetype(ULONG ulFileOffset)
{
size_t tIndex;
TRACE_MSG("eGetNotetype");
fail(aulFootnoteList == NULL && tFootnoteListLength != 0);
fail(aulEndnoteList == NULL && tEndnoteListLength != 0);
if (tFootnoteListLength == 0 && tEndnoteListLength == 0) {
return notetype_is_unknown;
}
if (tEndnoteListLength == 0) {
return notetype_is_footnote;
}
if (tFootnoteListLength == 0) {
return notetype_is_endnote;
}
for (tIndex = 0; tIndex < tFootnoteListLength; tIndex++) {
if (aulFootnoteList[tIndex] == ulFileOffset) {
return notetype_is_footnote;
}
}
for (tIndex = 0; tIndex < tEndnoteListLength; tIndex++) {
if (aulEndnoteList[tIndex] == ulFileOffset) {
return notetype_is_endnote;
}
}
return notetype_is_unknown;
}