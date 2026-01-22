#include "gdevprn.h"
#include "gdevpcl.h"
typedef struct {
short width;
short height;
}                 PaperFormat;
typedef unsigned char Byte;
typedef struct {
Byte * data;
short maxSize;
short current;
} ByteList;
typedef struct {
short  previousSize;
Byte   previousData[1500];
short  nbBlankLines;
short  nbLinesSent;
short  pageWidth;
short  pageHeight;
short  horizontalOffset;
short  resolution;
} Summary;
private const int DumpFinished = 0;
private const int DumpContinue = 1;
private const int HL7X0_LENGTH = 5;
private void  makeCommandsForSequence(Byte     * pSource,
short      length,
ByteList * pCommandList,
short      offset,
Byte     * pCommandCount,
short      rest);
private int dumpPage(gx_device_printer * pSource,
Byte              * pLineTmp,
ByteList          * pCommandList,
Summary           * pSummary
);
private void initSummary(Summary * s,short pw, short ph, short resolution);
private void resetPreviousData(Summary * s);
private void makeFullLine( Byte      * pCurrentLine,
Byte      * pPreviousLine,
short       lineWidth,
ByteList  * commandsList,
short       horizontalOffset
);
private void initByteList(ByteList *list, Byte *array, short maxSize,short initCurrent);
private void addByte(ByteList *list,Byte value );
private void addArray(ByteList *list, Byte *source, short nb);
private void addNBytes(ByteList * list, Byte value, short nb);
private Byte * currentPosition(ByteList * list);
private void addCodedNumber(ByteList * list, short number);
private int isThereEnoughRoom(ByteList * list, short biggest);
private short roomLeft(ByteList * list);
private void dumpToPrinter(ByteList * list,FILE * printStream);
private int hl7x0_print_page(gx_device_printer *, FILE *, int, int, ByteList *);
#ifdef X_DPI
#  define X_DPI2 X_DPI
#else
#  define X_DPI 300
#  define X_DPI2 600
#endif
#ifdef Y_DPI
#  define Y_DPI2 Y_DPI
#else
#  define Y_DPI 300
#  define Y_DPI2 600
#endif
#define LETTER_WIDTH 5100
#define LEFT_MARGIN  30
private const PaperFormat tableOfFormats[] = {
{ 2550, 3300 },
{ 2550, 4200 },
{ 2175, 3150 },
{ 2480, 3507 },
{ 2078, 2953 },
{ 1754, 2480 },
{ 1162, 2250 },
{ 1237, 2850 },
{ 1299, 2598 },
{ 1913, 2704 },
{ 2480, 4783 },
{ 3300, 2550 },
{ 4200, 2550 },
{ 3150, 2175 },
{ 3507, 2480 },
{ 2952, 2078 },
{ 2480, 1754 },
{ 2250, 1162 },
{ 2850, 1237 },
{ 2598, 1299 },
{ 2704, 1913 },
{ 4783, 2480 }
};
private short MaxLineLength(short resolution){
return (((156 * resolution / 150 ) * 5 )/4) + 8;
}
#define HL7X0_MARGINS_A4	0.1, 0.15, 0.07, 0.05
#define HL7X0_MARGINS_LETTER 0.275, 0.20, 0.25, 0.07
#define W sizeof(word)
#define HL720    0
#define HL730    0
private dev_proc_open_device(hl7x0_open);
private dev_proc_close_device(hl7x0_close);
private dev_proc_print_page(hl720_print_page);
private dev_proc_print_page(hl730_print_page);
private const gx_device_procs prn_hl_procs =
prn_params_procs(hl7x0_open, gdev_prn_output_page, hl7x0_close,
gdev_prn_get_params, gdev_prn_put_params);
const gx_device_printer far_data gs_hl7x0_device =
prn_device(prn_hl_procs, "hl7x0",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
1, hl720_print_page);
private int
hl7x0_open(gx_device *pdev)
{
static const float m_a4[4] = { HL7X0_MARGINS_A4 };
static const float m_letter[4] = { HL7X0_MARGINS_LETTER };
const float *m =
(gdev_pcl_paper_size(pdev) == PAPER_SIZE_A4 ? m_a4 : m_letter);
gx_device_set_margins(pdev, m, true);
return gdev_prn_open(pdev);
}
private int
hl7x0_close(gx_device *pdev)
{
gx_device_printer *const ppdev = (gx_device_printer *)pdev;
int code = gdev_prn_open_printer(pdev, 1);
if (code < 0)
return code;
fputs("@N@N@N@N@X", ppdev->file) ;
return gdev_prn_close_printer(pdev);
}
private int
hl720_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
Byte prefix[] ={
0x1B,'%','-','1','2','3','4','5','X'
,'@','P','J','L',0x0A
,'@','P','J','L',' ','E','N','T','E','R',' '
,'L','A','N','G','U','A','G','E'
,' ','=',' ','H','B','P',0x0A
,'@','L', 0x0
};
ByteList initCommand;
int x_dpi = pdev->x_pixels_per_inch;
initByteList(&initCommand,
prefix,
sizeof(prefix),
sizeof(prefix) - 1);
addByte(&initCommand, (Byte) ((((600/x_dpi) >> 1) \
| (((600/x_dpi) >> 1) << 2))));
return hl7x0_print_page(pdev, prn_stream, HL720, 300,
&initCommand);
}
private int
hl730_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	return hl720_print_page(pdev, prn_stream);
}
private int
hl7x0_print_page(gx_device_printer *pdev, FILE *printStream, int ptype,
int dots_per_inch, ByteList *initCommand)
{
Byte FormFeed[] = {'@','G',0x00,0x00,0x01,0xFF,'@','F'};
ByteList formFeedCommand;
int line_size       = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
int x_dpi = pdev->x_pixels_per_inch;
int num_rows = dev_print_scan_lines(pdev);
int result;
int sizeOfBuffer   = MaxLineLength(x_dpi) + 30;
Byte * storage      = (Byte *) gs_malloc(pdev->memory,
sizeOfBuffer + line_size,
1,
"hl7x0_print_page");
Summary pageSummary;
ByteList commandsBuffer;
initSummary(&pageSummary,
line_size,
num_rows,
x_dpi);
if ( storage == 0 )
return_error(gs_error_VMerror);
initByteList(&commandsBuffer, storage, sizeOfBuffer,0 );
if ( pdev->PageCount == 0 )
{
dumpToPrinter(initCommand, printStream);
}
do {
result = dumpPage(pdev,
storage + sizeOfBuffer,
&commandsBuffer,
&pageSummary);
dumpToPrinter(&commandsBuffer,printStream);
} while (result == DumpContinue);
initByteList(&formFeedCommand,
FormFeed,
sizeof(FormFeed),
sizeof(FormFeed));
dumpToPrinter(&formFeedCommand, printStream);
gs_free(pdev->memory, (char *)storage, storage_size_words, 1, "hl7X0_print_page");
return 0;
}
private short stripTrailingBlanks(Byte * line, short length){
short positionOfFirstZero = length - 1;
while (positionOfFirstZero > 0) {
if (line[positionOfFirstZero] != 0) {
return positionOfFirstZero + 1;
}
positionOfFirstZero -- ;
}
return 0;
}
private short horizontalOffset(short pixWidth,
short pixOffset,
short resolution){
return (((LETTER_WIDTH * resolution/600 - pixWidth) + pixOffset * 2) + 7) / 8;
}
private void initSummary(Summary * s,short pw, short ph, short resolution){
s->previousSize = -1 ;
s->nbBlankLines = 1;
s->nbLinesSent = 0;
s->pageWidth = pw;
s->pageHeight = ph;
s->horizontalOffset = horizontalOffset( pw * 8,LEFT_MARGIN, resolution) ;
s->resolution = resolution;
}
private void resetPreviousData(Summary * s){
memset(s->previousData,0,s->pageWidth);
}
private int dumpPage(gx_device_printer * pSource,
Byte              * pLineTmp,
ByteList          * pCommandList,
Summary           * pSummary
){
Byte * pSaveCommandStart;
short  lineNB;
short usefulLength;
short tmpLength;
pSaveCommandStart = currentPosition(pCommandList);
addNBytes(pCommandList,0,HL7X0_LENGTH);
for (lineNB = pSummary->nbLinesSent  ;
lineNB < pSummary->pageHeight ; lineNB ++ ) {
gdev_prn_copy_scan_lines(pSource,
lineNB,
pLineTmp,
pSummary->pageWidth);
usefulLength =  stripTrailingBlanks(pLineTmp,pSummary->pageWidth);
if (usefulLength != 0) {
if (pSummary->nbBlankLines != 0) {
if ( isThereEnoughRoom( pCommandList, pSummary->nbBlankLines )   ) {
addNBytes(pCommandList,0xff,pSummary->nbBlankLines);
pSummary->nbBlankLines = 0;
}
else {
short availableRoom = roomLeft(pCommandList);
addNBytes(pCommandList,0xff,availableRoom);
pSummary->nbBlankLines -= availableRoom;
break ;
}
resetPreviousData(pSummary);
pSummary->previousSize = 0;
}
if (!isThereEnoughRoom(pCommandList,MaxLineLength(pSummary->resolution))){
break;
}
if (pSummary->previousSize > usefulLength){
tmpLength = pSummary->previousSize;
}
else {
tmpLength = usefulLength;
}
if (pSummary->previousSize == -1 ) {
Byte *save = currentPosition(pCommandList);
addByte(pCommandList,0);
makeCommandsForSequence(pLineTmp,
tmpLength,
pCommandList,
pSummary->horizontalOffset,
save,
0);
}
else {
makeFullLine(pLineTmp,
pSummary->previousData,
tmpLength,
pCommandList,
pSummary->horizontalOffset);
}
pSummary->previousSize = tmpLength;
memcpy(pSummary->previousData,pLineTmp,tmpLength);
}
else {
pSummary->nbBlankLines++;
}
pSummary->nbLinesSent ++;
}
if (pCommandList->current > HL7X0_LENGTH){
short size = pCommandList->current - HL7X0_LENGTH;
*(pSaveCommandStart++)  = '@';
*(pSaveCommandStart++)  = 'G';
*(pSaveCommandStart++)  = (Byte) (size >> 16);
*(pSaveCommandStart++)  = (Byte) (size >> 8);
*(pSaveCommandStart++)  = (Byte) (size);
}
else {
pCommandList->current = 0;
}
if (lineNB == pSummary->pageHeight){
return DumpFinished;
}
else {
return DumpContinue;
}
}
private void makeFullLine( Byte      * pCurrentLine,
Byte      * pPreviousLine,
short       lineWidth,
ByteList  * commandsList,
short       horizontalOffset
){
Byte *pPreviousTmp;
Byte *pCurrentTmp;
Byte *pNumberOfCommands;
int loopCounter;
short remainingWidth;
Byte *pStartOfSequence;
if (lineWidth <= 0) {
addByte(commandsList,0xff);
return;
}
pNumberOfCommands = currentPosition(commandsList);
addByte(commandsList,0);
pPreviousTmp = pPreviousLine;
pCurrentTmp = pCurrentLine;
for (loopCounter = lineWidth ;  0 < loopCounter ; loopCounter -- )
*pPreviousTmp++ ^= *pCurrentTmp++;
pStartOfSequence = pPreviousLine;
remainingWidth = lineWidth;
while (true) {
#ifdef USE_POSSIBLY_FLAWED_COMPRESSION
while (true) {
if (remainingWidth == 0)
{
return;
}
if (*pStartOfSequence != 0)
break;
pStartOfSequence ++;
horizontalOffset ++;
--remainingWidth;
}
#endif
pPreviousTmp = pStartOfSequence + 1;
--remainingWidth;
#ifdef USE_POSSIBLY_FLAWED_COMPRESSION
while (remainingWidth != 0 && *pPreviousTmp != 0) {
++pPreviousTmp;
--remainingWidth;
}
#else
pPreviousTmp += remainingWidth;
remainingWidth = 0;
#endif
makeCommandsForSequence(pCurrentLine + (pStartOfSequence - pPreviousLine),
pPreviousTmp - pStartOfSequence,
commandsList,
horizontalOffset,
pNumberOfCommands,
remainingWidth);
if (*pNumberOfCommands == 0xfe
||
remainingWidth == 0 )
{
return;
}
pStartOfSequence = pPreviousTmp + 1;
horizontalOffset = 1;
--remainingWidth;
}
}
private void makeSequenceWithoutRepeat(
Byte     * pSequence,
short      lengthOfSequence,
ByteList * pCommandList,
short      offset             );
private void makeSequenceWithRepeat(
Byte     * pSequence,
short      lengthOfSequence,
ByteList * pCommandList,
short      offset             );
private void makeCommandsForSequence(Byte     * pSource,
short      length,
ByteList * pCommandList,
short      offset,
Byte     * pNumberOfCommands,
short      rest)         {
Byte * pStartOfSequence;
Byte * pEndOfSequence;
short  remainingLength = length - 1;
pStartOfSequence = pSource;
pEndOfSequence = pStartOfSequence + 1;
while (true) {
if (*pNumberOfCommands == 0xfd) {
makeSequenceWithoutRepeat(pStartOfSequence,
1 + remainingLength + rest,
pCommandList,
offset);
++*pNumberOfCommands;
return;
}
while (true) {
if (remainingLength == 0) {
makeSequenceWithoutRepeat(pStartOfSequence,
pEndOfSequence - pStartOfSequence,
pCommandList,
offset);
++*pNumberOfCommands;
return;
}
if (*pEndOfSequence == *(pEndOfSequence - 1)) {
break;
}
++ pEndOfSequence;
--remainingLength;
}
if (pStartOfSequence != pEndOfSequence - 1) {
makeSequenceWithoutRepeat(pStartOfSequence,
(pEndOfSequence - 1) - pStartOfSequence,
pCommandList,
offset);
++*pNumberOfCommands;
offset = 0;
pStartOfSequence = pEndOfSequence - 1;
if (*pNumberOfCommands == 0xfd) {
makeSequenceWithoutRepeat(pStartOfSequence,
1 + remainingLength + rest,
pCommandList,
offset);
++*pNumberOfCommands;
return;
}
}
while (true) {
if (remainingLength == 0) {
makeSequenceWithRepeat(pStartOfSequence,
pEndOfSequence - pStartOfSequence,
pCommandList,
offset);
++*pNumberOfCommands;
return;
}
if (*pEndOfSequence != *pStartOfSequence){
break;
}
++pEndOfSequence;
--remainingLength;
}
makeSequenceWithRepeat(pStartOfSequence,
pEndOfSequence - pStartOfSequence,
pCommandList,
offset);
++*pNumberOfCommands;
offset = 0;
pStartOfSequence = pEndOfSequence ++ ;
--remainingLength;
}
}
private void makeSequenceWithoutRepeat(
Byte     * pSequence,
short      lengthOfSequence,
ByteList * pCommandList,
short      offset             ){
static const short MAX_OFFSET         = 15;
static const short POSITION_OF_OFFSET = 3;
static const short MAX_LENGTH         =  7;
Byte tmpFirstByte = 0;
Byte * pSaveFirstByte;
short reducedLength = lengthOfSequence - 1;
pSaveFirstByte = currentPosition(pCommandList);
addByte( pCommandList, 0 );
if (offset >= MAX_OFFSET) {
addCodedNumber(pCommandList,offset - MAX_OFFSET);
tmpFirstByte |= MAX_OFFSET << POSITION_OF_OFFSET;
}
else
tmpFirstByte |= offset << POSITION_OF_OFFSET;
if (reducedLength >= MAX_LENGTH) {
addCodedNumber(pCommandList,reducedLength - MAX_LENGTH);
tmpFirstByte |= MAX_LENGTH ;
}
else
tmpFirstByte |= reducedLength ;
addArray(pCommandList, pSequence, lengthOfSequence);
*pSaveFirstByte = tmpFirstByte;
return ;
}
private void makeSequenceWithRepeat(
Byte     * pSequence,
short      lengthOfSequence,
ByteList * pCommandList,
short      offset             ){
static const short MAX_OFFSET         = 3;
static const short POSITION_OF_OFFSET = 5;
static const short MAX_LENGTH         =  31;
Byte tmpFirstByte = 0x80;
Byte * pSaveFirstByte;
short reducedLength = lengthOfSequence - 2;
pSaveFirstByte = currentPosition(pCommandList);
addByte( pCommandList, 0 );
if (offset >= MAX_OFFSET) {
addCodedNumber(pCommandList, offset - MAX_OFFSET);
tmpFirstByte |= MAX_OFFSET << POSITION_OF_OFFSET;
}
else
tmpFirstByte |= offset << POSITION_OF_OFFSET;
if (reducedLength >= MAX_LENGTH) {
addCodedNumber(pCommandList,reducedLength - MAX_LENGTH);
tmpFirstByte |= MAX_LENGTH ;
}
else
tmpFirstByte |= reducedLength ;
addByte(pCommandList, *pSequence );
*pSaveFirstByte = tmpFirstByte;
return ;
}
private void initByteList(ByteList *list, Byte *array, short maxSize, short initCurrent) {
list->current = initCurrent;
list->maxSize = maxSize;
list->data = array;
}
private void addByte(ByteList *list,Byte value ) {
if (list->current < list->maxSize)
list->data[list->current++] = value;
else
errprintf("Could not add byte to command\n");
}
private void addArray(ByteList *list, Byte *source, short nb){
if (list->current <= list->maxSize - nb)
{
memcpy(list->data + list->current, source , (size_t) nb);
list->current += nb;
}
else
errprintf("Could not add byte array to command\n");
}
private void addNBytes(ByteList * list, Byte value, short nb){
int i;
if (list->current <= list->maxSize - nb)
{
for (i = list->current ; i < (list->current + nb) ; i++)
{
list->data[i] = value;
}
list->current += nb;
}
else
errprintf("Could not add %d bytes to command\n",nb);
}
private Byte * currentPosition(ByteList * list) {
return &(list->data[list->current]);
}
private void addCodedNumber(ByteList * list, short number){
short q = number / 0xff;
short r = number % 0xff;
addNBytes(list, 0xff, q);
addByte(list,r);
}
private int isThereEnoughRoom(ByteList * list, short biggest){
return ((list->maxSize-list->current) >= biggest);
}
private short roomLeft(ByteList * list){
return list->maxSize - list->current;
}
private void dumpToPrinter(ByteList * list,FILE * printStream){
short loopCounter;
for (loopCounter = 0; loopCounter < list->current; loopCounter++)
{
fputc(list->data[loopCounter],printStream);
}
list->current = 0;
}