#include "gdevprn.h"
#include <stdlib.h>
#ifdef USE_BYTE_STREAM
# define BYTE_STREAM 1
#else
# define BYTE_STREAM 0
#endif
#define QUOTE_CHAR (char) 0x02
#define EOF_CHAR (char) 0x04
#define EXTRA_QUOTE1 (char) 0x11
#define EXTRA_QUOTE2 (char) 0x13
#define EXTRA_QUOTE3 EOF_CHAR
#define EXTRA_QUOTE4 EOF_CHAR
#define IMPRESSHEADER "jobheader onerror, prerasterization off"
#define CANON_CX
#ifdef CANON_CX
# define MAX_DPI 300
#endif
#ifndef MAX_DPI
# define MAX_DPI 300
#endif
#define getMagnification ( \
( pdev->x_pixels_per_inch > (MAX_DPI >> 1) ) ? 0 : \
( pdev->x_pixels_per_inch > (MAX_DPI >> 2) ) ? 1 : \
2 )
#define WIDTH_10THS DEFAULT_WIDTH_10THS
#define HEIGHT_10THS DEFAULT_HEIGHT_10THS
#ifdef CANON_CX
# define MARG_L 0.15
# define MARG_R ( (float)WIDTH_10THS / 10.0 - 8.0 - MARG_L)
#endif
#ifndef MARG_L
# define MARG_L 0.2
#endif
#ifndef MARG_R
# define MARG_R 0.2
#endif
#define MARG_T 0.1
#define MARG_B 0.2
#define IM_DEBUG 0
#define DebugMsg(Level,P1,P2) if (Level<=IM_DEBUG) {errprintf(P1,P2 );}
#define HorzBytesPerSw 4
#define HorzBitsPerSw (HorzBytesPerSw * 8)
#define VertBytesPerSw 32
#define TotalBytesPerSw (HorzBytesPerSw * VertBytesPerSw)
#if arch_sizeof_long == 4
# define BIGTYPE unsigned long int
#else
# if arch_sizeof_short == 4
# define BIGTYPE unsigned short int
# else
# if arch_sizeof_short == 2
# define BIGTYPE unsigned short
# endif
# endif
#endif
#ifndef BIGTYPE
#define BIGTYPE byte
#endif
#define BIGSIZE ( sizeof( BIGTYPE ) )
#define iSP 128
#define iSP1 129
#define iMPLUS 131
#define iMMINUS 132
#define iMMOVE 133
#define iSMOVE 134
#define iABS_H 135
#define iREL_H 136
#define iABS_V 137
#define iREL_V 138
#define iCRLF 197
#define iSET_HV_SYSTEM 205
#define iSET_ADV_DIRS 206
#define iPAGE 213
#define iENDPAGE 219
#define iBITMAP 235
#define iSET_MAGNIFICATION 236
#define iNOOP 254
#define iEOF 255
private dev_proc_print_page(imagen_print_page);
private dev_proc_open_device(imagen_prn_open);
private dev_proc_close_device(imagen_prn_close);
gx_device_procs imagen_procs =
prn_procs(imagen_prn_open, gdev_prn_output_page, imagen_prn_close);
#define ppdev ((gx_device_printer *)pdev)
const gx_device_printer far_data gs_imagen_device =
prn_device( imagen_procs,
"imagen",
WIDTH_10THS,
HEIGHT_10THS,
MAX_DPI,
MAX_DPI,
MARG_L,MARG_R,MARG_T,MARG_B,
1, imagen_print_page);
private void
iWrite(FILE *Out, byte Val)
{
char *hexList = "0123456789ABCDEF";
if (BYTE_STREAM &&
( Val == QUOTE_CHAR || Val == EOF_CHAR
|| Val == EXTRA_QUOTE1 || Val == EXTRA_QUOTE2
|| Val == EXTRA_QUOTE3 || Val == EXTRA_QUOTE4 ) ) {
fputc (QUOTE_CHAR, Out);
fputc ((char) hexList[Val / 0x10], Out);
fputc ((char) hexList[Val % 0x10], Out);
} else {
fputc(Val, Out);
}
}
void
iWrite2(FILE *Out, int Val)
{
iWrite(Out,(byte) (Val >> 8) & 0x00FF );
iWrite(Out,(byte) Val & 0x00FF );
}
private int
imagen_prn_open(gx_device *pdev)
{
int code;
char *impHeader;
DebugMsg(1,"%s\n","Start of imagen_prn_open");
DebugMsg(2,"BIGSIZE = %ld \n",BIGSIZE);
code = gdev_prn_open(pdev);
if ( code < 0 ) return code;
DebugMsg(2,"opening file: %s\n",ppdev->fname);
code = gdev_prn_open_printer(pdev, 1);
if ( code < 0 ) return code;
impHeader = getenv("IMPRESSHEADER");
if (impHeader == NULL ) {
impHeader = IMPRESSHEADER ;
}
fprintf(ppdev->file,"@document(language impress, %s)",impHeader);
code = gdev_prn_close_printer(pdev);
if ( code < 0 ) return code;
DebugMsg(1,"%s\n","End of imagen_prn_open");
return code;
}
private int
imagen_prn_close(gx_device *pdev)
{
int code;
DebugMsg(1,"%s\n","Start of imagen_prn_close");
code = gdev_prn_open_printer(pdev, 1);
if ( code < 0 ) return code;
iWrite(ppdev->file,iEOF);
if (BYTE_STREAM) {
fputc(EOF_CHAR,ppdev->file);
}
fflush(ppdev->file);
code = gdev_prn_close_printer(pdev);
if ( code < 0 ) return code;
code = gdev_prn_close(pdev);
DebugMsg(1,"%s\n","End of imagen_prn_close");
return(code);
}
private int
imagen_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
byte *in = (byte *)gs_malloc(pdev->memory, BIGSIZE, line_size / BIGSIZE + 1,
"imagen_print_page(in)");
byte *out;
byte *swatch;
byte *temp;
byte *swatchMap;
int lnum ;
int swatchLine;
int lastLine;
int swatchCount;
int startSwatch;
int endSwatch;
int Magnify;
int totalBlankSwatches;
int totalGreySwatches;
DebugMsg(1,"%s\n","Start of imagen_print_page");
Magnify = getMagnification ;
swatchCount = (line_size + HorzBytesPerSw - 1) / HorzBytesPerSw;
totalBlankSwatches = 0 ;
totalGreySwatches = 0 ;
DebugMsg(2,"Swatch count = %d\n",swatchCount);
DebugMsg(2,"Line size = %d\n",line_size );
out = (byte *)gs_malloc(pdev->memory, TotalBytesPerSw , swatchCount + 1,
"imagen_print_page(out)");
swatchMap = (byte *)gs_malloc(pdev->memory, BIGSIZE,swatchCount / BIGSIZE + 1,
"imagen_print_page(swatchMap)" );
if ( in == 0 || out == 0 )
return -1;
iWrite(prn_stream,iPAGE);
iWrite(prn_stream,iSET_MAGNIFICATION);
iWrite(prn_stream,Magnify);
lnum = 0;
while (lnum <= pdev->height) {
for (swatch = swatchMap; swatch < swatchMap + swatchCount ;
swatch += BIGSIZE ) {
* (BIGTYPE *)swatch = (BIGTYPE) 0;
}
swatchLine = 0;
lastLine = VertBytesPerSw - 1;
if (lnum + lastLine > pdev->height ) {
lnum = pdev->height - lastLine ;
};
DebugMsg (3,"lnum = %d \n",lnum);
for (swatchLine = 0 ; swatchLine <= lastLine; swatchLine++) {
for (temp = in + line_size; temp < in + line_size + BIGSIZE;temp++){
*temp = 0;
}
gdev_prn_copy_scan_lines(pdev, lnum + swatchLine, in, line_size);
DebugMsg(5,"Got scan line %d ", lnum + swatchLine);
DebugMsg(5,"line %d \n", swatchLine);
swatch = out + swatchLine * HorzBytesPerSw;
DebugMsg(5,"offset: swatch = %d \n",(int) (swatch - out) );
temp = in;
while ( temp < in + line_size ) {
* (BIGTYPE *)swatch = * (BIGTYPE *)temp;
if ( * (BIGTYPE *)temp ) {
swatchMap[(swatch - out)/TotalBytesPerSw] = (byte) 1 ;
}
temp += (BIGSIZE > HorzBytesPerSw) ? HorzBytesPerSw : BIGSIZE ;
swatch += (BIGSIZE > HorzBytesPerSw) ? HorzBytesPerSw : BIGSIZE ;
if ( ((temp - in) % HorzBytesPerSw ) == 0 ) {
swatch += (TotalBytesPerSw - HorzBytesPerSw) ;
}
}
}
startSwatch = 0;
while (startSwatch < swatchCount ) {
if (swatchMap[startSwatch] == 0 ) {
DebugMsg(6,"Skip blank %d \n",startSwatch);
totalBlankSwatches++;
startSwatch++;
} else {
totalGreySwatches++;
endSwatch = startSwatch;
while ( (endSwatch < swatchCount) && swatchMap[endSwatch] ) {
endSwatch++;
totalGreySwatches++;
}
DebugMsg(6,"Grey swatches %d ",startSwatch);
DebugMsg(6,"until %d \n",endSwatch);
iWrite(prn_stream, iABS_V);
iWrite2(prn_stream, lnum << Magnify);
iWrite(prn_stream,iABS_H);
iWrite2(prn_stream, startSwatch * HorzBitsPerSw << Magnify );
iWrite(prn_stream,iBITMAP);
iWrite(prn_stream,0x07);
iWrite(prn_stream,(endSwatch - startSwatch));
iWrite(prn_stream, 1) ;
for (swatch = out + startSwatch * TotalBytesPerSw;
swatch < out + endSwatch * TotalBytesPerSw; swatch++) {
iWrite(prn_stream,*swatch);
}
startSwatch = endSwatch;
}
}
lnum += lastLine + 1;
}
iWrite(prn_stream,iENDPAGE);
fflush(prn_stream);
gs_free(pdev->memory, (char *)swatchMap, BIGSIZE, swatchCount / BIGSIZE + 1,
"imagen_print_page(swatchMap)" );
gs_free(pdev->memory, (char *)out, TotalBytesPerSw, swatchCount+1, "imagen_print_page(out)");
gs_free(pdev->memory, (char *)in, BIGSIZE, line_size / BIGSIZE + 1, "imagen_print_page(in)");
DebugMsg(1,"Debug: Grey: %d \n",totalGreySwatches);
DebugMsg(1,"Debug: Blank: %d \n",totalBlankSwatches );
DebugMsg(1,"%s\n","End of imagen_print_page");
return 0;
}