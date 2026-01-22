#include "gdevprn.h"
#include "gdevpcl.h"
#include "malloc_.h"
#ifndef SHINGLING
#define SHINGLING 1
#endif
#ifndef DEPLETION
#define DEPLETION 1
#endif
#define X_DPI 300
#define Y_DPI 300
#define LINE_SIZE ((X_DPI * 85 / 10 + 63) / 64 * 8)
private dev_proc_print_page(djet500c_print_page);
private gx_device_procs djet500c_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
gdev_pcl_3bit_map_rgb_color, gdev_pcl_3bit_map_color_rgb);
const gx_device_printer far_data gs_djet500c_device =
prn_device(djet500c_procs, "djet500c",
85,
120,
X_DPI, Y_DPI,
0.25, 0.25, 0.25, 0.25,
3, djet500c_print_page);
private int djet500c_print_page(gx_device_printer *, FILE *);
static int mode2compress(byte *row, byte *end_row, byte *compressed);
private int
djet500c_print_page(gx_device_printer *pdev, FILE *fprn)
{
byte *bitData=NULL;
byte *plane1=NULL;
byte *plane2=NULL;
byte *plane3=NULL;
int bitSize=0;
int planeSize=0;
fputs("\033E",fprn);
fputs("\033*rbC", fprn);
fputs("\033*t300R", fprn);
fputs("\033&l26a0l1H", fprn);
fputs("\033*r3U", fprn);
fprintf(fprn, "\033*o%dD", DEPLETION);
fprintf(fprn, "\033*o%dQ", SHINGLING);
fputs("\033*p0x0Y", fprn);
fputs("\033*b2M", fprn);
fputs("\033*r0A", fprn);
{ int lnum;
int num_blank_lines = 0;
int lineSize = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
if (lineSize>bitSize)
{
if (bitData) free(bitData);
bitSize=lineSize;
bitData=(byte*)malloc(bitSize+16);
}
for (lnum=0; lnum<pdev->height; lnum++)
{
byte *endData;
gdev_prn_copy_scan_lines(pdev, lnum, bitData, lineSize);
endData = bitData + lineSize;
while ( (endData>bitData) && (endData[-1] == 0) )
endData--;
if (endData == bitData)
num_blank_lines++;
else
{ int count, k, i, lineLen;
memset(endData, 0, 7);
lineLen=((endData-bitData)+7)/8;
if (planeSize<lineLen)
{
if (plane1) free(plane1);
if (plane2) free(plane2);
if (plane3) free(plane3);
planeSize=lineLen;
plane1=(byte*)malloc(planeSize+8);
plane2=(byte*)malloc(planeSize+8);
plane3=(byte*)malloc(planeSize+8);
}
for (k=i=0; k<lineLen; i+=8, k++)
{
register ushort t, c;
for (c=t=0;t<8;t++)
c = (c<<1) | (bitData[t+i]&4);
plane3[k] = ~(byte)(c>>2);
for (c=t=0;t<8;t++)
c = (c<<1) | (bitData[t+i]&2);
plane2[k] = ~(byte)(c>>1);
for (c=t=0;t<8;t++)
c = (c<<1) | (bitData[t+i]&1);
plane1[k] = ~(byte)(c);
}
if (num_blank_lines > 0)
{
fprintf(fprn, "\033*b%dY", num_blank_lines);
num_blank_lines = 0;
}
fprintf(fprn, "\033*r%dS", lineLen*8);
count = mode2compress(plane1, plane1 + lineLen, bitData);
fprintf(fprn, "\033*b%dV", count);
fwrite(bitData, sizeof(byte), count, fprn);
count = mode2compress(plane2, plane2 + lineLen, bitData);
fprintf(fprn, "\033*b%dV", count);
fwrite(bitData, sizeof(byte), count, fprn);
count = mode2compress(plane3, plane3 + lineLen, bitData);
fprintf(fprn, "\033*b%dW", count);
fwrite(bitData, sizeof(byte), count, fprn);
}
}
}
fputs("\033*rbC", fprn);
fputs("\033*r1U", fprn);
fputs("\033E",fprn);
fputs("\033&l0H", fprn);
if (bitData) free(bitData);
if (plane1) free(plane1);
if (plane2) free(plane2);
if (plane3) free(plane3);
return 0;
}
static int
mode2compress(byte *row, byte *end_row, byte *compressed)
{
register byte *exam;
register byte *cptr = compressed;
int i, count, len;
byte test;
exam = row;
while (1)
{
test = *exam++;
while ((test != *exam) && (exam < end_row))
test = *exam++;
if (exam<end_row) exam--;
len=exam-row;
while (len>0)
{
count=len;
if (count>127) count=127;
*cptr++=count-1;
for (i=0;i<count;i++) *cptr++ = *row++;
len-=count;
}
if (exam>=end_row) break;
exam++;
while ((test == *exam) && (exam < end_row))
exam++;
len = exam-row;
while (len>0)
{
count=len;
if (count>127) count=127;
*cptr++=(257-count);
*cptr++=test;
len-=count;
}
if (exam>=end_row) break;
row = exam;
}
return (cptr-compressed);
}