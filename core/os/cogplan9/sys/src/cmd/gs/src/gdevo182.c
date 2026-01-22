#include "gdevprn.h"
private dev_proc_print_page(oki_print_page);
const gx_device_printer far_data gs_oki182_device =
prn_device(prn_std_procs, "oki182",
80,
110,
72,
72,
0, 0, 0, 0,
1, oki_print_page);
private void
oki_transpose(byte *in, byte *out, int scanBits, register int lineSize)
{
register bitMask = 0x80;
register byte *inPtr;
register byte outByte;
while (scanBits-- > 0) {
inPtr = in;
if (*inPtr & bitMask)
outByte = 0x81;
else
outByte = 0x80;
if (*(inPtr += lineSize) & bitMask)
outByte += 0x02;
if (*(inPtr += lineSize) & bitMask)
outByte += 0x04;
if (*(inPtr += lineSize) & bitMask)
outByte += 0x08;
if (*(inPtr += lineSize) & bitMask)
outByte += 0x10;
if (*(inPtr += lineSize) & bitMask)
outByte += 0x20;
if (*(inPtr += lineSize) & bitMask)
outByte += 0x40;
*out++ = outByte;
if ((bitMask >>= 1) == 0) {
bitMask = 0x80;
in ++;
}
}
}
private byte *
oki_compress(byte *in, int origWidth, int highRes,
int *numSpaces, int *newWidth)
{
int spaces = 0;
int columns_per_space = 6;
byte *in_end = in + origWidth;
while (in_end > in && in_end[-1] == 0x80)
in_end --;
if (highRes)
columns_per_space = 12;
while(in < in_end && in[0] == 0x80 && memcmp((char *)in,
(char *)in + 1, columns_per_space - 1) == 0) {
spaces++;
in += columns_per_space;
}
*numSpaces = spaces;
if (in_end > in)
*newWidth = in_end - in;
else
*newWidth = 0;
return(in);
}
private int
oki_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
int highRes = pdev->y_pixels_per_inch > 100;
int bits_per_column = 7;
int i, spaces, width;
int lcnt;
int line_size = gdev_prn_raster((gx_device_printer *)pdev);
byte *in = (byte *)gs_malloc(pdev->memory, 16, line_size, "oki_print_page(in)");
byte *out1 = (byte *)gs_malloc(pdev->memory, 8, line_size, "oki_print_page(out1)");
byte *out2 = (byte *)gs_malloc(pdev->memory, 8, line_size, "oki_print_page(out2)");
byte *out3;
int lnum = 0;
int skip = 0;
int code = 0;
if ( in == 0 || out1 == 0 || out2 == 0)
{	code = gs_error_VMerror;
gs_note_error(code);
goto bail;
}
fwrite("\030\034\033%C001\033%S0", 1, 12, prn_stream);
if (highRes) {
fwrite("\033R", 1, 2, prn_stream);
bits_per_column = 14;
}
while ( lnum < pdev->height ) {
code = gdev_prn_copy_scan_lines(pdev, lnum, in, line_size);
if ( code < 0 )
goto xit;
if ( in[0] == 0 && !memcmp((char *)in, (char *)in + 1,
line_size - 1)) {
lnum++;
if (highRes)
skip++;
else
skip += 2;
continue;
}
while ( skip > 127 ) {
fputs("\033%5\177", prn_stream);
skip -= 127;
}
if ( skip )
fprintf(prn_stream, "\033%%5%c",
(char) (skip & 0xff));
skip = 0;
code = gdev_prn_copy_scan_lines(pdev, lnum + 1,
in + line_size, (bits_per_column - 1) * line_size);
if ( code < 0 )
goto xit;
lcnt = code + 1;
if ( lcnt < bits_per_column )
memset(in + lcnt * line_size, 0,
(bits_per_column - lcnt) * line_size);
if (highRes) {
oki_transpose(in, out1, pdev->width, 2 * line_size);
oki_transpose(in + line_size, out2,
pdev->width, 2 * line_size);
} else
oki_transpose(in, out1, pdev->width, line_size);
out3 = oki_compress(out1, pdev->width, highRes,
&spaces, &width);
for (i=0; i < spaces; i++)
putc(' ', prn_stream);
fwrite("\003", 1, 1, prn_stream);
fwrite(out3, 1, width, prn_stream);
if (highRes) {
fprintf(prn_stream, "\003\002\015\033%%5%c", (char) 1);
out3 = oki_compress(out2, pdev->width, highRes,
&spaces, &width);
for (i=0; i < spaces; i++)
putc(' ', prn_stream);
fwrite("\003", 1, 1, prn_stream);
fwrite(out3, 1, width, prn_stream);
fprintf(prn_stream, "\003\002\015\033%%5%c", (char) 13);
} else
fwrite("\003\016\003\002", 1, 4, prn_stream);
lnum += bits_per_column;
}
xit:
fputc(014, prn_stream);
fflush(prn_stream);
bail:
if ( out1 != 0 )
gs_free(pdev->memory, (char *)out1, 8, line_size, "oki_print_page(out1)");
if ( out2 != 0 )
gs_free(pdev->memory, (char *)out2, 8, line_size, "oki_print_page(out2)");
if ( in != 0 )
gs_free(pdev->memory, (char *)in, 16, line_size, "oki_print_page(in)");
return code;
}