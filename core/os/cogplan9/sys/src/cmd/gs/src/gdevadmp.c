#include "gdevprn.h"
private dev_proc_print_page(dmp_print_page);
const gx_device_printer far_data gs_appledmp_device =
prn_device(prn_std_procs, "appledmp",
85,
110,
120, 72,
0, 0.5, 0.5, 0,
1, dmp_print_page);
const gx_device_printer far_data gs_iwlo_device =
prn_device(prn_std_procs, "iwlo",
85,
110,
160, 72,
0, 0.5, 0.5, 0,
1, dmp_print_page);
const gx_device_printer far_data gs_iwhi_device =
prn_device(prn_std_procs, "iwhi",
85,
110,
160, 144,
0, 0.5, 0.5, 0,
1, dmp_print_page);
const gx_device_printer far_data gs_iwlq_device =
prn_device(prn_std_procs, "iwlq",
85,
110,
320, 216,
0, 0, 0.5, 0,
1, dmp_print_page);
#define DMP 1
#define IWLO 2
#define IWHI 3
#define IWLQ 4
private int
dmp_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
int dev_type;
int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
int in_size = line_size * 8;
byte *buf1 = (byte *)gs_malloc(pdev->memory, in_size, 1, "dmp_print_page(buf1)");
byte *buf2 = (byte *)gs_malloc(pdev->memory, in_size, 1, "dmp_print_page(buf2)");
byte *prn = (byte *)gs_malloc(pdev->memory, 3*in_size, 1, "dmp_print_page(prn)");
byte *in = buf1;
byte *out = buf2;
int lnum = 0;
if ( buf1 == 0 || buf2 == 0 || prn == 0 )
{
if ( buf1 )
gs_free(pdev->memory, (char *)buf1, in_size, 1,
"dmp_print_page(buf1)");
if ( buf2 )
gs_free(pdev->memory, (char *)buf2, in_size, 1,
"dmp_print_page(buf2)");
if ( prn )
gs_free(pdev->memory, (char *)prn, in_size, 1,
"dmp_print_page(prn)");
return_error(gs_error_VMerror);
}
if ( pdev->y_pixels_per_inch == 216 )
dev_type = IWLQ;
else if ( pdev->y_pixels_per_inch == 144 )
dev_type = IWHI;
else if ( pdev->x_pixels_per_inch == 160 )
dev_type = IWLO;
else
dev_type = DMP;
fputs("\r\n\033>\033T16", prn_stream);
switch(dev_type)
{
case IWLQ:
fputs("\033P\033a3", prn_stream);
break;
case IWHI:
case IWLO:
fputs("\033P", prn_stream);
break;
case DMP:
default:
fputs("\033q", prn_stream);
break;
}
while ( lnum < pdev->height )
{
byte *inp;
byte *in_end;
byte *out_end;
int lcnt,ltmp;
int count, passes;
byte *prn_blk, *prn_end, *prn_tmp;
switch (dev_type)
{
case IWLQ: passes = 3; break;
case IWHI: passes = 2; break;
case IWLO:
case DMP:
default: passes = 1; break;
}
for (count = 0; count < passes; count++)
{
for (lcnt=0; lcnt<8; lcnt++)
{
switch(dev_type)
{
case IWLQ: ltmp = lcnt + 8*count; break;
case IWHI: ltmp = 2*lcnt + count; break;
case IWLO:
case DMP:
default: ltmp = lcnt; break;
}
if ((lnum+ltmp)>pdev->height)
memset(in+lcnt*line_size,0,line_size);
else
gdev_prn_copy_scan_lines(pdev,
lnum+ltmp, in + line_size*(7 - lcnt),
line_size);
}
out_end = out;
inp = in;
in_end = inp + line_size;
for ( ; inp < in_end; inp++, out_end += 8 )
{
gdev_prn_transpose_8x8(inp, line_size,
out_end, 1);
}
out_end = out;
switch (dev_type)
{
case IWLQ: prn_end = prn + count; break;
case IWHI: prn_end = prn + in_size*count; break;
case IWLO:
case DMP:
default: prn_end = prn; break;
}
while ( (int)(out_end-out) < in_size)
{
*prn_end = *(out_end++);
if ((dev_type) == IWLQ) prn_end += 3;
else prn_end++;
}
}
switch (dev_type)
{
case IWLQ:
prn_blk = prn;
prn_end = prn_blk + in_size * 3;
while (prn_end > prn && prn_end[-1] == 0 &&
prn_end[-2] == 0 && prn_end[-3] == 0)
{
prn_end -= 3;
}
while (prn_blk < prn_end && prn_blk[0] == 0 &&
prn_blk[1] == 0 && prn_blk[2] == 0)
{
prn_blk += 3;
}
if (prn_end != prn_blk)
{
if ((prn_blk - prn) > 7)
fprintf(prn_stream,"\033U%04d%c%c%c",
(int)((prn_blk - prn)/3),
0, 0, 0);
else
prn_blk = prn;
fprintf(prn_stream,"\033C%04d",
(int)((prn_end - prn_blk)/3));
fwrite(prn_blk, 1, (int)(prn_end - prn_blk),
prn_stream);
}
break;
case IWHI:
for (count = 0; count < 2; count++)
{
prn_blk = prn_tmp = prn + in_size*count;
prn_end = prn_blk + in_size;
while (prn_end > prn_blk && prn_end[-1] == 0)
prn_end--;
while (prn_blk < prn_end && prn_blk[0] == 0)
prn_blk++;
if (prn_end != prn_blk)
{
if ((prn_blk - prn_tmp) > 7)
fprintf(prn_stream,
"\033V%04d%c",
(int)(prn_blk-prn_tmp),
0);
else
prn_blk = prn_tmp;
fprintf(prn_stream,"\033G%04d",
(int)(prn_end - prn_blk));
fwrite(prn_blk, 1,
(int)(prn_end - prn_blk),
prn_stream);
}
if (!count) fputs("\033T01\r\n",prn_stream);
}
fputs("\033T15",prn_stream);
break;
case IWLO:
case DMP:
default:
prn_blk = prn;
prn_end = prn_blk + in_size;
while (prn_end > prn_blk && prn_end[-1] == 0)
prn_end--;
while (prn_blk < prn_end && prn_blk[0] == 0)
prn_blk++;
if (prn_end != prn_blk)
{
if ((prn_blk - prn) > 7)
fprintf(prn_stream,"\033V%04d%c",
(int)(prn_blk - prn), 0);
else
prn_blk = prn;
fprintf(prn_stream,"\033G%04d",
(int)(prn_end - prn_blk));
fwrite(prn_blk, 1, (int)(prn_end - prn_blk),
prn_stream);
}
break;
}
fputs("\r\n",prn_stream);
switch (dev_type)
{
case IWLQ: lnum += 24 ; break;
case IWHI: lnum += 16 ; break;
case IWLO:
case DMP:
default: lnum += 8 ; break;
}
}
if ( !(dev_type == DMP) )
fputs("\033T99\n\n\033r\n\n\n\n\033f", prn_stream);
fputs("\033T16\f\033<\033B\033E", prn_stream);
fflush(prn_stream);
gs_free(pdev->memory, (char *)prn, in_size, 1, "dmp_print_page(prn)");
gs_free(pdev->memory, (char *)buf2, in_size, 1, "dmp_print_page(buf2)");
gs_free(pdev->memory, (char *)buf1, in_size, 1, "dmp_print_page(buf1)");
return 0;
}