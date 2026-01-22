#include "gdevprn.h"
#define X_DPI 300
#define Y_DPI 300
private dev_proc_print_page(r4081_print_page);
const gx_device_printer far_data gs_r4081_device =
prn_device(prn_std_procs, "r4081",
85,
110,
X_DPI, Y_DPI,
0.25, 0.16, 0.25, 0.16,
1, r4081_print_page);
private int
r4081_print_page(gx_device_printer *pdev, FILE *prn_stream)
{
int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
int out_size = ((pdev->width + 7) & -8) ;
byte *out = (byte *)gs_malloc(pdev->memory, out_size, 1, "r4081_print_page(out)");
int lnum = 0;
int last = pdev->height;
if ( out == 0 )
{	if ( out )
gs_free(pdev->memory, (char *)out, out_size, 1,
"r4081_print_page(out)");
return -1;
}
while ( lnum < last )
{
gdev_prn_copy_scan_lines(pdev, lnum, (byte *)out, line_size);
if ( out[0] != 0 ||
memcmp((char *)out, (char *)out+1, line_size-1)
)
break;
lnum ++;
}
while (last > lnum) {
gdev_prn_copy_scan_lines(pdev, last-1, (byte *)out, line_size);
if ( out[0] != 0 ||
memcmp((char *)out, (char *)out+1, line_size-1)
)
break;
last --;
}
fprintf(prn_stream,"\033\rP\033\022YB2 \033\022G3,%d,%d,1,1,1,%d@",
out_size, last-lnum, (lnum+1)*720/Y_DPI);
while ( lnum < last )
{
gdev_prn_copy_scan_lines(pdev, lnum, (byte *)out, line_size);
fwrite(out, sizeof(char), line_size, prn_stream);
lnum ++;
}
fputs("\f\033\rP", prn_stream);
gs_free(pdev->memory, (char *)out, out_size, 1, "r4081_print_page(out)");
return 0;
}