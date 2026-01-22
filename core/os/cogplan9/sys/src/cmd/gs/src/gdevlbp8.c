#include "gdevprn.h"
#define X_DPI 300
#define Y_DPI 300
#define LINE_SIZE ((X_DPI * 85 / 10 + 7) / 8)
private dev_proc_print_page(lbp8_print_page);
private dev_proc_print_page(lips3_print_page);
const gx_device_printer far_data gs_lbp8_device =
prn_device(prn_std_procs, "lbp8",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0.16, 0.2, 0.32, 0.21,
1, lbp8_print_page);
const gx_device_printer far_data gs_lips3_device =
prn_device(prn_std_procs, "lips3",
82,
117,
X_DPI, Y_DPI,
0.16, 0.27, 0.23, 0.27,
1, lips3_print_page);
#define ESC (char)0x1b
#define CSI '\233'
#define DCS '\220'
#define ST '\234'
static const char lbp8_init[] = {
ESC, ';', ESC, 'c', ESC, ';',
ESC, '[', '2', '&', 'z',
ESC, '[', '1', '4', 'p',
ESC, '[', '1', '1', 'h',
ESC, '[', '7', ' ', 'I',
ESC, '[', '6', '3', 'k',
};
static const char *lbp8_end = NULL;
static const char lips3_init[] = {
ESC, '<',
DCS, '0', 'J', ST,
DCS, '3', '1', ';', '3', '0', '0', ';', '2', 'J', ST,
ESC, '<',
DCS, '2', 'y', 'P', 'r', 'i', 'n', 't', 'i', 'n', 'g', '(', 'g', 's', ')', ST,
ESC, '[', '?', '1', 'l',
ESC, '[', '?', '2', 'h',
ESC, '[', '1', '1', 'h',
ESC, '[', '7', ' ', 'I',
ESC, '[', 'f'
};
static const char lips3_end[] = {
DCS, '0', 'J', ST
};
private int
can_print_page(gx_device_printer *pdev, FILE *prn_stream,
const char *init, int init_size, const char *end, int end_size)
{
char data[LINE_SIZE*2];
char *out_data;
int last_line_nro = 0;
fwrite(init, init_size, 1, prn_stream);
{
int lnum;
int line_size = gdev_mem_bytes_per_scan_line((gx_device *)pdev);
byte rmask = (byte)(0xff << (-pdev->width & 7));
for ( lnum = 0; lnum < pdev->height; lnum++ ) {
char *end_data = data + LINE_SIZE;
gdev_prn_copy_scan_lines(pdev, lnum,
(byte *)data, line_size);
end_data[-1] &= rmask;
while ( end_data > data && end_data[-1] == 0 )
end_data--;
if ( end_data != data ) {
int num_cols = 0;
int out_count;
int zero_count;
out_data = data;
fprintf(prn_stream, "%c[%de",
ESC, lnum-last_line_nro );
last_line_nro = lnum;
while (out_data < end_data) {
while(out_data < end_data && *out_data == 0) {
num_cols += 8;
out_data++;
}
out_count = end_data - out_data;
zero_count = 0;
if (out_count>22) {
out_count = 1;
while(out_data+out_count+zero_count < end_data) {
if (out_data[zero_count+out_count] != 0) {
out_count += 1+zero_count;
zero_count = 0;
}
else {
zero_count++;
if (zero_count>20)
break;
}
}
}
if (out_count==0)
break;
fprintf(prn_stream, "%c[%d`",
ESC, num_cols );
fprintf(prn_stream, "%c[%d;%d;300;.r",
ESC, out_count, out_count);
fwrite(out_data, sizeof(char),
out_count, prn_stream);
out_data += out_count+zero_count;
num_cols += 8*(out_count+zero_count);
}
}
}
}
fprintf(prn_stream, "%c=", ESC);
if (end != NULL)
fwrite(end, end_size, 1, prn_stream);
return 0;
}
private int
lbp8_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	return can_print_page(pdev, prn_stream, lbp8_init, sizeof(lbp8_init),
lbp8_end, sizeof(lbp8_end));
}
private int
lips3_print_page(gx_device_printer *pdev, FILE *prn_stream)
{	return can_print_page(pdev, prn_stream, lips3_init, sizeof(lips3_init),
lips3_end, sizeof(lips3_end));
}