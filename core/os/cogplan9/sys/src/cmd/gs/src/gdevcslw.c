#include "gdevprn.h"
typedef ulong word;
#define W sizeof(word)
#define LW	0
private dev_proc_print_page(coslw_print_page);
const gx_device_printer gs_coslw2p_device =
prn_device(prn_std_procs, "coslw2p",
200, 400,
128, 128,
0, 0, 0, 0,
1, coslw_print_page);
const gx_device_printer gs_coslwxl_device =
prn_device(prn_std_procs, "coslwxl",
200, 400,
204, 204,
0, 0, 0, 0,
1, coslw_print_page);
private int
coslw_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
int line_size = gdev_mem_bytes_per_scan_line((gx_device *) pdev);
int line_size_words = (line_size + W - 1) / W;
uint storage_size_words = line_size_words * 8;
word *storage = (ulong *) gs_malloc(pdev->memory, storage_size_words, W,
"coslw_print_page");
word *data_words;
#define data ((byte *)data_words)
byte *out_data;
int num_rows = dev_print_scan_lines(pdev);
int bytes_per_line = 0;
int out_count;
int code = 0;
if (storage == 0)
return_error(gs_error_VMerror);
data_words = storage;
memset(data, 0, storage_size_words * W);
if (pdev->PageCount == 0) {
}
{
int lnum;
int num_blank_lines = 0;
word rmask = ~(word) 0 << (-pdev->width & (W * 8 - 1));
for (lnum = 0; lnum < num_rows; lnum++) {
register word *end_data =
data_words + line_size_words;
code = gdev_prn_copy_scan_lines(pdev, lnum,
(byte *) data, line_size);
if (code < 0)
break;
end_data[-1] &= rmask;
while (end_data > data_words && end_data[-1] == 0)
end_data--;
if (end_data == data_words) {
num_blank_lines++;
continue;
}
while (num_blank_lines > 0)
{
int this_blank = 255;
if (num_blank_lines < this_blank)
this_blank = num_blank_lines;
fprintf(prn_stream, "\033f\001%c", this_blank);
num_blank_lines -= this_blank;
}
out_data = data;
out_count = (byte *) end_data - data;
if (out_count > 56)
out_count = 56;
if (bytes_per_line != out_count)
{
fprintf(prn_stream, "\033D%c", out_count);
bytes_per_line = out_count;
}
fputs("\026", prn_stream);
fwrite(out_data, sizeof(byte), out_count, prn_stream);
}
}
fputs("\033E", prn_stream);
gs_free(pdev->memory, (char *)storage, storage_size_words, W, "coslw_print_page");
return code;
}