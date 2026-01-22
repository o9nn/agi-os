#include "gdevprn.h"
#include "gdevdljm.h"
#define MIN_SKIP_LINES 7
#define W sizeof(word)
int
dljet_mono_print_page(gx_device_printer * pdev, FILE * prn_stream,
int dots_per_inch, int features, const char *page_init)
{
return dljet_mono_print_page_copies(pdev, prn_stream, 1, dots_per_inch,
features, page_init);
}
int
dljet_mono_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies, int dots_per_inch, int features,
const char *page_init)
{
int line_size = gdev_mem_bytes_per_scan_line((gx_device *) pdev);
int line_size_words = (line_size + W - 1) / W;
uint storage_size_words = line_size_words * 8;
word *storage;
word
*data_words,
*out_row_words,
*out_row_alt_words,
*prev_row_words;
#define data ((byte *)data_words)
#define out_row ((byte *)out_row_words)
#define out_row_alt ((byte *)out_row_alt_words)
#define prev_row ((byte *)prev_row_words)
byte *out_data;
int x_dpi = (int)pdev->x_pixels_per_inch;
int y_dpi = (int)pdev->y_pixels_per_inch;
int y_dots_per_pixel = dots_per_inch / y_dpi;
int num_rows = dev_print_scan_lines(pdev);
int out_count;
int compression = -1;
static const char *const from2to3 = "\033*b3M";
static const char *const from3to2 = "\033*b2M";
int penalty_from2to3 = strlen(from2to3);
int penalty_from3to2 = strlen(from3to2);
int paper_size = gdev_pcl_paper_size((gx_device *) pdev);
int code = 0;
bool dup = pdev->Duplex;
bool dupset = pdev->Duplex_set >= 0;
if (num_copies != 1 && !(features & PCL_CAN_PRINT_COPIES))
return gx_default_print_page_copies(pdev, prn_stream, num_copies);
storage =
(ulong *)gs_alloc_byte_array(pdev->memory, storage_size_words, W,
"hpjet_print_page");
if (storage == 0)
return_error(gs_error_VMerror);
data_words = storage;
out_row_words = data_words + (line_size_words * 2);
out_row_alt_words = out_row_words + (line_size_words * 2);
prev_row_words = out_row_alt_words + (line_size_words * 2);
memset(data, 0, storage_size_words * W);
if (pdev->PageCount == 0) {
fputs("\033E", prn_stream);
if (features & PCL_CAN_SET_PAPER_SIZE) {
fprintf(prn_stream, "\033&l%dA", paper_size);
}
if (features & PCL_HAS_DUPLEX) {
if (dupset && dup)
fputs("\033&l1S", prn_stream);
else if (dupset && !dup)
fputs("\033&l0S", prn_stream);
else
fputs("\033&l1S", prn_stream);
}
}
if (features & PCL_CAN_SET_PAPER_SIZE){
fprintf(prn_stream, "\033&l%dA", paper_size);
}
fputs("\033&l0o0l0E", prn_stream);
fputs(page_init, prn_stream);
fprintf(prn_stream, "\033&l%dX", num_copies);
fputs("\033*rB\033*p0x0Y", prn_stream);
if (features & PCL_END_GRAPHICS_DOES_RESET) {
fputs(page_init, prn_stream);
fprintf(prn_stream, "\033&l%dX", num_copies);
}
fprintf(prn_stream, "\033*t%dR", x_dpi);
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
if (num_blank_lines == lnum) {
if (features & PCL_ANY_SPACING) {
if (num_blank_lines > 0)
fprintf(prn_stream, "\033*p+%dY",
num_blank_lines * y_dots_per_pixel);
fputs("\033*r1A", prn_stream);
} else if (features & PCL_MODE_3_COMPRESSION) {
fputs("\033*r1A", prn_stream);
#if 1
if (num_blank_lines > 0)
fputs("\033*b0W", prn_stream);
num_blank_lines = 0;
#else
for (; num_blank_lines; num_blank_lines--)
fputs("\033*b0W", prn_stream);
#endif
} else {
fputs("\033*r1A", prn_stream);
for (; num_blank_lines; num_blank_lines--)
fputs("\033*bW", prn_stream);
}
}
else if (num_blank_lines != 0) {
if ((num_blank_lines < MIN_SKIP_LINES && compression != 3) ||
!(features & PCL_ANY_SPACING)
) {
bool mode_3ns =
(features & PCL_MODE_3_COMPRESSION) &&
!(features & PCL_ANY_SPACING);
if (mode_3ns && compression != 2) {
fputs(from3to2, prn_stream);
compression = 2;
}
if (features & PCL_MODE_3_COMPRESSION) {
fputs("\033*b1Y", prn_stream);
num_blank_lines--;
}
if (mode_3ns) {
for (; num_blank_lines; num_blank_lines--)
fputs("\033*b0W", prn_stream);
} else {
for (; num_blank_lines; num_blank_lines--)
fputs("\033*bW", prn_stream);
}
} else if (features & PCL3_SPACING) {
fprintf(prn_stream, "\033*p+%dY",
num_blank_lines * y_dots_per_pixel);
} else {
fprintf(prn_stream, "\033*b%dY",
num_blank_lines);
}
memset(prev_row, 0, line_size);
}
num_blank_lines = 0;
if (features & PCL_MODE_3_COMPRESSION) {
int count3 = gdev_pcl_mode3compress(line_size, data,
prev_row, out_row);
int count2 = gdev_pcl_mode2compress(data_words, end_data,
out_row_alt);
int penalty3 =
(compression == 3 ? 0 : penalty_from2to3);
int penalty2 =
(compression == 2 ? 0 : penalty_from3to2);
if (count3 + penalty3 < count2 + penalty2) {
if (compression != 3)
fputs(from2to3, prn_stream);
compression = 3;
out_data = out_row;
out_count = count3;
} else {
if (compression != 2)
fputs(from3to2, prn_stream);
compression = 2;
out_data = out_row_alt;
out_count = count2;
}
} else if (features & PCL_MODE_2_COMPRESSION) {
out_data = out_row;
out_count = gdev_pcl_mode2compress(data_words, end_data,
out_row);
} else {
out_data = data;
out_count = (byte *) end_data - data;
}
fprintf(prn_stream, "\033*b%dW", out_count);
fwrite(out_data, sizeof(byte), out_count,
prn_stream);
}
}
fputs("\033*rB\f", prn_stream);
gs_free_object(pdev->memory, storage, "hpjet_print_page");
return code;
}