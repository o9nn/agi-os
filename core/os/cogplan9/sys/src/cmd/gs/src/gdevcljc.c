#include "math_.h"
#include "gdevprn.h"
#include "gdevpcl.h"
#define X_DPI 300
#define Y_DPI 300
private int
cljc_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
gs_memory_t *mem = pdev->memory;
uint raster = gx_device_raster((gx_device *)pdev, false);
int i;
int worst_case_comp_size = raster + (raster / 8) + 1;
byte *data = 0;
byte *cdata = 0;
byte *prow = 0;
int code = 0;
if (((data = gs_alloc_bytes(mem, raster, "cljc_print_page(data)")) == 0) ||
((cdata = gs_alloc_bytes(mem, worst_case_comp_size, "cljc_print_page(cdata)")) == 0) ||
((prow = gs_alloc_bytes(mem, worst_case_comp_size, "cljc_print_page(prow)")) == 0)) {
code = gs_note_error(gs_error_VMerror);
goto out;
}
fprintf(prn_stream, "\033E\033&u300D\033&l%dA",
gdev_pcl_paper_size((gx_device *) pdev));
fprintf(prn_stream, "\033*v1N\033*v1O");
fprintf(prn_stream, "\033*t4J\033*t%dR", (int)(pdev->HWResolution[0]));
fprintf(prn_stream, "\033*v6W%c%c%c%c%c%c", 0, 3, 0, 8, 8, 8);
fprintf(prn_stream, "\033&l0e-180u36Z\033*p0x0Y\033*r1A\033*b3M");
memset(prow, 0, worst_case_comp_size);
for (i = 0; i < pdev->height; i++) {
int compressed_size;
code = gdev_prn_copy_scan_lines(pdev, i, (byte *) data, raster);
if (code < 0)
break;
compressed_size = gdev_pcl_mode3compress(raster, data, prow, cdata);
fprintf(prn_stream, "\033*b%dW", compressed_size);
fwrite(cdata, sizeof(byte), compressed_size, prn_stream);
}
fputs("\033*rC\f", prn_stream);
out:
gs_free_object(mem, prow, "cljc_print_page(prow)");
gs_free_object(mem, cdata, "cljc_print_page(cdata)");
gs_free_object(mem, data, "cljc_print_page(data)");
return code;
}
private gx_device_procs cljc_procs =
prn_color_procs(gdev_prn_open, gdev_prn_output_page, gdev_prn_close,
gx_default_rgb_map_rgb_color, gx_default_rgb_map_color_rgb);
const gx_device_printer gs_cljet5c_device =
{
prn_device_body(gx_device_printer, cljc_procs, "cljet5c",
85, 110, X_DPI, Y_DPI,
0.167, 0.167,
0.167, 0.167,
3, 24, 255, 255, 256, 256,
cljc_print_page)
};