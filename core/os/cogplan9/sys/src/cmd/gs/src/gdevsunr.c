#include "gdevprn.h"
#define	RAS_MAGIC	0x59a66a95
#define RT_STANDARD	1
#define RMT_NONE	0
typedef struct sun_rasterfile_s {
int	ras_magic;
int	ras_width;
int	ras_height;
int	ras_depth;
int	ras_length;
int	ras_type;
int	ras_maptype;
int	ras_maplength;
} sun_rasterfile_t;
#ifndef X_DPI
#  define X_DPI 72
#endif
#ifndef Y_DPI
#  define Y_DPI 72
#endif
private dev_proc_print_page(sunhmono_print_page);
const gx_device_printer gs_sunhmono_device =
prn_device(prn_std_procs, "sunhmono",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
1, sunhmono_print_page);
private int
sunhmono_print_page(gx_device_printer * pdev, FILE * prn_stream)
{
int gsLineBytes = gdev_mem_bytes_per_scan_line((gx_device *) pdev);
int rasLineBytes = ROUND_UP(gsLineBytes, 2);
int lineCnt;
char *lineStorage;
byte *data;
sun_rasterfile_t ras;
int code = 0;
lineStorage = gs_malloc(pdev->memory, gsLineBytes, 1, "rasterfile_print_page(in)");
if (lineStorage == 0) {
code = gs_note_error(gs_error_VMerror);
goto out;
}
ras.ras_magic = RAS_MAGIC;
ras.ras_width = pdev->width;
ras.ras_height = pdev->height;
ras.ras_depth = 1;
ras.ras_length = (rasLineBytes * pdev->height);
ras.ras_type = RT_STANDARD;
ras.ras_maptype = RMT_NONE;
ras.ras_maplength = 0;
fwrite(&ras, 1, sizeof(ras), prn_stream);
for (lineCnt = 0; lineCnt < pdev->height; ++lineCnt) {
gdev_prn_get_bits(pdev, lineCnt, lineStorage, &data);
fwrite(data, 1, gsLineBytes, prn_stream);
if (gsLineBytes % 2)
fputc(0, prn_stream);
}
fwrite("};\n", 1, 3, prn_stream);
out:
gs_free(pdev->memory, lineStorage, gsLineBytes, 1, "rasterfile_print_page(in)");
return code;
}