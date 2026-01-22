#include "gdevprn.h"
#include "gdevdljm.h"
#ifdef X_DPI
# define X_DPI2 X_DPI
#else
# define X_DPI 300
# define X_DPI2 600
#endif
#ifdef Y_DPI
# define Y_DPI2 Y_DPI
#else
# define Y_DPI 300
# define Y_DPI2 600
#endif
#define DESKJET_MARGINS_LETTER (float)0.2, (float)0.45, (float)0.3, (float)0.05
#define DESKJET_MARGINS_A4 (float)0.125, (float)0.5, (float)0.143, (float)0.09
#define LASERJET_MARGINS_A4 (float)0.167, (float)0.167, (float)0.167, (float)0.167
#define LASERJET_MARGINS_LETTER (float)0.167, (float)0.167, (float)0.167, (float)0.167
private dev_proc_open_device(hpjet_open);
private dev_proc_close_device(hpjet_close);
private dev_proc_print_page_copies(djet_print_page_copies);
private dev_proc_print_page_copies(djet500_print_page_copies);
private dev_proc_print_page_copies(fs600_print_page_copies);
private dev_proc_print_page_copies(ljet_print_page_copies);
private dev_proc_print_page_copies(ljetplus_print_page_copies);
private dev_proc_print_page_copies(ljet2p_print_page_copies);
private dev_proc_print_page_copies(ljet3_print_page_copies);
private dev_proc_print_page_copies(ljet3d_print_page_copies);
private dev_proc_print_page_copies(ljet4_print_page_copies);
private dev_proc_print_page_copies(ljet4d_print_page_copies);
private dev_proc_print_page_copies(lp2563_print_page_copies);
private dev_proc_print_page_copies(oce9050_print_page_copies);
private dev_proc_get_params(hpjet_get_params);
private dev_proc_put_params(hpjet_put_params);
private const gx_device_procs prn_hp_procs =
prn_params_procs(hpjet_open, gdev_prn_output_page, hpjet_close,
hpjet_get_params, hpjet_put_params);
typedef struct gx_device_hpjet_s gx_device_hpjet;
struct gx_device_hpjet_s {
gx_device_common;
gx_prn_device_common;
int MediaPosition;
bool MediaPosition_set;
bool ManualFeed;
bool ManualFeed_set;
};
#define HPJET_DEVICE(procs, dname, w10, h10, xdpi, ydpi, lm, bm, rm, tm, color_bits, print_page_copies)\
{ prn_device_std_margins_body_copies(gx_device_hpjet, procs, dname, \
w10, h10, xdpi, ydpi, lm, tm, lm, bm, rm, tm, color_bits, \
print_page_copies), \
0, false, false, false }
const gx_device_hpjet gs_deskjet_device =
HPJET_DEVICE(prn_hp_procs, "deskjet",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
1, djet_print_page_copies);
const gx_device_hpjet gs_djet500_device =
HPJET_DEVICE(prn_hp_procs, "djet500",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
1, djet500_print_page_copies);
const gx_device_hpjet gs_fs600_device =
HPJET_DEVICE(prn_hp_procs, "fs600",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI2, Y_DPI2,
0.23, 0.0, 0.23, 0.04,
1, fs600_print_page_copies);
const gx_device_hpjet gs_laserjet_device =
HPJET_DEVICE(prn_hp_procs, "laserjet",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0.05, 0.25, 0.55, 0.25,
1, ljet_print_page_copies);
const gx_device_hpjet gs_ljetplus_device =
HPJET_DEVICE(prn_hp_procs, "ljetplus",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0.05, 0.25, 0.55, 0.25,
1, ljetplus_print_page_copies);
const gx_device_hpjet gs_ljet2p_device =
HPJET_DEVICE(prn_hp_procs, "ljet2p",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0.20, 0.25, 0.25, 0.25,
1, ljet2p_print_page_copies);
const gx_device_hpjet gs_ljet3_device =
HPJET_DEVICE(prn_hp_procs, "ljet3",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0.20, 0.25, 0.25, 0.25,
1, ljet3_print_page_copies);
const gx_device_hpjet gs_ljet3d_device =
HPJET_DEVICE(prn_hp_procs, "ljet3d",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0.20, 0.25, 0.25, 0.25,
1, ljet3d_print_page_copies);
const gx_device_hpjet gs_ljet4_device =
HPJET_DEVICE(prn_hp_procs, "ljet4",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI2, Y_DPI2,
0, 0, 0, 0,
1, ljet4_print_page_copies);
const gx_device_hpjet gs_ljet4d_device =
HPJET_DEVICE(prn_hp_procs, "ljet4d",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI2, Y_DPI2,
0, 0, 0, 0,
1, ljet4d_print_page_copies);
const gx_device_hpjet gs_lp2563_device =
HPJET_DEVICE(prn_hp_procs, "lp2563",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS,
X_DPI, Y_DPI,
0, 0, 0, 0,
1, lp2563_print_page_copies);
const gx_device_hpjet gs_oce9050_device =
HPJET_DEVICE(prn_hp_procs, "oce9050",
24 * 10, 24 * 10,
400, 400,
0, 0, 0, 0,
1, oce9050_print_page_copies);
private int
hpjet_open(gx_device * pdev)
{
gx_device_printer *const ppdev = (gx_device_printer *)pdev;
const float *m = 0;
bool move_origin = true;
if (ppdev->printer_procs.print_page_copies == djet_print_page_copies ||
ppdev->printer_procs.print_page_copies == djet500_print_page_copies
) {
static const float m_a4[4] =
{DESKJET_MARGINS_A4};
static const float m_letter[4] =
{DESKJET_MARGINS_LETTER};
m = (gdev_pcl_paper_size(pdev) == PAPER_SIZE_A4 ? m_a4 :
m_letter);
} else if (ppdev->printer_procs.print_page_copies == oce9050_print_page_copies ||
ppdev->printer_procs.print_page_copies == lp2563_print_page_copies
);
else {
static const float m_a4[4] =
{LASERJET_MARGINS_A4};
static const float m_letter[4] =
{LASERJET_MARGINS_LETTER};
m = (gdev_pcl_paper_size(pdev) == PAPER_SIZE_A4 ? m_a4 :
m_letter);
move_origin = false;
}
if (m != 0)
gx_device_set_margins(pdev, m, move_origin);
if (ppdev->printer_procs.print_page_copies == ljet3d_print_page_copies)
ppdev->Duplex = true, ppdev->Duplex_set = 0;
if (ppdev->printer_procs.print_page_copies == ljet4d_print_page_copies)
ppdev->Duplex = true, ppdev->Duplex_set = 0;
return gdev_prn_open(pdev);
}
private int
hpjet_close(gx_device * pdev)
{
gx_device_printer *const ppdev = (gx_device_printer *)pdev;
int code = gdev_prn_open_printer(pdev, 1);
if (code < 0)
return code;
if (ppdev->PageCount > 0) {
if (ppdev->Duplex_set >= 0 && ppdev->Duplex)
fputs("\033&l0H", ppdev->file);
fputs("\033E", ppdev->file);
}
return gdev_prn_close(pdev);
}
private void
hpjet_make_init(gx_device_printer *pdev, char *buf, const char *str)
{
gx_device_hpjet *dev = (gx_device_hpjet *)pdev;
int paper_source = -1;
int paper_source_tab[] = { 5, 1 };
if (dev->ManualFeed_set && dev->ManualFeed) paper_source = 2;
else if (dev->MediaPosition_set && dev->MediaPosition >= 0 &&
dev->MediaPosition < countof(paper_source_tab))
paper_source = paper_source_tab[dev->MediaPosition];
if (paper_source >= 0)
sprintf(buf, "%s\033&l%dH", str, paper_source);
else
sprintf(buf, "%s", str);
}
private int
djet_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
char init[80];
hpjet_make_init(pdev, init, "\033&k1W\033*b2M");
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
300, PCL_DJ_FEATURES, init);
}
private int
djet500_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
char init[80];
hpjet_make_init(pdev, init, "\033&k1W");
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
300, PCL_DJ500_FEATURES, init);
}
private int
fs600_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
int dots_per_inch = (int)pdev->y_pixels_per_inch;
char base_init[60];
char init[80];
sprintf(base_init, "\033*r0F\033&u%dD", dots_per_inch);
hpjet_make_init(pdev, init, base_init);
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
dots_per_inch, PCL_FS600_FEATURES,
init);
}
private int
ljet_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
char init[80];
hpjet_make_init(pdev, init, "\033*b0M");
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
300, PCL_LJ_FEATURES, init);
}
private int
ljetplus_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
char init[80];
hpjet_make_init(pdev, init, "\033*b0M");
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
300, PCL_LJplus_FEATURES, init);
}
private int
ljet2p_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
char init[80];
hpjet_make_init(pdev, init, "\033*r0F\033*b2M");
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
300, PCL_LJ2p_FEATURES, init);
}
private int
ljet3_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
char init[80];
hpjet_make_init(pdev, init, "\033&l-180u36Z\033*r0F");
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
300, PCL_LJ3_FEATURES, init);
}
private int
ljet3d_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
char init[80];
hpjet_make_init(pdev, init, "\033&l-180u36Z\033*r0F");
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
300, PCL_LJ3D_FEATURES, init);
}
private int
ljet4_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
int dots_per_inch = (int)pdev->y_pixels_per_inch;
char base_init[60];
char init[80];
sprintf(base_init, "\033&l-180u36Z\033*r0F\033&u%dD", dots_per_inch);
hpjet_make_init(pdev, init, base_init);
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
dots_per_inch, PCL_LJ4_FEATURES,
init);
}
private int
ljet4d_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
int dots_per_inch = (int)pdev->y_pixels_per_inch;
char base_init[60];
char init[80];
sprintf(base_init, "\033&l-180u36Z\033*r0F\033&u%dD", dots_per_inch);
hpjet_make_init(pdev, init, base_init);
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
dots_per_inch, PCL_LJ4D_FEATURES,
init);
}
private int
lp2563_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
char init[80];
hpjet_make_init(pdev, init, "\033*b0M");
return dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
300, PCL_LP2563B_FEATURES, init);
}
private int
oce9050_print_page_copies(gx_device_printer * pdev, FILE * prn_stream,
int num_copies)
{
int code;
char init[80];
fputs("\033%1B", prn_stream);
fputs("BP", prn_stream);
fputs("IN;", prn_stream);
fputs("\033%1A", prn_stream);
hpjet_make_init(pdev, init, "\033*b0M");
code = dljet_mono_print_page_copies(pdev, prn_stream, num_copies,
400, PCL_OCE9050_FEATURES, init);
fputs("\033%1B", prn_stream);
if (code == 0) {
fputs("PU", prn_stream);
fputs("SP0", prn_stream);
fputs("PG;", prn_stream);
fputs("\033E", prn_stream);
}
return code;
}
private int
hpjet_get_params(gx_device *pdev, gs_param_list *plist)
{
gx_device_hpjet *dev = (gx_device_hpjet *)pdev;
int code = gdev_prn_get_params(pdev, plist);
if (code >= 0)
code = param_write_bool(plist, "ManualFeed", &dev->ManualFeed);
return code;
}
private int
hpjet_put_params(gx_device *pdev, gs_param_list *plist)
{
gx_device_hpjet *dev = (gx_device_hpjet *)pdev;
int code;
bool ManualFeed;
bool ManualFeed_set = false;
int MediaPosition;
bool MediaPosition_set = false;
code = param_read_bool(plist, "ManualFeed", &ManualFeed);
if (code == 0) ManualFeed_set = true;
if (code >= 0) {
code = param_read_int(plist, "%MediaSource", &MediaPosition);
if (code == 0) MediaPosition_set = true;
else if (code < 0) {
if (param_read_null(plist, "%MediaSource") == 0) {
code = 0;
}
}
}
if (code >= 0)
code = gdev_prn_put_params(pdev, plist);
if (code >= 0) {
if (ManualFeed_set) {
dev->ManualFeed = ManualFeed;
dev->ManualFeed_set = true;
}
if (MediaPosition_set) {
dev->MediaPosition = MediaPosition;
dev->MediaPosition_set = true;
}
}
return code;
}