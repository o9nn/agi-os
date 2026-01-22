#include "gdevprn.h"
#include "gdevpccm.h"
#include "windows_.h"
#include <shellapi.h>
#include "gp_mswin.h"
#include "gp.h"
#include "gpcheck.h"
#include "commdlg.h"
typedef struct gx_device_win_pr2_s gx_device_win_pr2;
#undef wdev
#define wdev ((gx_device_win_pr2 *)dev)
private dev_proc_open_device(win_pr2_open);
private dev_proc_close_device(win_pr2_close);
private dev_proc_print_page(win_pr2_print_page);
private dev_proc_map_rgb_color(win_pr2_map_rgb_color);
private dev_proc_map_color_rgb(win_pr2_map_color_rgb);
private dev_proc_get_params(win_pr2_get_params);
private dev_proc_put_params(win_pr2_put_params);
private void win_pr2_set_bpp(gx_device * dev, int depth);
private const gx_device_procs win_pr2_procs =
prn_color_params_procs(win_pr2_open, gdev_prn_output_page, win_pr2_close,
win_pr2_map_rgb_color, win_pr2_map_color_rgb,
win_pr2_get_params, win_pr2_put_params);
#define PARENT_WINDOW HWND_DESKTOP
BOOL CALLBACK CancelDlgProc(HWND, UINT, WPARAM, LPARAM);
BOOL CALLBACK AbortProc2(HDC, int);
typedef struct gx_device_win_pr2_s gx_device_win_pr2;
struct gx_device_win_pr2_s {
gx_device_common;
gx_prn_device_common;
HDC hdcprn;
bool nocancel;
int doc_page_begin;
int doc_page_end;
int user_page_begin;
int user_page_end;
int user_copies;
int print_copies;
float user_media_size[2];
char doc_name[200];
char paper_name[64];
bool user_changed_settings;
int user_paper;
int user_orient;
int user_color;
int max_dpi;
int ratio;
int selected_bpp;
bool tumble;
int query_user;
HANDLE win32_hdevmode;
HANDLE win32_hdevnames;
DLGPROC lpfnAbortProc;
DLGPROC lpfnCancelProc;
HWND hDlgModeless;
bool use_old_spool_name;
gx_device_win_pr2* original_device;
};
gx_device_win_pr2 far_data gs_mswinpr2_device =
{
prn_device_std_body(gx_device_win_pr2, win_pr2_procs, "mswinpr2",
DEFAULT_WIDTH_10THS, DEFAULT_HEIGHT_10THS, 72.0, 72.0,
0, 0, 0, 0,
0, win_pr2_print_page),
0,
0,
0,
0,
0,
0,
1,
1,
{ 0.0, 0.0 },
{ 0 },
{ 0 },
0,
0,
0,
0,
0,
0,
0,
false,
-1,
NULL,
NULL,
NULL,
NULL,
NULL,
false,
NULL
};
private int win_pr2_getdc(gx_device_win_pr2 * dev);
private int win_pr2_update_dev(gx_device_win_pr2 * dev, LPDEVMODE pdevmode);
private int win_pr2_update_win(gx_device_win_pr2 * dev, LPDEVMODE pdevmode);
private int win_pr2_print_setup_interaction(gx_device_win_pr2 * dev, int mode);
private int win_pr2_write_user_settings(gx_device_win_pr2 * dev, gs_param_list * plist);
private int win_pr2_read_user_settings(gx_device_win_pr2 * dev, gs_param_list * plist);
private void win_pr2_copy_check(gx_device_win_pr2 * dev);
private int
win_pr2_open(gx_device * dev)
{
int code;
int depth;
PRINTDLG pd;
POINT offset;
POINT size;
float m[4];
FILE *pfile;
DOCINFO docinfo;
float ratio = 1.0;
win_pr2_copy_check(wdev);
if ((wdev->win32_hdevmode) &&
(wdev->win32_hdevnames)) {
LPDEVMODE devmode = (LPDEVMODE) GlobalLock(wdev->win32_hdevmode);
LPDEVNAMES devnames = (LPDEVNAMES) GlobalLock(wdev->win32_hdevnames);
const char* driver = (char*)(devnames)+(devnames->wDriverOffset);
const char* device = (char*)(devnames)+(devnames->wDeviceOffset);
const char* output = (char*)(devnames)+(devnames->wOutputOffset);
wdev->hdcprn = CreateDC(driver, device, output, devmode);
GlobalUnlock(wdev->win32_hdevmode);
GlobalUnlock(wdev->win32_hdevnames);
if (wdev->hdcprn == NULL) {
return gs_error_Fatal;
}
} else if (!win_pr2_getdc(wdev)) {
LPDEVMODE devmode = NULL;
memset(&pd, 0, sizeof(pd));
pd.lStructSize = sizeof(pd);
pd.hwndOwner = PARENT_WINDOW;
pd.Flags = PD_RETURNDC;
pd.nMinPage = wdev->doc_page_begin;
pd.nMaxPage = wdev->doc_page_end;
pd.nFromPage = wdev->user_page_begin;
pd.nToPage = wdev->user_page_end;
pd.nCopies = wdev->user_copies;
if (!PrintDlg(&pd)) {
return gs_error_Fatal;
}
devmode = GlobalLock(pd.hDevMode);
win_pr2_update_dev(wdev,devmode);
GlobalUnlock(pd.hDevMode);
if (wdev->win32_hdevmode)
GlobalFree(wdev->win32_hdevmode);
if (wdev->win32_hdevnames)
GlobalFree(wdev->win32_hdevnames);
wdev->hdcprn = pd.hDC;
wdev->win32_hdevmode = pd.hDevMode;
wdev->win32_hdevnames = pd.hDevNames;
pd.hDevMode = NULL;
pd.hDevNames = NULL;
}
if (!(GetDeviceCaps(wdev->hdcprn, RASTERCAPS) != RC_DIBTODEV)) {
errprintf( "Windows printer does not have RC_DIBTODEV\n");
DeleteDC(wdev->hdcprn);
return gs_error_limitcheck;
}
wdev->lpfnAbortProc = (DLGPROC) AbortProc2;
SetAbortProc(wdev->hdcprn, (ABORTPROC) wdev->lpfnAbortProc);
memset(&docinfo, 0, sizeof(docinfo));
docinfo.cbSize = sizeof(docinfo);
docinfo.lpszDocName = wdev->doc_name;
if (docinfo.lpszDocName[0] == 0) {
docinfo.lpszDocName = "Ghostscript output";
}
if (StartDoc(wdev->hdcprn, &docinfo) <= 0) {
errprintf("Printer StartDoc failed (error %08x)\n", GetLastError());
DeleteDC(wdev->hdcprn);
return gs_error_limitcheck;
}
dev->x_pixels_per_inch = (float)GetDeviceCaps(wdev->hdcprn, LOGPIXELSX);
dev->y_pixels_per_inch = (float)GetDeviceCaps(wdev->hdcprn, LOGPIXELSY);
wdev->ratio = 1;
if (wdev->max_dpi > 50) {
float dpi_x = dev->x_pixels_per_inch;
float dpi_y = dev->y_pixels_per_inch;
while ((dev->x_pixels_per_inch > wdev->max_dpi)
|| (dev->y_pixels_per_inch > wdev->max_dpi)) {
ratio += 1.0;
wdev->ratio ++;
dev->x_pixels_per_inch = dpi_x / ratio;
dev->y_pixels_per_inch = dpi_y / ratio;
}
}
size.x = GetDeviceCaps(wdev->hdcprn, PHYSICALWIDTH);
size.y = GetDeviceCaps(wdev->hdcprn, PHYSICALHEIGHT);
gx_device_set_width_height(dev, (int)(size.x / ratio), (int)(size.y / ratio));
offset.x = GetDeviceCaps(wdev->hdcprn, PHYSICALOFFSETX);
offset.y = GetDeviceCaps(wdev->hdcprn, PHYSICALOFFSETY);
m[0] = offset.x / dev->x_pixels_per_inch / ratio;
m[3] = offset.y / dev->y_pixels_per_inch / ratio;
m[2] = (size.x - offset.x - GetDeviceCaps(wdev->hdcprn, HORZRES)) / dev->x_pixels_per_inch / ratio;
m[1] = (size.y - offset.y - GetDeviceCaps(wdev->hdcprn, VERTRES)) / dev->y_pixels_per_inch / ratio;
gx_device_set_margins(dev, m, true);
depth = dev->color_info.depth;
if (depth == 0) {
depth = GetDeviceCaps(wdev->hdcprn, PLANES) * GetDeviceCaps(wdev->hdcprn, BITSPIXEL);
}
win_pr2_set_bpp(dev, depth);
pfile = gp_open_scratch_file(gp_scratch_file_name_prefix,
wdev->fname, "wb");
fclose(pfile);
code = gdev_prn_open(dev);
unlink(wdev->fname);
if (!wdev->nocancel) {
wdev->lpfnCancelProc = (DLGPROC) CancelDlgProc;
wdev->hDlgModeless = CreateDialog(phInstance, "CancelDlgBox",
PARENT_WINDOW, wdev->lpfnCancelProc);
ShowWindow(wdev->hDlgModeless, SW_HIDE);
}
return code;
};
private int
win_pr2_close(gx_device * dev)
{
int code;
int aborted = FALSE;
win_pr2_copy_check(wdev);
if (!wdev->nocancel) {
if (!wdev->hDlgModeless)
aborted = TRUE;
else
DestroyWindow(wdev->hDlgModeless);
wdev->hDlgModeless = 0;
}
if (aborted)
AbortDoc(wdev->hdcprn);
else
EndDoc(wdev->hdcprn);
DeleteDC(wdev->hdcprn);
if (wdev->win32_hdevmode != NULL) {
GlobalFree(wdev->win32_hdevmode);
wdev->win32_hdevmode = NULL;
}
if (wdev->win32_hdevnames != NULL) {
GlobalFree(wdev->win32_hdevnames);
wdev->win32_hdevnames = NULL;
}
code = gdev_prn_close(dev);
return code;
}
#undef wdev
#define wdev ((gx_device_win_pr2 *)pdev)
private int
win_pr2_print_page(gx_device_printer * pdev, FILE * file)
{
int raster = gdev_prn_raster(pdev);
ulong bmp_raster = raster + (-raster & 3);
ulong bmp_raster_multi;
int scan_lines, yslice, lines, i;
int width;
int depth = pdev->color_info.depth;
byte *row;
int y;
int code = 0;
MSG msg;
char dlgtext[32];
HGLOBAL hrow;
int ratio = ((gx_device_win_pr2 *)pdev)->ratio;
struct bmi_s {
BITMAPINFOHEADER h;
RGBQUAD pal[256];
} bmi;
scan_lines = dev_print_scan_lines(pdev);
width = (int)(pdev->width - ((dev_l_margin(pdev) + dev_r_margin(pdev) -
dev_x_offset(pdev)) * pdev->x_pixels_per_inch));
yslice = 65535 / bmp_raster;
bmp_raster_multi = bmp_raster * yslice;
hrow = GlobalAlloc(0, bmp_raster_multi);
row = GlobalLock(hrow);
if (row == 0)
return_error(gs_error_VMerror);
bmi.h.biSize = sizeof(bmi.h);
bmi.h.biWidth = pdev->width;
bmi.h.biHeight = yslice;
bmi.h.biPlanes = 1;
bmi.h.biBitCount = pdev->color_info.depth;
bmi.h.biCompression = 0;
bmi.h.biSizeImage = 0;
bmi.h.biXPelsPerMeter = 0;
bmi.h.biYPelsPerMeter = 0;
StartPage(wdev->hdcprn);
if (depth <= 8) {
int i;
gx_color_value rgb[3];
LPRGBQUAD pq;
bmi.h.biClrUsed = 1 << depth;
bmi.h.biClrImportant = 1 << depth;
for (i = 0; i != 1 << depth; i++) {
(*dev_proc(pdev, map_color_rgb)) ((gx_device *) pdev,
(gx_color_index) i, rgb);
pq = &bmi.pal[i];
pq->rgbRed = gx_color_value_to_byte(rgb[0]);
pq->rgbGreen = gx_color_value_to_byte(rgb[1]);
pq->rgbBlue = gx_color_value_to_byte(rgb[2]);
pq->rgbReserved = 0;
}
} else {
bmi.h.biClrUsed = 0;
bmi.h.biClrImportant = 0;
}
if (!wdev->nocancel) {
sprintf(dlgtext, "Printing page %d", (int)(pdev->PageCount) + 1);
SetWindowText(GetDlgItem(wdev->hDlgModeless, CANCEL_PRINTING), dlgtext);
ShowWindow(wdev->hDlgModeless, SW_SHOW);
}
for (y = 0; y < scan_lines;) {
if (y > scan_lines - yslice)
lines = scan_lines - y;
else
lines = yslice;
for (i = 0; i < lines; i++)
gdev_prn_copy_scan_lines(pdev, y + i,
row + (bmp_raster * (lines - 1 - i)), raster);
if (ratio > 1) {
StretchDIBits(wdev->hdcprn, 0, y*ratio, pdev->width*ratio, lines*ratio,
0, 0, pdev->width, lines,
row,
(BITMAPINFO FAR *) & bmi, DIB_RGB_COLORS, SRCCOPY);
} else {
SetDIBitsToDevice(wdev->hdcprn, 0, y, pdev->width, lines,
0, 0, 0, lines,
row,
(BITMAPINFO FAR *) & bmi, DIB_RGB_COLORS);
}
y += lines;
if (!wdev->nocancel) {
sprintf(dlgtext, "%d%% done", (int)(y * 100L / scan_lines));
SetWindowText(GetDlgItem(wdev->hDlgModeless, CANCEL_PCDONE), dlgtext);
}
while (PeekMessage(&msg, wdev->hDlgModeless, 0, 0, PM_REMOVE)) {
if ((wdev->hDlgModeless == 0) || !IsDialogMessage(wdev->hDlgModeless, &msg)) {
TranslateMessage(&msg);
DispatchMessage(&msg);
}
}
if ((!wdev->nocancel) && (wdev->hDlgModeless == 0)) {
break;
}
}
if ((!wdev->nocancel) && (wdev->hDlgModeless == 0))
code = gs_error_Fatal;
else {
if (!wdev->nocancel)
SetWindowText(GetDlgItem(wdev->hDlgModeless, CANCEL_PCDONE),
"Ejecting page...");
EndPage(wdev->hdcprn);
if (!wdev->nocancel)
ShowWindow(wdev->hDlgModeless, SW_HIDE);
}
GlobalUnlock(hrow);
GlobalFree(hrow);
return code;
}
private gx_color_index
win_pr2_map_rgb_color(gx_device * dev, const gx_color_value cv[])
{
gx_color_value r = cv[0];
gx_color_value g = cv[1];
gx_color_value b = cv[2];
switch (dev->color_info.depth) {
case 1:
return gdev_prn_map_rgb_color(dev, cv);
case 4:
return (r > (gx_max_color_value / 2 + 1) ? 4 : 0) +
(g > (gx_max_color_value / 2 + 1) ? 2 : 0) +
(b > (gx_max_color_value / 2 + 1) ? 1 : 0);
case 8:
return pc_8bit_map_rgb_color(dev, cv);
case 24:
return gx_color_value_to_byte(r) +
((uint) gx_color_value_to_byte(g) << 8) +
((ulong) gx_color_value_to_byte(b) << 16);
}
return 0;
}
private int
win_pr2_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
switch (dev->color_info.depth) {
case 1:
gdev_prn_map_color_rgb(dev, color, prgb);
break;
case 4:
prgb[0] = (color & 4) ? gx_max_color_value : 0;
prgb[1] = (color & 2) ? gx_max_color_value : 0;
prgb[2] = (color & 1) ? gx_max_color_value : 0;
break;
case 8:
pc_8bit_map_color_rgb(dev, color, prgb);
break;
case 24:
prgb[2] = gx_color_value_from_byte(color >> 16);
prgb[1] = gx_color_value_from_byte((color >> 8) & 0xff);
prgb[0] = gx_color_value_from_byte(color & 0xff);
break;
}
return 0;
}
void
win_pr2_set_bpp(gx_device * dev, int depth)
{
if (depth > 8) {
static const gx_device_color_info win_pr2_24color = dci_std_color(24);
dev->color_info = win_pr2_24color;
depth = 24;
} else if (depth >= 8) {
static const gx_device_color_info win_pr2_8color = dci_pc_8bit;
dev->color_info = win_pr2_8color;
depth = 8;
} else if (depth >= 3) {
static const gx_device_color_info win_pr2_4color = dci_values(3, 4, 1, 1, 2, 2);
dev->color_info = win_pr2_4color;
depth = 4;
} else {
static const gx_device_color_info win_pr2_1color = dci_std_color(1);
dev->color_info = win_pr2_1color;
depth = 1;
}
((gx_device_win_pr2 *)dev)->selected_bpp = depth;
dev->procs.encode_color = dev->procs.map_rgb_color;
dev->procs.decode_color = dev->procs.map_color_rgb;
if (depth == 1) {
dev->procs.get_color_mapping_procs =
gx_default_DevGray_get_color_mapping_procs;
dev->procs.get_color_comp_index =
gx_default_DevGray_get_color_comp_index;
}
else {
dev->procs.get_color_mapping_procs =
gx_default_DevRGB_get_color_mapping_procs;
dev->procs.get_color_comp_index =
gx_default_DevRGB_get_color_comp_index;
}
}
int
win_pr2_get_params(gx_device * pdev, gs_param_list * plist)
{
int code = gdev_prn_get_params(pdev, plist);
win_pr2_copy_check(wdev);
if (code >= 0)
code = param_write_bool(plist, "NoCancel",
&(wdev->nocancel));
if (code >= 0)
code = param_write_int(plist, "QueryUser",
&(wdev->query_user));
if (code >= 0)
code = win_pr2_write_user_settings(wdev, plist);
if ((code >= 0) && (wdev->Duplex_set > 0))
code = param_write_bool(plist, "Tumble",
&(wdev->tumble));
return code;
}
int
win_pr2_put_params(gx_device * pdev, gs_param_list * plist)
{
int ecode = 0, code;
int old_bpp = pdev->color_info.depth;
int bpp = old_bpp;
bool tumble = wdev->tumble;
bool nocancel = wdev->nocancel;
int queryuser = 0;
bool old_duplex = wdev->Duplex;
bool old_tumble = wdev->tumble;
int old_orient = wdev->user_orient;
int old_color = wdev->user_color;
int old_paper = wdev->user_paper;
int old_mx_dpi = wdev->max_dpi;
if (wdev->Duplex_set < 0) {
wdev->Duplex_set = 0;
wdev->Duplex = false;
wdev->tumble = false;
}
win_pr2_copy_check(wdev);
code = win_pr2_read_user_settings(wdev, plist);
switch (code = param_read_int(plist, "BitsPerPixel", &bpp)) {
case 0:
if (pdev->is_open) {
if (wdev->selected_bpp == bpp) {
break;
}
ecode = gs_error_rangecheck;
} else {
win_pr2_set_bpp(pdev, bpp);
break;
}
goto bppe;
default:
ecode = code;
bppe:param_signal_error(plist, "BitsPerPixel", ecode);
case 1:
break;
}
switch (code = param_read_bool(plist, "NoCancel", &nocancel)) {
case 0:
if (pdev->is_open) {
if (wdev->nocancel == nocancel) {
break;
}
ecode = gs_error_rangecheck;
} else {
wdev->nocancel = nocancel;
break;
}
goto nocancele;
default:
ecode = code;
nocancele:param_signal_error(plist, "NoCancel", ecode);
case 1:
break;
}
switch (code = param_read_bool(plist, "Tumble", &tumble)) {
case 0:
wdev->tumble = tumble;
break;
default:
ecode = code;
param_signal_error(plist, "Tumble", ecode);
case 1:
break;
}
switch (code = param_read_int(plist, "QueryUser", &queryuser)) {
case 0:
if ((queryuser > 0) &&
(queryuser < 4)) {
win_pr2_print_setup_interaction(wdev, queryuser);
}
break;
default:
ecode = code;
param_signal_error(plist, "QueryUser", ecode);
case 1:
break;
}
if (ecode >= 0)
ecode = gdev_prn_put_params(pdev, plist);
if (wdev->win32_hdevmode && wdev->hdcprn) {
if ( (old_duplex != wdev->Duplex)
|| (old_tumble != wdev->tumble)
|| (old_orient != wdev->user_orient)
|| (old_color != wdev->user_color)
|| (old_paper != wdev->user_paper)
|| (old_mx_dpi != wdev->max_dpi) ) {
LPDEVMODE pdevmode = GlobalLock(wdev->win32_hdevmode);
if (pdevmode) {
win_pr2_update_win(wdev, pdevmode);
ResetDC(wdev->hdcprn, pdevmode);
GlobalUnlock(pdevmode);
}
}
}
return ecode;
}
#undef wdev
private int
win_pr2_getdc(gx_device_win_pr2 * wdev)
{
char *device;
char *devices;
char *p;
char driverbuf[512];
char *driver;
char *output;
char *devcap;
int devcapsize;
int size;
int i, n;
POINT *pp;
int paperindex;
int paperwidth, paperheight;
int orientation;
int papersize;
char papername[64];
char drvname[32];
HINSTANCE hlib;
LPFNDEVMODE pfnExtDeviceMode;
LPFNDEVCAPS pfnDeviceCapabilities;
LPDEVMODE podevmode, pidevmode;
HANDLE hprinter;
if (is_spool(wdev->fname)) {
device = wdev->fname + 8;
wdev->use_old_spool_name = true;
} else if (strncmp("%printer%",wdev->fname,9) == 0) {
device = wdev->fname + 9;
wdev->use_old_spool_name = false;
} else {
return FALSE;
}
if ((devices = gs_malloc(wdev->memory, 4096, 1, "win_pr2_getdc")) == (char *)NULL)
return FALSE;
GetProfileString("Devices", NULL, "", devices, 4096);
p = devices;
while (*p) {
if (stricmp(p, device) == 0)
break;
p += strlen(p) + 1;
}
if (*p == '\0')
p = NULL;
gs_free(wdev->memory, devices, 4096, 1, "win_pr2_getdc");
if (p == NULL)
return FALSE;
GetProfileString("Devices", device, "", driverbuf, sizeof(driverbuf));
driver = strtok(driverbuf, ",");
output = strtok(NULL, ",");
if (is_win32s)
{
strcpy(drvname, driver);
strcat(drvname, ".drv");
driver = drvname;
}
if (!is_win32s) {
if (!OpenPrinter(device, &hprinter, NULL))
return FALSE;
size = DocumentProperties(NULL, hprinter, device, NULL, NULL, 0);
if ((podevmode = gs_malloc(wdev->memory, size, 1, "win_pr2_getdc")) == (LPDEVMODE) NULL) {
ClosePrinter(hprinter);
return FALSE;
}
if ((pidevmode = gs_malloc(wdev->memory, size, 1, "win_pr2_getdc")) == (LPDEVMODE) NULL) {
gs_free(wdev->memory, podevmode, size, 1, "win_pr2_getdc");
ClosePrinter(hprinter);
return FALSE;
}
DocumentProperties(NULL, hprinter, device, podevmode, NULL, DM_OUT_BUFFER);
pfnDeviceCapabilities = (LPFNDEVCAPS) DeviceCapabilities;
} else
{
hlib = LoadLibrary(driver);
if (hlib < (HINSTANCE) HINSTANCE_ERROR)
return FALSE;
pfnExtDeviceMode = (LPFNDEVMODE) GetProcAddress(hlib, "ExtDeviceMode");
if (pfnExtDeviceMode == (LPFNDEVMODE) NULL) {
FreeLibrary(hlib);
return FALSE;
}
pfnDeviceCapabilities = (LPFNDEVCAPS) GetProcAddress(hlib, "DeviceCapabilities");
if (pfnDeviceCapabilities == (LPFNDEVCAPS) NULL) {
FreeLibrary(hlib);
return FALSE;
}
size = pfnExtDeviceMode(NULL, hlib, NULL, device, output, NULL, NULL, 0);
if ((podevmode = gs_malloc(wdev->memory, size, 1, "win_pr2_getdc")) == (LPDEVMODE) NULL) {
FreeLibrary(hlib);
return FALSE;
}
if ((pidevmode = gs_malloc(wdev->memory, size, 1, "win_pr2_getdc")) == (LPDEVMODE) NULL) {
gs_free(wdev->memory, podevmode, size, 1, "win_pr2_getdc");
FreeLibrary(hlib);
return FALSE;
}
pfnExtDeviceMode(NULL, hlib, podevmode, device, output,
NULL, NULL, DM_OUT_BUFFER);
}
devcapsize = pfnDeviceCapabilities(device, output, DC_PAPERSIZE, NULL, NULL);
devcapsize *= sizeof(POINT);
if ((devcap = gs_malloc(wdev->memory, devcapsize, 1, "win_pr2_getdc")) == (LPBYTE) NULL)
return FALSE;
n = pfnDeviceCapabilities(device, output, DC_PAPERSIZE, devcap, NULL);
paperwidth = (int)(wdev->MediaSize[0] * 254 / 72);
paperheight = (int)(wdev->MediaSize[1] * 254 / 72);
papername[0] = '\0';
papersize = 0;
paperindex = -1;
orientation = 0;
pp = (POINT *) devcap;
for (i = 0; i < n; i++, pp++) {
if ((pp->x < paperwidth + 20) && (pp->x > paperwidth - 20) &&
(pp->y < paperheight + 20) && (pp->y > paperheight - 20)) {
paperindex = i;
paperwidth = pp->x;
paperheight = pp->y;
orientation = DMORIENT_PORTRAIT;
break;
}
}
if (paperindex < 0) {
pp = (POINT *) devcap;
for (i = 0; i < n; i++, pp++) {
if ((pp->x < paperheight + 20) && (pp->x > paperheight - 20) &&
(pp->y < paperwidth + 20) && (pp->y > paperwidth - 20)) {
paperindex = i;
paperwidth = pp->x;
paperheight = pp->y;
orientation = DMORIENT_LANDSCAPE;
break;
}
}
}
gs_free(wdev->memory, devcap, devcapsize, 1, "win_pr2_getdc");
devcapsize = pfnDeviceCapabilities(device, output, DC_PAPERS, NULL, NULL);
devcapsize *= sizeof(WORD);
if ((devcap = gs_malloc(wdev->memory, devcapsize, 1, "win_pr2_getdc")) == (LPBYTE) NULL)
return FALSE;
n = pfnDeviceCapabilities(device, output, DC_PAPERS, devcap, NULL);
if ((paperindex >= 0) && (paperindex < n))
papersize = ((WORD *) devcap)[paperindex];
gs_free(wdev->memory, devcap, devcapsize, 1, "win_pr2_getdc");
devcapsize = pfnDeviceCapabilities(device, output, DC_PAPERNAMES, NULL, NULL);
devcapsize *= 64;
if ((devcap = gs_malloc(wdev->memory, devcapsize, 1, "win_pr2_getdc")) == (LPBYTE) NULL)
return FALSE;
n = pfnDeviceCapabilities(device, output, DC_PAPERNAMES, devcap, NULL);
if ((paperindex >= 0) && (paperindex < n))
strcpy(papername, devcap + paperindex * 64);
gs_free(wdev->memory, devcap, devcapsize, 1, "win_pr2_getdc");
memcpy(pidevmode, podevmode, size);
pidevmode->dmFields = 0;
wdev->paper_name[0] = 0;
if ( (wdev->user_paper)
&& (wdev->user_paper != papersize) ) {
papersize = wdev->user_paper;
paperheight = 0;
paperwidth = 0;
papername[0] = 0;
}
if (wdev->user_orient) {
orientation = wdev->user_orient;
}
pidevmode->dmFields &= ~(DM_PAPERSIZE | DM_ORIENTATION | DM_COLOR | DM_PAPERLENGTH | DM_PAPERWIDTH | DM_DUPLEX);
pidevmode->dmFields |= DM_DEFAULTSOURCE;
pidevmode->dmDefaultSource = 0;
if (orientation) {
wdev->user_orient = orientation;
}
if (papersize) {
wdev->user_paper = papersize;
strcpy (wdev->paper_name, papername);
}
if (paperheight && paperwidth) {
pidevmode->dmFields |= (DM_PAPERLENGTH | DM_PAPERWIDTH);
pidevmode->dmPaperWidth = paperwidth;
pidevmode->dmPaperLength = paperheight;
wdev->user_media_size[0] = paperwidth / 254.0 * 72.0;
wdev->user_media_size[1] = paperheight / 254.0 * 72.0;
}
if (DeviceCapabilities(device, output, DC_DUPLEX, NULL, NULL)) {
wdev->Duplex_set = 1;
}
win_pr2_update_win(wdev, pidevmode);
if (!is_win32s) {
DocumentProperties(NULL, hprinter, device, podevmode, pidevmode, DM_IN_BUFFER | DM_OUT_BUFFER);
ClosePrinter(hprinter);
wdev->hdcprn = CreateDC(driver, device, NULL, podevmode);
} else
{
pfnExtDeviceMode(NULL, hlib, podevmode, device, output,
pidevmode, NULL, DM_IN_BUFFER | DM_OUT_BUFFER);
FreeLibrary(hlib);
if (is_win32s)
strtok(driver, ".");
wdev->hdcprn = CreateDC(driver, device, output, podevmode);
}
if (wdev->win32_hdevmode == NULL) {
wdev->win32_hdevmode = GlobalAlloc(0, sizeof(DEVMODE));
}
if (wdev->win32_hdevmode) {
LPDEVMODE pdevmode = (LPDEVMODE) GlobalLock(wdev->win32_hdevmode);
if (pdevmode) {
memcpy(pdevmode, podevmode, sizeof(DEVMODE));
GlobalUnlock(wdev->win32_hdevmode);
}
}
gs_free(wdev->memory, pidevmode, size, 1, "win_pr2_getdc");
gs_free(wdev->memory, podevmode, size, 1, "win_pr2_getdc");
if (wdev->hdcprn != (HDC) NULL)
return TRUE;
return FALSE;
}
private int
win_pr2_update_dev(gx_device_win_pr2 * dev, LPDEVMODE pdevmode)
{
if (pdevmode == 0)
return FALSE;
if (pdevmode->dmFields & DM_COLOR) {
dev->user_color = pdevmode->dmColor;
}
if (pdevmode->dmFields & DM_ORIENTATION) {
dev->user_orient = pdevmode->dmOrientation;
}
if (pdevmode->dmFields & DM_PAPERSIZE) {
dev->user_paper = pdevmode->dmPaperSize;
dev->user_media_size[0] = pdevmode->dmPaperWidth / 254.0 * 72.0;
dev->user_media_size[1] = pdevmode->dmPaperLength / 254.0 * 72.0;
dev->paper_name[0] = 0;
}
if (pdevmode->dmFields & DM_DUPLEX) {
dev->Duplex_set = 1;
dev->Duplex = pdevmode->dmDuplex == DMDUP_SIMPLEX ? false : true;
dev->tumble = pdevmode->dmDuplex == DMDUP_HORIZONTAL ? true : false;
}
return TRUE;
}
private int
win_pr2_update_win(gx_device_win_pr2 * dev, LPDEVMODE pdevmode)
{
if (dev->Duplex_set > 0) {
pdevmode->dmFields |= DM_DUPLEX;
pdevmode->dmDuplex = DMDUP_SIMPLEX;
if (dev->Duplex) {
if (dev->tumble == false) {
pdevmode->dmDuplex = DMDUP_VERTICAL;
} else {
pdevmode->dmDuplex = DMDUP_HORIZONTAL;
}
}
}
if (dev->user_color) {
pdevmode->dmColor = dev->user_color;
pdevmode->dmFields |= DM_COLOR;
}
if (dev->user_orient) {
pdevmode->dmFields |= DM_ORIENTATION;
pdevmode->dmOrientation = dev->user_orient;
}
if (dev->user_paper) {
pdevmode->dmFields |= DM_PAPERSIZE;
pdevmode->dmPaperSize = dev->user_paper;
}
return 0;
}
#define BEGIN_ARRAY_PARAM(pread, pname, pa, psize, e)\
switch ( code = pread(dict.list, (param_name = pname), &(pa)) )\
{\
case 0:\
if ( (pa).size != psize )\
ecode = gs_note_error(gs_error_rangecheck);\
else {
#define END_ARRAY_PARAM(pa, e)\
}\
goto e;\
default:\
ecode = code;\
e: param_signal_error(dict.list, param_name, ecode);\
case 1:\
(pa).data = 0; \
}
private int
win_pr2_read_user_settings(gx_device_win_pr2 * wdev, gs_param_list * plist)
{
gs_param_dict dict;
gs_param_string docn = { 0 };
const char* dict_name = "UserSettings";
const char* param_name = "";
int code = 0;
int ecode = 0;
switch (code = param_begin_read_dict(plist, dict_name, &dict, false)) {
default:
param_signal_error(plist, dict_name, code);
return code;
case 1:
break;
case 0:
{
gs_param_int_array ia;
BEGIN_ARRAY_PARAM(param_read_int_array, "DocumentRange", ia, 2, ia)
if ((ia.data[0] < 0) ||
(ia.data[1] < 0) ||
(ia.data[0] > ia.data[1]))
ecode = gs_note_error(gs_error_rangecheck);
wdev->doc_page_begin = ia.data[0];
wdev->doc_page_end = ia.data[1];
END_ARRAY_PARAM(ia, doc_range_error)
BEGIN_ARRAY_PARAM(param_read_int_array, "SelectedRange", ia, 2, ia)
if ((ia.data[0] < 0) ||
(ia.data[1] < 0) ||
(ia.data[0] > ia.data[1]))
ecode = gs_note_error(gs_error_rangecheck);
wdev->user_page_begin = ia.data[0];
wdev->user_page_end = ia.data[1];
END_ARRAY_PARAM(ia, sel_range_error)
param_read_int(dict.list, "Copies", &wdev->user_copies);
param_read_int(dict.list, "Paper", &wdev->user_paper);
param_read_int(dict.list, "Orientation", &wdev->user_orient);
param_read_int(dict.list, "Color", &wdev->user_color);
param_read_int(dict.list, "MaxResolution", &wdev->max_dpi);
switch (code = param_read_string(dict.list, (param_name = "DocumentName"), &docn)) {
case 0:
if (docn.size < sizeof(wdev->doc_name))
break;
code = gs_error_rangecheck;
default:
ecode = code;
param_signal_error(plist, param_name, ecode);
case 1:
docn.data = 0;
break;
}
param_end_read_dict(plist, dict_name, &dict);
if (docn.data) {
memcpy(wdev->doc_name, docn.data, docn.size);
wdev->doc_name[docn.size] = 0;
}
wdev->print_copies = 1;
if (wdev->win32_hdevmode) {
LPDEVMODE devmode = (LPDEVMODE) GlobalLock(wdev->win32_hdevmode);
if (devmode) {
devmode->dmCopies = wdev->user_copies;
devmode->dmPaperSize = wdev->user_paper;
devmode->dmOrientation = wdev->user_orient;
devmode->dmColor = wdev->user_color;
GlobalUnlock(wdev->win32_hdevmode);
}
}
}
break;
}
return code;
}
private int
win_pr2_write_user_settings(gx_device_win_pr2 * wdev, gs_param_list * plist)
{
gs_param_dict dict;
gs_param_int_array range;
gs_param_float_array box;
gs_param_string docn;
gs_param_string papn;
int array[2];
const char* pname = "UserSettings";
int code;
dict.size = 12;
code = param_begin_write_dict(plist, pname, &dict, false);
if (code < 0) return code;
array[0] = wdev->doc_page_begin;
array[1] = wdev->doc_page_end;
range.data = array;
range.size = 2;
range.persistent = false;
code = param_write_int_array(dict.list, "DocumentRange", &range);
if (code < 0) goto error;
array[0] = wdev->user_page_begin;
array[1] = wdev->user_page_end;
range.data = array;
range.size = 2;
range.persistent = false;
code = param_write_int_array(dict.list, "SelectedRange", &range);
if (code < 0) goto error;
box.data = wdev->user_media_size;
box.size = 2;
box.persistent = false;
code = param_write_float_array(dict.list, "MediaSize", &box);
if (code < 0) goto error;
code = param_write_int(dict.list, "Copies", &wdev->user_copies);
if (code < 0) goto error;
code = param_write_int(dict.list, "Paper", &wdev->user_paper);
if (code < 0) goto error;
code = param_write_int(dict.list, "Orientation", &wdev->user_orient);
if (code < 0) goto error;
code = param_write_int(dict.list, "Color", &wdev->user_color);
if (code < 0) goto error;
code = param_write_int(dict.list, "MaxResolution", &wdev->max_dpi);
if (code < 0) goto error;
code = param_write_int(dict.list, "PrintCopies", &wdev->print_copies);
if (code < 0) goto error;
docn.data = (const byte*)wdev->doc_name;
docn.size = strlen(wdev->doc_name);
docn.persistent = false;
code = param_write_string(dict.list, "DocumentName", &docn);
if (code < 0) goto error;
papn.data = (const byte*)wdev->paper_name;
papn.size = strlen(wdev->paper_name);
papn.persistent = false;
code = param_write_string(dict.list, "PaperName", &papn);
if (code < 0) goto error;
code = param_write_bool(dict.list, "UserChangedSettings", &wdev->user_changed_settings);
error:
param_end_write_dict(plist, pname, &dict);
return code;
}
private int
win_pr2_print_setup_interaction(gx_device_win_pr2 * wdev, int mode)
{
PRINTDLG pd;
LPDEVMODE devmode;
LPDEVNAMES devnames;
wdev->user_changed_settings = FALSE;
wdev->query_user = mode;
memset(&pd, 0, sizeof(pd));
pd.lStructSize = sizeof(pd);
pd.hwndOwner = PARENT_WINDOW;
switch (mode) {
case 2: pd.Flags = PD_PRINTSETUP; break;
case 3: pd.Flags = PD_RETURNDEFAULT; break;
default: pd.Flags = 0; break;
}
pd.Flags |= PD_USEDEVMODECOPIES;
pd.nMinPage = wdev->doc_page_begin;
pd.nMaxPage = wdev->doc_page_end;
pd.nFromPage = wdev->user_page_begin;
pd.nToPage = wdev->user_page_end;
pd.nCopies = wdev->user_copies;
if (!PrintDlg(&pd)) return FALSE;
devmode = (LPDEVMODE) GlobalLock(pd.hDevMode);
devnames = (LPDEVNAMES) GlobalLock(pd.hDevNames);
wdev->user_changed_settings = TRUE;
if (wdev->use_old_spool_name) {
sprintf(wdev->fname, "\\\\spool\\%s", (char*)(devnames)+(devnames->wDeviceOffset));
} else {
sprintf(wdev->fname, "%%printer%%%s", (char*)(devnames)+(devnames->wDeviceOffset));
}
if (mode == 3) {
devmode->dmCopies = wdev->user_copies * wdev->print_copies;
pd.nCopies = 1;
}
wdev->user_page_begin = pd.nFromPage;
wdev->user_page_end = pd.nToPage;
wdev->user_copies = devmode->dmCopies;
wdev->print_copies = pd.nCopies;
wdev->user_media_size[0] = devmode->dmPaperWidth / 254.0 * 72.0;
wdev->user_media_size[1] = devmode->dmPaperLength / 254.0 * 72.0;
wdev->user_paper = devmode->dmPaperSize;
wdev->user_orient = devmode->dmOrientation;
wdev->user_color = devmode->dmColor;
if (devmode->dmFields & DM_DUPLEX) {
wdev->Duplex_set = 1;
wdev->Duplex = devmode->dmDuplex == DMDUP_SIMPLEX ? false : true;
wdev->tumble = devmode->dmDuplex == DMDUP_HORIZONTAL ? true : false;
}
{
float xppinch = 0;
float yppinch = 0;
const char* driver = (char*)(devnames)+(devnames->wDriverOffset);
const char* device = (char*)(devnames)+(devnames->wDeviceOffset);
const char* output = (char*)(devnames)+(devnames->wOutputOffset);
HDC hic = CreateIC(driver, device, output, devmode);
if (hic) {
xppinch = (float)GetDeviceCaps(hic, LOGPIXELSX);
yppinch = (float)GetDeviceCaps(hic, LOGPIXELSY);
wdev->user_media_size[0] = GetDeviceCaps(hic, PHYSICALWIDTH) * 72.0 / xppinch;
wdev->user_media_size[1] = GetDeviceCaps(hic, PHYSICALHEIGHT) * 72.0 / yppinch;
DeleteDC(hic);
}
}
devmode = NULL;
devnames = NULL;
GlobalUnlock(pd.hDevMode);
GlobalUnlock(pd.hDevNames);
if (wdev->win32_hdevmode != NULL) {
GlobalFree(wdev->win32_hdevmode);
}
if (wdev->win32_hdevnames != NULL) {
GlobalFree(wdev->win32_hdevnames);
}
wdev->win32_hdevmode = pd.hDevMode;
wdev->win32_hdevnames = pd.hDevNames;
return TRUE;
}
private void
win_pr2_copy_check(gx_device_win_pr2 * wdev)
{
HGLOBAL hdevmode = wdev->win32_hdevmode;
HGLOBAL hdevnames = wdev->win32_hdevnames;
DWORD devmode_len = (hdevmode) ? GlobalSize(hdevmode) : 0;
DWORD devnames_len = (hdevnames) ? GlobalSize(hdevnames) : 0;
if (wdev->original_device == wdev)
return;
wdev->hdcprn = NULL;
wdev->win32_hdevmode = NULL;
wdev->win32_hdevnames = NULL;
wdev->original_device = wdev;
if (devmode_len) {
wdev->win32_hdevmode = GlobalAlloc(0, devmode_len);
if (wdev->win32_hdevmode) {
memcpy(GlobalLock(wdev->win32_hdevmode), GlobalLock(hdevmode), devmode_len);
GlobalUnlock(wdev->win32_hdevmode);
GlobalUnlock(hdevmode);
}
}
if (devnames_len) {
wdev->win32_hdevnames = GlobalAlloc(0, devnames_len);
if (wdev->win32_hdevnames) {
memcpy(GlobalLock(wdev->win32_hdevnames), GlobalLock(hdevnames), devnames_len);
GlobalUnlock(wdev->win32_hdevnames);
GlobalUnlock(hdevnames);
}
}
}
BOOL CALLBACK
CancelDlgProc(HWND hDlg, UINT message, WPARAM wParam, LPARAM lParam)
{
switch (message) {
case WM_INITDIALOG:
SetWindowText(hDlg, szAppName);
return TRUE;
case WM_COMMAND:
switch (LOWORD(wParam)) {
case IDCANCEL:
DestroyWindow(hDlg);
EndDialog(hDlg, 0);
return TRUE;
}
}
return FALSE;
}
BOOL CALLBACK
AbortProc2(HDC hdcPrn, int code)
{
process_interrupts(NULL);
if (code == SP_OUTOFDISK)
return (FALSE);
return (TRUE);
}