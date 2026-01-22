#define INCL_DOS
#define INCL_DOSERRORS
#define INCL_WINWINDOWMGR
#define INCL_DEV
#define INCL_GPIBITMAPS
#include <os2.h>
#include "string_.h"
#include <stdlib.h>
#include "gx.h"
#include "gserrors.h"
#include "gxdevice.h"
#include "gp.h"
#include "gpcheck.h"
#include "gsparam.h"
#include "gdevpccm.h"
#include "gxdevmem.h"
#include "gdevpm.h"
#ifdef __DLL__
#include "gsdll.h"
#include "gsdllos2.h"
#endif
#define MIN_COMMIT 4096
#define ID_NAME "GSPMDRV_%u_%u"
#define INITIAL_RESOLUTION 96
#define INITIAL_WIDTH ((INITIAL_RESOLUTION * 85 + 5) / 10)
#define INITIAL_HEIGHT ((INITIAL_RESOLUTION * 110 + 5) / 10)
#define pmdev ((gx_device_pm *)dev)
#define pm_gsview_sizeof 80
typedef struct gx_device_pm_s gx_device_pm;
#define gx_device_pm_common\
int BitsPerPixel;\
int UpdateInterval;\
char GSVIEW[pm_gsview_sizeof];\
BOOL dll;\
int nColors;\
BOOL updating;\
HTIMER update_timer;\
HEV sync_event;\
HEV next_event;\
HMTX bmp_mutex;\
HQUEUE drv_queue;\
HQUEUE term_queue;\
ULONG session_id;\
PID process_id;\
PID gspid;\
unsigned char *bitmap;\
ULONG committed;\
PBITMAPINFO2 bmi
struct gx_device_pm_s {
gx_device_common;
gx_device_pm_common;
gx_device_memory mdev;
};
private dev_proc_open_device(pm_open);
private dev_proc_get_initial_matrix(pm_get_initial_matrix);
private dev_proc_sync_output(pm_sync_output);
private dev_proc_output_page(pm_output_page);
private dev_proc_close_device(pm_close);
private dev_proc_map_rgb_color(pm_map_rgb_color);
private dev_proc_map_color_rgb(pm_map_color_rgb);
private dev_proc_fill_rectangle(pm_fill_rectangle);
private dev_proc_copy_mono(pm_copy_mono);
private dev_proc_copy_color(pm_copy_color);
private dev_proc_get_bits(pm_get_bits);
private dev_proc_get_params(pm_get_params);
private dev_proc_put_params(pm_put_params);
private gx_device_procs pm_procs =
{
pm_open,
pm_get_initial_matrix,
pm_sync_output,
pm_output_page,
pm_close,
pm_map_rgb_color,
pm_map_color_rgb,
pm_fill_rectangle,
NULL,
pm_copy_mono,
pm_copy_color,
NULL,
pm_get_bits,
pm_get_params,
pm_put_params,
NULL,
gx_default_get_xfont_procs,
NULL,
NULL,
gx_page_device_get_page_device
};
#ifdef __DLL__
gx_device_pm far_data gs_os2dll_device =
{
std_device_std_body(gx_device_pm, &pm_procs, "os2dll",
INITIAL_WIDTH, INITIAL_HEIGHT,
INITIAL_RESOLUTION, INITIAL_RESOLUTION),
{0},
24,
5000,
"\0",
1
};
#endif
gx_device_pm far_data gs_os2pm_device =
{
std_device_std_body(gx_device_pm, &pm_procs, "os2pm",
INITIAL_WIDTH, INITIAL_HEIGHT,
INITIAL_RESOLUTION, INITIAL_RESOLUTION),
{0},
24,
5000,
"\0",
0
};
#define pm_color_value(z)\
((((z) >> (gx_color_value_bits - 5)) << 3) +\
((z) >> (gx_color_value_bits - 3)))
private void pm_makepalette(gx_device_pm *);
private void pm_update(gx_device_pm *);
private uint pm_set_bits_per_pixel(gx_device_pm *, int);
private uint pm_palette_size(gx_device_pm *);
private int pm_alloc_bitmap(gx_device_pm *, gx_device *);
private int pm_run_gspmdrv(gx_device_pm *);
private void pm_write_bmp(gx_device_pm *);
int
pm_open(gx_device * dev)
{
int ccode;
CHAR id[128];
CHAR name[128];
PTIB pptib;
PPIB pppib;
if (!pmdev->dll && (_osmode == DOS_MODE)) {
eprintf("os2pm driver can't be used under DOS\n");
return gs_error_limitcheck;
}
if (DosGetInfoBlocks(&pptib, &pppib)) {
eprintf("\npm_open: Couldn't get pid\n");
return gs_error_limitcheck;
}
#ifdef __DLL__
if (pppib->pib_ultype == 3)
pmdev->gspid = pppib->pib_ulpid;
else
#endif
pmdev->gspid = pppib->pib_ulppid;
sprintf(id, ID_NAME, pmdev->gspid, (ULONG) dev);
#ifdef __DLL__
if (pmdev->dll) {
if (DosAllocMem((PPVOID) & pmdev->bitmap,
13 * 1024 * 1024, PAG_READ | PAG_WRITE)) {
eprintf("pm_open: failed allocating BMP memory\n");
return gs_error_limitcheck;
}
} else
#endif
{
sprintf(name, SHARED_NAME, *pmdev->GSVIEW ? pmdev->GSVIEW : id);
if (DosAllocSharedMem((PPVOID) & pmdev->bitmap, name,
13 * 1024 * 1024, PAG_READ | PAG_WRITE)) {
eprintf1("pm_open: failed allocating shared BMP memory %s\n", name);
return gs_error_limitcheck;
}
}
if (DosSetMem(pmdev->bitmap, MIN_COMMIT, PAG_COMMIT | PAG_DEFAULT)) {
DosFreeMem(pmdev->bitmap);
eprintf("pm_open: failed committing BMP memory\n");
return gs_error_limitcheck;
}
pmdev->committed = MIN_COMMIT;
if (pmdev->dll) {
sprintf(name, MUTEX_NAME, id);
if (DosCreateMutexSem(name, &(pmdev->bmp_mutex), 0, FALSE)) {
DosFreeMem(pmdev->bitmap);
DosCloseEventSem(pmdev->sync_event);
DosCloseQueue(pmdev->drv_queue);
eprintf1("pm_open: failed to create mutex semaphore %s\n", name);
return gs_error_limitcheck;
}
} else {
if (*pmdev->GSVIEW) {
APIRET rc;
rc = 0;
if (!rc) {
sprintf(name, NEXT_NAME, pmdev->GSVIEW);
rc = DosOpenEventSem(name, &pmdev->next_event);
}
if (!rc) {
sprintf(name, MUTEX_NAME, pmdev->GSVIEW);
rc = DosOpenMutexSem(name, &pmdev->bmp_mutex);
}
if (!rc) {
PID owner_pid;
sprintf(name, QUEUE_NAME, pmdev->GSVIEW);
rc = DosOpenQueue(&owner_pid, &pmdev->drv_queue, name);
}
if (rc) {
DosFreeMem(pmdev->bitmap);
DosCloseEventSem(pmdev->next_event);
eprintf2("pm_open: failed to open %s, rc = %u\n", name, rc);
return gs_error_limitcheck;
}
} else {
sprintf(name, SYNC_NAME, id);
if (DosCreateEventSem(name, &(pmdev->sync_event), 0, FALSE)) {
DosFreeMem(pmdev->bitmap);
eprintf1("pm_open: failed to create event semaphore %s\n", name);
return gs_error_limitcheck;
}
sprintf(name, MUTEX_NAME, id);
if (DosCreateMutexSem(name, &(pmdev->bmp_mutex), 0, FALSE)) {
DosFreeMem(pmdev->bitmap);
DosCloseEventSem(pmdev->sync_event);
DosCloseQueue(pmdev->drv_queue);
eprintf1("pm_open: failed to create mutex semaphore %s\n", name);
return gs_error_limitcheck;
}
}
}
if ((pm_set_bits_per_pixel(pmdev, pmdev->BitsPerPixel) < 0) ||
(gdev_mem_device_for_bits(dev->color_info.depth) == 0)) {
if (!pmdev->dll) {
if (*pmdev->GSVIEW) {
DosCloseQueue(pmdev->drv_queue);
DosCloseEventSem(pmdev->next_event);
} else
DosCloseEventSem(pmdev->sync_event);
}
DosCloseMutexSem(pmdev->bmp_mutex);
DosFreeMem(pmdev->bitmap);
return gs_error_limitcheck;
}
pmdev->bmi = (PBITMAPINFO2) pmdev->bitmap;
pmdev->bmi->cbFix = 40;
pmdev->bmi->cx = dev->width;
pmdev->bmi->cy = dev->height;
pmdev->bmi->cPlanes = 1;
pmdev->bmi->cBitCount = dev->color_info.depth;
pmdev->bmi->ulCompression = BCA_UNCOMP;
pmdev->bmi->cbImage = 0;
pmdev->bmi->cxResolution = (ULONG) (dev->x_pixels_per_inch / 25.4 * 1000);
pmdev->bmi->cyResolution = (ULONG) (dev->y_pixels_per_inch / 25.4 * 1000);
if (pmdev->BitsPerPixel <= 8) {
pmdev->bmi->cclrUsed = 1 << (pmdev->BitsPerPixel);
pmdev->bmi->cclrImportant = pmdev->nColors;
} else {
pmdev->bmi->cclrUsed = 0;
pmdev->bmi->cclrImportant = 0;
}
pm_makepalette(pmdev);
ccode = pm_alloc_bitmap((gx_device_pm *) dev, dev);
if (ccode < 0) {
if (!pmdev->dll) {
if (*pmdev->GSVIEW) {
DosCloseQueue(pmdev->drv_queue);
DosCloseEventSem(pmdev->next_event);
} else
DosCloseEventSem(pmdev->sync_event);
}
DosCloseMutexSem(pmdev->bmp_mutex);
DosFreeMem(pmdev->bitmap);
return ccode;
}
if (*pmdev->GSVIEW)
return 0;
#ifdef __DLL__
if (pmdev->dll && pgsdll_callback) {
(*pgsdll_callback) (GSDLL_DEVICE, (unsigned char *)pmdev, 1);
return 0;
}
#endif
ccode = pm_run_gspmdrv(pmdev);
if (ccode < 0) {
DosFreeMem(pmdev->bitmap);
DosCloseEventSem(pmdev->sync_event);
DosCloseMutexSem(pmdev->bmp_mutex);
}
return ccode;
}
private void
pm_get_initial_matrix(gx_device * dev, gs_matrix * pmat)
{
pmat->xx = dev->x_pixels_per_inch / 72.0;
pmat->xy = 0;
pmat->yx = 0;
pmat->yy = dev->y_pixels_per_inch / 72.0;
pmat->tx = 0;
pmat->ty = 0;
if (*pmdev->GSVIEW)
pm_update((gx_device_pm *) dev);
}
int
pm_sync_output(gx_device * dev)
{
#ifdef __DLL__
if (pmdev->dll && pgsdll_callback) {
(*pgsdll_callback) (GSDLL_SYNC, (unsigned char *)dev, 0);
return 0;
}
#endif
if (*pmdev->GSVIEW) {
APIRET rc;
rc = DosWriteQueue(pmdev->drv_queue, GS_SYNC, 0, NULL, 0);
if (rc)
eprintf1("pm_sync_output: DosWriteQueue error %d\n", rc);
} else {
if (pmdev->updating)
DosStopTimer(pmdev->update_timer);
DosPostEventSem(pmdev->sync_event);
}
pmdev->updating = FALSE;
return (0);
}
private int
pm_do_output_page(gx_device * dev, int copies, int flush)
{
int code;
APIRET rc;
#ifdef DEBUG
pm_write_bmp(pmdev);
#endif
#ifdef __DLL__
if (pmdev->dll && pgsdll_callback) {
(*pgsdll_callback) (GSDLL_PAGE, (unsigned char *)dev, 0);
return 0;
}
#endif
if (*pmdev->GSVIEW) {
if (copies == -2) {
rc = DosWriteQueue(pmdev->drv_queue, GS_END, 0, NULL, 0);
if (rc)
eprintf1("pm_output_page: DosWriteQueue error %d\n", rc);
} else if (copies == -1) {
rc = DosWriteQueue(pmdev->drv_queue, GS_BEGIN, 0, NULL, 0);
if (rc)
eprintf1("pm_output_page: DosWriteQueue error %d\n", rc);
} else {
ULONG count;
pmdev->updating = FALSE;
rc = DosWriteQueue(pmdev->drv_queue, GS_PAGE, 0, NULL, 0);
if (rc)
eprintf1("pm_output_page: DosWriteQueue error %d\n", rc);
DosWaitEventSem(pmdev->next_event, SEM_INDEFINITE_WAIT);
DosResetEventSem(pmdev->next_event, &count);
}
code = 0;
} else {
code = pm_sync_output(dev);
rc = DosSelectSession(pmdev->session_id);
if (rc) {
DosSleep(2000);
rc = DosSelectSession(pmdev->session_id);
if (rc == ERROR_SMG_NO_TARGET_WINDOW) {
DosSleep(5000);
rc = DosSelectSession(pmdev->session_id);
}
if ((rc == ERROR_SMG_SESSION_NOT_FOUND) ||
(rc == ERROR_SMG_INVALID_SESSION_ID)) {
REQUESTDATA Request;
ULONG DataLength;
PVOID DataAddress;
PULONG QueueEntry;
BYTE ElemPriority;
DosStopSession(STOP_SESSION_SPECIFIED, pmdev->session_id);
Request.pid = pmdev->gspid;
Request.ulData = 0;
DosReadQueue(pmdev->term_queue, &Request, &DataLength,
&DataAddress, 0, DCWW_WAIT, &ElemPriority, (HEV) NULL);
DosCloseQueue(pmdev->term_queue);
pmdev->term_queue = (HQUEUE) 0;
pm_run_gspmdrv(pmdev);
DosSleep(2000);
rc = DosSelectSession(pmdev->session_id);
}
if (rc == ERROR_SMG_SESSION_NOT_FOREGRND)
DosBeep(400, 50);
else if (rc)
eprintf1("pm_output_page: Select Session error code %u\n", rc);
}
}
return code;
}
int
pm_output_page(gx_device * dev, int copies, int flush)
{
int code = pm_do_output_page(dev, copies, flush);
if (code >= 0)
code = gx_finish_output_page(dev, copies, flush);
return code;
}
int
pm_close(gx_device * dev)
{
APIRET rc;
#ifdef __DLL__
if (pmdev->dll) {
DosRequestMutexSem(pmdev->bmp_mutex, 60000);
if (pgsdll_callback)
(*pgsdll_callback) (GSDLL_DEVICE, (unsigned char *)dev, 0);
DosReleaseMutexSem(pmdev->bmp_mutex);
} else
#endif
{
if (*pmdev->GSVIEW) {
rc = DosWriteQueue(pmdev->drv_queue, GS_CLOSE, 0, NULL, 0);
if (rc)
eprintf1("pm_close: DosWriteQueue error %d\n", rc);
} else {
REQUESTDATA Request;
ULONG DataLength;
PVOID DataAddress;
PULONG QueueEntry;
BYTE ElemPriority;
DosStopSession(STOP_SESSION_SPECIFIED, pmdev->session_id);
Request.pid = pmdev->gspid;
Request.ulData = 0;
DosReadQueue(pmdev->term_queue, &Request, &DataLength,
&DataAddress, 0, DCWW_WAIT, &ElemPriority, (HEV) NULL);
DosCloseQueue(pmdev->term_queue);
}
}
DosFreeMem(pmdev->bitmap);
pmdev->bitmap = (unsigned char *)NULL;
pmdev->committed = 0;
if (!pmdev->dll) {
if (*pmdev->GSVIEW) {
DosCloseQueue(pmdev->drv_queue);
DosCloseEventSem(pmdev->next_event);
} else {
DosCloseEventSem(pmdev->sync_event);
if (pmdev->updating)
DosStopTimer(pmdev->update_timer);
pmdev->updating = FALSE;
}
}
DosCloseMutexSem(pmdev->bmp_mutex);
return (0);
}
gx_color_index
pm_map_rgb_color(gx_device * dev, const gx_color_value cv[])
{
gx_color_value r = cv[0];
gx_color_value g = cv[1];
gx_color_value b = cv[2];
switch (dev->color_info.depth) {
case 24:
return ((b >> (gx_color_value_bits - 8)) << 16) +
((g >> (gx_color_value_bits - 8)) << 8) +
((r >> (gx_color_value_bits - 8)));
case 8:{
int i;
RGB2 *prgb;
byte cr, cg, cb;
cr = pm_color_value(r);
cg = pm_color_value(g);
cb = pm_color_value(b);
prgb = (RGB2 *) ((PBYTE) pmdev->bmi + pmdev->bmi->cbFix);
for (i = 0; i < pmdev->nColors; i++, prgb++) {
if (!((cr ^ prgb->bRed) & 0xf8) &&
!((cg ^ prgb->bGreen) & 0xf8) &&
!((cb ^ prgb->bBlue) & 0xf8)
)
return ((gx_color_index) i);
}
if (i < 230) {
prgb->bRed = cr;
prgb->bGreen = cg;
prgb->bBlue = cb;
prgb->fcOptions = 0;
pmdev->nColors = i + 1;
pmdev->bmi->cclrImportant = pmdev->nColors;
if (*pmdev->GSVIEW) {
APIRET rc;
rc = DosWriteQueue(pmdev->drv_queue, GS_PALCHANGE, 0, NULL, 0);
if (rc)
eprintf1("pm_sync_output: DosWriteQueue error %d\n", rc);
}
return ((gx_color_index) i);
}
return (gx_no_color_index);
}
case 4:
return pc_4bit_map_rgb_color(dev, cv);
}
return (gx_default_map_rgb_color(dev, cv));
}
int
pm_map_color_rgb(gx_device * dev, gx_color_index color,
gx_color_value prgb[3])
{
gx_color_value one;
switch (dev->color_info.depth) {
case 24:
one = (gx_color_value) (gx_max_color_value / 255);
prgb[0] = ((color) & 255) * one;
prgb[1] = ((color >> 8) & 255) * one;
prgb[2] = ((color >> 16) & 255) * one;
break;
case 8:
if (!dev->is_open)
return -1;
{
RGB2 *argb = (RGB2 *) ((PBYTE) pmdev->bmi + pmdev->bmi->cbFix);
one = (gx_color_value) (gx_max_color_value / 255);
prgb[0] = argb[(int)color].bRed * one;
prgb[1] = argb[(int)color].bGreen * one;
prgb[2] = argb[(int)color].bBlue * one;
}
break;
case 4:
pc_4bit_map_color_rgb(dev, color, prgb);
break;
default:
prgb[0] = prgb[1] = prgb[2] =
(int)color ? gx_max_color_value : 0;
}
return 0;
}
#define pmmdev ((gx_device *)&pmdev->mdev)
#define pmmproc(proc) (*dev_proc(&pmdev->mdev, proc))
private int
pm_fill_rectangle(gx_device * dev, int x, int y, int w, int h,
gx_color_index color)
{
pmmproc(fill_rectangle) (pmmdev, x, y, w, h, color);
pm_update((gx_device_pm *) dev);
return 0;
}
private int
pm_copy_mono(gx_device * dev,
const byte * base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h,
gx_color_index zero, gx_color_index one)
{
pmmproc(copy_mono) (pmmdev, base, sourcex, raster, id,
x, y, w, h, zero, one);
pm_update((gx_device_pm *) dev);
return 0;
}
private int
pm_copy_color(gx_device * dev,
const byte * base, int sourcex, int raster, gx_bitmap_id id,
int x, int y, int w, int h)
{
pmmproc(copy_color) (pmmdev, base, sourcex, raster, id,
x, y, w, h);
pm_update((gx_device_pm *) dev);
return 0;
}
int
pm_get_bits(gx_device * dev, int y, byte * str, byte ** actual_data)
{
return pmmproc(get_bits) (pmmdev, y, str, actual_data);
}
int
pm_get_params(gx_device * dev, gs_param_list * plist)
{
int code = gx_default_get_params(dev, plist);
gs_param_string gvs;
gvs.data = pmdev->GSVIEW, gvs.size = strlen(gvs.data),
gvs.persistent = false;
code < 0 ||
(code = param_write_int(plist, "UpdateInterval", &pmdev->UpdateInterval)) < 0 ||
(code = param_write_string(plist, "GSVIEW", &gvs)) < 0;
return code;
}
int
pm_put_params(gx_device * dev, gs_param_list * plist)
{
int ecode = 0, code;
bool reopen = false;
bool is_open = dev->is_open;
int width = dev->width;
int height = dev->height;
int old_bpp = dev->color_info.depth;
int bpp = old_bpp;
int uii = pmdev->UpdateInterval;
gs_param_string gsvs;
switch (code = param_read_string(plist, "GSVIEW", &gsvs)) {
case 0:
if (gsvs.size == strlen(pmdev->GSVIEW) &&
!memcmp(pmdev->GSVIEW, gsvs.data, gsvs.size)
) {
gsvs.data = 0;
break;
}
if (dev->is_open)
ecode = gs_error_rangecheck;
else if (gsvs.size >= pm_gsview_sizeof)
ecode = gs_error_limitcheck;
else
break;
goto gsve;
default:
ecode = code;
gsve:param_signal_error(plist, "GSVIEW", ecode);
case 1:
gsvs.data = 0;
break;
}
switch (code = param_read_int(plist, "UpdateInterval", &uii)) {
case 0:
if (uii < 0)
ecode = gs_error_rangecheck;
else
break;
goto uie;
default:
ecode = code;
uie:param_signal_error(plist, "UpdateInterval", ecode);
case 1:
break;
}
switch (code = param_read_int(plist, "BitsPerPixel", &bpp)) {
case 0:
if (dev->is_open && bpp != old_bpp)
ecode = gs_error_rangecheck;
else {
code = pm_set_bits_per_pixel(pmdev, bpp);
if (code < 0)
ecode = code;
else
break;
}
goto bppe;
default:
ecode = code;
bppe:param_signal_error(plist, "BitsPerPixel", ecode);
case 1:
break;
}
if (ecode >= 0) {
dev->is_open = false;
ecode = gx_default_put_params(dev, plist);
dev->is_open = is_open;
}
if (ecode < 0) {
if (bpp != old_bpp)
pm_set_bits_per_pixel(pmdev, old_bpp);
return ecode;
}
if (DosRequestMutexSem(pmdev->bmp_mutex, 20000) == ERROR_TIMEOUT)
eprintf("pm_put_params: mutex timeout\n");
if (is_open && (old_bpp != bpp ||
dev->width != width || dev->height != height)
) {
int ccode;
ccode = pm_alloc_bitmap(pmdev, dev);
if (ccode < 0) {
dev->width = width;
dev->height = height;
pm_set_bits_per_pixel(pmdev, old_bpp);
pm_alloc_bitmap(pmdev, dev);
DosReleaseMutexSem(pmdev->bmp_mutex);
return ccode;
}
reopen = true;
}
pmdev->UpdateInterval = uii;
if (gsvs.data != 0) {
memcpy(pmdev->GSVIEW, gsvs.data, gsvs.size);
pmdev->GSVIEW[gsvs.size] = 0;
}
if (dev->is_open && reopen) {
pmdev->bmi->cx = dev->width;
pmdev->bmi->cy = dev->height;
pmdev->bmi->cBitCount = dev->color_info.depth;
pmdev->bmi->cclrUsed = 1 << (pmdev->BitsPerPixel);
pmdev->bmi->cclrImportant = pmdev->nColors;
pm_makepalette(pmdev);
{
int i;
gx_color_value cv[GX_DEVICE_COLOR_MAX_COMPONENTS];
for (i=0; i<GX_DEVICE_COLOR_MAX_COMPONENTS; i++)
cv[i] = (pmdev->color_info.polarity == GX_CINFO_POLARITY_ADDITIVE)
? gx_max_color_value : 0;
dev_proc(pmdev, fill_rectangle)((gx_device *)pmdev,
0, 0, pmdev->width, pmdev->height,
pmdev->procs.encode_color((gx_device *)pmdev, cv));
}
#ifdef __DLL__
if (pmdev->dll && pgsdll_callback)
(*pgsdll_callback) (GSDLL_SIZE, (unsigned char *)dev,
(dev->width & 0xffff) + ((dev->height & 0xffff) << 16));
#endif
}
DosReleaseMutexSem(pmdev->bmp_mutex);
return 0;
}
#ifdef __DLL__
unsigned long GSDLLAPI
gsdll_get_bitmap(unsigned char *device, unsigned char **pbitmap)
{
gx_device *dev = (gx_device *) device;
*pbitmap = (unsigned char *)(pmdev->bmi);
return 0;
}
int GSDLLAPI
gsdll_lock_device(unsigned char *device, int flag)
{
gx_device *dev = (gx_device *) device;
APIRET rc;
if (flag)
rc = DosRequestMutexSem(pmdev->bmp_mutex, 60000);
else
rc = DosReleaseMutexSem(pmdev->bmp_mutex);
return rc;
}
#endif
#undef pmdev
private int
pm_run_gspmdrv(gx_device_pm * pmdev)
{
int ccode;
PCHAR pdrvname = "gspmdrv.exe";
CHAR error_message[256];
CHAR term_queue_name[128];
CHAR id[128];
CHAR arg[1024];
STARTDATA sdata;
APIRET rc;
PTIB pptib;
PPIB pppib;
CHAR progname[256];
PCHAR tail;
sprintf(id, ID_NAME, pmdev->gspid, (ULONG) pmdev);
sprintf(term_queue_name, "\\QUEUES\\TERMQ_%s", id);
if (DosCreateQueue(&(pmdev->term_queue), QUE_FIFO, term_queue_name)) {
eprintf("pm_run_gspmdrv: failed to create termination queue\n");
return gs_error_limitcheck;
}
if ((rc = DosGetInfoBlocks(&pptib, &pppib)) != 0) {
eprintf1("pm_run_gspmdrv: Couldn't get module handle, rc = %d\n", rc);
return gs_error_limitcheck;
}
if ((rc = DosQueryModuleName(pppib->pib_hmte, sizeof(progname) - 1, progname)) != 0) {
eprintf1("pm_run_gspmdrv: Couldn't get module name, rc = %d\n", rc);
return gs_error_limitcheck;
}
if ((tail = strrchr(progname, '\\')) != (PCHAR) NULL) {
tail++;
*tail = '\0';
} else
tail = progname;
strcat(progname, pdrvname);
sprintf(arg, "-d %s", id);
sdata.Length = sizeof(sdata);
sdata.Related = SSF_RELATED_CHILD;
sdata.FgBg = SSF_FGBG_BACK;
sdata.TraceOpt = 0;
sdata.PgmTitle = "Ghostscript PM driver session";
sdata.PgmName = progname;
sdata.PgmInputs = arg;
sdata.TermQ = term_queue_name;
sdata.Environment = pppib->pib_pchenv;
sdata.InheritOpt = 0;
sdata.SessionType = SSF_TYPE_DEFAULT;
sdata.IconFile = NULL;
sdata.PgmHandle = 0;
sdata.PgmControl = 0;
sdata.InitXPos = 0;
sdata.InitYPos = 0;
sdata.InitXSize = 0;
sdata.InitYSize = 0;
sdata.ObjectBuffer = error_message;
sdata.ObjectBuffLen = sizeof(error_message);
rc = DosStartSession(&sdata, &pmdev->session_id, &pmdev->process_id);
if (rc == ERROR_FILE_NOT_FOUND) {
sdata.PgmName = pdrvname;
rc = DosStartSession(&sdata, &pmdev->session_id, &pmdev->process_id);
}
if (rc) {
eprintf2("pm_run_gspmdrv: failed to run %s, rc = %d\n", sdata.PgmName, rc);
eprintf1("pm_run_gspmdrv: error_message: %s\n", error_message);
return gs_error_limitcheck;
}
return 0;
}
private int
pm_alloc_bitmap(gx_device_pm * pmdev, gx_device * param_dev)
{
gx_device_memory mdev;
byte *base;
ulong data_size;
uint ptr_size;
uint pal_size;
uint raster;
ULONG rc;
ULONG needed;
gs_make_mem_device(&mdev, gdev_mem_device_for_bits(pmdev->color_info.depth), 0, 0, (gx_device *) pmdev);
mdev.width = param_dev->width;
mdev.height = param_dev->height;
raster = gdev_mem_raster(&mdev);
data_size = (ulong) raster *mdev.height;
ptr_size = sizeof(byte **) * mdev.height;
pal_size = pm_palette_size(pmdev);
needed = pmdev->bmi->cbFix + pal_size + data_size + ptr_size;
needed = (needed + MIN_COMMIT - 1) & (~(MIN_COMMIT - 1));
if (needed > pmdev->committed) {
if (rc = DosSetMem(pmdev->bitmap + pmdev->committed,
needed - pmdev->committed,
PAG_COMMIT | PAG_DEFAULT)) {
eprintf1("No memory in pm_alloc_bitmap, rc = %d\n", rc);
return gs_error_limitcheck;
}
pmdev->committed = needed;
}
#ifdef __DLL__
if (pmdev->dll && (needed < pmdev->committed)) {
if (rc = DosSetMem(pmdev->bitmap + needed,
pmdev->committed - needed,
PAG_DECOMMIT)) {
eprintf1("Failed to decommit memory in pm_alloc_bitmap, rc = %d\n", rc);
return gs_error_limitcheck;
}
pmdev->committed = needed;
}
#endif
base = pmdev->bitmap + pmdev->bmi->cbFix + pm_palette_size(pmdev);
pmdev->mdev = mdev;
pmdev->mdev.base = (byte *) base;
pmmproc(open_device) ((gx_device *) & pmdev->mdev);
pmdev->bmi->cbImage = data_size;
return 0;
}
private void
pm_makepalette(gx_device_pm * pmdev)
{
int i, val;
RGB2 *argb = (RGB2 *) ((PBYTE) pmdev->bmi + pmdev->bmi->cbFix);
if (pmdev->BitsPerPixel > 8)
return;
for (i = 0; i < pmdev->nColors; i++) {
switch (pmdev->nColors) {
case 64:
argb[i].bRed = ((i & 0x30) >> 4) * 85;
argb[i].bGreen = ((i & 0xC) >> 2) * 85;
argb[i].bBlue = (i & 3) * 85;
argb[i].fcOptions = 0;
argb[i + 64].bRed = argb[i + 64].bGreen = argb[i + 64].bBlue = 0;
argb[i + 64].fcOptions = 0;
argb[i + 128].bRed = argb[i + 128].bGreen = argb[i + 128].bBlue = 0;
argb[i + 128].fcOptions = 0;
argb[i + 192].bRed = argb[i + 192].bGreen = argb[i + 192].bBlue = 0;
argb[i + 192].fcOptions = 0;
break;
case 16:
val = (i & 8 ? 255 : 128);
argb[i].bRed = i & 4 ? val : 0;
argb[i].bGreen = i & 2 ? val : 0;
argb[i].bBlue = i & 1 ? val : 0;
if (i == 8) {
argb[i].bRed =
argb[i].bGreen =
argb[i].bBlue = 192;
argb[i].fcOptions = 0;
}
break;
case 2:
argb[i].bRed =
argb[i].bGreen =
argb[i].bBlue = (i ? 255 : 0);
argb[i].fcOptions = 0;
break;
}
}
}
private void
pm_update(gx_device_pm * pmdev)
{
if (pmdev->updating)
return;
if (!pmdev->UpdateInterval)
return;
if (*pmdev->GSVIEW) {
APIRET rc;
rc = DosWriteQueue(pmdev->drv_queue, GS_UPDATING, 0, NULL, 0);
if (rc)
eprintf1("pm_update: DosWriteQueue error %d\n", rc);
} else {
DosStartTimer(pmdev->UpdateInterval, (HSEM) pmdev->sync_event,
&pmdev->update_timer);
}
pmdev->updating = TRUE;
}
private void
set_color_info(gx_device_color_info * pdci, int nc, int depth, int maxgray, int maxcolor)
{
pdci->num_components = pdci->max_components = nc;
pdci->depth = depth;
pdci->gray_index = 0;
pdci->max_gray = maxgray;
pdci->max_color = maxcolor;
pdci->dither_grays = maxgray + 1;
pdci->dither_colors = maxcolor + 1;
pdci->separable_and_linear = GX_CINFO_UNKNOWN_SEP_LIN;
switch (nc) {
case 1:
pdci->polarity = GX_CINFO_POLARITY_ADDITIVE;
pdci->cm_name = "DeviceGray";
break;
case 3:
pdci->polarity = GX_CINFO_POLARITY_ADDITIVE;
pdci->cm_name = "DeviceRGB";
break;
case 4:
pdci->polarity = GX_CINFO_POLARITY_SUBTRACTIVE;
pdci->cm_name = "DeviceCMYK";
break;
default:
break;
}
}
private void
set_color_procs(gx_device * pdev,
dev_t_proc_encode_color((*encode_color), gx_device),
dev_t_proc_decode_color((*decode_color), gx_device),
dev_t_proc_get_color_mapping_procs((*get_color_mapping_procs), gx_device),
dev_t_proc_get_color_comp_index((*get_color_comp_index), gx_device))
{
#if 0
pdev->procs.map_rgb_color = encode_color;
pdev->procs.map_color_rgb = decode_color;
#endif
pdev->procs.get_color_mapping_procs = get_color_mapping_procs;
pdev->procs.get_color_comp_index = get_color_comp_index;
pdev->procs.encode_color = encode_color;
pdev->procs.decode_color = decode_color;
}
private void
set_gray_color_procs(gx_device * pdev,
dev_t_proc_encode_color((*encode_color), gx_device),
dev_t_proc_decode_color((*decode_color), gx_device))
{
set_color_procs(pdev, encode_color, decode_color,
gx_default_DevGray_get_color_mapping_procs,
gx_default_DevGray_get_color_comp_index);
}
private void
set_rgb_color_procs(gx_device * pdev,
dev_t_proc_encode_color((*encode_color), gx_device),
dev_t_proc_decode_color((*decode_color), gx_device))
{
set_color_procs(pdev, encode_color, decode_color,
gx_default_DevRGB_get_color_mapping_procs,
gx_default_DevRGB_get_color_comp_index);
}
private uint
pm_set_bits_per_pixel(gx_device_pm * pmdev, int bpp)
{
gx_device * pdev = (gx_device *) pmdev;
gx_device_color_info dci = pmdev->color_info;
switch (bpp) {
case 24:
set_color_info(&dci, 3, bpp, 255, 255);
set_rgb_color_procs(pdev, pm_map_rgb_color, pm_map_color_rgb);
pmdev->nColors = (1 << 24);
break;
case 8:
set_color_info(&dci, 3, 8, 7, 31);
set_rgb_color_procs(pdev, pm_map_rgb_color, pm_map_color_rgb);
pmdev->nColors = 64;
break;
case 4:
set_color_info(&dci, 3, 4, 1, 1);
set_rgb_color_procs(pdev, pm_map_rgb_color, pm_map_color_rgb);
pmdev->nColors = 16;
break;
case 1:
set_color_info(&dci, 1, 1, 1, 0);
set_gray_color_procs(pdev, gx_default_gray_encode,
gx_default_w_b_map_color_rgb);
pmdev->nColors = 2;
break;
default:
return (gs_error_rangecheck);
}
pmdev->BitsPerPixel = bpp;
dci.anti_alias = pmdev->color_info.anti_alias;
pmdev->color_info = dci;
set_linear_color_bits_mask_shift(pdev);
return 0;
}
private uint
pm_palette_size(gx_device_pm * pmdev)
{
switch (pmdev->color_info.depth) {
case 24:
return 0;
case 8:
return 256 * sizeof(RGB2);
case 4:
return 16 * sizeof(RGB2);
}
return 2 * sizeof(RGB2);
}
private void
pm_write_bmp(gx_device_pm * pmdev)
{
BITMAPFILEHEADER2 bmfh;
uint bmfh_length = sizeof(BITMAPFILEHEADER2) - sizeof(BITMAPINFOHEADER2);
uint length;
ULONG fh;
ULONG action;
ULONG count;
bmfh.usType = 0x4d42;
length = pmdev->bmi->cbFix + pm_palette_size(pmdev)
+ ((gdev_mem_raster(&pmdev->mdev) * pmdev->mdev.height));
bmfh.cbSize = bmfh_length + length;
bmfh.xHotspot = bmfh.yHotspot = 0;
bmfh.offBits = bmfh_length + pmdev->bmi->cbFix + pm_palette_size(pmdev);
if (DosOpen("out.bmp",
&fh,
&action,
0,
FILE_NORMAL,
OPEN_ACTION_CREATE_IF_NEW | OPEN_ACTION_REPLACE_IF_EXISTS,
OPEN_ACCESS_WRITEONLY | OPEN_SHARE_DENYREADWRITE,
0)) {
eprintf("error opening out.bmp\n");
return;
}
if (DosWrite(fh, (PBYTE) & bmfh, bmfh_length, &count))
eprintf("error writing header for out.bmp\n");
if (DosWrite(fh, pmdev->bitmap, length, &count))
eprintf("error writing out.bmp\n");
if (DosClose(fh))
eprintf("error closing out.bmp\n");
}