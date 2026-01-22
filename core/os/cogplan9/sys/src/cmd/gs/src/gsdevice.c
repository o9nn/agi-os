#include "ctype_.h"
#include "memory_.h"
#include "string_.h"
#include "gx.h"
#include "gp.h"
#include "gscdefs.h"
#include "gserrors.h"
#include "gsfname.h"
#include "gsstruct.h"
#include "gspath.h"
#include "gspaint.h"
#include "gsmatrix.h"
#include "gscoord.h"
#include "gzstate.h"
#include "gxcmap.h"
#include "gxdevice.h"
#include "gxdevmem.h"
#include "gxiodev.h"
#include "gxcspace.h"
extern_gs_lib_device_list();
void
gx_device_finalize(void *vptr)
{
gx_device * const dev = (gx_device *)vptr;
if (dev->finalize)
dev->finalize(dev);
discard(gs_closedevice(dev));
if (dev->stype_is_dynamic)
gs_free_const_object(dev->memory->non_gc_memory, dev->stype,
"gx_device_finalize");
}
void
gx_device_free_local(gx_device *dev)
{
gx_device_finalize(dev);
}
private
ENUM_PTRS_WITH(device_forward_enum_ptrs, gx_device_forward *fdev) return 0;
case 0: ENUM_RETURN(gx_device_enum_ptr(fdev->target));
ENUM_PTRS_END
private RELOC_PTRS_WITH(device_forward_reloc_ptrs, gx_device_forward *fdev)
{
fdev->target = gx_device_reloc_ptr(fdev->target, gcst);
}
RELOC_PTRS_END
public_st_device();
public_st_device_forward();
public_st_device_null();
gx_device *
gx_device_enum_ptr(gx_device * dev)
{
if (dev == 0 || dev->memory == 0)
return 0;
return dev;
}
gx_device *
gx_device_reloc_ptr(gx_device * dev, gc_state_t * gcst)
{
if (dev == 0 || dev->memory == 0)
return dev;
return RELOC_OBJ(dev);
}
void
gx_device_set_procs(gx_device * dev)
{
if (dev->static_procs != 0) {
dev->procs = *dev->static_procs;
dev->static_procs = 0;
}
}
int
gs_flushpage(gs_state * pgs)
{
gx_device *dev = gs_currentdevice(pgs);
return (*dev_proc(dev, sync_output)) (dev);
}
int
gs_copypage(gs_state * pgs)
{
return gs_output_page(pgs, 1, 0);
}
int
gs_output_page(gs_state * pgs, int num_copies, int flush)
{
gx_device *dev = gs_currentdevice(pgs);
if (dev->IgnoreNumCopies)
num_copies = 1;
return (*dev_proc(dev, output_page)) (dev, num_copies, flush);
}
int
gx_finish_output_page(gx_device *dev, int num_copies, int flush)
{
dev->PageCount += num_copies;
return 0;
}
int
gs_copyscanlines(gx_device * dev, int start_y, byte * data, uint size,
int *plines_copied, uint * pbytes_copied)
{
uint line_size = gx_device_raster(dev, 0);
uint count = size / line_size;
uint i;
byte *dest = data;
for (i = 0; i < count; i++, dest += line_size) {
int code = (*dev_proc(dev, get_bits)) (dev, start_y + i, dest, NULL);
if (code < 0) {
if (start_y + i == dev->height)
break;
return_error(code);
}
}
if (plines_copied != NULL)
*plines_copied = i;
if (pbytes_copied != NULL)
*pbytes_copied = i * line_size;
return 0;
}
gx_device *
gs_currentdevice(const gs_state * pgs)
{
return pgs->device;
}
const char *
gs_devicename(const gx_device * dev)
{
return dev->dname;
}
void
gs_deviceinitialmatrix(gx_device * dev, gs_matrix * pmat)
{
fill_dev_proc(dev, get_initial_matrix, gx_default_get_initial_matrix);
(*dev_proc(dev, get_initial_matrix)) (dev, pmat);
}
const gx_device *
gs_getdevice(int index)
{
const gx_device *const *list;
int count = gs_lib_device_list(&list, NULL);
if (index < 0 || index >= count)
return 0;
return list[index];
}
private void
gx_device_make_struct_type(gs_memory_struct_type_t *st,
const gx_device *dev)
{
const gx_device_procs *procs = dev->static_procs;
if (procs == 0)
procs = &dev->procs;
if (dev->stype)
*st = *dev->stype;
else if (procs->get_xfont_procs == gx_forward_get_xfont_procs)
*st = st_device_forward;
else
*st = st_device;
st->ssize = dev->params_size;
}
int
gs_copydevice2(gx_device ** pnew_dev, const gx_device * dev, bool keep_open,
gs_memory_t * mem)
{
gx_device *new_dev;
const gs_memory_struct_type_t *std = dev->stype;
const gs_memory_struct_type_t *new_std;
gs_memory_struct_type_t *a_std = 0;
int code;
if (dev->stype_is_dynamic) {
a_std = (gs_memory_struct_type_t *)
gs_alloc_bytes_immovable(mem->non_gc_memory, sizeof(*std),
"gs_copydevice(stype)");
if (!a_std)
return_error(gs_error_VMerror);
*a_std = *std;
new_std = a_std;
} else if (std != 0 && std->ssize == dev->params_size) {
new_std = std;
} else {
a_std = (gs_memory_struct_type_t *)
gs_alloc_bytes_immovable(mem->non_gc_memory, sizeof(*std),
"gs_copydevice(stype)");
if (!a_std)
return_error(gs_error_VMerror);
gx_device_make_struct_type(a_std, dev);
new_std = a_std;
}
new_dev = gs_alloc_struct_immovable(mem, gx_device, new_std,
"gs_copydevice(device)");
if (new_dev == 0)
return_error(gs_error_VMerror);
gx_device_init(new_dev, dev, mem, false);
gx_device_set_procs(new_dev);
new_dev->stype = new_std;
new_dev->stype_is_dynamic = new_std != std;
new_dev->is_open = dev->is_open && keep_open;
fill_dev_proc(new_dev, finish_copydevice, gx_default_finish_copydevice);
code = dev_proc(new_dev, finish_copydevice)(new_dev, dev);
if (code < 0) {
gs_free_object(mem, new_dev, "gs_copydevice(device)");
if (a_std)
gs_free_object(dev->memory->non_gc_memory, a_std, "gs_copydevice(stype)");
return code;
}
*pnew_dev = new_dev;
return 0;
}
int
gs_copydevice(gx_device ** pnew_dev, const gx_device * dev, gs_memory_t * mem)
{
return gs_copydevice2(pnew_dev, dev, false, mem);
}
int
gs_opendevice(gx_device *dev)
{
if (dev->is_open)
return 0;
check_device_separable(dev);
gx_device_fill_in_procs(dev);
{
int code = (*dev_proc(dev, open_device))(dev);
if (code < 0)
return_error(code);
dev->is_open = true;
return 1;
}
}
int
gs_imager_putdeviceparams(gs_imager_state *pis, gx_device *dev,
gs_param_list *plist)
{
int code = gs_putdeviceparams(dev, plist);
if (code >= 0)
gx_set_cmap_procs(pis, dev);
return code;
}
private void
gs_state_update_device(gs_state *pgs)
{
gx_set_cmap_procs((gs_imager_state *)pgs, pgs->device);
gx_unset_dev_color(pgs);
}
int
gs_state_putdeviceparams(gs_state *pgs, gs_param_list *plist)
{
int code = gs_putdeviceparams(pgs->device, plist);
if (code >= 0)
gs_state_update_device(pgs);
return code;
}
int
gs_setdevice(gs_state * pgs, gx_device * dev)
{
int code = gs_setdevice_no_erase(pgs, dev);
if (code == 1)
code = gs_erasepage(pgs);
return code;
}
int
gs_setdevice_no_erase(gs_state * pgs, gx_device * dev)
{
int open_code = 0, code;
if (!dev->is_open) {
gx_device_fill_in_procs(dev);
if (gs_device_is_memory(dev)) {
gx_device *odev = gs_currentdevice_inline(pgs);
while (odev != 0 && gs_device_is_memory(odev))
odev = ((gx_device_memory *)odev)->target;
gx_device_set_target(((gx_device_forward *)dev), odev);
}
code = open_code = gs_opendevice(dev);
if (code < 0)
return code;
}
gs_setdevice_no_init(pgs, dev);
pgs->ctm_default_set = false;
if ((code = gs_initmatrix(pgs)) < 0 ||
(code = gs_initclip(pgs)) < 0
)
return code;
pgs->in_cachedevice = 0;
pgs->in_charpath = (gs_char_path_mode) 0;
return open_code;
}
int
gs_setdevice_no_init(gs_state * pgs, gx_device * dev)
{
if (pgs->device != NULL && pgs->device->rc.ref_count == 1 &&
pgs->device != dev) {
int code = gs_closedevice(pgs->device);
if (code < 0)
return code;
}
rc_assign(pgs->device, dev, "gs_setdevice_no_init");
gs_state_update_device(pgs);
return pgs->overprint ? gs_do_set_overprint(pgs) : 0;
}
void
gx_device_init(gx_device * dev, const gx_device * proto, gs_memory_t * mem,
bool internal)
{
memcpy(dev, proto, proto->params_size);
dev->memory = mem;
dev->retained = !internal;
rc_init(dev, mem, (internal ? 0 : 1));
}
void
gs_make_null_device(gx_device_null *dev_null, gx_device *dev,
gs_memory_t * mem)
{
gx_device_init((gx_device *)dev_null, (const gx_device *)&gs_null_device,
mem, true);
gx_device_set_target((gx_device_forward *)dev_null, dev);
if (dev) {
gx_device *dn = (gx_device *)dev_null;
set_dev_proc(dn, get_color_mapping_procs, gx_forward_get_color_mapping_procs);
set_dev_proc(dn, get_color_comp_index, gx_forward_get_color_comp_index);
set_dev_proc(dn, encode_color, gx_forward_encode_color);
set_dev_proc(dn, decode_color, gx_forward_decode_color);
gx_device_copy_color_params(dn, dev);
}
}
bool gs_is_null_device(gx_device *dev)
{
return dev->procs.fill_path == gs_null_device.procs.fill_path;
}
void
gx_device_retain(gx_device *dev, bool retained)
{
int delta = (int)retained - (int)dev->retained;
if (delta) {
dev->retained = retained;
rc_adjust_only(dev, delta, "gx_device_retain");
}
}
int
gs_nulldevice(gs_state * pgs)
{
if (pgs->device == 0 || !gx_device_is_null(pgs->device)) {
gx_device *ndev;
int code = gs_copydevice(&ndev, (const gx_device *)&gs_null_device,
pgs->memory);
if (code < 0)
return code;
rc_init(ndev, pgs->memory, 0);
return gs_setdevice_no_erase(pgs, ndev);
}
return 0;
}
int
gs_closedevice(gx_device * dev)
{
int code = 0;
if (dev->is_open) {
code = (*dev_proc(dev, close_device))(dev);
dev->is_open = false;
if (code < 0)
return_error(code);
}
return code;
}
void
gx_set_device_only(gs_state * pgs, gx_device * dev)
{
rc_assign(pgs->device, dev, "gx_set_device_only");
}
uint
gx_device_raster(const gx_device * dev, bool pad)
{
ulong bits = (ulong) dev->width * dev->color_info.depth;
return (pad ? bitmap_raster(bits) : (uint) ((bits + 7) >> 3));
}
int
gx_device_adjust_resolution(gx_device * dev,
int actual_width, int actual_height, int fit)
{
double width_ratio = (double)actual_width / dev->width;
double height_ratio = (double)actual_height / dev->height;
double ratio =
(fit ? min(width_ratio, height_ratio) :
max(width_ratio, height_ratio));
dev->HWResolution[0] *= ratio;
dev->HWResolution[1] *= ratio;
gx_device_set_width_height(dev, actual_width, actual_height);
return 0;
}
void
gx_device_set_margins(gx_device * dev, const float *margins  ,
bool move_origin)
{
int i;
for (i = 0; i < 4; ++i)
dev->HWMargins[i] = margins[i] * 72.0;
if (move_origin) {
dev->Margins[0] = -margins[0] * dev->MarginsHWResolution[0];
dev->Margins[1] = -margins[3] * dev->MarginsHWResolution[1];
}
}
private void
gx_device_TrayOrientationRotate(gx_device *dev)
{
if ( dev->TrayOrientation == 90 || dev->TrayOrientation == 270) {
int tmp = dev->height;
dev->height = dev->width;
dev->width = tmp;
}
}
void
gx_device_set_width_height(gx_device * dev, int width, int height)
{
dev->width = width;
dev->height = height;
dev->MediaSize[0] = width * 72.0 / dev->HWResolution[0];
dev->MediaSize[1] = height * 72.0 / dev->HWResolution[1];
gx_device_TrayOrientationRotate(dev);
}
void
gx_device_set_resolution(gx_device * dev, floatp x_dpi, floatp y_dpi)
{
dev->HWResolution[0] = x_dpi;
dev->HWResolution[1] = y_dpi;
dev->width = (int)(dev->MediaSize[0] * x_dpi / 72.0 + 0.5);
dev->height = (int)(dev->MediaSize[1] * y_dpi / 72.0 + 0.5);
gx_device_TrayOrientationRotate(dev);
}
void
gx_device_set_media_size(gx_device * dev, floatp media_width, floatp media_height)
{
dev->MediaSize[0] = media_width;
dev->MediaSize[1] = media_height;
dev->width = (int)(media_width * dev->HWResolution[0] / 72.0 + 0.499);
dev->height = (int)(media_height * dev->HWResolution[1] / 72.0 + 0.499);
gx_device_TrayOrientationRotate(dev);
}
void
gx_device_copy_color_procs(gx_device *dev, const gx_device *target)
{
dev_proc_map_cmyk_color((*from_cmyk)) =
dev_proc(dev, map_cmyk_color);
dev_proc_map_rgb_color((*from_rgb)) =
dev_proc(dev, map_rgb_color);
dev_proc_map_color_rgb((*to_rgb)) =
dev_proc(dev, map_color_rgb);
if (from_cmyk == gx_forward_map_cmyk_color ||
from_cmyk == cmyk_1bit_map_cmyk_color ||
from_cmyk == cmyk_8bit_map_cmyk_color) {
from_cmyk = dev_proc(target, map_cmyk_color);
set_dev_proc(dev, map_cmyk_color,
(from_cmyk == cmyk_1bit_map_cmyk_color ||
from_cmyk == cmyk_8bit_map_cmyk_color ?
from_cmyk : gx_forward_map_cmyk_color));
}
if (from_rgb == gx_forward_map_rgb_color ||
from_rgb == gx_default_rgb_map_rgb_color) {
from_rgb = dev_proc(target, map_rgb_color);
set_dev_proc(dev, map_rgb_color,
(from_rgb == gx_default_rgb_map_rgb_color ?
from_rgb : gx_forward_map_rgb_color));
}
if (to_rgb == gx_forward_map_color_rgb ||
to_rgb == cmyk_1bit_map_color_rgb ||
to_rgb == cmyk_8bit_map_color_rgb) {
to_rgb = dev_proc(target, map_color_rgb);
set_dev_proc(dev, map_color_rgb,
(to_rgb == cmyk_1bit_map_color_rgb ||
to_rgb == cmyk_8bit_map_color_rgb ?
to_rgb : gx_forward_map_color_rgb));
}
}
#define COPY_PARAM(p) dev->p = target->p
void
gx_device_copy_color_params(gx_device *dev, const gx_device *target)
{
COPY_PARAM(color_info);
COPY_PARAM(cached_colors);
gx_device_copy_color_procs(dev, target);
}
void
gx_device_copy_params(gx_device *dev, const gx_device *target)
{
#define COPY_ARRAY_PARAM(p) memcpy(dev->p, target->p, sizeof(dev->p))
COPY_PARAM(width);
COPY_PARAM(height);
COPY_ARRAY_PARAM(MediaSize);
COPY_ARRAY_PARAM(ImagingBBox);
COPY_PARAM(ImagingBBox_set);
COPY_ARRAY_PARAM(HWResolution);
COPY_ARRAY_PARAM(MarginsHWResolution);
COPY_ARRAY_PARAM(Margins);
COPY_ARRAY_PARAM(HWMargins);
COPY_PARAM(PageCount);
#undef COPY_ARRAY_PARAM
gx_device_copy_color_params(dev, target);
}
#undef COPY_PARAM
private int
gx_parse_output_format(gs_parsed_file_name_t *pfn, const char **pfmt)
{
bool have_format = false, field = 0;
int width[2], int_width = sizeof(int) * 3, w = 0;
uint i;
width[0] = width[1] = 0;
for (i = 0; i < pfn->len; ++i)
if (pfn->fname[i] == '%') {
if (i + 1 < pfn->len && pfn->fname[i + 1] == '%')
continue;
if (have_format)
return_error(gs_error_undefinedfilename);
have_format = true;
sw:
if (++i == pfn->len)
return_error(gs_error_undefinedfilename);
switch (pfn->fname[i]) {
case 'l':
int_width = sizeof(long) * 3;
case ' ': case '#': case '+': case '-':
goto sw;
case '.':
if (field)
return_error(gs_error_undefinedfilename);
field = 1;
continue;
case '0': case '1': case '2': case '3': case '4':
case '5': case '6': case '7': case '8': case '9':
width[field] = width[field] * 10 + pfn->fname[i] - '0';
goto sw;
case 'd': case 'i': case 'u': case 'o': case 'x': case 'X':
*pfmt = &pfn->fname[i];
continue;
default:
return_error(gs_error_undefinedfilename);
}
}
if (have_format) {
w = max(width[0], width[1]);
w = max(w, int_width) + 5;
}
return w;
}
int
gx_parse_output_file_name(gs_parsed_file_name_t *pfn, const char **pfmt,
const char *fname, uint fnlen)
{
int code;
*pfmt = 0;
pfn->memory = 0;
pfn->iodev = NULL;
pfn->fname = NULL;
pfn->len = 0;
if (fnlen == 0)
return 0;
code = gs_parse_file_name(pfn, fname, fnlen);
if (code < 0) {
if (fname[0] == '%') {
pfn->len = fnlen;
pfn->fname = fname;
code = gx_parse_output_format(pfn, pfmt);
}
if (code < 0)
return code;
}
if (!pfn->iodev) {
if ( (pfn->len == 1) && (pfn->fname[0] == '-') ) {
pfn->iodev = gs_findiodevice((const byte *)"%stdout", 7);
pfn->fname = NULL;
} else if (pfn->fname[0] == '|') {
pfn->iodev = gs_findiodevice((const byte *)"%pipe", 5);
pfn->fname++, pfn->len--;
} else
pfn->iodev = iodev_default;
if (!pfn->iodev)
return_error(gs_error_undefinedfilename);
}
if (!pfn->fname)
return 0;
code = gx_parse_output_format(pfn, pfmt);
if (code < 0)
return code;
if (strlen(pfn->iodev->dname) + pfn->len + code >= gp_file_name_sizeof)
return_error(gs_error_undefinedfilename);
return 0;
}
int
gx_device_open_output_file(const gx_device * dev, char *fname,
bool binary, bool positionable, FILE ** pfile)
{
gs_parsed_file_name_t parsed;
const char *fmt;
char pfname[gp_file_name_sizeof];
int code = gx_parse_output_file_name(&parsed, &fmt, fname, strlen(fname));
if (code < 0)
return code;
if (parsed.iodev && !strcmp(parsed.iodev->dname, "%stdout%")) {
if (parsed.fname)
return_error(gs_error_undefinedfilename);
*pfile = dev->memory->gs_lib_ctx->fstdout;
return gp_setmode_binary(*pfile, true);
}
if (fmt) {
long count1 = dev->PageCount + 1;
while (*fmt != 'l' && *fmt != '%')
--fmt;
if (*fmt == 'l')
sprintf(pfname, parsed.fname, count1);
else
sprintf(pfname, parsed.fname, (int)count1);
parsed.fname = pfname;
parsed.len = strlen(parsed.fname);
}
if (positionable || (parsed.iodev && parsed.iodev != iodev_default)) {
char fmode[4];
if (!parsed.fname)
return_error(gs_error_undefinedfilename);
strcpy(fmode, gp_fmode_wb);
if (positionable)
strcat(fmode, "+");
code = parsed.iodev->procs.fopen(parsed.iodev, parsed.fname, fmode,
pfile, NULL, 0);
if (code)
eprintf1("**** Could not open the file %s .\n", parsed.fname);
return code;
}
*pfile = gp_open_printer((fmt ? pfname : fname), binary);
if (*pfile)
return 0;
eprintf1("**** Could not open the file %s .\n", (fmt ? pfname : fname));
return_error(gs_error_invalidfileaccess);
}
int
gx_device_close_output_file(const gx_device * dev, const char *fname,
FILE *file)
{
gs_parsed_file_name_t parsed;
const char *fmt;
int code = gx_parse_output_file_name(&parsed, &fmt, fname, strlen(fname));
if (code < 0)
return code;
if (parsed.iodev) {
if (!strcmp(parsed.iodev->dname, "%stdout%"))
return 0;
if (parsed.iodev != iodev_default)
return parsed.iodev->procs.fclose(parsed.iodev, file);
}
gp_close_printer(file, (parsed.fname ? parsed.fname : fname));
return 0;
}