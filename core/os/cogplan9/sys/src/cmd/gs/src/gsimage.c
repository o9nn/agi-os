#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gscspace.h"
#include "gsmatrix.h"
#include "gsimage.h"
#include "gxarith.h"
#include "gxdevice.h"
#include "gxiparam.h"
#include "gxpath.h"
#include "gzstate.h"
typedef struct image_enum_plane_s {
gs_string row;
uint pos;
gs_const_string source;
} image_enum_plane_t;
struct gs_image_enum_s {
gs_memory_t *memory;
gx_device *dev;
gx_image_enum_common_t *info;
int num_planes;
int height;
bool wanted_varies;
int plane_index;
int y;
bool error;
byte wanted[gs_image_max_planes];
byte client_wanted[gs_image_max_planes];
image_enum_plane_t planes[gs_image_max_planes];
gx_image_plane_t image_planes[gs_image_max_planes];
};
gs_private_st_composite(st_gs_image_enum, gs_image_enum, "gs_image_enum",
gs_image_enum_enum_ptrs, gs_image_enum_reloc_ptrs);
#define gs_image_enum_num_ptrs 2
private
ENUM_PTRS_WITH(gs_image_enum_enum_ptrs, gs_image_enum *eptr)
{
index -= gs_image_enum_num_ptrs;
if (index < eptr->num_planes)
ENUM_RETURN_STRING_PTR(gs_image_enum, planes[index].source);
index -= eptr->num_planes;
if (index < eptr->num_planes)
ENUM_RETURN_STRING_PTR(gs_image_enum, planes[index].row);
return 0;
}
ENUM_PTR(0, gs_image_enum, dev);
ENUM_PTR(1, gs_image_enum, info);
ENUM_PTRS_END
private RELOC_PTRS_WITH(gs_image_enum_reloc_ptrs, gs_image_enum *eptr)
{
int i;
RELOC_PTR(gs_image_enum, dev);
RELOC_PTR(gs_image_enum, info);
for (i = 0; i < eptr->num_planes; i++)
RELOC_CONST_STRING_PTR(gs_image_enum, planes[i].source);
for (i = 0; i < eptr->num_planes; i++)
RELOC_STRING_PTR(gs_image_enum, planes[i].row);
}
RELOC_PTRS_END
int
gs_image_begin_typed(const gs_image_common_t * pic, gs_state * pgs,
bool uses_color, gx_image_enum_common_t ** ppie)
{
gx_device *dev = gs_currentdevice(pgs);
gx_clip_path *pcpath;
int code = gx_effective_clip_path(pgs, &pcpath);
if (code < 0)
return code;
if (uses_color) {
gx_set_dev_color(pgs);
code = gs_state_color_load(pgs);
if (code < 0)
return code;
}
return gx_device_begin_typed_image(dev, (const gs_imager_state *)pgs,
NULL, pic, NULL, pgs->dev_color, pcpath, pgs->memory, ppie);
}
private void
image_enum_init(gs_image_enum * penum)
{
penum->info = 0;
penum->dev = 0;
penum->plane_index = 0;
penum->num_planes = 0;
}
gs_image_enum *
gs_image_enum_alloc(gs_memory_t * mem, client_name_t cname)
{
gs_image_enum *penum =
gs_alloc_struct(mem, gs_image_enum, &st_gs_image_enum, cname);
if (penum != 0) {
penum->memory = mem;
image_enum_init(penum);
}
return penum;
}
int
gs_image_init(gs_image_enum * penum, const gs_image_t * pim, bool multi,
gs_state * pgs)
{
gs_image_t image;
gx_image_enum_common_t *pie;
int code;
image = *pim;
if (image.ImageMask) {
image.ColorSpace = NULL;
if (pgs->in_cachedevice <= 1)
image.adjust = false;
} else {
if (pgs->in_cachedevice)
return_error(gs_error_undefined);
if (image.ColorSpace == NULL) {
static gs_color_space cs;
gs_cspace_init_DeviceGray(pgs->memory, &cs);
image.ColorSpace = &cs;
}
}
code = gs_image_begin_typed((const gs_image_common_t *)&image, pgs,
image.ImageMask | image.CombineWithColor,
&pie);
if (code < 0)
return code;
return gs_image_enum_init(penum, pie, (const gs_data_image_t *)&image,
pgs);
}
inline uint
gs_image_bytes_per_plane_row(const gs_image_enum * penum, int plane)
{
const gx_image_enum_common_t *pie = penum->info;
return (pie->plane_widths[plane] * pie->plane_depths[plane] + 7) >> 3;
}
private void
cache_planes(gs_image_enum *penum)
{
int i;
if (penum->wanted_varies) {
penum->wanted_varies =
!gx_image_planes_wanted(penum->info, penum->wanted);
for (i = 0; i < penum->num_planes; ++i)
if (penum->wanted[i])
penum->image_planes[i].raster =
gs_image_bytes_per_plane_row(penum, i);
else
penum->image_planes[i].data = 0;
}
}
private void
next_plane(gs_image_enum *penum)
{
int px = penum->plane_index;
do {
if (++px == penum->num_planes)
px = 0;
} while (!penum->wanted[px]);
penum->plane_index = px;
}
private void
begin_planes(gs_image_enum *penum)
{
cache_planes(penum);
penum->plane_index = -1;
next_plane(penum);
}
static int
gs_image_common_init(gs_image_enum * penum, gx_image_enum_common_t * pie,
const gs_data_image_t * pim, gx_device * dev)
{
int i;
if (pim->Width == 0 || pim->Height == 0) {
gx_image_end(pie, false);
return 1;
}
image_enum_init(penum);
penum->dev = dev;
penum->info = pie;
penum->num_planes = pie->num_planes;
penum->height = pim->Height;
for (i = 0; i < pie->num_planes; ++i) {
penum->planes[i].pos = 0;
penum->planes[i].source.size = 0;
penum->planes[i].row.data = 0;
penum->planes[i].row.size = 0;
penum->image_planes[i].data_x = 0;
}
penum->y = 0;
penum->error = false;
penum->wanted_varies = true;
begin_planes(penum);
return 0;
}
int
gs_image_enum_init(gs_image_enum * penum, gx_image_enum_common_t * pie,
const gs_data_image_t * pim, gs_state *pgs)
{
return gs_image_common_init(penum, pie, pim,
(pgs->in_charpath ? NULL :
gs_currentdevice_inline(pgs)));
}
const byte *
gs_image_planes_wanted(gs_image_enum *penum)
{
int i;
for (i = 0; i < penum->num_planes; ++i)
penum->client_wanted[i] =
(penum->wanted[i] &&
penum->planes[i].pos + penum->planes[i].source.size <
penum->image_planes[i].raster);
return penum->client_wanted;
}
private gs_memory_t *
gs_image_row_memory(const gs_image_enum *penum)
{
return gs_memory_stable(penum->memory);
}
private void
free_row_buffers(gs_image_enum *penum, int num_planes, client_name_t cname)
{
int i;
for (i = num_planes - 1; i >= 0; --i) {
if_debug3('b', "[b]free plane %d row (0x%lx,%u)\n",
i, (ulong)penum->planes[i].row.data,
penum->planes[i].row.size);
gs_free_string(gs_image_row_memory(penum), penum->planes[i].row.data,
penum->planes[i].row.size, cname);
penum->planes[i].row.data = 0;
penum->planes[i].row.size = 0;
}
}
int
gs_image_next(gs_image_enum * penum, const byte * dbytes, uint dsize,
uint * pused)
{
int px = penum->plane_index;
int num_planes = penum->num_planes;
int i, code;
uint used[gs_image_max_planes];
gs_const_string plane_data[gs_image_max_planes];
if (penum->planes[px].source.size != 0)
return_error(gs_error_rangecheck);
for (i = 0; i < num_planes; i++)
plane_data[i].size = 0;
plane_data[px].data = dbytes;
plane_data[px].size = dsize;
penum->error = false;
code = gs_image_next_planes(penum, plane_data, used);
*pused = used[px];
if (code >= 0)
next_plane(penum);
return code;
}
int
gs_image_next_planes(gs_image_enum * penum,
gs_const_string *plane_data ,
uint *used )
{
const int num_planes = penum->num_planes;
int i;
int code = 0;
#ifdef DEBUG
if (gs_debug_c('b')) {
int pi;
for (pi = 0; pi < num_planes; ++pi)
dprintf6("[b]plane %d source=0x%lx,%u pos=%u data=0x%lx,%u\n",
pi, (ulong)penum->planes[pi].source.data,
penum->planes[pi].source.size, penum->planes[pi].pos,
(ulong)plane_data[pi].data, plane_data[pi].size);
}
#endif
for (i = 0; i < num_planes; ++i) {
used[i] = 0;
if (penum->wanted[i] && plane_data[i].size != 0) {
penum->planes[i].source.size = plane_data[i].size;
penum->planes[i].source.data = plane_data[i].data;
}
}
for (;;) {
int h = (penum->wanted_varies ? 1 : max_int);
for (i = 0; i < num_planes; ++i) {
int pos, size;
uint raster;
if (!penum->wanted[i])
continue;
pos = penum->planes[i].pos;
size = penum->planes[i].source.size;
raster = penum->image_planes[i].raster;
if (size > 0) {
if (pos < raster && (pos != 0 || size < raster)) {
int copy = min(size, raster - pos);
uint old_size = penum->planes[i].row.size;
if (raster > old_size) {
gs_memory_t *mem = gs_image_row_memory(penum);
byte *old_data = penum->planes[i].row.data;
byte *row =
(old_data == 0 ?
gs_alloc_string(mem, raster,
"gs_image_next(row)") :
gs_resize_string(mem, old_data, old_size, raster,
"gs_image_next(row)"));
if_debug5('b', "[b]plane %d row (0x%lx,%u) => (0x%lx,%u)\n",
i, (ulong)old_data, old_size,
(ulong)row, raster);
if (row == 0) {
code = gs_note_error(gs_error_VMerror);
free_row_buffers(penum, i, "gs_image_next(row)");
break;
}
penum->planes[i].row.data = row;
penum->planes[i].row.size = raster;
}
memcpy(penum->planes[i].row.data + pos,
penum->planes[i].source.data, copy);
penum->planes[i].source.data += copy;
penum->planes[i].source.size = size -= copy;
penum->planes[i].pos = pos += copy;
used[i] += copy;
}
}
if (h == 0)
continue;
if (pos == raster) {
h = min(h, 1);
penum->image_planes[i].data = penum->planes[i].row.data;
} else if (pos == 0 && size >= raster) {
h = min(h, size / raster);
penum->image_planes[i].data = penum->planes[i].source.data;
} else
h = 0;
}
if (h == 0 || code != 0)
break;
if (penum->dev == 0) {
if (penum->y + h < penum->height)
code = 0;
else
h = penum->height - penum->y, code = 1;
} else {
code = gx_image_plane_data_rows(penum->info, penum->image_planes,
h, &h);
if_debug2('b', "[b]used %d, code=%d\n", h, code);
penum->error = code < 0;
}
penum->y += h;
if (h == 0)
break;
for (i = 0; i < num_planes; ++i) {
int count;
if (!penum->wanted[i])
continue;
count = penum->image_planes[i].raster * h;
if (penum->planes[i].pos) {
penum->planes[i].pos = 0;
} else {
penum->planes[i].source.data += count;
penum->planes[i].source.size -= count;
used[i] += count;
}
}
cache_planes(penum);
if (code > 0)
break;
}
for (i = 0; i < num_planes; ++i)
plane_data[i] = penum->planes[i].source;
return code;
}
int
gs_image_cleanup(gs_image_enum * penum)
{
int code = 0;
free_row_buffers(penum, penum->num_planes, "gs_image_cleanup(row)");
if (penum->info != 0)
code = gx_image_end(penum->info, !penum->error);
return code;
}
int
gs_image_cleanup_and_free_enum(gs_image_enum * penum)
{
int code = gs_image_cleanup(penum);
gs_free_object(penum->memory, penum, "gs_image_cleanup_and_free_enum");
return code;
}