#include "math_.h"
#include "memory_.h"
#include "stdio_.h"
#include <assert.h>
#include "gconfigv.h"
#include "gdebug.h"
#include "strimpl.h"
#include "siscale.h"
#if USE_FPU <= 0
typedef int PixelWeight;
#  if arch_ints_are_short
typedef long AccumTmp;
#  else
typedef int AccumTmp;
#  endif
#define num_weight_bits\
((sizeof(AccumTmp) - maxSizeofPixel) * 8 - (LOG2_MAX_ISCALE_SUPPORT + 1))
#define numScaleBits  ((maxSizeofPixel - sizeof(PixelTmp)) * 8 )
#define fixedScaleFactor  ((int) (1 << numScaleBits))
#define scale_PixelWeight(factor) ((int)((factor) * (1 << num_weight_bits)))
#define unscale_AccumTmp(atemp, fraction_bits) arith_rshift(atemp, fraction_bits)
#define NEED_FRACTION_BITS
#else
typedef float PixelWeight;
typedef double AccumTmp;
#define num_weight_bits 0
#define fixedScaleFactor 1
#define scale_PixelWeight(factor) (factor)
#define unscale_AccumTmp(atemp, fraction_bits) ((int)(atemp))
#endif
typedef byte PixelTmp;
typedef int PixelTmp2;
#define minPixelTmp 0
#define maxPixelTmp 255
#define unitPixelTmp 255
#define maxSizeofPixel 2
typedef struct {
PixelWeight weight;
} CONTRIB;
typedef struct {
int index;
int n;
int first_pixel;
} CLIST;
typedef struct stream_IScale_state_s {
stream_image_scale_state_common;
int sizeofPixelIn;
int sizeofPixelOut;
double xscale, yscale;
void  *src;
void  *dst;
PixelTmp *tmp;
CLIST *contrib;
CONTRIB *items;
int src_y;
uint src_offset, src_size;
int dst_y;
uint dst_offset, dst_size;
CLIST dst_next_list;
int dst_last_index;
CONTRIB dst_items[MAX_ISCALE_SUPPORT];
} stream_IScale_state;
gs_private_st_ptrs5(st_IScale_state, stream_IScale_state,
"ImageScaleEncode/Decode state",
iscale_state_enum_ptrs, iscale_state_reloc_ptrs,
dst, src, tmp, contrib, items);
#define Mitchell_support 2.0
#define B (1.0 / 3.0)
#define C (1.0 / 3.0)
private double
Mitchell_filter(double t)
{
double t2 = t * t;
if (t < 0)
t = -t;
if (t < 1)
return
((12 - 9 * B - 6 * C) * (t * t2) +
(-18 + 12 * B + 6 * C) * t2 +
(6 - 2 * B)) / 6;
else if (t < 2)
return
((-1 * B - 6 * C) * (t * t2) +
(6 * B + 30 * C) * t2 +
(-12 * B - 48 * C) * t +
(8 * B + 24 * C)) / 6;
else
return 0;
}
#define filter_support Mitchell_support
#define filter_proc Mitchell_filter
#define fproc(t) filter_proc(t)
#define fWidthIn filter_support
#define CLAMP(v, mn, mx)\
(v < mn ? mn : v > mx ? mx : v)
#define min_scale ((fWidthIn * 2) / (MAX_ISCALE_SUPPORT - 1.01))
private int
contrib_pixels(double scale)
{
return (int)(fWidthIn / (scale >= 1.0 ? 1.0 : max(scale, min_scale))
* 2 + 1);
}
private int
calculate_contrib(
CLIST * contrib,
CONTRIB * items,
double scale,
int input_index,
int size,
int limit,
int modulus,
int stride,
double rescale_factor
)
{
double scaled_factor = scale_PixelWeight(rescale_factor);
double WidthIn, fscale;
bool squeeze;
int npixels;
int i, j;
int last_index = -1;
if (scale < 1.0) {
double clamped_scale = max(scale, min_scale);
WidthIn = fWidthIn / clamped_scale;
fscale = 1.0 / clamped_scale;
squeeze = true;
} else {
WidthIn = fWidthIn;
fscale = 1.0;
squeeze = false;
}
npixels = (int)(WidthIn * 2 + 1);
for (i = 0; i < size; ++i) {
double center = (input_index + i) / scale;
int left = (int)ceil(center - WidthIn);
int right = (int)floor(center + WidthIn);
#define clamp_pixel(j)\
(j < 0 ? (-j >= limit ? limit - 1 : -j) :\
j >= limit ? (j >> 1 >= limit ? 0 : (limit - j) + limit - 1) :\
j)
int lmin =
(left < 0 ? 0 : left);
int lmax =
(left < 0 ? (-left >= limit ? limit - 1 : -left) : left);
int rmin =
(right >= limit ?
(right >> 1 >= limit ? 0 : (limit - right) + limit - 1) :
right);
int rmax =
(right >= limit ? limit - 1 : right);
int first_pixel = min(lmin, rmin);
int last_pixel = max(lmax, rmax);
CONTRIB *p;
if (last_pixel > last_index)
last_index = last_pixel;
contrib[i].first_pixel = (first_pixel % modulus) * stride;
contrib[i].n = last_pixel - first_pixel + 1;
contrib[i].index = i * npixels;
p = items + contrib[i].index;
for (j = 0; j < npixels; ++j)
p[j].weight = 0;
if (squeeze) {
for (j = left; j <= right; ++j) {
double weight =
fproc((center - j) / fscale) / fscale;
int n = clamp_pixel(j);
int k = n - first_pixel;
p[k].weight +=
(PixelWeight) (weight * scaled_factor);
}
} else {
for (j = left; j <= right; ++j) {
double weight = fproc(center - j);
int n = clamp_pixel(j);
int k = n - first_pixel;
p[k].weight +=
(PixelWeight) (weight * scaled_factor);
}
}
}
return last_index;
}
private void
zoom_x(PixelTmp * tmp, const void  *src, int sizeofPixelIn,
int tmp_width, int WidthIn, int Colors, const CLIST * contrib,
const CONTRIB * items)
{
int c, i;
#ifdef NEED_FRACTION_BITS
const int fraction_bits =
(sizeofPixelIn - sizeof(PixelTmp)) * 8 + num_weight_bits;
#endif
for (c = 0; c < Colors; ++c) {
PixelTmp *tp = tmp + c;
const CLIST *clp = contrib;
if_debug1('W', "[W]zoom_x color %d:", c);
#define zoom_x_loop(PixelIn, PixelIn2)\
const PixelIn *raster = (const PixelIn *)src + c;\
for ( i = 0; i < tmp_width; tp += Colors, ++clp, ++i )\
{	AccumTmp weight = 0;\
{ int j = clp->n;\
const PixelIn *pp = raster + clp->first_pixel;\
const CONTRIB *cp = items + clp->index;\
switch ( Colors )\
{\
case 1:\
for ( ; j > 0; pp += 1, ++cp, --j )\
weight += *pp * cp->weight;\
break;\
case 3:\
for ( ; j > 0; pp += 3, ++cp, --j )\
weight += *pp * cp->weight;\
break;\
default:\
for ( ; j > 0; pp += Colors, ++cp, --j )\
weight += *pp * cp->weight;\
}\
}\
{ PixelIn2 pixel = unscale_AccumTmp(weight, fraction_bits);\
if_debug1('W', " %ld", (long)pixel);\
*tp =\
(PixelTmp)CLAMP(pixel, minPixelTmp, maxPixelTmp);\
}\
}
if (sizeofPixelIn == 1) {
zoom_x_loop(byte, int)
} else {
#if arch_ints_are_short
zoom_x_loop(bits16, long)
#else
zoom_x_loop(bits16, int)
#endif
}
if_debug0('W', "\n");
}
}
private void
zoom_y(void  *dst, int sizeofPixelOut, uint MaxValueOut,
const PixelTmp * tmp, int WidthOut, int tmp_width,
int Colors, const CLIST * contrib, const CONTRIB * items)
{
int kn = WidthOut * Colors;
int cn = contrib->n;
int first_pixel = contrib->first_pixel;
const CONTRIB *cbp = items + contrib->index;
int kc;
PixelTmp2 max_weight = MaxValueOut;
#ifdef NEED_FRACTION_BITS
const int fraction_bits =
(sizeof(PixelTmp) - sizeofPixelOut) * 8 + num_weight_bits;
#endif
if_debug0('W', "[W]zoom_y: ");
#define zoom_y_loop(PixelOut)\
for ( kc = 0; kc < kn; ++kc ) {\
AccumTmp weight = 0;\
{ const PixelTmp *pp = &tmp[kc + first_pixel];\
int j = cn;\
const CONTRIB *cp = cbp;\
for ( ; j > 0; pp += kn, ++cp, --j )\
weight += *pp * cp->weight;\
}\
{ PixelTmp2 pixel = unscale_AccumTmp(weight, fraction_bits);\
if_debug1('W', " %d", pixel);\
((PixelOut *)dst)[kc] =\
(PixelOut)CLAMP(pixel, 0, max_weight);\
}\
}
if (sizeofPixelOut == 1) {
zoom_y_loop(byte)
} else {
zoom_y_loop(bits16)
}
if_debug0('W', "\n");
}
#define tmp_width params.WidthOut
#define tmp_height params.HeightIn
private void s_IScale_release(stream_state * st);
private void
calculate_dst_contrib(stream_IScale_state * ss, int y)
{
uint row_size = ss->params.WidthOut * ss->params.Colors;
int last_index =
calculate_contrib(&ss->dst_next_list, ss->dst_items, ss->yscale,
y, 1, ss->params.HeightIn, MAX_ISCALE_SUPPORT, row_size,
(double)ss->params.MaxValueOut / (fixedScaleFactor * unitPixelTmp) );
int first_index_mod = ss->dst_next_list.first_pixel / row_size;
ss->dst_last_index = last_index;
last_index %= MAX_ISCALE_SUPPORT;
if (last_index < first_index_mod) {
CONTRIB shuffle[MAX_ISCALE_SUPPORT];
int i;
for (i = 0; i < MAX_ISCALE_SUPPORT; ++i)
shuffle[i].weight =
(i <= last_index ?
ss->dst_items[i + MAX_ISCALE_SUPPORT - first_index_mod].weight :
i >= first_index_mod ?
ss->dst_items[i - first_index_mod].weight :
0);
memcpy(ss->dst_items, shuffle, MAX_ISCALE_SUPPORT * sizeof(CONTRIB));
ss->dst_next_list.n = MAX_ISCALE_SUPPORT;
ss->dst_next_list.first_pixel = 0;
}
#ifdef DEBUG
if (gs_debug_c('w')) {
dprintf1("[w]calc dest contrib for y = %d\n", y);
}
#endif
}
private void
s_IScale_set_defaults(stream_state * st)
{
stream_IScale_state *const ss = (stream_IScale_state *) st;
ss->src = 0;
ss->dst = 0;
ss->tmp = 0;
ss->contrib = 0;
ss->items = 0;
}
private int
s_IScale_init(stream_state * st)
{
stream_IScale_state *const ss = (stream_IScale_state *) st;
gs_memory_t *mem = ss->memory;
ss->sizeofPixelIn = ss->params.BitsPerComponentIn / 8;
ss->sizeofPixelOut = ss->params.BitsPerComponentOut / 8;
ss->xscale = (double)ss->params.WidthOut / (double)ss->params.WidthIn;
ss->yscale = (double)ss->params.HeightOut / (double)ss->params.HeightIn;
ss->src_y = 0;
ss->src_size = ss->params.WidthIn * ss->sizeofPixelIn * ss->params.Colors;
ss->src_offset = 0;
ss->dst_y = 0;
ss->dst_size = ss->params.WidthOut * ss->sizeofPixelOut * ss->params.Colors;
ss->dst_offset = 0;
ss->tmp = (PixelTmp *) gs_alloc_byte_array(mem,
min(ss->tmp_height, MAX_ISCALE_SUPPORT),
ss->tmp_width * ss->params.Colors * sizeof(PixelTmp),
"image_scale tmp");
ss->contrib = (CLIST *) gs_alloc_byte_array(mem,
max(ss->params.WidthOut, ss->params.HeightOut),
sizeof(CLIST), "image_scale contrib");
ss->items = (CONTRIB *) gs_alloc_byte_array(mem,
contrib_pixels(ss->xscale) * ss->params.WidthOut,
sizeof(CONTRIB), "image_scale contrib[*]");
ss->dst = gs_alloc_byte_array(mem, ss->params.WidthOut * ss->params.Colors,
ss->sizeofPixelOut, "image_scale dst");
ss->src = gs_alloc_byte_array(mem, ss->params.WidthIn * ss->params.Colors,
ss->sizeofPixelIn, "image_scale src");
if (ss->tmp == 0 || ss->contrib == 0 || ss->items == 0 ||
ss->dst == 0 || ss->src == 0
) {
s_IScale_release(st);
return ERRC;
}
calculate_contrib(ss->contrib, ss->items, ss->xscale,
0, ss->params.WidthOut, ss->params.WidthIn, ss->params.WidthIn,
ss->params.Colors, (double)unitPixelTmp * fixedScaleFactor / ss->params.MaxValueIn);
calculate_dst_contrib(ss, 0);
return 0;
}
private int
s_IScale_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_IScale_state *const ss = (stream_IScale_state *) st;
top:while (ss->src_y > ss->dst_last_index) {
uint wleft = pw->limit - pw->ptr;
if (ss->dst_y == ss->params.HeightOut)
return EOFC;
if (wleft == 0)
return 1;
if (ss->dst_offset == 0) {
byte *row;
if (wleft >= ss->dst_size) {
row = pw->ptr + 1;
pw->ptr += ss->dst_size;
} else {
row = ss->dst;
}
zoom_y(row, ss->sizeofPixelOut, ss->params.MaxValueOut, ss->tmp,
ss->params.WidthOut, ss->tmp_width, ss->params.Colors,
&ss->dst_next_list, ss->dst_items);
if ((void *)row != ss->dst)
goto adv;
} {
uint wcount = ss->dst_size - ss->dst_offset;
uint ncopy = min(wleft, wcount);
memcpy(pw->ptr + 1, (byte *) ss->dst + ss->dst_offset, ncopy);
pw->ptr += ncopy;
ss->dst_offset += ncopy;
if (ncopy != wcount)
return 1;
ss->dst_offset = 0;
}
adv:++(ss->dst_y);
if (ss->dst_y != ss->params.HeightOut)
calculate_dst_contrib(ss, ss->dst_y);
}
{
uint rleft = pr->limit - pr->ptr;
uint rcount = ss->src_size - ss->src_offset;
if (rleft == 0)
return 0;
#ifdef DEBUG
assert(ss->src_y < ss->params.HeightIn);
#endif
if (rleft >= rcount) {
const byte *row;
if (ss->src_offset == 0) {
row = pr->ptr + 1;
} else {
row = ss->src;
memcpy((byte *) ss->src + ss->src_offset, pr->ptr + 1,
rcount);
ss->src_offset = 0;
}
if_debug2('w', "[w]zoom_x y = %d to tmp row %d\n",
ss->src_y, (ss->src_y % MAX_ISCALE_SUPPORT));
zoom_x(ss->tmp + (ss->src_y % MAX_ISCALE_SUPPORT) *
ss->tmp_width * ss->params.Colors, row,
ss->sizeofPixelIn, ss->tmp_width, ss->params.WidthIn,
ss->params.Colors, ss->contrib, ss->items);
pr->ptr += rcount;
++(ss->src_y);
goto top;
} else {
memcpy((byte *) ss->src + ss->src_offset, pr->ptr + 1, rleft);
ss->src_offset += rleft;
pr->ptr += rleft;
return 0;
}
}
}
private void
s_IScale_release(stream_state * st)
{
stream_IScale_state *const ss = (stream_IScale_state *) st;
gs_memory_t *mem = ss->memory;
gs_free_object(mem, (void *)ss->src, "image_scale src");
ss->src = 0;
gs_free_object(mem, ss->dst, "image_scale dst");
ss->dst = 0;
gs_free_object(mem, ss->items, "image_scale contrib[*]");
ss->items = 0;
gs_free_object(mem, ss->contrib, "image_scale contrib");
ss->contrib = 0;
gs_free_object(mem, ss->tmp, "image_scale tmp");
ss->tmp = 0;
}
const stream_template s_IScale_template = {
&st_IScale_state, s_IScale_init, s_IScale_process, 1, 1,
s_IScale_release, s_IScale_set_defaults
};