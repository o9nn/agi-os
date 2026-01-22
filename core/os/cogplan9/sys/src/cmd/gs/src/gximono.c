#include "gx.h"
#include "memory_.h"
#include "gpcheck.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gxmatrix.h"
#include "gsccolor.h"
#include "gspaint.h"
#include "gsutil.h"
#include "gxdevice.h"
#include "gxcmap.h"
#include "gxdcolor.h"
#include "gxistate.h"
#include "gxdevmem.h"
#include "gdevmem.h"
#include "gxcpath.h"
#include "gximage.h"
#include "gzht.h"
iclass_proc(gs_image_class_3_mono);
private irender_proc(image_render_mono);
irender_proc_t
gs_image_class_3_mono(gx_image_enum * penum)
{
if (penum->spp == 1) {
penum->slow_loop =
(penum->masked && !color_is_pure(&penum->icolor1)) ||
penum->use_rop;
if (!(penum->slow_loop || penum->posture != image_portrait))
penum->clip_image &= ~(image_clip_xmin | image_clip_xmax);
if_debug0('b', "[b]render=mono\n");
penum->dxx =
float2fixed(penum->matrix.xx + fixed2float(fixed_epsilon) / 2);
if (penum->use_mask_color) {
gx_image_scale_mask_colors(penum, 0);
if (penum->mask_color.values[0] <= 0)
color_set_null(&penum->icolor0);
if (penum->mask_color.values[1] >= 255)
color_set_null(&penum->icolor1);
}
return &image_render_mono;
}
return 0;
}
private int
image_render_mono(gx_image_enum * penum, const byte * buffer, int data_x,
uint w, int h, gx_device * dev)
{
const gs_imager_state *pis = penum->pis;
gs_logical_operation_t lop = penum->log_op;
const bool masked = penum->masked;
const gs_color_space *pcs = NULL;
cs_proc_remap_color((*remap_color)) = NULL;
gs_client_color cc;
gx_device_color *pdevc = &penum->icolor1;
bool tiles_fit;
uint mask_base = penum->mask_color.values[0];
uint mask_limit =
(penum->use_mask_color ?
penum->mask_color.values[1] - mask_base + 1 : 0);
#define IMAGE_SET_GRAY(sample_value)\
BEGIN\
pdevc = &penum->clues[sample_value].dev_color;\
if (!color_is_set(pdevc)) {\
if ((uint)(sample_value - mask_base) < mask_limit)\
color_set_null(pdevc);\
else {\
decode_sample(sample_value, cc, 0);\
code = (*remap_color)(&cc, pcs, pdevc, pis, dev, gs_color_select_source);\
if (code < 0)\
goto err;\
}\
} else if (!color_is_pure(pdevc)) {\
if (!tiles_fit) {\
code = gx_color_load_select(pdevc, pis, dev, gs_color_select_source);\
if (code < 0)\
goto err;\
}\
}\
END
gx_dda_fixed_point next;
gx_dda_step_fixed dxx2, dxx3, dxx4;
const byte *psrc_initial = buffer + data_x;
const byte *psrc = psrc_initial;
const byte *rsrc = psrc;
const byte *endp = psrc + w;
const byte *stop = endp;
fixed xrun;
byte run;
int htrun = (masked ? 255 : -2);
int code = 0;
if (h == 0)
return 0;
if (pis == 0 || !gx_check_tile_cache_current(pis)) {
image_init_clues(penum, penum->bps, penum->spp);
}
tiles_fit = (pis && penum->device_color ? gx_check_tile_cache(pis) : false);
next = penum->dda.pixel0;
xrun = dda_current(next.x);
if (!masked) {
pcs = penum->pcs;
remap_color = pcs->type->remap_color;
}
run = *psrc;
{
byte last = stop[-1];
while (stop > psrc && stop[-1] == last)
--stop;
}
if (penum->slow_loop || penum->posture != image_portrait) {
fixed yrun;
const fixed pdyx = dda_current(penum->dda.row.x) - penum->cur.x;
const fixed pdyy = dda_current(penum->dda.row.y) - penum->cur.y;
dev_proc_fill_parallelogram((*fill_pgram)) =
dev_proc(dev, fill_parallelogram);
#define xl dda_current(next.x)
#define ytf dda_current(next.y)
yrun = ytf;
if (masked) {
pdevc = &penum->icolor1;
code = gx_color_load(pdevc, pis, dev);
if (code < 0)
return code;
if (stop <= psrc)
goto last;
if (penum->posture == image_portrait) {
fixed ax =
(penum->matrix.xx < 0 ? -penum->adjust : penum->adjust);
fixed ay =
(pdyy < 0 ? -penum->adjust : penum->adjust);
fixed dyy = pdyy + (ay << 1);
yrun -= ay;
dda_translate(next.x, -ax);
ax <<= 1;
dxx2 = next.x.step;
dda_step_add(dxx2, next.x.step);
dxx3 = dxx2;
dda_step_add(dxx3, next.x.step);
dxx4 = dxx3;
dda_step_add(dxx4, next.x.step);
for (;;) {
while (!psrc[0])
if (!psrc[1]) {
if (!psrc[2]) {
if (!psrc[3]) {
psrc += 4;
dda_state_next(next.x.state, dxx4);
continue;
}
psrc += 3;
dda_state_next(next.x.state, dxx3);
break;
}
psrc += 2;
dda_state_next(next.x.state, dxx2);
break;
} else {
++psrc;
dda_next(next.x);
break;
}
xrun = xl;
if (psrc >= stop)
break;
for (; *psrc; ++psrc)
dda_next(next.x);
code = (*fill_pgram)(dev, xrun, yrun,
xl - xrun + ax, fixed_0, fixed_0, dyy,
pdevc, lop);
if (code < 0)
goto err;
rsrc = psrc;
if (psrc >= stop)
break;
}
} else if (penum->posture == image_landscape) {
fixed ax =
(pdyx < 0 ? -penum->adjust : penum->adjust);
fixed dyx = pdyx + (ax << 1);
fixed ay =
(penum->matrix.xy < 0 ? -penum->adjust : penum->adjust);
xrun -= ax;
dda_translate(next.y, -ay);
ay <<= 1;
for (;;) {
for (; !*psrc; ++psrc)
dda_next(next.y);
yrun = ytf;
if (psrc >= stop)
break;
for (; *psrc; ++psrc)
dda_next(next.y);
code = (*fill_pgram)(dev, xrun, yrun, fixed_0,
ytf - yrun + ay, dyx, fixed_0,
pdevc, lop);
if (code < 0)
goto err;
rsrc = psrc;
if (psrc >= stop)
break;
}
} else {
for (;;) {
for (; !*psrc; ++psrc) {
dda_next(next.x);
dda_next(next.y);
}
yrun = ytf;
xrun = xl;
if (psrc >= stop)
break;
for (; *psrc; ++psrc) {
dda_next(next.x);
dda_next(next.y);
}
code = (*fill_pgram)(dev, xrun, yrun, xl - xrun,
ytf - yrun, pdyx, pdyy, pdevc, lop);
if (code < 0)
goto err;
rsrc = psrc;
if (psrc >= stop)
break;
}
}
} else if (penum->posture == image_portrait ||
penum->posture == image_landscape
) {
if (stop <= psrc)
goto last;
for (;;) {
if (*psrc != run) {
if (run != htrun) {
htrun = run;
IMAGE_SET_GRAY(run);
}
code = (*fill_pgram)(dev, xrun, yrun, xl - xrun,
ytf - yrun, pdyx, pdyy,
pdevc, lop);
if (code < 0)
goto err;
yrun = ytf;
xrun = xl;
rsrc = psrc;
if (psrc >= stop)
break;
run = *psrc;
}
psrc++;
dda_next(next.x);
dda_next(next.y);
}
} else {
stop = endp;
for (;;) {
if (run != htrun) {
htrun = run;
IMAGE_SET_GRAY(run);
}
code = (*fill_pgram) (dev, xrun, yrun, xl - xrun,
ytf - yrun, pdyx, pdyy, pdevc, lop);
if (code < 0)
goto err;
yrun = ytf;
xrun = xl;
rsrc = psrc;
if (psrc >= stop)
break;
run = *psrc++;
dda_next(next.x);
dda_next(next.y);
}
}
last:if (stop < endp && (*stop || !masked)) {
if (!masked) {
IMAGE_SET_GRAY(*stop);
}
dda_advance(next.x, endp - stop);
dda_advance(next.y, endp - stop);
code = (*fill_pgram) (dev, xrun, yrun, xl - xrun,
ytf - yrun, pdyx, pdyy, pdevc, lop);
}
#undef xl
#undef ytf
} else {
const fixed adjust = penum->adjust;
const fixed dxx = penum->dxx;
fixed xa = (dxx >= 0 ? adjust : -adjust);
const int yt = penum->yci, iht = penum->hci;
dev_proc_fill_rectangle((*fill_proc)) =
dev_proc(dev, fill_rectangle);
int xmin = fixed2int_pixround(penum->clip_outer.p.x);
int xmax = fixed2int_pixround(penum->clip_outer.q.x);
#define xl dda_current(next.x)
xrun = xrun - xa + (fixed_half - fixed_epsilon);
dda_translate(next.x, xa + (fixed_half - fixed_epsilon));
xa <<= 1;
dxx2 = next.x.step;
dda_step_add(dxx2, next.x.step);
dxx3 = dxx2;
dda_step_add(dxx3, next.x.step);
dxx4 = dxx3;
dda_step_add(dxx4, next.x.step);
if (stop > psrc)
for (;;) {
skf:if (psrc[0] == run) {
if (psrc[1] == run) {
if (psrc[2] == run) {
if (psrc[3] == run) {
psrc += 4;
dda_state_next(next.x.state, dxx4);
goto skf;
} else {
psrc += 4;
dda_state_next(next.x.state, dxx3);
}
} else {
psrc += 3;
dda_state_next(next.x.state, dxx2);
}
} else {
psrc += 2;
dda_next(next.x);
}
} else
psrc++;
{
int xi = fixed2int_var(xrun);
int wi = fixed2int_var(xl) - xi;
int xei;
if (wi <= 0) {
if (wi == 0)
goto mt;
xi += wi, wi = -wi;
}
if ((xei = xi + wi) > xmax || xi < xmin) {
if (xi < xmin)
wi -= xmin - xi, xi = xmin;
if (xei > xmax)
wi -= xei - xmax;
if (wi <= 0)
goto mt;
}
switch (run) {
case 0:
if (masked)
goto mt;
if (!color_is_pure(&penum->icolor0))
goto ht;
code = (*fill_proc) (dev, xi, yt, wi, iht,
penum->icolor0.colors.pure);
break;
case 255:
if (!color_is_pure(&penum->icolor1))
goto ht;
code = (*fill_proc) (dev, xi, yt, wi, iht,
penum->icolor1.colors.pure);
break;
default:
ht:
if (run != htrun) {
IMAGE_SET_GRAY(run);
htrun = run;
}
code = gx_fill_rectangle_device_rop(xi, yt, wi, iht,
pdevc, dev, lop);
}
if (code < 0)
goto err;
mt:xrun = xl - xa;
rsrc = psrc - 1;
if (psrc > stop) {
--psrc;
break;
}
run = psrc[-1];
}
dda_next(next.x);
}
if (*stop != 0 || !masked) {
int xi = fixed2int_var(xrun);
int wi, xei;
dda_advance(next.x, endp - stop);
wi = fixed2int_var(xl) - xi;
if (wi <= 0) {
if (wi == 0)
goto lmt;
xi += wi, wi = -wi;
}
if ((xei = xi + wi) > xmax || xi < xmin) {
if (xi < xmin)
wi -= xmin - xi, xi = xmin;
if (xei > xmax)
wi -= xei - xmax;
if (wi <= 0)
goto lmt;
}
IMAGE_SET_GRAY(*stop);
code = gx_fill_rectangle_device_rop(xi, yt, wi, iht,
pdevc, dev, lop);
lmt:;
}
}
#undef xl
if (code >= 0)
return 1;
err:
penum->used.x = rsrc - psrc_initial;
penum->used.y = 0;
return code;
}