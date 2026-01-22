#include "gx.h"
#include "gserrors.h"
#include "gxdevice.h"
#include "gxcindex.h"
#include "vdtrace.h"
int
gx_default_fill_linear_color_scanline(gx_device *dev, const gs_fill_attributes *fa,
int i0, int j, int w,
const frac31 *c0, const int32_t *c0f, const int32_t *cg_num, int32_t cg_den)
{
frac31 c[GX_DEVICE_COLOR_MAX_COMPONENTS];
ulong f[GX_DEVICE_COLOR_MAX_COMPONENTS];
int i, i1 = i0 + w, bi = i0, k;
gx_color_index ci0 = 0, ci1;
const gx_device_color_info *cinfo = &dev->color_info;
int n = cinfo->num_components;
int si, ei, code;
if (j < fixed2int(fa->clip->p.y) ||
j > fixed2int_ceiling(fa->clip->q.y))
return 0;
for (k = 0; k < n; k++) {
int shift = cinfo->comp_shift[k];
int bits = cinfo->comp_bits[k];
c[k] = c0[k];
f[k] = c0f[k];
ci0 |= (gx_color_index)(c[k] >> (sizeof(c[k]) * 8 - 1 - bits)) << shift;
}
for (i = i0 + 1; i < i1; i++) {
ci1 = 0;
for (k = 0; k < n; k++) {
int shift = cinfo->comp_shift[k];
int bits = cinfo->comp_bits[k];
int32_t m = f[k] + cg_num[k];
c[k] += m / cg_den;
m -= m / cg_den * cg_den;
if (m < 0) {
c[k]--;
m += cg_den;
}
f[k] = m;
ci1 |= (gx_color_index)(c[k] >> (sizeof(c[k]) * 8 - 1 - bits)) << shift;
}
if (ci1 != ci0) {
si = max(bi, fixed2int(fa->clip->p.x));
ei = min(i, fixed2int_ceiling(fa->clip->q.x));
if (si < ei) {
if (fa->swap_axes) {
vd_rect(int2fixed(j), int2fixed(si), int2fixed(j + 1), int2fixed(ei), 1, (ulong)ci0);
code = dev_proc(dev, fill_rectangle)(dev, j, si, 1, ei - si, ci0);
} else {
vd_rect(int2fixed(si), int2fixed(j), int2fixed(ei), int2fixed(j + 1), 1, (ulong)ci0);
code = dev_proc(dev, fill_rectangle)(dev, si, j, ei - si, 1, ci0);
}
if (code < 0)
return code;
}
bi = i;
ci0 = ci1;
}
}
si = max(bi, fixed2int(fa->clip->p.x));
ei = min(i, fixed2int_ceiling(fa->clip->q.x));
if (si < ei) {
if (fa->swap_axes) {
vd_rect(int2fixed(j), int2fixed(si), int2fixed(j + 1), int2fixed(ei), 1, (ulong)ci0);
return dev_proc(dev, fill_rectangle)(dev, j, si, 1, ei - si, ci0);
} else {
vd_rect(int2fixed(si), int2fixed(j), int2fixed(ei), int2fixed(j + 1), 1, (ulong)ci0);
return dev_proc(dev, fill_rectangle)(dev, si, j, ei - si, 1, ci0);
}
}
return 0;
}