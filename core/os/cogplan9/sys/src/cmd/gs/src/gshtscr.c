#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gxarith.h"
#include "gzstate.h"
#include "gxdevice.h"
#include "gzht.h"
#include "gswts.h"
static const bool FORCE_STRIP_HALFTONES = false;
private_st_gs_screen_enum();
private
ENUM_PTRS_WITH(screen_enum_enum_ptrs, gs_screen_enum *eptr)
{
if (index < 1 + st_ht_order_max_ptrs) {
gs_ptr_type_t ret =
ENUM_USING(st_ht_order, &eptr->order, sizeof(eptr->order),
index - 1);
if (ret == 0)
ENUM_RETURN(0);
return ret;
}
return ENUM_USING(st_halftone, &eptr->halftone, sizeof(eptr->halftone),
index - (1 + st_ht_order_max_ptrs));
}
ENUM_PTR(0, gs_screen_enum, pgs);
ENUM_PTRS_END
private RELOC_PTRS_WITH(screen_enum_reloc_ptrs, gs_screen_enum *eptr)
{
RELOC_PTR(gs_screen_enum, pgs);
RELOC_USING(st_halftone, &eptr->halftone, sizeof(gs_halftone));
RELOC_USING(st_ht_order, &eptr->order, sizeof(gx_ht_order));
}
RELOC_PTRS_END
private bool screen_accurate_screens;
void
gs_setaccuratescreens(bool accurate)
{
screen_accurate_screens = accurate;
}
bool
gs_currentaccuratescreens(void)
{
return screen_accurate_screens;
}
private bool screen_use_wts;
void
gs_setusewts(bool use_wts)
{
screen_use_wts = use_wts;
}
bool
gs_currentusewts(void)
{
return screen_use_wts;
}
private uint screen_min_screen_levels;
void
gs_setminscreenlevels(uint levels)
{
screen_min_screen_levels = levels;
}
uint
gs_currentminscreenlevels(void)
{
return screen_min_screen_levels;
}
init_proc(gs_gshtscr_init);
int
gs_gshtscr_init(gs_memory_t *mem)
{
gs_setaccuratescreens(false);
gs_setminscreenlevels(1);
return 0;
}
void
gx_compute_cell_values(gx_ht_cell_params_t * phcp)
{
const int M = phcp->M, N = phcp->N, M1 = phcp->M1, N1 = phcp->N1;
const uint m = any_abs(M), n = any_abs(N);
const uint m1 = any_abs(M1), n1 = any_abs(N1);
const ulong C = phcp->C = (ulong)m * m1 + (ulong)n * n1;
const int D = phcp->D = igcd(m1, n);
const int D1 = phcp->D1 = igcd(m, n1);
phcp->W = C / D, phcp->W1 = C / D1;
if (M1 && N) {
int h = 0, k = 0, dy = 0;
int shift;
while (dy != D)
if (dy > D) {
if (M1 > 0)
++k;
else
--k;
dy -= m1;
} else {
if (N > 0)
++h;
else
--h;
dy += n;
}
shift = h * M + k * N1;
phcp->S = imod(-shift, phcp->W);
} else
phcp->S = 0;
if_debug12('h', "[h]MNR=(%d,%d)/%d, M'N'R'=(%d,%d)/%d => C=%lu, D=%d, D'=%d, W=%u, W'=%u, S=%d\n",
M, N, phcp->R, M1, N1, phcp->R1,
C, D, D1, phcp->W, phcp->W1, phcp->S);
}
private int pick_cell_size(gs_screen_halftone * ph,
const gs_matrix * pmat, ulong max_size, uint min_levels, bool accurate,
gx_ht_cell_params_t * phcp);
gs_screen_enum *
gs_screen_enum_alloc(gs_memory_t * mem, client_name_t cname)
{
return gs_alloc_struct(mem, gs_screen_enum, &st_gs_screen_enum, cname);
}
int
gs_screen_init(gs_screen_enum * penum, gs_state * pgs,
gs_screen_halftone * phsp)
{
return gs_screen_init_accurate(penum, pgs, phsp,
screen_accurate_screens);
}
int
gs_screen_init_memory(gs_screen_enum * penum, gs_state * pgs,
gs_screen_halftone * phsp, bool accurate, gs_memory_t * mem)
{
int code =
gs_screen_order_init_memory(&penum->order, pgs, phsp, accurate, mem);
if (code < 0)
return code;
return
gs_screen_enum_init_memory(penum, &penum->order, pgs, phsp, mem);
}
int
gs_screen_order_alloc(gx_ht_order *porder, gs_memory_t *mem)
{
uint num_levels = porder->params.W * porder->params.D;
int code;
if (!FORCE_STRIP_HALFTONES &&
((ulong)porder->params.W1 * bitmap_raster(porder->params.W) +
num_levels * sizeof(*porder->levels) +
porder->params.W * porder->params.W1 * sizeof(gx_ht_bit)) <=
porder->screen_params.max_size) {
code = gx_ht_alloc_order(porder, porder->params.W,
porder->params.W1, 0,
num_levels, mem);
porder->height = porder->orig_height = porder->params.D;
porder->shift = porder->orig_shift = porder->params.S;
} else {
code = gx_ht_alloc_order(porder, porder->params.W,
porder->params.D, porder->params.S,
num_levels, mem);
}
return code;
}
int
gs_screen_order_init_memory(gx_ht_order * porder, const gs_state * pgs,
gs_screen_halftone * phsp, bool accurate,
gs_memory_t * mem)
{
gs_matrix imat;
ulong max_size = max_tile_cache_bytes;
int code;
if (phsp->frequency < 0.1)
return_error(gs_error_rangecheck);
gs_deviceinitialmatrix(gs_currentdevice(pgs), &imat);
code = pick_cell_size(phsp, &imat, max_size,
screen_min_screen_levels, accurate,
&porder->params);
if (code < 0)
return code;
gx_compute_cell_values(&porder->params);
porder->screen_params.matrix = imat;
porder->screen_params.max_size = max_size;
return gs_screen_order_alloc(porder, mem);
}
private int
pick_cell_size(gs_screen_halftone * ph, const gs_matrix * pmat, ulong max_size,
uint min_levels, bool accurate, gx_ht_cell_params_t * phcp)
{
const bool landscape = (pmat->xy != 0.0 || pmat->yx != 0.0);
const bool reflected = pmat->xy * pmat->yx > pmat->xx * pmat->yy;
const int reflection = (reflected ? -1 : 1);
const int rotation =
(landscape ? (pmat->yx < 0 ? 90 : -90) : pmat->xx < 0 ? 180 : 0);
const double f0 = ph->frequency, a0 = ph->angle;
const double T =
fabs((landscape ? pmat->yx / pmat->xy : pmat->xx / pmat->yy));
gs_point uv0;
#define u0 uv0.x
#define v0 uv0.y
int rt = 1;
double f = 0, a = 0;
double e_best = 1000;
bool better;
{
gs_matrix rmat;
gs_make_rotation(a0 * reflection + rotation, &rmat);
gs_distance_transform(72.0 / f0, 0.0, &rmat, &uv0);
gs_distance_transform(u0, v0, pmat, &uv0);
if_debug10('h', "[h]Requested: f=%g a=%g mat=[%g %g %g %g] max_size=%lu min_levels=%u =>\n     u=%g v=%g\n",
ph->frequency, ph->angle,
pmat->xx, pmat->xy, pmat->yx, pmat->yy,
max_size, min_levels, u0, v0);
}
if (u0 == 0 && v0 == 0)
return_error(gs_error_rangecheck);
while ((fabs(u0) + fabs(v0)) * rt < 4)
++rt;
try_size:
better = false;
{
double fm0 = u0 * rt;
double fn0 = v0 * rt;
int m0 = (int)floor(u0 * rt + 0.0001);
int n0 = (int)floor(v0 * rt + 0.0001);
gx_ht_cell_params_t p;
p.R = p.R1 = rt;
for (p.M = m0 + 1; p.M >= m0; p.M--)
for (p.N = n0 + 1; p.N >= n0; p.N--) {
long raster, wt, wt_size;
double fr, ar, ft, at, f_diff, a_diff, f_err, a_err;
p.M1 = (int)floor(p.M / T + 0.5);
p.N1 = (int)floor(p.N * T + 0.5);
gx_compute_cell_values(&p);
if_debug3('h', "[h]trying m=%d, n=%d, r=%d\n", p.M, p.N, rt);
wt = p.W;
if (wt >= max_short)
continue;
raster = bitmap_raster(wt);
if (raster > max_size / p.D || raster > max_long / wt)
continue;
wt_size = raster * wt;
if (landscape)
ar = atan2(p.M * pmat->xy, p.N * pmat->yx),
fr = 72.0 * (p.M == 0 ? pmat->xy / p.N * cos(ar) :
pmat->yx / p.M * sin(ar));
else
ar = atan2(p.N * pmat->xx, p.M * pmat->yy),
fr = 72.0 * (p.M == 0 ? pmat->yy / p.N * sin(ar) :
pmat->xx / p.M * cos(ar));
ft = fabs(fr) * rt;
at = (ar * radians_to_degrees - rotation) * reflection;
at -= floor(at / 180.0) * 180.0;
at += floor(a0 / 180.0) * 180.0;
f_diff = fabs(ft - f0);
a_diff = fabs(at - a0);
f_err = f_diff / fabs(f0);
a_err = a_diff;
if_debug5('h', " ==> d=%d, wt=%ld, wt_size=%ld, f=%g, a=%g\n",
p.D, wt, bitmap_raster(wt) * wt, ft, at);
{
double error =
(fn0 - p.N) * (fn0 - p.N) + (fm0 - p.M) * (fm0 - p.M);
error /= p.N * p.N + p.M * p.M;
error = sqrt(error);
if (error > e_best)
continue;
e_best = error;
}
*phcp = p;
f = ft, a = at;
better = true;
if_debug3('h', "*** best wt_size=%ld, f_diff=%g, a_diff=%g\n",
wt_size, f_diff, a_diff);
if (f_err <= 0.01 && a_err <= 0.9 )
goto done;
}
}
if (phcp->C < min_levels) {
++rt;
goto try_size;
}
if (better) {
if (accurate) {
++rt;
goto try_size;
}
} else {
if (rt == 1)
return_error(gs_error_rangecheck);
}
done:
if_debug5('h', "[h]Chosen: f=%g a=%g M=%d N=%d R=%d\n",
f, a, phcp->M, phcp->N, phcp->R);
ph->actual_frequency = f;
ph->actual_angle = a;
return 0;
#undef u0
#undef v0
}
int
gs_screen_enum_init_memory(gs_screen_enum * penum, const gx_ht_order * porder,
gs_state * pgs, const gs_screen_halftone * phsp,
gs_memory_t * mem)
{
penum->pgs = pgs;
penum->order = *porder;
penum->halftone.rc.memory = mem;
penum->halftone.type = ht_type_screen;
penum->halftone.params.screen = *phsp;
penum->x = penum->y = 0;
if (porder->wse == NULL) {
penum->strip = porder->num_levels / porder->width;
penum->shift = porder->shift;
{
const int M = porder->params.M, N = porder->params.N, R = porder->params.R;
const int M1 = porder->params.M1, N1 = porder->params.N1, R1 = porder->params.R1;
double Q = 2.0 / ((long)M * M1 + (long)N * N1);
penum->mat.xx = Q * (R * M1);
penum->mat.xy = Q * (-R1 * N);
penum->mat.yx = Q * (R * N1);
penum->mat.yy = Q * (R1 * M);
penum->mat.tx = -1.0;
penum->mat.ty = -1.0;
gs_matrix_invert(&penum->mat, &penum->mat_inv);
}
if_debug7('h', "[h]Screen: (%dx%d)/%d [%f %f %f %f]\n",
porder->width, porder->height, porder->params.R,
penum->mat.xx, penum->mat.xy,
penum->mat.yx, penum->mat.yy);
}
return 0;
}
int
gs_screen_currentpoint(gs_screen_enum * penum, gs_point * ppt)
{
gs_point pt;
int code;
double sx, sy;
gs_point spot_center;
if (penum->order.wse) {
int code;
code = gs_wts_screen_enum_currentpoint(penum->order.wse, ppt);
if (code > 0) {
wts_sort_blue(penum->order.wse);
}
return code;
}
if (penum->y >= penum->strip) {
gx_ht_construct_spot_order(&penum->order);
return 1;
}
if ((code = gs_point_transform(penum->x + 0.501, penum->y + 0.498, &penum->mat, &pt)) < 0)
return code;
sx = ceil( pt.x / 2 ) * 2;
sy = ceil( pt.y / 2 ) * 2;
if ((code = gs_point_transform(sx, sy, &penum->mat_inv, &spot_center)) < 0)
return code;
spot_center.x = floor(spot_center.x) + 0.5;
spot_center.y = floor(spot_center.y) + 0.5;
if ((code = gs_distance_transform(penum->x - spot_center.x + 0.501,
penum->y - spot_center.y + 0.498,
&penum->mat, &pt)) < 0)
return code;
pt.x += 1;
pt.y += 1;
if (pt.x < -1.0)
pt.x += ((int)(-ceil(pt.x)) + 1) & ~1;
else if (pt.x >= 1.0)
pt.x -= ((int)pt.x + 1) & ~1;
if (pt.y < -1.0)
pt.y += ((int)(-ceil(pt.y)) + 1) & ~1;
else if (pt.y >= 1.0)
pt.y -= ((int)pt.y + 1) & ~1;
*ppt = pt;
return 0;
}
int
gs_screen_next(gs_screen_enum * penum, floatp value)
{
if (penum->order.wse) {
return gs_wts_screen_enum_next (penum->order.wse, value);
} else {
ht_sample_t sample;
int width = penum->order.width;
gx_ht_bit *bits = (gx_ht_bit *)penum->order.bit_data;
if (value < -1.0 || value > 1.0)
return_error(gs_error_rangecheck);
sample = (long long)(value * max_ht_sample) + max_ht_sample;
#ifdef DEBUG
if (gs_debug_c('H')) {
gs_point pt;
gs_screen_currentpoint(penum, &pt);
dlprintf6("[H]sample x=%d y=%d (%f,%f): %f -> %u\n",
penum->x, penum->y, pt.x, pt.y, value, sample);
}
#endif
bits[penum->y * width + penum->x].mask = sample;
if (++(penum->x) >= width)
penum->x = 0, ++(penum->y);
return 0;
}
}
int
gs_screen_install(gs_screen_enum * penum)
{
gx_device_halftone dev_ht;
int code;
dev_ht.rc.memory = penum->halftone.rc.memory;
dev_ht.order = penum->order;
dev_ht.components = 0;
if ((code = gx_ht_install(penum->pgs, &penum->halftone, &dev_ht)) < 0)
gx_device_halftone_release(&dev_ht, dev_ht.rc.memory);
return code;
}