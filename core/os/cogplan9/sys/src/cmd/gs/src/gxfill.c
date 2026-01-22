#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gxfixed.h"
#include "gxdevice.h"
#include "gzpath.h"
#include "gzcpath.h"
#include "gxdcolor.h"
#include "gxhttile.h"
#include "gxistate.h"
#include "gxpaint.h"
#include "gxfdrop.h"
#include "gxfill.h"
#include "gsptype2.h"
#include "gdevddrw.h"
#include "gzspotan.h"
#include "memory_.h"
#include "stdint_.h"
#include "vdtrace.h"
#ifdef DEBUG
stats_fill_t stats_fill;
#endif
#define INSIDE_PATH_P(inside, rule) ((inside & rule) != 0)
private int
x_order(const active_line *lp1, const active_line *lp2)
{
bool s1;
INCR(order);
if (lp1->x_current < lp2->x_current)
return -1;
else if (lp1->x_current > lp2->x_current)
return 1;
if ((s1 = lp1->start.x < lp1->end.x) != (lp2->start.x < lp2->end.x))
return (s1 ? 1 : -1);
INCR(slow_order);
{
fixed dx1 = lp1->end.x - lp1->start.x,
dy1 = lp1->end.y - lp1->start.y;
fixed dx2 = lp2->end.x - lp2->start.x,
dy2 = lp2->end.y - lp2->start.y;
double diff = (double)dx1 * dy2 - (double)dx2 * dy1;
return (diff < 0 ? -1 : diff > 0 ? 1 : 0);
}
}
gs_private_st_simple(st_active_line, active_line, "active_line");
#ifdef DEBUG
private void
print_active_line(const char *label, const active_line * alp)
{
dlprintf5("[f]%s 0x%lx(%d): x_current=%f x_next=%f\n",
label, (ulong) alp, alp->direction,
fixed2float(alp->x_current), fixed2float(alp->x_next));
dlprintf5("    start=(%f,%f) pt_end=0x%lx(%f,%f)\n",
fixed2float(alp->start.x), fixed2float(alp->start.y),
(ulong) alp->pseg,
fixed2float(alp->end.x), fixed2float(alp->end.y));
dlprintf2("    prev=0x%lx next=0x%lx\n",
(ulong) alp->prev, (ulong) alp->next);
}
private void
print_line_list(const active_line * flp)
{
const active_line *lp;
for (lp = flp; lp != 0; lp = lp->next) {
fixed xc = lp->x_current, xn = lp->x_next;
dlprintf3("[f]0x%lx(%d): x_current/next=%g",
(ulong) lp, lp->direction,
fixed2float(xc));
if (xn != xc)
dprintf1("/%g", fixed2float(xn));
dputc('\n');
}
}
inline private void
print_al(const char *label, const active_line * alp)
{
if (gs_debug_c('F'))
print_active_line(label, alp);
}
#else
#define print_al(label,alp) DO_NOTHING
#endif
private inline bool
is_spotan_device(gx_device * dev)
{
return dev->memory != NULL &&
gs_object_type(dev->memory, dev) == &st_device_spot_analyzer;
}
private void free_line_list(line_list *);
private int add_y_list(gx_path *, line_list *);
private int add_y_line_aux(const segment * prev_lp, const segment * lp,
const gs_fixed_point *curr, const gs_fixed_point *prev, int dir, line_list *ll);
private void insert_x_new(active_line *, line_list *);
private bool end_x_line(active_line *, const line_list *, bool);
private void step_al(active_line *alp, bool move_iterator);
#define FILL_LOOP_PROC(proc) int proc(line_list *, fixed band_mask)
private FILL_LOOP_PROC(spot_into_scan_lines);
private FILL_LOOP_PROC(spot_into_trapezoids);
private inline void
init_line_list(line_list *ll, gs_memory_t * mem)
{
ll->memory = mem;
ll->active_area = 0;
ll->next_active = ll->local_active;
ll->limit = ll->next_active + MAX_LOCAL_ACTIVE;
ll->close_count = 0;
ll->y_list = 0;
ll->y_line = 0;
ll->h_list0 = ll->h_list1 = 0;
ll->margin_set0.margin_list = ll->margin_set1.margin_list = 0;
ll->margin_set0.margin_touched = ll->margin_set1.margin_touched = 0;
ll->margin_set0.y = ll->margin_set1.y = 0;
ll->free_margin_list = 0;
ll->local_margin_alloc_count = 0;
ll->margin_set0.sect = ll->local_section0;
ll->margin_set1.sect = ll->local_section1;
INCR(fill);
}
private inline void
unclose_path(gx_path * ppath, int count)
{
subpath *psub;
for (psub = ppath->first_subpath; count != 0;
psub = (subpath *) psub->last->next
)
if (psub->last == (segment *) & psub->closer) {
segment *prev = psub->closer.prev, *next = psub->closer.next;
prev->next = next;
if (next)
next->prev = prev;
psub->last = prev;
count--;
}
}
void
gx_adjust_if_empty(const gs_fixed_rect * pbox, gs_fixed_point * adjust)
{
const fixed
dx = pbox->q.x - pbox->p.x, dy = pbox->q.y - pbox->p.y;
if (dx < fixed_half && dx > 0 && (dy >= int2fixed(2) || dy < 0)) {
adjust->x = arith_rshift_1(fixed_1 + fixed_epsilon - dx);
if_debug1('f', "[f]thin adjust_x=%g\n",
fixed2float(adjust->x));
} else if (dy < fixed_half && dy > 0 && (dx >= int2fixed(2) || dx < 0)) {
adjust->y = arith_rshift_1(fixed_1 + fixed_epsilon - dy);
if_debug1('f', "[f]thin adjust_y=%g\n",
fixed2float(adjust->y));
}
}
private int
gx_general_fill_path(gx_device * pdev, const gs_imager_state * pis,
gx_path * ppath, const gx_fill_params * params,
const gx_device_color * pdevc, const gx_clip_path * pcpath)
{
gs_fixed_point adjust;
gs_logical_operation_t lop = pis->log_op;
gs_fixed_rect ibox, bbox, sbox;
gx_device_clip cdev;
gx_device *dev = pdev;
gx_device *save_dev = dev;
gx_path ffpath;
gx_path *pfpath;
int code;
int max_fill_band = dev->max_fill_band;
#define NO_BAND_MASK ((fixed)(-1) << (sizeof(fixed) * 8 - 1))
const bool is_character = params->adjust.x == -1;
bool fill_by_trapezoids;
bool pseudo_rasterization;
bool big_path = ppath->subpath_count > 50;
fill_options fo;
line_list lst;
*(const fill_options **)&lst.fo = &fo;
gx_path_bbox(ppath, &ibox);
# define SMALL_CHARACTER 500
pseudo_rasterization = (is_character &&
!is_spotan_device(dev) &&
ibox.q.y - ibox.p.y < SMALL_CHARACTER * fixed_scale &&
ibox.q.x - ibox.p.x < SMALL_CHARACTER * fixed_scale);
if (is_character)
adjust.x = adjust.y = 0;
else
adjust = params->adjust;
if (params->fill_zero_width && !pseudo_rasterization)
gx_adjust_if_empty(&ibox, &adjust);
lst.bbox_left = fixed2int(ibox.p.x - adjust.x - fixed_epsilon);
lst.bbox_width = fixed2int(fixed_ceiling(ibox.q.x + adjust.x)) - lst.bbox_left;
if (vd_enabled) {
fixed x0 = int2fixed(fixed2int(ibox.p.x - adjust.x - fixed_epsilon));
fixed x1 = int2fixed(fixed2int(ibox.q.x + adjust.x + fixed_scale - fixed_epsilon));
fixed y0 = int2fixed(fixed2int(ibox.p.y - adjust.y - fixed_epsilon));
fixed y1 = int2fixed(fixed2int(ibox.q.y + adjust.y + fixed_scale - fixed_epsilon)), k;
for (k = x0; k <= x1; k += fixed_scale)
vd_bar(k, y0, k, y1, 1, RGB(128, 128, 128));
for (k = y0; k <= y1; k += fixed_scale)
vd_bar(x0, k, x1, k, 1, RGB(128, 128, 128));
}
if_debug6('f', "[f]adjust=%g,%g bbox=(%g,%g),(%g,%g)\n",
fixed2float(adjust.x), fixed2float(adjust.y),
fixed2float(ibox.p.x), fixed2float(ibox.p.y),
fixed2float(ibox.q.x), fixed2float(ibox.q.y));
if (pcpath)
gx_cpath_inner_box(pcpath, &bbox);
else
(*dev_proc(dev, get_clipping_box)) (dev, &bbox);
if (!rect_within(ibox, bbox)) {
if (pcpath)
gx_cpath_outer_box(pcpath, &bbox);
if_debug4('f', "   outer_box=(%g,%g),(%g,%g)\n",
fixed2float(bbox.p.x), fixed2float(bbox.p.y),
fixed2float(bbox.q.x), fixed2float(bbox.q.y));
rect_intersect(ibox, bbox);
if (ibox.p.x - adjust.x >= ibox.q.x + adjust.x ||
ibox.p.y - adjust.y >= ibox.q.y + adjust.y
) {
return 0;
}
if (pcpath) {
dev = (gx_device *) & cdev;
gx_make_clip_device(&cdev, gx_cpath_list(pcpath));
cdev.target = save_dev;
cdev.max_fill_band = save_dev->max_fill_band;
(*dev_proc(dev, open_device)) (dev);
}
}
if (adjust.x == fixed_half)
fo.adjust_left = fixed_half - fixed_epsilon,
fo.adjust_right = fixed_half ;
else
fo.adjust_left = fo.adjust_right = adjust.x;
if (adjust.y == fixed_half)
fo.adjust_below = fixed_half - fixed_epsilon,
fo.adjust_above = fixed_half ;
else
fo.adjust_below = fo.adjust_above = adjust.y;
init_line_list(&lst, ppath->memory);
sbox.p.x = ibox.p.x - adjust.x;
sbox.p.y = ibox.p.y - adjust.y;
sbox.q.x = ibox.q.x + adjust.x;
sbox.q.y = ibox.q.y + adjust.y;
fo.pseudo_rasterization = pseudo_rasterization;
fo.pdevc = pdevc;
fo.lop = lop;
fo.fixed_flat = float2fixed(params->flatness);
fo.ymin = ibox.p.y;
fo.ymax = ibox.q.y;
fo.dev = dev;
fo.lop = lop;
fo.pbox = &sbox;
fo.rule = params->rule;
fo.is_spotan = is_spotan_device(dev);
fo.fill_direct = color_writes_pure(pdevc, lop);
fo.fill_rect = (fo.fill_direct ? dev_proc(dev, fill_rectangle) : NULL);
fo.fill_trap = dev_proc(dev, fill_trapezoid);
fill_by_trapezoids =
(pseudo_rasterization || !gx_path_has_curves(ppath) ||
params->flatness >= 1.0 || fo.is_spotan);
if (fill_by_trapezoids && !fo.is_spotan && !lop_is_idempotent(lop)) {
gs_fixed_rect rbox;
if (gx_path_is_rectangular(ppath, &rbox)) {
int x0 = fixed2int_pixround(rbox.p.x - fo.adjust_left);
int y0 = fixed2int_pixround(rbox.p.y - fo.adjust_below);
int x1 = fixed2int_pixround(rbox.q.x + fo.adjust_right);
int y1 = fixed2int_pixround(rbox.q.y + fo.adjust_above);
return gx_fill_rectangle_device_rop(x0, y0, x1 - x0, y1 - y0,
pdevc, dev, lop);
}
if (fo.adjust_left | fo.adjust_right | fo.adjust_below | fo.adjust_above)
fill_by_trapezoids = false;
}
gx_path_init_local(&ffpath, ppath->memory);
if (!big_path && !gx_path_has_curves(ppath))
pfpath = ppath;
else if (is_spotan_device(dev))
pfpath = ppath;
else if (!big_path && gx_path__check_curves(ppath, pco_small_curves, fo.fixed_flat))
pfpath = ppath;
else {
code = gx_path_copy_reducing(ppath, &ffpath, fo.fixed_flat, NULL,
pco_small_curves);
if (code < 0)
return code;
pfpath = &ffpath;
if (big_path) {
code = gx_path_merge_contacting_contours(pfpath);
if (code < 0)
return code;
}
}
fo.fill_by_trapezoids = fill_by_trapezoids;
if ((code = add_y_list(pfpath, &lst)) < 0)
goto nope;
{
FILL_LOOP_PROC((*fill_loop));
if (fill_by_trapezoids)
fill_loop = spot_into_trapezoids;
else
fill_loop = spot_into_scan_lines;
if (lst.bbox_width > MAX_LOCAL_SECTION && fo.pseudo_rasterization) {
lst.margin_set0.sect = (section *)gs_alloc_struct_array(pdev->memory, lst.bbox_width * 2,
section, &st_section, "section");
if (lst.margin_set0.sect == 0)
return_error(gs_error_VMerror);
lst.margin_set1.sect = lst.margin_set0.sect + lst.bbox_width;
}
if (fo.pseudo_rasterization) {
init_section(lst.margin_set0.sect, 0, lst.bbox_width);
init_section(lst.margin_set1.sect, 0, lst.bbox_width);
}
code = (*fill_loop)
(&lst, (max_fill_band == 0 ? NO_BAND_MASK : int2fixed(-max_fill_band)));
if (lst.margin_set0.sect != lst.local_section0 &&
lst.margin_set0.sect != lst.local_section1)
gs_free_object(pdev->memory, min(lst.margin_set0.sect, lst.margin_set1.sect), "section");
}
nope:if (lst.close_count != 0)
unclose_path(pfpath, lst.close_count);
free_line_list(&lst);
if (pfpath != ppath)
gx_path_free(pfpath, "gx_general_fill_path");
#ifdef DEBUG
if (gs_debug_c('f')) {
dlputs("[f]  # alloc    up  down horiz step slowx  iter  find  band bstep bfill\n");
dlprintf5(" %5ld %5ld %5ld %5ld %5ld",
stats_fill.fill, stats_fill.fill_alloc,
stats_fill.y_up, stats_fill.y_down,
stats_fill.horiz);
dlprintf4(" %5ld %5ld %5ld %5ld",
stats_fill.x_step, stats_fill.slow_x,
stats_fill.iter, stats_fill.find_y);
dlprintf3(" %5ld %5ld %5ld\n",
stats_fill.band, stats_fill.band_step,
stats_fill.band_fill);
dlputs("[f]    afill slant shall sfill mqcrs order slowo\n");
dlprintf7("       %5ld %5ld %5ld %5ld %5ld %5ld %5ld\n",
stats_fill.afill, stats_fill.slant,
stats_fill.slant_shallow, stats_fill.sfill,
stats_fill.mq_cross, stats_fill.order,
stats_fill.slow_order);
}
#endif
return code;
}
int
gx_default_fill_path(gx_device * pdev, const gs_imager_state * pis,
gx_path * ppath, const gx_fill_params * params,
const gx_device_color * pdevc, const gx_clip_path * pcpath)
{
int code;
if (gx_dc_is_pattern2_color(pdevc)) {
gx_clip_path cpath_intersection;
gx_path path_intersection;
gx_path_init_local(&path_intersection, pdev->memory);
gx_cpath_init_local_shared(&cpath_intersection, pcpath, pdev->memory);
if ((code = gx_cpath_intersect(&cpath_intersection, ppath, params->rule, (gs_imager_state *)pis)) >= 0)
code = gx_cpath_to_path(&cpath_intersection, &path_intersection);
if (code >= 0)
code = gx_dc_pattern2_fill_path(pdevc, &path_intersection, NULL, pdev);
gx_path_free(&path_intersection, "shading_fill_path_intersection");
gx_cpath_free(&cpath_intersection, "shading_fill_cpath_intersection");
} else {
bool got_dc = false;
vd_save;
if (vd_allowed('F') || vd_allowed('f')) {
if (!vd_enabled) {
vd_get_dc( (params->adjust.x | params->adjust.y) ? 'F' : 'f');
got_dc = vd_enabled;
}
if (vd_enabled) {
vd_set_shift(0, 100);
vd_set_scale(VD_SCALE);
vd_set_origin(0, 0);
vd_erase(RGB(192, 192, 192));
}
} else
vd_disable;
code = gx_general_fill_path(pdev, pis, ppath, params, pdevc, pcpath);
if (got_dc)
vd_release_dc;
vd_restore;
}
return code;
}
private void
free_line_list(line_list *ll)
{
gs_memory_t *mem = ll->memory;
active_line *alp;
while ((alp = ll->active_area) != 0) {
active_line *next = alp->alloc_next;
gs_free_object(mem, alp, "active line");
ll->active_area = next;
}
free_all_margins(ll);
}
private inline active_line *
make_al(line_list *ll)
{
active_line *alp = ll->next_active;
if (alp == ll->limit) {
alp = gs_alloc_struct(ll->memory, active_line,
&st_active_line, "active line");
if (alp == 0)
return NULL;
alp->alloc_next = ll->active_area;
ll->active_area = alp;
INCR(fill_alloc);
} else
ll->next_active++;
return alp;
}
private void
insert_y_line(line_list *ll, active_line *alp)
{
active_line *yp = ll->y_line;
active_line *nyp;
fixed y_start = alp->start.y;
vd_bar(alp->start.x, alp->start.y, alp->end.x, alp->end.y, 0, RGB(255, 0, 0));
if (yp == 0) {
alp->next = alp->prev = 0;
ll->y_list = alp;
} else if (y_start >= yp->start.y) {
while (INCR_EXPR(y_up),
((nyp = yp->next) != NULL &&
y_start > nyp->start.y)
)
yp = nyp;
alp->next = nyp;
alp->prev = yp;
yp->next = alp;
if (nyp)
nyp->prev = alp;
} else {
while (INCR_EXPR(y_down),
((nyp = yp->prev) != NULL &&
y_start < nyp->start.y)
)
yp = nyp;
alp->prev = nyp;
alp->next = yp;
yp->prev = alp;
if (nyp)
nyp->next = alp;
else
ll->y_list = alp;
}
ll->y_line = alp;
vd_bar(alp->start.x, alp->start.y, alp->end.x, alp->end.y, 1, RGB(0, 255, 0));
print_al("add ", alp);
}
typedef struct contour_cursor_s {
segment *prev, *pseg, *pfirst, *plast;
gx_flattened_iterator *fi;
bool more_flattened;
bool first_flattened;
int dir;
bool monotonic_y;
bool monotonic_x;
bool crossing;
} contour_cursor;
private inline int
compute_dir(const fill_options *fo, fixed y0, fixed y1)
{
if (max(y0, y1) < fo->ymin)
return 2;
if (min(y0, y1) > fo->ymax)
return 2;
return (y0 < y1 ? DIR_UP :
y0 > y1 ? DIR_DOWN : DIR_HORIZONTAL);
}
private inline int
add_y_curve_part(line_list *ll, segment *s0, segment *s1, int dir,
gx_flattened_iterator *fi, bool more1, bool step_back, bool monotonic_x)
{
active_line *alp = make_al(ll);
if (alp == NULL)
return_error(gs_error_VMerror);
alp->pseg = (dir == DIR_UP ? s1 : s0);
alp->direction = dir;
alp->fi = *fi;
alp->more_flattened = more1;
if (dir != DIR_UP && more1)
gx_flattened_iterator__switch_to_backscan(&alp->fi, more1);
if (step_back) {
do {
alp->more_flattened = gx_flattened_iterator__prev(&alp->fi);
if (compute_dir(ll->fo, alp->fi.ly0, alp->fi.ly1) != 2)
break;
} while (alp->more_flattened);
}
step_al(alp, false);
alp->monotonic_y = false;
alp->monotonic_x = monotonic_x;
insert_y_line(ll, alp);
return 0;
}
private inline int
add_y_line(const segment * prev_lp, const segment * lp, int dir, line_list *ll)
{
return add_y_line_aux(prev_lp, lp, &lp->pt, &prev_lp->pt, dir, ll);
}
private inline int
start_al_pair(line_list *ll, contour_cursor *q, contour_cursor *p)
{
int code;
if (q->monotonic_y)
code = add_y_line(q->prev, q->pseg, DIR_DOWN, ll);
else
code = add_y_curve_part(ll, q->prev, q->pseg, DIR_DOWN, q->fi,
!q->first_flattened, false, q->monotonic_x);
if (code < 0)
return code;
if (p->monotonic_y)
code = add_y_line(p->prev, p->pseg, DIR_UP, ll);
else
code = add_y_curve_part(ll, p->prev, p->pseg, DIR_UP, p->fi,
p->more_flattened, false, p->monotonic_x);
return code;
}
private int
start_al_pair_from_min(line_list *ll, contour_cursor *q)
{
int dir, code;
const fill_options * const fo = ll->fo;
do {
q->more_flattened = gx_flattened_iterator__next(q->fi);
dir = compute_dir(fo, q->fi->ly0, q->fi->ly1);
if (q->fi->ly0 > fo->ymax && ll->y_break > q->fi->y0)
ll->y_break = q->fi->ly0;
if (q->fi->ly1 > fo->ymax && ll->y_break > q->fi->ly1)
ll->y_break = q->fi->ly1;
if (dir == DIR_UP && ll->main_dir == DIR_DOWN && q->fi->ly0 >= fo->ymin) {
code = add_y_curve_part(ll, q->prev, q->pseg, DIR_DOWN, q->fi,
true, true, q->monotonic_x);
if (code < 0)
return code;
code = add_y_curve_part(ll, q->prev, q->pseg, DIR_UP, q->fi,
q->more_flattened, false, q->monotonic_x);
if (code < 0)
return code;
} else if (q->fi->ly0 < fo->ymin && q->fi->ly1 >= fo->ymin) {
code = add_y_curve_part(ll, q->prev, q->pseg, DIR_UP, q->fi,
q->more_flattened, false, q->monotonic_x);
if (code < 0)
return code;
} else if (q->fi->ly0 >= fo->ymin && q->fi->ly1 < fo->ymin) {
code = add_y_curve_part(ll, q->prev, q->pseg, DIR_DOWN, q->fi,
true, false, q->monotonic_x);
if (code < 0)
return code;
}
q->first_flattened = false;
q->dir = dir;
ll->main_dir = (dir == DIR_DOWN ? DIR_DOWN :
dir == DIR_UP ? DIR_UP : ll->main_dir);
if (!q->more_flattened)
break;
} while(q->more_flattened);
return 0;
}
private inline void
init_contour_cursor(line_list *ll, contour_cursor *q)
{
const fill_options * const fo = ll->fo;
if (q->pseg->type == s_curve) {
curve_segment *s = (curve_segment *)q->pseg;
fixed ymin = min(min(q->prev->pt.y, s->p1.y), min(s->p2.y, s->pt.y));
fixed ymax = max(max(q->prev->pt.y, s->p1.y), max(s->p2.y, s->pt.y));
bool in_band = ymin <= fo->ymax && ymax >= fo->ymin;
q->crossing = ymin < fo->ymin && ymax >= fo->ymin;
q->monotonic_y = !in_band ||
(!q->crossing &&
((q->prev->pt.y <= s->p1.y && s->p1.y <= s->p2.y && s->p2.y <= s->pt.y) ||
(q->prev->pt.y >= s->p1.y && s->p1.y >= s->p2.y && s->p2.y >= s->pt.y)));
q->monotonic_x =
((q->prev->pt.x <= s->p1.x && s->p1.x <= s->p2.x && s->p2.x <= s->pt.x) ||
(q->prev->pt.x >= s->p1.x && s->p1.x >= s->p2.x && s->p2.x >= s->pt.x));
} else
q->monotonic_y = true;
if (!q->monotonic_y) {
curve_segment *s = (curve_segment *)q->pseg;
int k = gx_curve_log2_samples(q->prev->pt.x, q->prev->pt.y, s, fo->fixed_flat);
gx_flattened_iterator__init(q->fi, q->prev->pt.x, q->prev->pt.y, s, k);
} else {
q->dir = compute_dir(fo, q->prev->pt.y, q->pseg->pt.y);
gx_flattened_iterator__init_line(q->fi,
q->prev->pt.x, q->prev->pt.y, q->pseg->pt.x, q->pseg->pt.y);
vd_bar(q->prev->pt.x, q->prev->pt.y, q->pseg->pt.x, q->pseg->pt.y, 1, RGB(0, 0, 255));
}
q->first_flattened = true;
}
private int
scan_contour(line_list *ll, contour_cursor *q)
{
contour_cursor p;
gx_flattened_iterator fi, save_fi;
segment *pseg;
int code;
bool only_horizontal = true, saved = false;
const fill_options * const fo = ll->fo;
contour_cursor save_q;
p.fi = &fi;
save_q.dir = 2;
ll->main_dir = DIR_HORIZONTAL;
for (; ; q->pseg = q->prev, q->prev = q->prev->prev) {
init_contour_cursor(ll, q);
while(gx_flattened_iterator__next(q->fi)) {
q->first_flattened = false;
q->dir = compute_dir(fo, q->fi->ly0, q->fi->ly1);
ll->main_dir = (q->dir == DIR_DOWN ? DIR_DOWN :
q->dir == DIR_UP ? DIR_UP : ll->main_dir);
}
q->dir = compute_dir(fo, q->fi->ly0, q->fi->ly1);
q->more_flattened = false;
ll->main_dir = (q->dir == DIR_DOWN ? DIR_DOWN :
q->dir == DIR_UP ? DIR_UP : ll->main_dir);
if (ll->main_dir != DIR_HORIZONTAL) {
only_horizontal = false;
break;
}
if (!saved && q->dir != 2) {
save_q = *q;
save_fi = *q->fi;
saved = true;
}
if (q->prev == q->pfirst)
break;
}
if (saved) {
*q = save_q;
*q->fi = save_fi;
}
for (pseg = q->pfirst; pseg != q->plast; pseg = pseg->next) {
p.prev = pseg;
p.pseg = pseg->next;
if (!fo->pseudo_rasterization || only_horizontal
|| p.prev->pt.x != p.pseg->pt.x || p.prev->pt.y != p.pseg->pt.y
|| p.pseg->type == s_curve) {
init_contour_cursor(ll, &p);
p.more_flattened = gx_flattened_iterator__next(p.fi);
p.dir = compute_dir(fo, p.fi->ly0, p.fi->ly1);
if (p.fi->ly0 > fo->ymax && ll->y_break > p.fi->ly0)
ll->y_break = p.fi->ly0;
if (p.fi->ly1 > fo->ymax && ll->y_break > p.fi->ly1)
ll->y_break = p.fi->ly1;
if (p.monotonic_y && p.dir == DIR_HORIZONTAL &&
!fo->pseudo_rasterization &&
fixed2int_pixround(p.pseg->pt.y - fo->adjust_below) <
fixed2int_pixround(p.pseg->pt.y + fo->adjust_above)) {
code = add_y_line(p.prev, p.pseg, DIR_HORIZONTAL, ll);
if (code < 0)
return code;
}
if (p.monotonic_y && p.dir == DIR_HORIZONTAL &&
fo->pseudo_rasterization && only_horizontal) {
code = add_y_line(p.prev, p.pseg, DIR_HORIZONTAL, ll);
if (code < 0)
return code;
}
if (p.fi->ly0 >= fo->ymin && p.dir == DIR_UP && ll->main_dir == DIR_DOWN) {
code = start_al_pair(ll, q, &p);
if (code < 0)
return code;
}
if (p.fi->ly0 < fo->ymin && p.fi->ly1 >= fo->ymin) {
if (p.monotonic_y)
code = add_y_line(p.prev, p.pseg, DIR_UP, ll);
else
code = add_y_curve_part(ll, p.prev, p.pseg, DIR_UP, p.fi,
p.more_flattened, false, p.monotonic_x);
if (code < 0)
return code;
}
if (p.fi->ly0 >= fo->ymin && p.fi->ly1 < fo->ymin) {
if (p.monotonic_y)
code = add_y_line(p.prev, p.pseg, DIR_DOWN, ll);
else
code = add_y_curve_part(ll, p.prev, p.pseg, DIR_DOWN, p.fi,
!p.first_flattened, false, p.monotonic_x);
if (code < 0)
return code;
}
ll->main_dir = (p.dir == DIR_DOWN ? DIR_DOWN :
p.dir == DIR_UP ? DIR_UP : ll->main_dir);
if (!p.monotonic_y && p.more_flattened) {
code = start_al_pair_from_min(ll, &p);
if (code < 0)
return code;
}
if (p.dir == DIR_DOWN || p.dir == DIR_HORIZONTAL) {
gx_flattened_iterator *fi1 = q->fi;
q->prev = p.prev;
q->pseg = p.pseg;
q->monotonic_y = p.monotonic_y;
q->more_flattened = p.more_flattened;
q->first_flattened = p.first_flattened;
q->fi = p.fi;
q->dir = p.dir;
p.fi = fi1;
}
}
}
q->fi = NULL;
return 0;
}
private int
add_y_list(gx_path * ppath, line_list *ll)
{
subpath *psub = ppath->first_subpath;
int close_count = 0;
int code;
contour_cursor q;
gx_flattened_iterator fi;
ll->y_break = max_fixed;
for (;psub; psub = (subpath *)psub->last->next) {
segment *pfirst = (segment *)psub;
segment *plast = psub->last, *prev;
q.fi = &fi;
if (plast->type != s_line_close) {
line_close_segment *lp = &psub->closer;
segment *next = plast->next;
lp->next = next;
lp->prev = plast;
plast->next = (segment *) lp;
if (next)
next->prev = (segment *) lp;
lp->type = s_line_close;
lp->pt = psub->pt;
lp->sub = psub;
psub->last = plast = (segment *) lp;
ll->close_count++;
}
prev = plast->prev;
if (ll->fo->pseudo_rasterization && prev != pfirst &&
prev->pt.x == plast->pt.x && prev->pt.y == plast->pt.y) {
plast = prev;
prev = prev->prev;
}
q.prev = prev;
q.pseg = plast;
q.pfirst = pfirst;
q.plast = plast;
code = scan_contour(ll, &q);
if (code < 0)
return code;
}
return close_count;
}
private void
step_al(active_line *alp, bool move_iterator)
{
bool forth = (alp->direction == DIR_UP || !alp->fi.curve);
if (move_iterator) {
if (forth)
alp->more_flattened = gx_flattened_iterator__next(&alp->fi);
else
alp->more_flattened = gx_flattened_iterator__prev(&alp->fi);
} else
vd_bar(alp->fi.lx0, alp->fi.ly0, alp->fi.lx1, alp->fi.ly1, 1, RGB(0, 0, 255));
alp->start.x = (forth ? alp->fi.lx0 : alp->fi.lx1);
alp->start.y = (forth ? alp->fi.ly0 : alp->fi.ly1);
alp->end.x = (forth ? alp->fi.lx1 : alp->fi.lx0);
alp->end.y = (forth ? alp->fi.ly1 : alp->fi.ly0);
alp->diff.x = alp->end.x - alp->start.x;
alp->diff.y = alp->end.y - alp->start.y;
SET_NUM_ADJUST(alp);
(alp)->y_fast_max = MAX_MINUS_NUM_ADJUST(alp) /
((alp->diff.x >= 0 ? alp->diff.x : -alp->diff.x) | 1) + alp->start.y;
}
private void
init_al(active_line *alp, const segment *s0, const segment *s1, const line_list *ll)
{
const segment *ss = (alp->direction == DIR_UP ? s1 : s0);
bool curve = (ss != NULL && ss->type == s_curve);
if (curve) {
if (alp->direction == DIR_UP) {
const curve_segment *cs = (const curve_segment *)s1;
int k = gx_curve_log2_samples(s0->pt.x, s0->pt.y, cs, ll->fo->fixed_flat);
gx_flattened_iterator__init(&alp->fi,
s0->pt.x, s0->pt.y, (const curve_segment *)s1, k);
step_al(alp, true);
if (!ll->fo->fill_by_trapezoids) {
alp->monotonic_y = (s0->pt.y <= cs->p1.y && cs->p1.y <= cs->p2.y && cs->p2.y <= cs->pt.y);
alp->monotonic_x = (s0->pt.x <= cs->p1.x && cs->p1.x <= cs->p2.x && cs->p2.x <= cs->pt.x) ||
(s0->pt.x >= cs->p1.x && cs->p1.x >= cs->p2.x && cs->p2.x >= cs->pt.x);
}
} else {
const curve_segment *cs = (const curve_segment *)s0;
int k = gx_curve_log2_samples(s1->pt.x, s1->pt.y, cs, ll->fo->fixed_flat);
bool more;
gx_flattened_iterator__init(&alp->fi,
s1->pt.x, s1->pt.y, (const curve_segment *)s0, k);
alp->more_flattened = false;
do {
more = gx_flattened_iterator__next(&alp->fi);
alp->more_flattened |= more;
} while(more);
gx_flattened_iterator__switch_to_backscan(&alp->fi, alp->more_flattened);
step_al(alp, false);
if (!ll->fo->fill_by_trapezoids) {
alp->monotonic_y = (s0->pt.y >= cs->p1.y && cs->p1.y >= cs->p2.y && cs->p2.y >= cs->pt.y);
alp->monotonic_x = (s0->pt.x <= cs->p1.x && cs->p1.x <= cs->p2.x && cs->p2.x <= cs->pt.x) ||
(s0->pt.x >= cs->p1.x && cs->p1.x >= cs->p2.x && cs->p2.x >= cs->pt.x);
}
}
} else {
gx_flattened_iterator__init_line(&alp->fi,
s0->pt.x, s0->pt.y, s1->pt.x, s1->pt.y);
step_al(alp, true);
alp->monotonic_x = alp->monotonic_y = true;
}
alp->pseg = s1;
}
private int
add_y_line_aux(const segment * prev_lp, const segment * lp,
const gs_fixed_point *curr, const gs_fixed_point *prev, int dir, line_list *ll)
{
active_line *alp = make_al(ll);
if (alp == NULL)
return_error(gs_error_VMerror);
alp->more_flattened = false;
switch ((alp->direction = dir)) {
case DIR_UP:
init_al(alp, prev_lp, lp, ll);
break;
case DIR_DOWN:
init_al(alp, lp, prev_lp, ll);
break;
case DIR_HORIZONTAL:
alp->start = *prev;
alp->end = *curr;
alp->pseg = prev_lp;
break;
default:
return_error(gs_error_unregistered);
}
insert_y_line(ll, alp);
return 0;
}
private void
insert_x_new(active_line * alp, line_list *ll)
{
active_line *next;
active_line *prev = &ll->x_head;
vd_bar(alp->start.x, alp->start.y, alp->end.x, alp->end.y, 1, RGB(128, 128, 0));
alp->x_current = alp->start.x;
alp->x_next = alp->start.x;
while (INCR_EXPR(x_step),
(next = prev->next) != 0 && x_order(next, alp) < 0
)
prev = next;
alp->next = next;
alp->prev = prev;
if (next != 0)
next->prev = alp;
prev->next = alp;
}
private inline void
insert_h_new(active_line * alp, line_list *ll)
{
alp->next = ll->h_list0;
alp->prev = 0;
if (ll->h_list0 != 0)
ll->h_list0->prev = alp;
ll->h_list0 = alp;
}
private inline void
remove_al(const line_list *ll, active_line *alp)
{
active_line *nlp = alp->next;
alp->prev->next = nlp;
if (nlp)
nlp->prev = alp->prev;
if_debug1('F', "[F]drop 0x%lx\n", (ulong) alp);
}
private bool
end_x_line(active_line *alp, const line_list *ll, bool update)
{
const segment *pseg = alp->pseg;
const segment *next =
(alp->direction == DIR_UP ?
(
pseg->type == s_line_close ?
((const line_close_segment *)pseg)->sub->next :
pseg->next) :
(
pseg->type == s_start ?
((const subpath *)pseg)->last->prev :
pseg->prev)
);
if (alp->end.y < alp->start.y) {
remove_al(ll, alp);
return true;
} else if (alp->more_flattened)
return false;
init_al(alp, pseg, next, ll);
if (alp->start.y > alp->end.y) {
remove_al(ll, alp);
return true;
}
alp->x_current = alp->x_next = alp->start.x;
vd_bar(alp->start.x, alp->start.y, alp->end.x, alp->end.y, 1, RGB(128, 0, 128));
print_al("repl", alp);
return false;
}
private inline int
add_margin(line_list * ll, active_line * flp, active_line * alp, fixed y0, fixed y1)
{ vd_bar(alp->start.x, alp->start.y, alp->end.x, alp->end.y, 1, RGB(255, 255, 255));
vd_bar(flp->start.x, flp->start.y, flp->end.x, flp->end.y, 1, RGB(255, 255, 255));
return continue_margin_common(ll, &ll->margin_set0, flp, alp, y0, y1);
}
private inline int
continue_margin(line_list * ll, active_line * flp, active_line * alp, fixed y0, fixed y1)
{
return continue_margin_common(ll, &ll->margin_set0, flp, alp, y0, y1);
}
private inline int
complete_margin(line_list * ll, active_line * flp, active_line * alp, fixed y0, fixed y1)
{
return continue_margin_common(ll, &ll->margin_set1, flp, alp, y0, y1);
}
private int
fill_slant_adjust(const line_list *ll,
const active_line *flp, const active_line *alp, fixed y, fixed y1)
{
const fill_options * const fo = ll->fo;
const fixed Yb = y - fo->adjust_below;
const fixed Ya = y + fo->adjust_above;
const fixed Y1b = y1 - fo->adjust_below;
const fixed Y1a = y1 + fo->adjust_above;
const gs_fixed_edge *plbot, *prbot, *pltop, *prtop;
gs_fixed_edge vert_left, slant_left, vert_right, slant_right;
int code;
INCR(slant);
vd_quad(flp->x_current, y, alp->x_current, y,
alp->x_next, y1, flp->x_next, y1, 1, VD_TRAP_COLOR);
if (flp->start.x < flp->end.x) {
vert_left.start.x = vert_left.end.x = flp->x_current - fo->adjust_left;
vert_left.start.y = Yb, vert_left.end.y = Ya;
vert_right.start.x = vert_right.end.x = alp->x_next + fo->adjust_right;
vert_right.start.y = Y1b, vert_right.end.y = Y1a;
slant_left.start.y = flp->start.y + fo->adjust_above;
slant_left.end.y = flp->end.y + fo->adjust_above;
slant_right.start.y = alp->start.y - fo->adjust_below;
slant_right.end.y = alp->end.y - fo->adjust_below;
plbot = &vert_left, prbot = &slant_right;
pltop = &slant_left, prtop = &vert_right;
} else {
vert_left.start.x = vert_left.end.x = flp->x_next - fo->adjust_left;
vert_left.start.y = Y1b, vert_left.end.y = Y1a;
vert_right.start.x = vert_right.end.x = alp->x_current + fo->adjust_right;
vert_right.start.y = Yb, vert_right.end.y = Ya;
slant_left.start.y = flp->start.y - fo->adjust_below;
slant_left.end.y = flp->end.y - fo->adjust_below;
slant_right.start.y = alp->start.y + fo->adjust_above;
slant_right.end.y = alp->end.y + fo->adjust_above;
plbot = &slant_left, prbot = &vert_right;
pltop = &vert_left, prtop = &slant_right;
}
slant_left.start.x = flp->start.x - fo->adjust_left;
slant_left.end.x = flp->end.x - fo->adjust_left;
slant_right.start.x = alp->start.x + fo->adjust_right;
slant_right.end.x = alp->end.x + fo->adjust_right;
if (Ya >= Y1b) {
int iYb = fixed2int_var_pixround(Yb);
int iYa = fixed2int_var_pixround(Ya);
int iY1b = fixed2int_var_pixround(Y1b);
int iY1a = fixed2int_var_pixround(Y1a);
INCR(slant_shallow);
if (iY1b > iYb) {
code = fo->fill_trap(fo->dev, plbot, prbot,
Yb, Y1b, false, fo->pdevc, fo->lop);
if (code < 0)
return code;
}
if (iYa > iY1b) {
int ix = fixed2int_var_pixround(vert_left.start.x);
int iw = fixed2int_var_pixround(vert_right.start.x) - ix;
code = gx_fill_rectangle_device_rop(ix, iY1b, iw, iYa - iY1b,
fo->pdevc, fo->dev, fo->lop);
if (code < 0)
return code;
}
if (iY1a > iYa)
code = fo->fill_trap(fo->dev, pltop, prtop,
Ya, Y1a, false, fo->pdevc, fo->lop);
else
code = 0;
} else {
fixed Yac;
if (fo->pbox->p.y < Ya) {
code = fo->fill_trap(fo->dev, plbot, prbot,
Yb, Ya, false, fo->pdevc, fo->lop);
if (code < 0)
return code;
Yac = Ya;
} else
Yac = fo->pbox->p.y;
if (fo->pbox->q.y > Y1b) {
code = fo->fill_trap(fo->dev, &slant_left, &slant_right,
Yac, Y1b, false, fo->pdevc, fo->lop);
if (code < 0)
return code;
code = fo->fill_trap(fo->dev, pltop, prtop,
Y1b, Y1a, false, fo->pdevc, fo->lop);
} else
code = fo->fill_trap(fo->dev, &slant_left, &slant_right,
Yac, fo->pbox->q.y, false, fo->pdevc, fo->lop);
}
return code;
}
private inline void
resort_x_line(active_line * alp)
{
active_line *prev = alp->prev;
active_line *next = alp->next;
prev->next = next;
if (next)
next->prev = prev;
while (x_order(prev, alp) > 0) {
if_debug2('F', "[F]swap 0x%lx,0x%lx\n",
(ulong) alp, (ulong) prev);
next = prev, prev = prev->prev;
}
alp->next = next;
alp->prev = prev;
if (next)
next->prev = alp;
prev->next = alp;
}
private inline void
move_al_by_y(line_list *ll, fixed y1)
{
fixed x;
active_line *alp, *nlp;
for (x = min_fixed, alp = ll->x_list; alp != 0; alp = nlp) {
bool notend = false;
alp->x_current = alp->x_next;
nlp = alp->next;
if (alp->end.y == y1 && alp->more_flattened) {
step_al(alp, true);
alp->x_current = alp->x_next = alp->start.x;
notend = (alp->end.y >= alp->start.y);
}
if (alp->end.y > y1 || notend || !end_x_line(alp, ll, true)) {
if (alp->x_next <= x)
resort_x_line(alp);
else
x = alp->x_next;
}
}
if (ll->x_list != 0 && ll->fo->pseudo_rasterization) {
for (alp = ll->x_list; alp->next != 0; ) {
if (alp->start.x == alp->end.x &&
alp->start.x == alp->next->start.x &&
alp->next->start.x == alp->next->end.x &&
alp->direction > alp->next->direction) {
active_line *prev = alp->prev;
active_line *next = alp->next;
active_line *next2 = next->next;
if (prev)
prev->next = next;
else
ll->x_list = next;
next->prev = prev;
alp->prev = next;
alp->next = next2;
next->next = alp;
if (next2)
next2->prev = alp;
} else
alp = alp->next;
}
}
}
private inline int
process_h_segments(line_list *ll, fixed y)
{
active_line *alp, *nlp;
int code, inserted = 0;
for (alp = ll->x_list; alp != 0; alp = nlp) {
nlp = alp->next;
if (alp->start.y == y && alp->end.y == y) {
if (ll->fo->pseudo_rasterization) {
code = add_y_line_aux(NULL, NULL, &alp->start, &alp->end, DIR_HORIZONTAL, ll);
if (code < 0)
return code;
}
inserted = 1;
}
}
return inserted;
}
private inline int
loop_fill_trap_np(const line_list *ll, const gs_fixed_edge *le, const gs_fixed_edge *re, fixed y, fixed y1)
{
const fill_options * const fo = ll->fo;
fixed ybot = max(y, fo->pbox->p.y);
fixed ytop = min(y1, fo->pbox->q.y);
if (ybot >= ytop)
return 0;
vd_quad(le->start.x, ybot, re->start.x, ybot, re->end.x, ytop, le->end.x, ytop, 1, VD_TRAP_COLOR);
return (*fo->fill_trap)
(fo->dev, le, re, ybot, ytop, false, fo->pdevc, fo->lop);
}
#define TEMPLATE_slant_into_trapezoids slant_into_trapezoids__fd
#define FILL_DIRECT 1
#include "gxfillts.h"
#undef TEMPLATE_slant_into_trapezoids
#undef FILL_DIRECT
#define TEMPLATE_slant_into_trapezoids slant_into_trapezoids__nd
#define FILL_DIRECT 0
#include "gxfillts.h"
#undef TEMPLATE_slant_into_trapezoids
#undef FILL_DIRECT
#define COVERING_PIXEL_CENTERS(y, y1, adjust_below, adjust_above)\
(fixed_pixround(y - adjust_below) < fixed_pixround(y1 + adjust_above))
private bool
intersect(active_line *endp, active_line *alp, fixed y, fixed y1, fixed *p_y_new)
{
fixed y_new, dy;
fixed dx_old = alp->x_current - endp->x_current;
fixed dx_den = dx_old + endp->x_next - alp->x_next;
if (dx_den <= dx_old)
return false;
dy = y1 - y;
if_debug3('F', "[F]cross: dy=%g, dx_old=%g, dx_new=%g\n",
fixed2float(dy), fixed2float(dx_old),
fixed2float(dx_den - dx_old));
y_new =
((dy | dx_old) < 1L << (size_of(fixed) * 4 - 1) ?
dy * dx_old / dx_den :
(INCR_EXPR(mq_cross), fixed_mult_quo(dy, dx_old, dx_den)))
+ y;
if_debug3('F', "[F]cross y=%g, y_new=%g, y1=%g\n",
fixed2float(y), fixed2float(y_new),
fixed2float(y1));
if (y_new <= y) {
fixed ys, xs0, xs1, ye, xe0, xe1, dy, dx0, dx1;
INCR(cross_slow);
if (endp->start.y < alp->start.y)
ys = alp->start.y,
xs0 = AL_X_AT_Y(endp, ys), xs1 = alp->start.x;
else
ys = endp->start.y,
xs0 = endp->start.x, xs1 = AL_X_AT_Y(alp, ys);
if (endp->end.y > alp->end.y)
ye = alp->end.y,
xe0 = AL_X_AT_Y(endp, ye), xe1 = alp->end.x;
else
ye = endp->end.y,
xe0 = endp->end.x, xe1 = AL_X_AT_Y(alp, ye);
dy = ye - ys;
dx0 = xe0 - xs0;
dx1 = xe1 - xs1;
if (dx0 == dx1) {
y_new = y1;
} else {
double cross = (double)(xs0 - xs1) / (dx1 - dx0);
y_new = (fixed)(ys + cross * dy);
if (y_new <= y) {
INCR(cross_low);
y_new = y + fixed_epsilon;
}
}
}
*p_y_new = y_new;
return true;
}
private inline void
set_x_next(active_line *endp, active_line *alp, fixed x)
{
while(endp != alp) {
endp->x_next = x;
endp = endp->next;
}
}
private inline int
coord_weight(const active_line *alp)
{
return 1 + min(any_abs((int)((int64_t)alp->diff.y * 8 / alp->diff.x)), 256);
}
private void
intersect_al(line_list *ll, fixed y, fixed *y_top, int draw, bool all_bands)
{
fixed x = min_fixed, y1 = *y_top;
active_line *alp, *stopx = NULL;
active_line *endp = NULL;
if (y == y1) {
} else if (ll->fo->pseudo_rasterization || draw >= 0 || all_bands) {
for (alp = stopx = ll->x_list;
INCR_EXPR(find_y), alp != 0;
endp = alp, alp = alp->next
) {
fixed nx = AL_X_AT_Y(alp, y1);
fixed y_new;
alp->x_next = nx;
if (nx >= x)
x = nx;
else if (alp->x_current >= endp->x_current &&
intersect(endp, alp, y, y1, &y_new)) {
if (y_new <= y1) {
stopx = endp;
y1 = y_new;
if (endp->diff.x == 0)
nx = endp->start.x;
else if (alp->diff.x == 0)
nx = alp->start.x;
else {
fixed nx0 = AL_X_AT_Y(endp, y1);
fixed nx1 = AL_X_AT_Y(alp, y1);
if (nx0 != nx1) {
int64_t w0 = coord_weight(endp);
int64_t w1 = coord_weight(alp);
nx = (fixed)((w0 * nx0 + w1 * nx1) / (w0 + w1));
} else
nx = nx0;
}
endp->x_next = alp->x_next = nx;
draw = 0;
}
if (nx > x)
x = nx;
}
}
for (alp = ll->x_list; alp != stopx; alp = alp->next)
alp->x_next = AL_X_AT_Y(alp, y1);
if (ll->x_list != NULL) {
for (;;) {
fixed x1;
double sx = 0xbaadf00d;
int k = 0xbaadf00d, n;
endp = ll->x_list;
x1 = endp->x_next;
for (alp = endp->next; alp != NULL; x1 = alp->x_next, alp = alp->next)
if (alp->x_next < x1)
break;
if (alp == NULL)
break;
x1 = endp->x_next;
n = 0;
for (alp = endp->next; alp != NULL; alp = alp->next) {
x = alp->x_next;
if (x < x1) {
if (n == 0) {
if (endp->diff.x == 0) {
k = -1;
sx = x1;
} else {
k = coord_weight(endp);
sx = (double)x1 * k;
}
n = 1;
}
n++;
if (alp->diff.x == 0) {
if (k <= -1) {
sx += x;
k--;
} else {
k = -1;
sx = x;
}
} else if (k > 0) {
int w = coord_weight(alp);
sx += (double)x * w;
k += w;
}
} else {
if (n > 1) {
k = any_abs(k);
set_x_next(endp, alp, (fixed)((sx + k / 2) / k));
}
x1 = alp->x_next;
n = 0;
endp = alp;
}
}
if (n > 1) {
k = any_abs(k);
set_x_next(endp, alp, (fixed)((sx + k / 2) / k));
}
}
}
} else {
for (alp = ll->x_list; alp != stopx; alp = alp->next)
alp->x_next = AL_X_AT_Y(alp, y1);
}
#ifdef DEBUG
if (gs_debug_c('F')) {
dlprintf1("[F]after loop: y1=%f\n", fixed2float(y1));
print_line_list(ll->x_list);
}
#endif
*y_top = y1;
}
#define IS_SPOTAN 1
#define PSEUDO_RASTERIZATION 0
#define FILL_ADJUST 0
#define FILL_DIRECT 1
#define TEMPLATE_spot_into_trapezoids spot_into_trapezoids__spotan
#include "gxfilltr.h"
#undef IS_SPOTAN
#undef PSEUDO_RASTERIZATION
#undef FILL_ADJUST
#undef FILL_DIRECT
#undef TEMPLATE_spot_into_trapezoids
#define IS_SPOTAN 0
#define PSEUDO_RASTERIZATION 1
#define FILL_ADJUST 0
#define FILL_DIRECT 1
#define TEMPLATE_spot_into_trapezoids spot_into_trapezoids__pr_fd
#include "gxfilltr.h"
#undef IS_SPOTAN
#undef PSEUDO_RASTERIZATION
#undef FILL_ADJUST
#undef FILL_DIRECT
#undef TEMPLATE_spot_into_trapezoids
#define IS_SPOTAN 0
#define PSEUDO_RASTERIZATION 1
#define FILL_ADJUST 0
#define FILL_DIRECT 0
#define TEMPLATE_spot_into_trapezoids spot_into_trapezoids__pr_nd
#include "gxfilltr.h"
#undef IS_SPOTAN
#undef PSEUDO_RASTERIZATION
#undef FILL_ADJUST
#undef FILL_DIRECT
#undef TEMPLATE_spot_into_trapezoids
#define IS_SPOTAN 0
#define PSEUDO_RASTERIZATION 0
#define FILL_ADJUST 1
#define FILL_DIRECT 1
#define TEMPLATE_spot_into_trapezoids spot_into_trapezoids__aj_fd
#include "gxfilltr.h"
#undef IS_SPOTAN
#undef PSEUDO_RASTERIZATION
#undef FILL_ADJUST
#undef FILL_DIRECT
#undef TEMPLATE_spot_into_trapezoids
#define IS_SPOTAN 0
#define PSEUDO_RASTERIZATION 0
#define FILL_ADJUST 1
#define FILL_DIRECT 0
#define TEMPLATE_spot_into_trapezoids spot_into_trapezoids__aj_nd
#include "gxfilltr.h"
#undef IS_SPOTAN
#undef PSEUDO_RASTERIZATION
#undef FILL_ADJUST
#undef FILL_DIRECT
#undef TEMPLATE_spot_into_trapezoids
#define IS_SPOTAN 0
#define PSEUDO_RASTERIZATION 0
#define FILL_ADJUST 0
#define FILL_DIRECT 1
#define TEMPLATE_spot_into_trapezoids spot_into_trapezoids__nj_fd
#include "gxfilltr.h"
#undef IS_SPOTAN
#undef PSEUDO_RASTERIZATION
#undef FILL_ADJUST
#undef FILL_DIRECT
#undef TEMPLATE_spot_into_trapezoids
#define IS_SPOTAN 0
#define PSEUDO_RASTERIZATION 0
#define FILL_ADJUST 0
#define FILL_DIRECT 0
#define TEMPLATE_spot_into_trapezoids spot_into_trapezoids__nj_nd
#include "gxfilltr.h"
#undef IS_SPOTAN
#undef PSEUDO_RASTERIZATION
#undef FILL_ADJUST
#undef FILL_DIRECT
#undef TEMPLATE_spot_into_trapezoids
private int
spot_into_trapezoids(line_list *ll, fixed band_mask)
{
const fill_options * const fo = ll->fo;
if (fo->is_spotan)
return spot_into_trapezoids__spotan(ll, band_mask);
if (fo->pseudo_rasterization) {
if (fo->fill_direct)
return spot_into_trapezoids__pr_fd(ll, band_mask);
else
return spot_into_trapezoids__pr_nd(ll, band_mask);
}
if (fo->adjust_below | fo->adjust_above | fo->adjust_left | fo->adjust_right) {
if (fo->fill_direct)
return spot_into_trapezoids__aj_fd(ll, band_mask);
else
return spot_into_trapezoids__aj_nd(ll, band_mask);
} else {
if (fo->fill_direct)
return spot_into_trapezoids__nj_fd(ll, band_mask);
else
return spot_into_trapezoids__nj_nd(ll, band_mask);
}
}
typedef int coord_value_t;
#define MIN_COORD_VALUE min_int
#define MAX_COORD_VALUE max_int
typedef struct coord_range_s coord_range_t;
struct coord_range_s {
coord_value_t rmin, rmax;
coord_range_t *prev, *next;
coord_range_t *alloc_next;
};
gs_private_st_simple(st_coord_range, coord_range_t, "coord_range_t");
typedef struct coord_range_list_s {
gs_memory_t *memory;
struct rl_ {
coord_range_t *first, *next, *limit;
} local;
coord_range_t *allocated;
coord_range_t *freed;
coord_range_t *current;
coord_range_t first, last;
} coord_range_list_t;
private coord_range_t *
range_alloc(coord_range_list_t *pcrl)
{
coord_range_t *pcr;
if (pcrl->freed) {
pcr = pcrl->freed;
pcrl->freed = pcr->next;
} else if (pcrl->local.next < pcrl->local.limit)
pcr = pcrl->local.next++;
else {
pcr = gs_alloc_struct(pcrl->memory, coord_range_t, &st_coord_range,
"range_alloc");
if (pcr == 0)
return 0;
pcr->alloc_next = pcrl->allocated;
pcrl->allocated = pcr;
}
return pcr;
}
private void
range_delete(coord_range_list_t *pcrl, coord_range_t *pcr)
{
if_debug3('Q', "[Qr]delete 0x%lx: [%d,%d)\n", (ulong)pcr, pcr->rmin,
pcr->rmax);
pcr->prev->next = pcr->next;
pcr->next->prev = pcr->prev;
pcr->next = pcrl->freed;
pcrl->freed = pcr;
}
private void
range_list_clear(coord_range_list_t *pcrl)
{
if_debug0('Q', "[Qr]clearing\n");
pcrl->first.next = &pcrl->last;
pcrl->last.prev = &pcrl->first;
pcrl->current = &pcrl->last;
}
private void range_list_clear(coord_range_list_t *);
private void
range_list_init(coord_range_list_t *pcrl, coord_range_t *pcr_local,
int num_local, gs_memory_t *mem)
{
pcrl->memory = mem;
pcrl->local.first = pcrl->local.next = pcr_local;
pcrl->local.limit = pcr_local + num_local;
pcrl->allocated = pcrl->freed = 0;
pcrl->first.rmin = pcrl->first.rmax = MIN_COORD_VALUE;
pcrl->first.prev = 0;
pcrl->last.rmin = pcrl->last.rmax = MAX_COORD_VALUE;
pcrl->last.next = 0;
range_list_clear(pcrl);
}
private void
range_list_reset(coord_range_list_t *pcrl)
{
if (pcrl->first.next != &pcrl->last) {
pcrl->last.prev->next = pcrl->freed;
pcrl->freed = pcrl->first.next;
range_list_clear(pcrl);
}
}
inline private void
range_list_rescan(coord_range_list_t *pcrl)
{
pcrl->current = pcrl->first.next;
}
private void
range_list_free(coord_range_list_t *pcrl)
{
coord_range_t *pcr;
while ((pcr = pcrl->allocated) != 0) {
coord_range_t *next = pcr->alloc_next;
gs_free_object(pcrl->memory, pcr, "range_list_free");
pcrl->allocated = next;
}
}
private int
range_list_add(coord_range_list_t *pcrl, coord_value_t rmin, coord_value_t rmax)
{
coord_range_t *pcr = pcrl->current;
if (rmin >= rmax)
return 0;
top:
if (rmax < pcr->rmin) {
if (rmin > pcr->prev->rmax)
goto ins;
pcr = pcr->prev;
goto top;
}
if (rmin > pcr->rmax) {
pcr = pcr->next;
if (rmax < pcr->rmin)
goto ins;
goto top;
}
while (rmin <= pcr->prev->rmax) {
if (!pcr->prev->prev)
break;
pcr->rmin = pcr->prev->rmin;
range_delete(pcrl, pcr->prev);
}
while (rmax >= pcr->next->rmin) {
if (!pcr->next->next)
break;
pcr->rmax = pcr->next->rmax;
range_delete(pcrl, pcr->next);
}
if (rmin < pcr->rmin) {
if_debug3('Q', "[Qr]update 0x%lx => [%d,%d)\n", (ulong)pcr, rmin,
pcr->rmax);
pcr->rmin = rmin;
}
if (rmax > pcr->rmax) {
if_debug3('Q', "[Qr]update 0x%lx => [%d,%d)\n", (ulong)pcr, pcr->rmin,
rmax);
pcr->rmax = rmax;
}
pcrl->current = pcr->next;
return 0;
ins:
{
coord_range_t *prev = range_alloc(pcrl);
if (prev == 0)
return_error(gs_error_VMerror);
if_debug3('Q', "[Qr]insert 0x%lx: [%d,%d)\n", (ulong)prev, rmin, rmax);
prev->rmin = rmin, prev->rmax = rmax;
(prev->prev = pcr->prev)->next = prev;
prev->next = pcr;
pcr->prev = prev;
}
pcrl->current = pcr;
return 0;
}
private int
merge_ranges(coord_range_list_t *pcrl, const line_list *ll, fixed y_min, fixed y_top)
{
active_line *alp, *nlp;
int code = 0;
range_list_rescan(pcrl);
for (alp = ll->x_list; alp != 0 && code >= 0; alp = nlp) {
fixed x0 = alp->x_current, x1, xt;
nlp = alp->next;
if (alp->start.y < y_min)
continue;
if (alp->monotonic_x && alp->monotonic_y && alp->fi.y3 <= y_top) {
vd_bar(alp->start.x, alp->start.y, alp->end.x, alp->end.y, 0, RGB(255, 0, 0));
x1 = alp->fi.x3;
if (x0 > x1)
xt = x0, x0 = x1, x1 = xt;
code = range_list_add(pcrl,
fixed2int_pixround(x0 - ll->fo->adjust_left),
fixed2int_rounded(x1 + ll->fo->adjust_right));
alp->more_flattened = false;
} else {
x1 = x0;
for (;;) {
if (alp->end.y <= y_top)
xt = alp->end.x;
else
xt = AL_X_AT_Y(alp, y_top);
x0 = min(x0, xt);
x1 = max(x1, xt);
if (!alp->more_flattened || alp->end.y > y_top)
break;
step_al(alp, true);
if (alp->end.y < alp->start.y) {
remove_al(ll, alp);
break;
}
}
code = range_list_add(pcrl,
fixed2int_pixround(x0 - ll->fo->adjust_left),
fixed2int_rounded(x1 + ll->fo->adjust_right));
}
}
return code;
}
#define FILL_DIRECT 1
#define TEMPLATE_spot_into_scanlines spot_into_scan_lines_fd
#include "gxfillsl.h"
#undef FILL_DIRECT
#undef TEMPLATE_spot_into_scanlines
#define FILL_DIRECT 0
#define TEMPLATE_spot_into_scanlines spot_into_scan_lines_nd
#include "gxfillsl.h"
#undef FILL_DIRECT
#undef TEMPLATE_spot_into_scanlines
private int
spot_into_scan_lines(line_list *ll, fixed band_mask)
{
if (ll->fo->fill_direct)
return spot_into_scan_lines_fd(ll, band_mask);
else
return spot_into_scan_lines_nd(ll, band_mask);
}