#include <assert.h>
#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gzpath.h"
#include "gxfixed.h"
#include "gxdevice.h"
#include "gxdcolor.h"
#include "gxfdrop.h"
#include "gxfill.h"
#include "vdtrace.h"
#define INTERTRAP_STEM_BUG 0
gs_private_st_simple(st_margin, margin, "margin");
gs_public_st_simple(st_section, section, "section");
void init_section(section *sect, int i0, int i1)
{ int i;
for (i = i0; i < i1; i++) {
# if ADJUST_SERIF && CHECK_SPOT_CONTIGUITY
sect[i].x0 = fixed_1;
sect[i].x1 = 0;
# endif
sect[i].y0 = sect[i].y1 = -1;
}
}
private margin * alloc_margin(line_list * ll)
{ margin *m;
assert(ll->fo->pseudo_rasterization);
if (ll->free_margin_list != 0) {
m = ll->free_margin_list;
ll->free_margin_list = ll->free_margin_list->next;
} else if (ll->local_margin_alloc_count < MAX_LOCAL_ACTIVE) {
m = ll->local_margins + ll->local_margin_alloc_count;
++ ll->local_margin_alloc_count;
} else {
m = gs_alloc_struct(ll->memory, margin, &st_margin, "filling contiguity margin");
}
return m;
}
private void release_margin_list(line_list * ll, margin_set *ms)
{ margin * m1 = ms->margin_list;
if (m1 == 0)
return;
while (m1->next != 0)
m1 = m1->next;
m1->next = ll->free_margin_list;
ll->free_margin_list = ms->margin_list;
ms->margin_list = ms->margin_touched = 0;
}
void free_all_margins(line_list * ll)
{ margin * m = ll->free_margin_list;
ll->free_margin_list = 0;
while (m != 0) {
margin * m1 = m->next;
if (m < ll->local_margins || m >= ll->local_margins + MAX_LOCAL_ACTIVE)
gs_free_object(ll->memory, m, "filling contiguity margin");
m = m1;
}
}
private int store_margin(line_list * ll, margin_set * set, int ii0, int ii1)
{
int i0 = ii0, i1 = ii1;
margin *m0 = set->margin_touched, *m1;
assert(ii0 >= 0 && ii1 <= ll->bbox_width);
set->margin_touched = 0;
if (m0 != 0) {
margin *m_last = m0, *mb, *me;
assert(set->margin_list != 0);
if (i1 < m0->ibeg) {
do {
m0 = m0->prev;
} while (m0 != 0 && i0 <= m0->iend);
m1 = (m0 == 0 ? set->margin_list : m0)->next;
while (m1 != 0 && m1->ibeg <= i1) {
m_last = m1;
m1 = m1->next;
}
} else if (i0 > m0->iend) {
m1 = m0;
do {
m_last = m1;
m1 = m1->next;
} while (m1 != 0 && i1 >= m1->ibeg);
m0 = (m1 == 0 ? m_last : m1->prev);
while (m0 != 0 && m0->iend >= i0)
m0 = m0->prev;
} else {
m1 = m0;
while (m1 != 0 && m1->ibeg <= i1) {
m_last = m1;
m1 = m1->next;
}
while (m0 != 0 && m0->iend >= i0)
m0 = m0->prev;
}
mb = (m0 == 0 ? set->margin_list : m0->next);
if (mb != 0 && mb != m1) {
me = (m1 == 0 ? m_last : m1->prev);
if (me != 0) {
if (mb != me && me->prev != 0) {
margin *mf = me->prev;
if (mb->prev != 0)
mb->prev->next = mf->next;
if (mf->next != 0)
mf->next->prev = mb->prev;
if (set->margin_list == mb)
set->margin_list = mf->next;
mf->next = ll->free_margin_list;
ll->free_margin_list = mb;
i0 = min(i0, mb->ibeg);
i1 = max(i1, mf->iend);
}
}
}
me = (m0 == 0 ? set->margin_list : m0->next);
if (me == 0)
m0 = m0;
else if (me->iend < i0)
m0 = me;
else if (me->ibeg > i1)
m0 = me->prev;
else if (me->iend >= i0 && me->ibeg <= i1) {
me->ibeg = min(i0, me->ibeg);
me->iend = max(i1, me->iend);
set->margin_touched = me;
return 0;
}
}
m1 = alloc_margin(ll);
if (m1 == 0)
return_error(gs_error_VMerror);
if (m0 != 0) {
m1->next = m0->next;
m1->prev = m0;
m0->next = m1;
if (m1->next!= 0)
m1->next->prev = m1;
} else {
m1->next = set->margin_list;
m1->prev = 0;
if (set->margin_list != 0)
set->margin_list->prev = m1;
set->margin_list = m1;
}
m1->ibeg = i0;
m1->iend = i1;
set->margin_touched = m1;
return 0;
}
private inline int to_interval(int x, int l, int u)
{ return x < l ? l : x > u ? u : x;
}
private inline fixed Y_AT_X(active_line *alp, fixed xp)
{ return alp->start.y + fixed_mult_quo(xp - alp->start.x, alp->diff.y, alp->diff.x);
}
private int margin_boundary(line_list * ll, margin_set * set, active_line * alp,
fixed xx0, fixed xx1, fixed yy0, fixed yy1, int dir, fixed y0, fixed y1)
{ section *sect = set->sect;
fixed x0, x1, xmin, xmax;
int xp0, xp;
int i0, i;
# if !CHECK_SPOT_CONTIGUITY
int i1;
# endif
if (yy0 > yy1)
return 0;
if (alp == 0)
x0 = xx0, x1 = xx1;
else {
x0 = (yy0 == y0 ? alp->x_current : AL_X_AT_Y(alp, yy0));
x1 = (yy1 == y1 ? alp->x_next : AL_X_AT_Y(alp, yy1));
}
xmin = min(x0, x1);
xmax = max(x0, x1);
# if !CHECK_SPOT_CONTIGUITY
xp0 = fixed_floor(xmin) + fixed_half;
i0 = fixed2int(xp0) - ll->bbox_left;
if (xp0 < xmin) {
xp0 += fixed_1;
i0++;
}
assert(i0 >= 0);
for (i = i0, xp = xp0; xp < xmax && i < ll->bbox_width; xp += fixed_1, i++) {
fixed y = (alp == 0 ? yy0 : Y_AT_X(alp, xp));
fixed dy = y - set->y;
bool ud;
short *b, h;
section *s = &sect[i];
if (dy < 0)
dy = 0;
if (dy >= fixed_1)
dy = fixed_1;
vd_circle(xp, y, 2, 0);
ud = (alp == 0 ? (dir > 0) : ((alp->start.x - alp->end.x) * dir > 0));
b = (ud ? &s->y0 : &s->y1);
h = (short)dy;
if (*b == -1 || (*b != -2 && ( ud ? *b > h : *b < h)))
*b = h;
}
# else
xp0 = fixed_floor(xmin) + fixed_half;
i0 = fixed2int(xp0) - ll->bbox_left;
if (xp0 < xmin) {
i0++;
xp0 += fixed_1;
}
for (i = i0, xp = xp0; xp < xmax; xp += fixed_1, i++) {
section *s = &sect[i];
fixed y = (alp==0 ? yy0 : Y_AT_X(alp, xp));
fixed dy = y - set->y;
bool ud;
short *b, h;
if (dy < 0)
dy = 0;
if (dy >= fixed_1)
dy = fixed_1;
vd_circle(xp, y, 2, 0);
ud = (alp == 0 ? (dir > 0) : ((alp->start.x - alp->end.x) * dir > 0));
b = (ud ? &s->y0 : &s->y1);
h = (short)dy;
if (*b == -1 || (*b != -2 && ( ud ? *b > h : *b < h)))
*b = h;
}
assert(i0 >= 0 && i <= ll->bbox_width);
# endif
if (i > i0)
return store_margin(ll, set, i0, i);
return 0;
}
int continue_margin_common(line_list * ll, margin_set * set, active_line * flp, active_line * alp, fixed y0, fixed y1)
{ int code;
# if ADJUST_SERIF
section *sect = set->sect;
fixed yy0 = max(max(y0, alp->start.y), set->y);
fixed yy1 = min(min(y1, alp->end.y), set->y + fixed_1);
if (yy0 <= yy1) {
fixed x00 = (yy0 == y0 ? flp->x_current : AL_X_AT_Y(flp, yy0));
fixed x10 = (yy0 == y0 ? alp->x_current : AL_X_AT_Y(alp, yy0));
fixed x01 = (yy1 == y1 ? flp->x_next : AL_X_AT_Y(flp, yy1));
fixed x11 = (yy1 == y1 ? alp->x_next : AL_X_AT_Y(alp, yy1));
fixed xmin = min(x00, x01), xmax = max(x10, x11);
int i0 = fixed2int(xmin) - ll->bbox_left, i;
int i1 = fixed2int_ceiling(xmax) - ll->bbox_left;
for (i = i0; i < i1; i++) {
section *s = &sect[i];
int x_pixel = int2fixed(i + ll->bbox_left);
int xl = max(xmin - x_pixel, 0);
int xu = min(xmax - x_pixel, fixed_1);
s->x0 = min(s->x0, xl);
s->x1 = max(s->x1, xu);
x_pixel+=0;
}
code = store_margin(ll, set, i0, i1);
if (code < 0)
return code;
}
# endif
code = margin_boundary(ll, set, flp, 0, 0, yy0, yy1, 1, y0, y1);
if (code < 0)
return code;
return margin_boundary(ll, set, alp, 0, 0, yy0, yy1, -1, y0, y1);
}
private inline int mark_margin_interior(line_list * ll, margin_set * set, active_line * flp, active_line * alp, fixed y, fixed y0, fixed y1)
{
section *sect = set->sect;
fixed x0 = (y == y0 ? flp->x_current : y == y1 ? flp->x_next : AL_X_AT_Y(flp, y));
fixed x1 = (y == y0 ? alp->x_current : y == y1 ? alp->x_next : AL_X_AT_Y(alp, y));
int i0 = fixed2int(x0), ii0, ii1, i, code;
if (int2fixed(i0) + fixed_half < x0)
i0++;
ii0 = i0 - ll->bbox_left;
ii1 = fixed2int_var_pixround(x1) - ll->bbox_left;
if (ii0 < ii1) {
assert(ii0 >= 0 && ii1 <= ll->bbox_width);
for (i = ii0; i < ii1; i++) {
sect[i].y0 = sect[i].y1 = -2;
vd_circle(int2fixed(i + ll->bbox_left) + fixed_half, y, 3, RGB(255, 0, 0));
}
code = store_margin(ll, set, ii0, ii1);
if (code < 0)
return code;
}
return 0;
}
int margin_interior(line_list * ll, active_line * flp, active_line * alp, fixed y0, fixed y1)
{ int code;
fixed yy0, yy1;
yy0 = ll->margin_set0.y;
if (y0 <= yy0 && yy0 <= y1) {
code = mark_margin_interior(ll, &ll->margin_set0, flp, alp, yy0, y0, y1);
if (code < 0)
return code;
}
yy1 = ll->margin_set1.y + fixed_1;
if (y0 <= yy1 && yy1 <= y1) {
code = mark_margin_interior(ll, &ll->margin_set1, flp, alp, yy1, y0, y1);
if (code < 0)
return code;
}
return 0;
}
private inline int process_h_sect(line_list * ll, margin_set * set, active_line * hlp0,
active_line * plp, active_line * flp, int side, fixed y0, fixed y1)
{
active_line *hlp = hlp0;
fixed y = hlp->start.y;
fixed x0 = (plp != 0 ? (y == y0 ? plp->x_current : y == y1 ? plp->x_next : AL_X_AT_Y(plp, y))
: int2fixed(ll->bbox_left));
fixed x1 = (flp != 0 ? (y == y0 ? flp->x_current : y == y1 ? flp->x_next : AL_X_AT_Y(flp, y))
: int2fixed(ll->bbox_left + ll->bbox_width));
int code;
for (; hlp != 0; hlp = hlp->next) {
fixed xx0 = max(x0, min(hlp->start.x, hlp->end.x));
fixed xx1 = min(x1, max(hlp->start.x, hlp->end.x));
if (xx0 < xx1) {
vd_bar(xx0, y, xx1, y, 1, RGB(255, 0, 255));
code = margin_boundary(ll, set, 0, xx0, xx1, y, y, side, 0, 0);
if (code < 0)
return code;
}
}
return 0;
}
private inline int process_h_side(line_list * ll, margin_set * set, active_line * hlp,
active_line * plp, active_line * flp, active_line * alp, int side, fixed y0, fixed y1)
{ if (plp != 0 || flp != 0 || (plp == 0 && flp == 0 && alp == 0)) {
int code = process_h_sect(ll, set, hlp, plp, flp, -side, y0, y1);
if (code < 0)
return code;
}
if (flp != 0 && alp != 0) {
int code = process_h_sect(ll, set, hlp, flp, alp, side, y0, y1);
if (code < 0)
return code;
}
return 0;
}
private inline int process_h_list(line_list * ll, active_line * hlp, active_line * plp,
active_line * flp, active_line * alp, int side, fixed y0, fixed y1)
{ fixed y = hlp->start.y;
if (ll->margin_set0.y <= y && y <= ll->margin_set0.y + fixed_1) {
int code = process_h_side(ll, &ll->margin_set0, hlp, plp, flp, alp, side, y0, y1);
if (code < 0)
return code;
}
if (ll->margin_set1.y <= y && y <= ll->margin_set1.y + fixed_1) {
int code = process_h_side(ll, &ll->margin_set1, hlp, plp, flp, alp, side, y0, y1);
if (code < 0)
return code;
}
return 0;
}
int process_h_lists(line_list * ll, active_line * plp, active_line * flp, active_line * alp,
fixed y0, fixed y1)
{
if (y0 == y1) {
return 0;
}
if (ll->h_list0 != 0) {
int code = process_h_list(ll, ll->h_list0, plp, flp, alp, 1, y0, y1);
if (code < 0)
return code;
}
if (ll->h_list1 != 0) {
int code = process_h_list(ll, ll->h_list1, plp, flp, alp, -1, y0, y1);
if (code < 0)
return code;
}
return 0;
}
private inline int compute_padding(section *s)
{
return (s->y0 < 0 || s->y1 < 0 ? -2 :
s->y1 < fixed_half ? 0 :
s->y0 > fixed_half ? 1 :
fixed_half - s->y0 < s->y1 - fixed_half ? 1 : 0);
}
private int fill_margin(gx_device * dev, const line_list * ll, margin_set *ms, int i0, int i1)
{
section *sect = ms->sect;
int iy = fixed2int_var_pixround(ms->y);
int i, ir, h = -2, code;
const fill_options * const fo = ll->fo;
const bool FILL_DIRECT = fo->fill_direct;
assert(i0 >= 0 && i1 <= ll->bbox_width);
ir = i0;
for (i = i0; i < i1; i++) {
int y0 = sect[i].y0, y1 = sect[i].y1, hh;
if (y0 == -1)
y0 = 0;
if (y1 == -1)
y1 = fixed_scale - 1;
hh = compute_padding(&sect[i]);
# if ADJUST_SERIF
if (hh >= 0) {
# if !CHECK_SPOT_CONTIGUITY
if (i == i0 && i + 1 < i1) {
int hhh = compute_padding(&sect[i + 1]);
hh = hhh;
} else if (i == i1 - 1 && i > i0)
hh = h;
# else
if (sect[i].x0 > 0 && sect[i].x1 == fixed_1 && i + 1 < i1) {
# if INTERTRAP_STEM_BUG
int hhh = hh;
# endif
hh = (i + 1 < i1 ? compute_padding(&sect[i + 1]) : -2);
# if INTERTRAP_STEM_BUG
if (i > i0 && i + 1 < i1 && hh == -2 &&
compute_padding(&sect[i - 1]) == -2) {
hh = hhh;
}
# endif
} else if (sect[i].x0 == 0 && sect[i].x1 < fixed_1) {
# if INTERTRAP_STEM_BUG
int hhh = hh;
# endif
hh = h;
# if INTERTRAP_STEM_BUG
if (i > i0 && i + 1 < i1 && hh == -2 &&
compute_padding(&sect[i - 1]) == -2) {
DO_NOTHING;
}
# endif
}
# endif
}
# endif
if (h != hh) {
if (h >= 0) {
VD_RECT(ir + ll->bbox_left, iy + h, i - ir, 1, VD_MARG_COLOR);
code = LOOP_FILL_RECTANGLE_DIRECT(fo, ir + ll->bbox_left, iy + h, i - ir, 1);
if (code < 0)
return code;
}
ir = i;
h = hh;
}
}
if (h >= 0) {
VD_RECT(ir + ll->bbox_left, iy + h, i - ir, 1, VD_MARG_COLOR);
code = LOOP_FILL_RECTANGLE_DIRECT(fo, ir + ll->bbox_left, iy + h, i - ir, 1);
if (code < 0)
return code;
}
init_section(sect, i0, i1);
return 0;
}
int close_margins(gx_device * dev, line_list * ll, margin_set *ms)
{ margin *m = ms->margin_list;
int code;
for (; m != 0; m = m->next) {
code = fill_margin(dev, ll, ms, m->ibeg, m->iend);
if (code < 0)
return code;
}
release_margin_list(ll, ms);
return 0;
}
int start_margin_set(gx_device * dev, line_list * ll, fixed y0)
{ int code;
fixed ym = fixed_pixround(y0) - fixed_half;
margin_set s;
if (ll->margin_set0.y == ym)
return 0;
s = ll->margin_set1;
ll->margin_set1 = ll->margin_set0;
ll->margin_set0 = s;
code = close_margins(dev, ll, &ll->margin_set0);
ll->margin_set0.y = ym;
return code;
}