#include "math_.h"
#include "gx.h"
#include "gserrors.h"
#include "gspath.h"
#include "gsstruct.h"
#include "gxfixed.h"
#include "gxarith.h"
#include "gzpath.h"
public_st_path_enum();
int
gx_path_current_point(const gx_path * ppath, gs_fixed_point * ppt)
{
if (!path_position_valid(ppath))
return_error(gs_error_nocurrentpoint);
ppt->x = ppath->position.x, ppt->y = ppath->position.y;
return 0;
}
int
gx_path_subpath_start_point(const gx_path * ppath, gs_fixed_point * ppt)
{
const subpath *psub = ppath->current_subpath;
if (!psub)
return_error(gs_error_nocurrentpoint);
*ppt = psub->pt;
return 0;
}
int
gx_path_bbox(gx_path * ppath, gs_fixed_rect * pbox)
{
if (ppath->bbox_accurate) {
*pbox = ppath->bbox;
return 0;
}
if (ppath->first_subpath == 0) {
int code = gx_path_current_point(ppath, &pbox->p);
if (code < 0) {
pbox->p.x = pbox->p.y = 0;
}
pbox->q = pbox->p;
return code;
}
if (ppath->box_last == ppath->current_subpath->last) {
*pbox = ppath->bbox;
} else {
fixed px, py, qx, qy;
const segment *pseg = ppath->box_last;
if (pseg == 0) {
pseg = (const segment *)ppath->first_subpath;
px = qx = pseg->pt.x;
py = qy = pseg->pt.y;
} else {
px = ppath->bbox.p.x, py = ppath->bbox.p.y;
qx = ppath->bbox.q.x, qy = ppath->bbox.q.y;
}
#define ADJUST_BBOX(pt)\
if ((pt).x < px) px = (pt).x;\
else if ((pt).x > qx) qx = (pt).x;\
if ((pt).y < py) py = (pt).y;\
else if ((pt).y > qy) qy = (pt).y
while ((pseg = pseg->next) != 0) {
switch (pseg->type) {
case s_curve:
ADJUST_BBOX(((const curve_segment *)pseg)->p1);
ADJUST_BBOX(((const curve_segment *)pseg)->p2);
default:
ADJUST_BBOX(pseg->pt);
}
}
#undef ADJUST_BBOX
#define STORE_BBOX(b)\
(b).p.x = px, (b).p.y = py, (b).q.x = qx, (b).q.y = qy;
STORE_BBOX(*pbox);
STORE_BBOX(ppath->bbox);
#undef STORE_BBOX
ppath->box_last = ppath->current_subpath->last;
}
return 0;
}
int
gx_path_bbox_set(gx_path * ppath, gs_fixed_rect * pbox)
{
if (ppath->bbox_set) {
*pbox = ppath->bbox;
return 0;
} else
return gx_path_bbox(ppath, pbox);
}
#undef gx_path_has_curves
bool
gx_path_has_curves(const gx_path * ppath)
{
return gx_path_has_curves_inline(ppath);
}
#define gx_path_has_curves(ppath)\
gx_path_has_curves_inline(ppath)
#undef gx_path_is_void
bool
gx_path_is_void(const gx_path * ppath)
{
return gx_path_is_void_inline(ppath);
}
#define gx_path_is_void(ppath)\
gx_path_is_void_inline(ppath)
bool
gx_path_is_null(const gx_path * ppath)
{
return gx_path_is_null_inline(ppath);
}
gx_path_rectangular_type
gx_subpath_is_rectangular(const subpath * pseg0, gs_fixed_rect * pbox,
const subpath ** ppnext)
{
const segment *pseg1, *pseg2, *pseg3, *pseg4;
gx_path_rectangular_type type;
if (pseg0->curve_count == 0 &&
(pseg1 = pseg0->next) != 0 &&
(pseg2 = pseg1->next) != 0 &&
(pseg3 = pseg2->next) != 0
) {
if ((pseg4 = pseg3->next) == 0 || pseg4->type == s_start)
type = prt_open;
else if (pseg4->type != s_line)
type = prt_closed;
else if (pseg4->pt.x != pseg0->pt.x ||
pseg4->pt.y != pseg0->pt.y
)
return prt_none;
else if (pseg4->next == 0 || pseg4->next->type == s_start)
type = prt_fake_closed;
else if (pseg4->next->type != s_line)
type = prt_closed;
else
return prt_none;
{
fixed x0 = pseg0->pt.x, y0 = pseg0->pt.y;
fixed x2 = pseg2->pt.x, y2 = pseg2->pt.y;
if ((x0 == pseg1->pt.x && pseg1->pt.y == y2 &&
x2 == pseg3->pt.x && pseg3->pt.y == y0) ||
(x0 == pseg3->pt.x && pseg3->pt.y == y2 &&
x2 == pseg1->pt.x && pseg1->pt.y == y0)
) {
if (x0 < x2)
pbox->p.x = x0, pbox->q.x = x2;
else
pbox->p.x = x2, pbox->q.x = x0;
if (y0 < y2)
pbox->p.y = y0, pbox->q.y = y2;
else
pbox->p.y = y2, pbox->q.y = y0;
while (pseg4 != 0 && pseg4->type != s_start)
pseg4 = pseg4->next;
*ppnext = (const subpath *)pseg4;
return type;
}
}
}
return prt_none;
}
gx_path_rectangular_type
gx_path_is_rectangular(const gx_path * ppath, gs_fixed_rect * pbox)
{
const subpath *pnext;
return
(gx_path_subpath_count(ppath) == 1 ?
gx_subpath_is_rectangular(ppath->first_subpath, pbox, &pnext) :
prt_none);
}
int
gx_path_translate(gx_path * ppath, fixed dx, fixed dy)
{
segment *pseg;
#define update_xy(pt)\
pt.x += dx, pt.y += dy
if (ppath->box_last != 0) {
update_xy(ppath->bbox.p);
update_xy(ppath->bbox.q);
}
if (path_position_valid(ppath))
update_xy(ppath->position);
for (pseg = (segment *) (ppath->first_subpath); pseg != 0;
pseg = pseg->next
)
switch (pseg->type) {
case s_curve:
#define pcseg ((curve_segment *)pseg)
update_xy(pcseg->p1);
update_xy(pcseg->p2);
#undef pcseg
default:
update_xy(pseg->pt);
}
#undef update_xy
return 0;
}
void
gx_point_scale_exp2(gs_fixed_point * pt, int sx, int sy)
{
if (sx >= 0)
pt->x <<= sx;
else
pt->x >>= -sx;
if (sy >= 0)
pt->y <<= sy;
else
pt->y >>= -sy;
}
void
gx_rect_scale_exp2(gs_fixed_rect * pr, int sx, int sy)
{
gx_point_scale_exp2(&pr->p, sx, sy);
gx_point_scale_exp2(&pr->q, sx, sy);
}
int
gx_path_scale_exp2_shared(gx_path * ppath, int log2_scale_x, int log2_scale_y,
bool segments_shared)
{
segment *pseg;
gx_rect_scale_exp2(&ppath->bbox, log2_scale_x, log2_scale_y);
#define SCALE_XY(pt) gx_point_scale_exp2(&pt, log2_scale_x, log2_scale_y)
SCALE_XY(ppath->position);
if (!segments_shared) {
for (pseg = (segment *) (ppath->first_subpath); pseg != 0;
pseg = pseg->next
)
switch (pseg->type) {
case s_curve:
SCALE_XY(((curve_segment *)pseg)->p1);
SCALE_XY(((curve_segment *)pseg)->p2);
default:
SCALE_XY(pseg->pt);
}
}
#undef SCALE_XY
return 0;
}
int
gx_path_copy_reversed(const gx_path * ppath_old, gx_path * ppath)
{
const subpath *psub = ppath_old->first_subpath;
#ifdef DEBUG
if (gs_debug_c('P'))
gx_dump_path(ppath_old, "before reversepath");
#endif
nsp:
if (psub) {
const segment *prev = psub->last;
const segment *pseg;
segment_notes notes =
(prev == (const segment *)psub ? sn_none :
psub->next->notes);
segment_notes prev_notes;
int code;
if (!psub->is_closed) {
code = gx_path_add_point(ppath, prev->pt.x, prev->pt.y);
if (code < 0)
return code;
}
do {
pseg = prev;
prev_notes = notes;
prev = pseg->prev;
notes = pseg->notes;
prev_notes = (prev_notes & sn_not_first) |
(notes & ~sn_not_first);
switch (pseg->type) {
case s_start:
if (psub->is_closed) {
code =
gx_path_close_subpath_notes(ppath, prev_notes);
if (code < 0)
return code;
}
psub = (const subpath *)psub->last->next;
goto nsp;
case s_curve:
{
const curve_segment *pc =
(const curve_segment *)pseg;
code = gx_path_add_curve_notes(ppath,
pc->p2.x, pc->p2.y,
pc->p1.x, pc->p1.y,
prev->pt.x, prev->pt.y, prev_notes);
break;
}
case s_line:
code = gx_path_add_line_notes(ppath,
prev->pt.x, prev->pt.y, prev_notes);
break;
case s_line_close:
code = gx_path_add_point(ppath, prev->pt.x,
prev->pt.y);
break;
default:
return_error(gs_error_Fatal);
}
} while (code >= 0);
return code;
}
#undef sn_not_end
if (ppath_old->first_subpath == 0 &&
path_last_is_moveto(ppath_old)
) {
int code = gx_path_add_point(ppath, ppath_old->position.x,
ppath_old->position.y);
if (code < 0)
return code;
}
#ifdef DEBUG
if (gs_debug_c('P'))
gx_dump_path(ppath, "after reversepath");
#endif
return 0;
}
gs_path_enum *
gs_path_enum_alloc(gs_memory_t * mem, client_name_t cname)
{
return gs_alloc_struct(mem, gs_path_enum, &st_path_enum, cname);
}
int
gx_path_enum_init(gs_path_enum * penum, const gx_path * ppath)
{
penum->memory = 0;
penum->path = ppath;
penum->copied_path = 0;
penum->pseg = (const segment *)ppath->first_subpath;
penum->moveto_done = false;
penum->notes = sn_none;
return 0;
}
int
gx_path_enum_next(gs_path_enum * penum, gs_fixed_point ppts[3])
{
const segment *pseg = penum->pseg;
if (pseg == 0) {
const gx_path *ppath = penum->path;
if (path_last_is_moveto(ppath) && !penum->moveto_done) {
penum->moveto_done = true;
penum->notes = sn_none;
ppts[0] = ppath->position;
return gs_pe_moveto;
}
return 0;
}
penum->pseg = pseg->next;
penum->notes = pseg->notes;
switch (pseg->type) {
case s_start:
ppts[0] = pseg->pt;
return gs_pe_moveto;
case s_line:
ppts[0] = pseg->pt;
return gs_pe_lineto;
case s_line_close:
ppts[0] = pseg->pt;
return gs_pe_closepath;
case s_curve:
#define pcseg ((const curve_segment *)pseg)
ppts[0] = pcseg->p1;
ppts[1] = pcseg->p2;
ppts[2] = pseg->pt;
return gs_pe_curveto;
#undef pcseg
default:
lprintf1("bad type %x in gx_path_enum_next!\n", pseg->type);
return_error(gs_error_Fatal);
}
}
segment_notes
gx_path_enum_notes(const gs_path_enum * penum)
{
return penum->notes;
}
bool
gx_path_enum_backup(gs_path_enum * penum)
{
const segment *pseg = penum->pseg;
if (pseg != 0) {
if ((pseg = pseg->prev) == 0)
return false;
penum->pseg = pseg;
return true;
}
{
const gx_path *ppath = penum->path;
if (path_last_is_moveto(ppath) && penum->moveto_done) {
penum->moveto_done = false;
return true;
} {
const subpath *psub = ppath->current_subpath;
if (psub == 0)
return false;
penum->pseg = psub->last;
return true;
}
}
}