#include "gx.h"
#include "gserrors.h"
#include "gsstruct.h"
#include "gxfixed.h"
#include "gzpath.h"
#include "vdtrace.h"
private int path_alloc_copy(gx_path *);
private int gx_path_new_subpath(gx_path *);
#ifdef DEBUG
private void gx_print_segment(const segment *);
#  define trace_segment(msg, pseg)\
if ( gs_debug_c('P') ) dlprintf(msg), gx_print_segment(pseg);
#else
#  define trace_segment(msg, pseg) DO_NOTHING
#endif
#define outside_bbox(ppath, px, py)\
(px < ppath->bbox.p.x || px > ppath->bbox.q.x ||\
py < ppath->bbox.p.y || py > ppath->bbox.q.y)
#define check_in_bbox(ppath, px, py)\
if ( outside_bbox(ppath, px, py) )\
return_error(gs_error_rangecheck)
public_st_path();
private_st_path_segments();
private_st_segment();
private_st_line();
private_st_line_close();
private_st_curve();
private_st_subpath();
private rc_free_proc(rc_free_path_segments);
private rc_free_proc(rc_free_path_segments_local);
private int
gz_path_add_point(gx_path *, fixed, fixed),
gz_path_add_line_notes(gx_path *, fixed, fixed, segment_notes),
gz_path_add_curve_notes(gx_path *, fixed, fixed, fixed, fixed, fixed, fixed, segment_notes),
gz_path_close_subpath_notes(gx_path *, segment_notes);
private byte gz_path_state_flags(gx_path *ppath, byte flags);
private gx_path_procs default_path_procs = {
gz_path_add_point,
gz_path_add_line_notes,
gz_path_add_curve_notes,
gz_path_close_subpath_notes,
gz_path_state_flags
};
private int
gz_path_bbox_add_point(gx_path *, fixed, fixed),
gz_path_bbox_add_line_notes(gx_path *, fixed, fixed, segment_notes),
gz_path_bbox_add_curve_notes(gx_path *, fixed, fixed, fixed, fixed, fixed, fixed, segment_notes),
gz_path_bbox_close_subpath_notes(gx_path *, segment_notes);
private gx_path_procs path_bbox_procs = {
gz_path_bbox_add_point,
gz_path_bbox_add_line_notes,
gz_path_bbox_add_curve_notes,
gz_path_bbox_close_subpath_notes,
gz_path_state_flags
};
private void
gx_path_init_contents(gx_path * ppath)
{
ppath->box_last = 0;
ppath->first_subpath = ppath->current_subpath = 0;
ppath->subpath_count = 0;
ppath->curve_count = 0;
path_update_newpath(ppath);
ppath->bbox_set = 0;
ppath->bbox_accurate = 0;
}
private int
path_alloc_segments(gx_path_segments ** ppsegs, gs_memory_t * mem,
client_name_t cname)
{
mem = gs_memory_stable(mem);
rc_alloc_struct_1(*ppsegs, gx_path_segments, &st_path_segments,
mem, return_error(gs_error_VMerror), cname);
(*ppsegs)->rc.free = rc_free_path_segments;
return 0;
}
int
gx_path_init_contained_shared(gx_path * ppath, const gx_path * shared,
gs_memory_t * mem, client_name_t cname)
{
if (shared) {
if (shared->segments == &shared->local_segments) {
lprintf1("Attempt to share (local) segments of path 0x%lx!\n",
(ulong) shared);
return_error(gs_error_Fatal);
}
*ppath = *shared;
rc_increment(ppath->segments);
} else {
int code = path_alloc_segments(&ppath->segments, mem, cname);
if (code < 0)
return code;
gx_path_init_contents(ppath);
}
ppath->memory = mem;
ppath->allocation = path_allocated_contained;
ppath->procs = &default_path_procs;
return 0;
}
gx_path *
gx_path_alloc_shared(const gx_path * shared, gs_memory_t * mem,
client_name_t cname)
{
gx_path *ppath = gs_alloc_struct(mem, gx_path, &st_path, cname);
if (ppath == 0)
return 0;
ppath->procs = &default_path_procs;
if (shared) {
if (shared->segments == &shared->local_segments) {
lprintf1("Attempt to share (local) segments of path 0x%lx!\n",
(ulong) shared);
gs_free_object(mem, ppath, cname);
return 0;
}
*ppath = *shared;
rc_increment(ppath->segments);
} else {
int code = path_alloc_segments(&ppath->segments, mem, cname);
if (code < 0) {
gs_free_object(mem, ppath, cname);
return 0;
}
gx_path_init_contents(ppath);
}
ppath->memory = mem;
ppath->allocation = path_allocated_on_heap;
return ppath;
}
int
gx_path_init_local_shared(gx_path * ppath, const gx_path * shared,
gs_memory_t * mem)
{
if (shared) {
if (shared->segments == &shared->local_segments) {
lprintf1("Attempt to share (local) segments of path 0x%lx!\n",
(ulong) shared);
return_error(gs_error_Fatal);
}
*ppath = *shared;
rc_increment(ppath->segments);
} else {
rc_init_free(&ppath->local_segments, mem, 1,
rc_free_path_segments_local);
ppath->segments = &ppath->local_segments;
gx_path_init_contents(ppath);
}
ppath->memory = mem;
ppath->allocation = path_allocated_on_stack;
ppath->procs = &default_path_procs;
return 0;
}
void
gx_path_init_bbox_accumulator(gx_path * ppath)
{
ppath->box_last = 0;
ppath->subpath_count = 0;
ppath->curve_count = 0;
ppath->local_segments.contents.subpath_first = 0;
ppath->local_segments.contents.subpath_current = 0;
ppath->segments = 0;
path_update_newpath(ppath);
ppath->bbox.p.x = ppath->bbox.q.x = 0;
ppath->bbox.p.y = ppath->bbox.q.y = 0;
ppath->bbox_set = 0;
ppath->bbox_accurate = 1;
ppath->memory = NULL;
ppath->allocation = path_allocated_on_stack;
ppath->procs = &path_bbox_procs;
}
int
gx_path_unshare(gx_path * ppath)
{
int code = 0;
if (gx_path_is_shared(ppath))
code = path_alloc_copy(ppath);
return code;
}
void
gx_path_free(gx_path * ppath, client_name_t cname)
{
rc_decrement(ppath->segments, cname);
ppath->box_last = 0;
ppath->segments = 0;
if (ppath->allocation == path_allocated_on_heap)
gs_free_object(ppath->memory, ppath, cname);
}
int
gx_path_assign_preserve(gx_path * ppto, gx_path * ppfrom)
{
gx_path_segments *fromsegs = ppfrom->segments;
gx_path_segments *tosegs = ppto->segments;
gs_memory_t *mem = ppto->memory;
gx_path_allocation_t allocation = ppto->allocation;
if (fromsegs == &ppfrom->local_segments) {
if (tosegs == &ppto->local_segments || gx_path_is_shared(ppto)) {
int code = path_alloc_segments(&tosegs, ppto->memory,
"gx_path_assign");
if (code < 0)
return code;
rc_decrement(ppto->segments, "gx_path_assign");
} else {
rc_free_path_segments_local(tosegs->rc.memory, tosegs,
"gx_path_assign");
}
tosegs->contents = fromsegs->contents;
ppfrom->segments = tosegs;
rc_increment(tosegs);
} else {
rc_increment(fromsegs);
rc_decrement(tosegs, "gx_path_assign");
}
*ppto = *ppfrom;
ppto->memory = mem;
ppto->allocation = allocation;
return 0;
}
int
gx_path_assign_free(gx_path * ppto, gx_path * ppfrom)
{
if (ppto->segments == &ppto->local_segments &&
ppfrom->segments == &ppfrom->local_segments &&
!gx_path_is_shared(ppto)
) {
#define fromsegs (&ppfrom->local_segments)
#define tosegs (&ppto->local_segments)
gs_memory_t *mem = ppto->memory;
gx_path_allocation_t allocation = ppto->allocation;
rc_free_path_segments_local(tosegs->rc.memory, tosegs,
"gx_path_assign_free");
*ppto = *ppfrom;
rc_increment(fromsegs);
ppto->segments = tosegs;
ppto->memory = mem;
ppto->allocation = allocation;
#undef fromsegs
#undef tosegs
} else {
int code = gx_path_assign_preserve(ppto, ppfrom);
if (code < 0)
return code;
}
gx_path_free(ppfrom, "gx_path_assign_free");
return 0;
}
private void
rc_free_path_segments_local(gs_memory_t * mem, void *vpsegs,
client_name_t cname)
{
gx_path_segments *psegs = (gx_path_segments *) vpsegs;
segment *pseg;
mem = gs_memory_stable(mem);
if (psegs->contents.subpath_first == 0)
return;
pseg = (segment *) psegs->contents.subpath_current->last;
while (pseg) {
segment *prev = pseg->prev;
trace_segment("[P]release", pseg);
gs_free_object(mem, pseg, cname);
pseg = prev;
}
}
private void
rc_free_path_segments(gs_memory_t * mem, void *vpsegs, client_name_t cname)
{
rc_free_path_segments_local(mem, vpsegs, cname);
gs_free_object(mem, vpsegs, cname);
}
#define path_unshare(ppath)\
BEGIN\
if ( gx_path_is_shared(ppath) ) {\
int code_;\
if( (code_ = path_alloc_copy(ppath)) < 0 ) return code_;\
}\
END
#define path_open()\
BEGIN\
if ( !path_is_drawing(ppath) ) {\
int code_;\
if ( !path_position_valid(ppath) )\
return_error(gs_error_nocurrentpoint);\
code_ = gx_path_new_subpath(ppath);\
if ( code_ < 0 ) return code_;\
}\
END
#define path_alloc_segment(pseg,ctype,pstype,stype,snotes,cname)\
path_unshare(ppath);\
psub = ppath->current_subpath;\
if( !(pseg = gs_alloc_struct(gs_memory_stable(ppath->memory), ctype,\
pstype, cname)) )\
return_error(gs_error_VMerror);\
pseg->type = stype, pseg->notes = snotes, pseg->next = 0
#define path_alloc_link(pseg)\
{ segment *prev = psub->last;\
prev->next = (segment *)pseg;\
pseg->prev = prev;\
psub->last = (segment *)pseg;\
}
int
gx_path_new(gx_path * ppath)
{
gx_path_segments *psegs = ppath->segments;
if (gx_path_is_shared(ppath)) {
int code = path_alloc_segments(&ppath->segments, ppath->memory,
"gx_path_new");
if (code < 0)
return code;
rc_decrement(psegs, "gx_path_new");
} else {
rc_free_path_segments_local(psegs->rc.memory, psegs, "gx_path_new");
}
gx_path_init_contents(ppath);
return 0;
}
private int
gx_path_new_subpath(gx_path * ppath)
{
subpath *psub;
subpath *spp;
path_alloc_segment(spp, subpath, &st_subpath, s_start, sn_none,
"gx_path_new_subpath");
spp->last = (segment *) spp;
spp->curve_count = 0;
spp->is_closed = 0;
spp->pt = ppath->position;
if (!psub) {
ppath->first_subpath = spp;
spp->prev = 0;
} else {
segment *prev = psub->last;
prev->next = (segment *) spp;
spp->prev = prev;
}
ppath->current_subpath = spp;
ppath->subpath_count++;
trace_segment("[P]", (const segment *)spp);
return 0;
}
private inline void
gz_path_bbox_add(gx_path * ppath, fixed x, fixed y)
{
if (!ppath->bbox_set) {
ppath->bbox.p.x = ppath->bbox.q.x = x;
ppath->bbox.p.y = ppath->bbox.q.y = y;
ppath->bbox_set = 1;
} else {
if (ppath->bbox.p.x > x)
ppath->bbox.p.x = x;
if (ppath->bbox.p.y > y)
ppath->bbox.p.y = y;
if (ppath->bbox.q.x < x)
ppath->bbox.q.x = x;
if (ppath->bbox.q.y < y)
ppath->bbox.q.y = y;
}
}
private inline void
gz_path_bbox_move(gx_path * ppath, fixed x, fixed y)
{
ppath->position.x = x;
ppath->position.y = y;
ppath->state_flags |= psf_position_valid;
}
int
gx_path_add_point(gx_path * ppath, fixed x, fixed y)
{
return ppath->procs->add_point(ppath, x, y);
}
private int
gz_path_add_point(gx_path * ppath, fixed x, fixed y)
{
if (ppath->bbox_set)
check_in_bbox(ppath, x, y);
ppath->position.x = x;
ppath->position.y = y;
path_update_moveto(ppath);
return 0;
}
private int
gz_path_bbox_add_point(gx_path * ppath, fixed x, fixed y)
{
gz_path_bbox_move(ppath, x, y);
return 0;
}
int
gx_path_add_relative_point(gx_path * ppath, fixed dx, fixed dy)
{
if (!path_position_in_range(ppath))
return_error((path_position_valid(ppath) ? gs_error_limitcheck :
gs_error_nocurrentpoint));
{
fixed nx = ppath->position.x + dx, ny = ppath->position.y + dy;
if (((nx ^ dx) < 0 && (ppath->position.x ^ dx) >= 0) ||
((ny ^ dy) < 0 && (ppath->position.y ^ dy) >= 0)
)
return_error(gs_error_limitcheck);
if (ppath->bbox_set)
check_in_bbox(ppath, nx, ny);
ppath->position.x = nx;
ppath->position.y = ny;
}
path_update_moveto(ppath);
return 0;
}
#define path_set_point(pseg, fx, fy)\
(pseg)->pt.x = ppath->position.x = (fx),\
(pseg)->pt.y = ppath->position.y = (fy)
int
gx_path_add_line_notes(gx_path * ppath, fixed x, fixed y, segment_notes notes)
{
return ppath->procs->add_line(ppath, x, y, notes);
}
private int
gz_path_add_line_notes(gx_path * ppath, fixed x, fixed y, segment_notes notes)
{
subpath *psub;
line_segment *lp;
if (ppath->bbox_set)
check_in_bbox(ppath, x, y);
path_open();
path_alloc_segment(lp, line_segment, &st_line, s_line, notes,
"gx_path_add_line");
path_alloc_link(lp);
path_set_point(lp, x, y);
path_update_draw(ppath);
trace_segment("[P]", (segment *) lp);
return 0;
}
private int
gz_path_bbox_add_line_notes(gx_path * ppath, fixed x, fixed y, segment_notes notes)
{
gz_path_bbox_add(ppath, x, y);
gz_path_bbox_move(ppath, x, y);
return 0;
}
int
gx_path_add_lines_notes(gx_path *ppath, const gs_fixed_point *ppts, int count,
segment_notes notes)
{
subpath *psub;
segment *prev;
line_segment *lp = 0;
int i;
int code = 0;
if (count <= 0)
return 0;
path_unshare(ppath);
path_open();
psub = ppath->current_subpath;
prev = psub->last;
for (i = 0; i < count; i++) {
fixed x = ppts[i].x;
fixed y = ppts[i].y;
line_segment *next;
if (ppath->bbox_set && outside_bbox(ppath, x, y)) {
code = gs_note_error(gs_error_rangecheck);
break;
}
if (!(next = gs_alloc_struct(gs_memory_stable(ppath->memory),
line_segment, &st_line,
"gx_path_add_lines"))
) {
code = gs_note_error(gs_error_VMerror);
break;
}
lp = next;
lp->type = s_line;
lp->notes = notes;
prev->next = (segment *) lp;
lp->prev = prev;
lp->pt.x = x;
lp->pt.y = y;
prev = (segment *) lp;
trace_segment("[P]", (segment *) lp);
}
if (lp != 0)
ppath->position.x = lp->pt.x,
ppath->position.y = lp->pt.y,
psub->last = (segment *) lp,
lp->next = 0,
path_update_draw(ppath);
return code;
}
int
gx_path_add_rectangle(gx_path * ppath, fixed x0, fixed y0, fixed x1, fixed y1)
{
gs_fixed_point pts[3];
int code;
pts[0].x = x0;
pts[1].x = pts[2].x = x1;
pts[2].y = y0;
pts[0].y = pts[1].y = y1;
if ((code = gx_path_add_point(ppath, x0, y0)) < 0 ||
(code = gx_path_add_lines(ppath, pts, 3)) < 0 ||
(code = gx_path_close_subpath(ppath)) < 0
)
return code;
return 0;
}
int
gx_path_add_curve_notes(gx_path * ppath,
fixed x1, fixed y1, fixed x2, fixed y2, fixed x3, fixed y3,
segment_notes notes)
{
return ppath->procs->add_curve(ppath, x1, y1, x2, y2, x3, y3, notes);
}
private int
gz_path_add_curve_notes(gx_path * ppath,
fixed x1, fixed y1, fixed x2, fixed y2, fixed x3, fixed y3,
segment_notes notes)
{
subpath *psub;
curve_segment *lp;
if (ppath->bbox_set) {
check_in_bbox(ppath, x1, y1);
check_in_bbox(ppath, x2, y2);
check_in_bbox(ppath, x3, y3);
}
path_open();
path_alloc_segment(lp, curve_segment, &st_curve, s_curve, notes,
"gx_path_add_curve");
path_alloc_link(lp);
lp->p1.x = x1;
lp->p1.y = y1;
lp->p2.x = x2;
lp->p2.y = y2;
path_set_point(lp, x3, y3);
psub->curve_count++;
ppath->curve_count++;
path_update_draw(ppath);
trace_segment("[P]", (segment *) lp);
return 0;
}
private int
gz_path_bbox_add_curve_notes(gx_path * ppath,
fixed x1, fixed y1, fixed x2, fixed y2, fixed x3, fixed y3,
segment_notes notes)
{
gz_path_bbox_add(ppath, x1, y1);
gz_path_bbox_add(ppath, x2, y2);
gz_path_bbox_add(ppath, x3, y3);
gz_path_bbox_move(ppath, x3, y3);
return 0;
}
int
gx_path_add_partial_arc_notes(gx_path * ppath,
fixed x3, fixed y3, fixed xt, fixed yt, floatp fraction, segment_notes notes)
{
fixed x0 = ppath->position.x, y0 = ppath->position.y;
vd_curveto(x0 + (fixed) ((xt - x0) * fraction),
y0 + (fixed) ((yt - y0) * fraction),
x3 + (fixed) ((xt - x3) * fraction),
y3 + (fixed) ((yt - y3) * fraction),
x3, y3);
return gx_path_add_curve_notes(ppath,
x0 + (fixed) ((xt - x0) * fraction),
y0 + (fixed) ((yt - y0) * fraction),
x3 + (fixed) ((xt - x3) * fraction),
y3 + (fixed) ((yt - y3) * fraction),
x3, y3, notes | sn_from_arc);
}
int
gx_path_add_path(gx_path * ppath, gx_path * ppfrom)
{
path_unshare(ppfrom);
path_unshare(ppath);
if (ppfrom->first_subpath) {
if (ppath->first_subpath) {
subpath *psub = ppath->current_subpath;
segment *pseg = psub->last;
subpath *pfsub = ppfrom->first_subpath;
pseg->next = (segment *) pfsub;
pfsub->prev = pseg;
} else
ppath->first_subpath = ppfrom->first_subpath;
ppath->current_subpath = ppfrom->current_subpath;
ppath->subpath_count += ppfrom->subpath_count;
ppath->curve_count += ppfrom->curve_count;
}
ppath->position = ppfrom->position;
ppath->state_flags = ppfrom->state_flags;
gx_path_init_contents(ppfrom);
return 0;
}
int
gx_path_add_char_path(gx_path * to_path, gx_path * from_path,
gs_char_path_mode mode)
{
int code;
gs_fixed_rect bbox;
switch (mode) {
default:
gx_path_new(from_path);
return 0;
case cpm_charwidth: {
gs_fixed_point cpt;
code = gx_path_current_point(from_path, &cpt);
if (code < 0)
break;
return gx_path_add_point(to_path, cpt.x, cpt.y);
}
case cpm_true_charpath:
case cpm_false_charpath:
return gx_path_add_path(to_path, from_path);
case cpm_true_charboxpath:
gx_path_bbox(from_path, &bbox);
code = gx_path_add_rectangle(to_path, bbox.p.x, bbox.p.y,
bbox.q.x, bbox.q.y);
break;
case cpm_false_charboxpath:
gx_path_bbox(from_path, &bbox);
code = gx_path_add_point(to_path, bbox.p.x, bbox.p.y);
if (code >= 0)
code = gx_path_add_line(to_path, bbox.q.x, bbox.q.y);
break;
}
if (code < 0)
return code;
gx_path_new(from_path);
return 0;
}
int
gx_path_close_subpath_notes(gx_path * ppath, segment_notes notes)
{
return ppath->procs->close_subpath(ppath, notes);
}
private int
gz_path_close_subpath_notes(gx_path * ppath, segment_notes notes)
{
subpath *psub;
line_close_segment *lp;
int code;
if (!path_subpath_open(ppath))
return 0;
if (path_last_is_moveto(ppath)) {
code = gx_path_new_subpath(ppath);
if (code < 0)
return code;
}
path_alloc_segment(lp, line_close_segment, &st_line_close,
s_line_close, notes, "gx_path_close_subpath");
path_alloc_link(lp);
path_set_point(lp, psub->pt.x, psub->pt.y);
lp->sub = psub;
psub->is_closed = 1;
path_update_closepath(ppath);
trace_segment("[P]", (segment *) lp);
return 0;
}
private int
gz_path_bbox_close_subpath_notes(gx_path * ppath, segment_notes notes)
{
return 0;
}
byte
gz_path_state_flags(gx_path *ppath, byte flags)
{
byte flags_old = ppath->state_flags;
ppath->state_flags = flags;
return flags_old;
}
byte
gx_path_get_state_flags(gx_path *ppath)
{
byte flags = ppath->procs->state_flags(ppath, 0);
ppath->procs->state_flags(ppath, flags);
return flags;
}
void
gx_path_set_state_flags(gx_path *ppath, byte flags)
{
ppath->procs->state_flags(ppath, flags);
}
bool
gx_path_is_drawing(gx_path *ppath)
{
return path_is_drawing(ppath);
}
int
gx_path_pop_close_notes(gx_path * ppath, segment_notes notes)
{
subpath *psub = ppath->current_subpath;
segment *pseg;
segment *prev;
if (psub == 0 || (pseg = psub->last) == 0 ||
pseg->type != s_line
)
return_error(gs_error_unknownerror);
prev = pseg->prev;
prev->next = 0;
psub->last = prev;
gs_free_object(ppath->memory, pseg, "gx_path_pop_close_subpath");
return gx_path_close_subpath_notes(ppath, notes);
}
private int
path_alloc_copy(gx_path * ppath)
{
gx_path path_new;
int code;
gx_path_init_local(&path_new, ppath->memory);
code = gx_path_copy(ppath, &path_new);
if (code < 0) {
gx_path_free(&path_new, "path_alloc_copy error");
return code;
}
return gx_path_assign_free(ppath, &path_new);
}
#ifdef DEBUG
void
gx_dump_path(const gx_path * ppath, const char *tag)
{
dlprintf2("[P]Path 0x%lx %s:\n", (ulong) ppath, tag);
gx_path_print(ppath);
}
void
gx_path_print(const gx_path * ppath)
{
const segment *pseg = (const segment *)ppath->first_subpath;
dlprintf5("   state_flags=%d subpaths=%d, curves=%d, point=(%f,%f)\n",
ppath->state_flags, ppath->subpath_count, ppath->curve_count,
fixed2float(ppath->position.x),
fixed2float(ppath->position.y));
dlprintf5("   box=(%f,%f),(%f,%f) last=0x%lx\n",
fixed2float(ppath->bbox.p.x), fixed2float(ppath->bbox.p.y),
fixed2float(ppath->bbox.q.x), fixed2float(ppath->bbox.q.y),
(ulong) ppath->box_last);
dlprintf4("   segments=0x%lx (refct=%ld, first=0x%lx, current=0x%lx)\n",
(ulong) ppath->segments, (long)ppath->segments->rc.ref_count,
(ulong) ppath->segments->contents.subpath_first,
(ulong) ppath->segments->contents.subpath_current);
while (pseg) {
dlputs("");
gx_print_segment(pseg);
pseg = pseg->next;
}
}
private void
gx_print_segment(const segment * pseg)
{
double px = fixed2float(pseg->pt.x);
double py = fixed2float(pseg->pt.y);
char out[80];
sprintf(out, "   0x%lx<0x%lx,0x%lx>:%u",
(ulong) pseg, (ulong) pseg->prev, (ulong) pseg->next, pseg->notes);
switch (pseg->type) {
case s_start:{
const subpath *const psub = (const subpath *)pseg;
dprintf5("%s: %1.4f %1.4f moveto\t%% #curves=%d last=0x%lx\n",
out, px, py, psub->curve_count, (ulong) psub->last);
break;
}
case s_curve:{
const curve_segment *const pcur = (const curve_segment *)pseg;
dprintf7("%s: %1.4f %1.4f %1.4f %1.4f %1.4f %1.4f curveto\n",
out, fixed2float(pcur->p1.x), fixed2float(pcur->p1.y),
fixed2float(pcur->p2.x), fixed2float(pcur->p2.y), px, py);
break;
}
case s_line:
dprintf3("%s: %1.4f %1.4f lineto\n", out, px, py);
break;
case s_line_close:{
const line_close_segment *const plc =
(const line_close_segment *)pseg;
dprintf4("%s: closepath\t%% %1.4f %1.4f 0x%lx\n",
out, px, py, (ulong) (plc->sub));
break;
}
default:
dprintf4("%s: %1.4f %1.4f <type 0x%x>\n", out, px, py, pseg->type);
}
}
#endif