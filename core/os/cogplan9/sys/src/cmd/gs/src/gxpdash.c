#include "math_.h"
#include "gx.h"
#include "gsmatrix.h"
#include "gscoord.h"
#include "gxfixed.h"
#include "gsline.h"
#include "gzline.h"
#include "gzpath.h"
private int subpath_expand_dashes(const subpath *, gx_path *,
const gs_imager_state *,
const gx_dash_params *);
int
gx_path_add_dash_expansion(const gx_path * ppath_old, gx_path * ppath,
const gs_imager_state * pis)
{
const subpath *psub;
const gx_dash_params *dash = &gs_currentlineparams(pis)->dash;
int code = 0;
if (dash->pattern_size == 0)
return gx_path_copy(ppath_old, ppath);
for (psub = ppath_old->first_subpath; psub != 0 && code >= 0;
psub = (const subpath *)psub->last->next
)
code = subpath_expand_dashes(psub, ppath, pis, dash);
return code;
}
private int
subpath_expand_dashes(const subpath * psub, gx_path * ppath,
const gs_imager_state * pis, const gx_dash_params * dash)
{
const float *pattern = dash->pattern;
int count, index;
bool ink_on;
double elt_length;
fixed x0 = psub->pt.x, y0 = psub->pt.y;
fixed x, y;
const segment *pseg;
int wrap = (dash->init_ink_on && psub->is_closed ? -1 : 0);
int drawing = wrap;
segment_notes notes = ~sn_not_first;
int code;
if ((code = gx_path_add_point(ppath, x0, y0)) < 0)
return code;
top:count = dash->pattern_size;
ink_on = dash->init_ink_on;
index = dash->init_index;
elt_length = dash->init_dist_left;
x = x0, y = y0;
pseg = (const segment *)psub;
while ((pseg = pseg->next) != 0 && pseg->type != s_start) {
fixed sx = pseg->pt.x, sy = pseg->pt.y;
fixed udx = sx - x, udy = sy - y;
double length, dx, dy;
double scale = 1;
double left;
if (!(udx | udy)) {
if (gs_currentlinecap((const gs_state *)pis) != gs_cap_round) {
continue;
}
dx = 0, dy = 0, length = 0;
} else {
gs_point d;
dx = udx, dy = udy;
gs_imager_idtransform(pis, dx, dy, &d);
length = hypot(d.x, d.y) * (1.0 / fixed_1);
if (gs_imager_currentdashadapt(pis)) {
double reps = length / dash->pattern_length;
scale = reps / ceil(reps);
count = dash->pattern_size;
ink_on = dash->init_ink_on;
index = dash->init_index;
elt_length = dash->init_dist_left * scale;
}
}
left = length;
while (left > elt_length) {
double fraction = elt_length / length;
fixed nx = x + (fixed) (dx * fraction);
fixed ny = y + (fixed) (dy * fraction);
if (ink_on) {
if (drawing >= 0)
code = gx_path_add_line_notes(ppath, nx, ny,
notes & pseg->notes);
notes |= sn_not_first;
} else {
if (drawing > 0)
return 0;
code = gx_path_add_point(ppath, nx, ny);
notes &= ~sn_not_first;
drawing = 0;
}
if (code < 0)
return code;
left -= elt_length;
ink_on = !ink_on;
if (++index == count)
index = 0;
elt_length = pattern[index] * scale;
x = nx, y = ny;
}
elt_length -= left;
on:if (ink_on) {
if (drawing >= 0) {
code =
(pseg->type == s_line_close && drawing > 0 ?
gx_path_close_subpath_notes(ppath,
notes & pseg->notes) :
gx_path_add_line_notes(ppath, sx, sy,
notes & pseg->notes));
notes |= sn_not_first;
}
} else {
code = gx_path_add_point(ppath, sx, sy);
notes &= ~sn_not_first;
if (elt_length < fixed2float(fixed_epsilon) &&
(pseg->next == 0 || pseg->next->type == s_start)
) {
if (code < 0)
return code;
elt_length = 0;
ink_on = true;
if (++index == count)
index = 0;
elt_length = pattern[index] * scale;
goto on;
}
if (drawing > 0)
return code;
drawing = 0;
}
if (code < 0)
return code;
x = sx, y = sy;
}
if (wrap && drawing <= 0) {
drawing = 1;
goto top;
}
return 0;
}