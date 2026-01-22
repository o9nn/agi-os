#include "math_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gzstate.h"
#include "gscoord.h"
#include "gsline.h"
#include "gzline.h"
#define pgs_lp gs_currentlineparams_inline(pgs)
int
gs_setlinewidth(gs_state * pgs, floatp width)
{
gx_set_line_width(pgs_lp, width);
return 0;
}
float
gs_currentlinewidth(const gs_state * pgs)
{
return gx_current_line_width(pgs_lp);
}
int
gs_setlinecap(gs_state * pgs, gs_line_cap cap)
{
if ((uint) cap > gs_line_cap_max)
return_error(gs_error_rangecheck);
pgs_lp->cap = cap;
return 0;
}
gs_line_cap
gs_currentlinecap(const gs_state * pgs)
{
return pgs_lp->cap;
}
int
gs_setlinejoin(gs_state * pgs, gs_line_join join)
{
if ((uint) join > gs_line_join_max)
return_error(gs_error_rangecheck);
pgs_lp->join = join;
return 0;
}
gs_line_join
gs_currentlinejoin(const gs_state * pgs)
{
return pgs_lp->join;
}
int
gx_set_miter_limit(gx_line_params * plp, floatp limit)
{
if (limit < 1.0)
return_error(gs_error_rangecheck);
plp->miter_limit = limit;
{
double limit_squared = limit * limit;
if (limit_squared < 2.0001 && limit_squared > 1.9999)
plp->miter_check = 1.0e6;
else
plp->miter_check =
sqrt(limit_squared - 1) * 2 / (limit_squared - 2);
}
return 0;
}
int
gs_setmiterlimit(gs_state * pgs, floatp limit)
{
return gx_set_miter_limit(pgs_lp, limit);
}
float
gs_currentmiterlimit(const gs_state * pgs)
{
return pgs_lp->miter_limit;
}
int
gx_set_dash(gx_dash_params * dash, const float *pattern, uint length,
floatp offset, gs_memory_t * mem)
{
uint n = length;
const float *dfrom = pattern;
bool ink = true;
int index = 0;
float pattern_length = 0.0;
float dist_left;
float *ppat = dash->pattern;
while (n--) {
float elt = *dfrom++;
if (elt < 0)
return_error(gs_error_rangecheck);
pattern_length += elt;
}
if (length == 0) {
dist_left = 0.0;
if (mem && ppat) {
gs_free_object(mem, ppat, "gx_set_dash(old pattern)");
ppat = 0;
}
} else {
uint size = length * sizeof(float);
if (pattern_length == 0)
return_error(gs_error_rangecheck);
#define f_mod(a, b) ((a) - floor((a) / (b)) * (b))
if (length & 1) {
float length2 = pattern_length * 2;
dist_left = f_mod(offset, length2);
if (dist_left >= pattern_length)
dist_left -= pattern_length, ink = !ink;
} else
dist_left = f_mod(offset, pattern_length);
while ((dist_left -= pattern[index]) >= 0 &&
(dist_left > 0 || pattern[index] != 0)
)
ink = !ink, index++;
if (mem) {
if (ppat == 0)
ppat = (float *)gs_alloc_bytes(mem, size,
"gx_set_dash(pattern)");
else if (length != dash->pattern_size)
ppat = gs_resize_object(mem, ppat, size,
"gx_set_dash(pattern)");
if (ppat == 0)
return_error(gs_error_VMerror);
}
memcpy(ppat, pattern, length * sizeof(float));
}
dash->pattern = ppat;
dash->pattern_size = length;
dash->offset = offset;
dash->pattern_length = pattern_length;
dash->init_ink_on = ink;
dash->init_index = index;
dash->init_dist_left = -dist_left;
return 0;
}
int
gs_setdash(gs_state * pgs, const float *pattern, uint length, floatp offset)
{
return gx_set_dash(&pgs_lp->dash, pattern, length, offset,
pgs->memory);
}
uint
gs_currentdash_length(const gs_state * pgs)
{
return pgs_lp->dash.pattern_size;
}
const float *
gs_currentdash_pattern(const gs_state * pgs)
{
return pgs_lp->dash.pattern;
}
float
gs_currentdash_offset(const gs_state * pgs)
{
return pgs_lp->dash.offset;
}
const gx_line_params *
gs_currentlineparams(const gs_imager_state * pis)
{
return gs_currentlineparams_inline(pis);
}
int
gs_imager_setflat(gs_imager_state * pis, floatp flat)
{
if (flat <= 0.2)
flat = 0.2;
else if (flat > 100)
flat = 100;
pis->flatness = flat;
return 0;
}
int
gs_setflat(gs_state * pgs, floatp flat)
{
return gs_imager_setflat((gs_imager_state *) pgs, flat);
}
float
gs_currentflat(const gs_state * pgs)
{
return pgs->flatness;
}
int
gs_setstrokeadjust(gs_state * pgs, bool stroke_adjust)
{
pgs->stroke_adjust = stroke_adjust;
return 0;
}
bool
gs_currentstrokeadjust(const gs_state * pgs)
{
return pgs->stroke_adjust;
}
void
gs_setdashadapt(gs_state * pgs, bool adapt)
{
pgs_lp->dash.adapt = adapt;
}
bool
gs_imager_currentdashadapt(const gs_imager_state * pis)
{
return gs_currentlineparams_inline(pis)->dash.adapt;
}
bool
gs_currentdashadapt(const gs_state * pgs)
{
return gs_imager_currentdashadapt((const gs_imager_state *)pgs);
}
int
gs_setcurvejoin(gs_state * pgs, int join)
{
if (join < -1 || join > gs_line_join_max)
return_error(gs_error_rangecheck);
pgs_lp->curve_join = join;
return 0;
}
int
gs_currentcurvejoin(const gs_state * pgs)
{
return pgs_lp->curve_join;
}
void
gs_setaccuratecurves(gs_state * pgs, bool accurate)
{
pgs->accurate_curves = accurate;
}
bool
gs_imager_currentaccuratecurves(const gs_imager_state * pis)
{
return pis->accurate_curves;
}
bool
gs_currentaccuratecurves(const gs_state * pgs)
{
return gs_imager_currentaccuratecurves((const gs_imager_state *)pgs);
}
int
gx_set_dot_length(gx_line_params * plp, floatp length, bool absolute)
{
if (length < 0)
return_error(gs_error_rangecheck);
plp->dot_length = length;
plp->dot_length_absolute = absolute;
return 0;
}
int
gs_setdotlength(gs_state * pgs, floatp length, bool absolute)
{
return gx_set_dot_length(pgs_lp, length, absolute);
}
float
gs_currentdotlength(const gs_state * pgs)
{
return pgs_lp->dot_length;
}
bool
gs_currentdotlength_absolute(const gs_state * pgs)
{
return pgs_lp->dot_length_absolute;
}
int
gs_setdotorientation(gs_state *pgs)
{
if (is_xxyy(&pgs->ctm) || is_xyyx(&pgs->ctm))
return gs_currentmatrix(pgs, &pgs_lp->dot_orientation);
return_error(gs_error_rangecheck);
}
int
gs_dotorientation(gs_state *pgs)
{
return gs_setmatrix(pgs, &pgs_lp->dot_orientation);
}