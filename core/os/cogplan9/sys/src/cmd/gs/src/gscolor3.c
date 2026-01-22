#include "gx.h"
#include "gserrors.h"
#include "gsmatrix.h"
#include "gscolor3.h"
#include "gsptype2.h"
#include "gxcolor2.h"
#include "gxcspace.h"
#include "gxdcolor.h"
#include "gxpcolor.h"
#include "gzstate.h"
#include "gzpath.h"
#include "gxpaint.h"
#include "gxshade.h"
int
gs_setsmoothness(gs_state * pgs, floatp smoothness)
{
pgs->smoothness =
(smoothness < 0 ? 0 : smoothness > 1 ? 1 : smoothness);
return 0;
}
float
gs_currentsmoothness(const gs_state * pgs)
{
return pgs->smoothness;
}
int
gs_shfill(gs_state * pgs, const gs_shading_t * psh)
{
gs_pattern2_template_t pat;
gx_path cpath;
gs_matrix imat;
gs_client_color cc;
gs_color_space cs;
gx_device_color devc;
int code;
gs_pattern2_init(&pat);
pat.Shading = psh;
gs_make_identity(&imat);
code = gs_make_pattern(&cc, (gs_pattern_template_t *)&pat, &imat, pgs,
pgs->memory);
if (code < 0)
return code;
code = gs_pattern2_set_shfill(&cc);
if (code < 0)
return code;
gs_cspace_init(&cs, &gs_color_space_type_Pattern, pgs->memory, false);
cs.params.pattern.has_base_space = false;
code = cs.type->remap_color(&cc, &cs, &devc, (gs_imager_state *)pgs,
pgs->device, gs_color_select_texture);
if (code >= 0) {
gx_path_init_local(&cpath, pgs->memory);
code = gx_cpath_to_path(pgs->clip_path, &cpath);
if (code >= 0)
code = gx_fill_path(&cpath, &devc, pgs, gx_rule_winding_number,
fixed_0, fixed_0);
gx_path_free(&cpath, "gs_shfill");
}
gs_pattern_reference(&cc, -1);
return code;
}