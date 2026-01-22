#include "math_.h"
#include "gx.h"
#include "gxchrout.h"
#include "gxfarith.h"
#include "gxistate.h"
double
gs_char_flatness(const gs_imager_state *pis, floatp default_scale)
{
double cxx = fabs(pis->ctm.xx), cyy = fabs(pis->ctm.yy);
if (is_fzero(cxx) || (cyy < cxx && !is_fzero(cyy)))
cxx = cyy;
if (!is_xxyy(&pis->ctm)) {
double cxy = fabs(pis->ctm.xy), cyx = fabs(pis->ctm.yx);
if (is_fzero(cxx) || (cxy < cxx && !is_fzero(cxy)))
cxx = cxy;
if (is_fzero(cxx) || (cyx < cxx && !is_fzero(cyx)))
cxx = cyx;
}
cxx *= 0.001 / default_scale;
if (cxx > pis->flatness)
cxx = pis->flatness;
if (cxx < 0.2)
cxx = 0;
return cxx;
}