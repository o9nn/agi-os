#include "gx.h"
#include "gsalpha.h"
#include "gxdcolor.h"
#include "gzstate.h"
int
gs_setalpha(gs_state * pgs, floatp alpha)
{
pgs->alpha =
(gx_color_value) (alpha < 0 ? 0 : alpha > 1 ? gx_max_color_value :
alpha * gx_max_color_value);
gx_unset_dev_color(pgs);
return 0;
}
float
gs_currentalpha(const gs_state * pgs)
{
return (float)pgs->alpha / gx_max_color_value;
}