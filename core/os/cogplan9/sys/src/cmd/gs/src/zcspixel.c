#include "ghost.h"
#include "oper.h"
#include "igstate.h"
#include "gscspace.h"
#include "gsmatrix.h"
#include "gscolor2.h"
#include "gscpixel.h"
#include "ialloc.h"
private int
zsetdevicepixelspace(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref depth;
gs_color_space cs;
int code;
check_read_type(*op, t_array);
if (r_size(op) != 2)
return_error(e_rangecheck);
array_get(imemory, op, 1L, &depth);
check_type_only(depth, t_integer);
code = gs_cspace_init_DevicePixel(imemory, &cs, (int)depth.value.intval);
if (code < 0)
return code;
code = gs_setcolorspace(igs, &cs);
if (code >= 0)
pop(1);
return code;
}
const op_def zcspixel_op_defs[] =
{
{"1.setdevicepixelspace", zsetdevicepixelspace},
op_def_end(0)
};