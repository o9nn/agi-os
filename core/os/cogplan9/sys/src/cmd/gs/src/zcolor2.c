#include "ghost.h"
#include "string_.h"
#include "oper.h"
#include "gsmatrix.h"
#include "gsstruct.h"
#include "gxcspace.h"
#include "gscolor2.h"
#include "igstate.h"
#include "store.h"
private int
zusealternate(i_ctx_t * i_ctx_p)
{
os_ptr                  op = osp;
const gs_color_space *  pcs = gs_currentcolorspace(igs);
push(1);
make_bool(op, cs_base_space(pcs) != 0);
return 0;
}
const op_def    zcolor2_l2_op_defs[] = {
op_def_begin_level2(),
{ "0.usealternate", zusealternate },
op_def_end(0)
};