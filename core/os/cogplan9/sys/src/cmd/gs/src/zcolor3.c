#include "ghost.h"
#include "oper.h"
#include "igstate.h"
private int
zsetuseciecolor(i_ctx_t * i_ctx_p)
{
os_ptr op = osp;
istate->use_cie_color = *op;
pop(1);
return 0;
}
const op_def zcolor3_l3_op_defs[] = {
op_def_begin_ll3(),
{ "0.setuseciecolor", zsetuseciecolor },
op_def_end(0)
};