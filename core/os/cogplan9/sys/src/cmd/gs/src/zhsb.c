#include "ghost.h"
#include "oper.h"
#include "igstate.h"
#include "store.h"
#include "gshsb.h"
private int
zcurrenthsbcolor(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
float par[3];
gs_currenthsbcolor(igs, par);
push(3);
make_floats(op - 2, par, 3);
return 0;
}
private int
zsethsbcolor(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
double par[3];
int code;
if ((code = num_params(op, 3, par)) < 0 ||
(code = gs_sethsbcolor(igs, par[0], par[1], par[2])) < 0
)
return code;
make_null(&istate->colorspace.array);
pop(3);
return 0;
}
const op_def zhsb_op_defs[] =
{
{"0currenthsbcolor", zcurrenthsbcolor},
{"3sethsbcolor", zsethsbcolor},
op_def_end(0)
};