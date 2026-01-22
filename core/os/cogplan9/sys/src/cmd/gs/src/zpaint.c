#include "ghost.h"
#include "oper.h"
#include "gspaint.h"
#include "igstate.h"
private int
zfill(i_ctx_t *i_ctx_p)
{
return gs_fill(igs);
}
private int
zeofill(i_ctx_t *i_ctx_p)
{
return gs_eofill(igs);
}
private int
zstroke(i_ctx_t *i_ctx_p)
{
return gs_stroke(igs);
}
private int
zfillpage(i_ctx_t *i_ctx_p)
{
return gs_fillpage(igs);
}
private int
zimagepath(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code;
check_type(op[-2], t_integer);
check_type(op[-1], t_integer);
check_read_type(*op, t_string);
if (r_size(op) < ((op[-2].value.intval + 7) >> 3) * op[-1].value.intval)
return_error(e_rangecheck);
code = gs_imagepath(igs,
(int)op[-2].value.intval, (int)op[-1].value.intval,
op->value.const_bytes);
if (code >= 0)
pop(3);
return code;
}
const op_def zpaint_op_defs[] =
{
{"0eofill", zeofill},
{"0fill", zfill},
{"0stroke", zstroke},
{"0.fillpage", zfillpage},
{"3.imagepath", zimagepath},
op_def_end(0)
};