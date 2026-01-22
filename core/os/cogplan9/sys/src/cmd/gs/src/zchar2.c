#include "ghost.h"
#include "oper.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gxfont.h"
#include "gxfont1.h"
#include "gxtype1.h"
#include "ichar1.h"
private int
ztype2execchar(i_ctx_t *i_ctx_p)
{
return charstring_execchar(i_ctx_p, (1 << (int)ft_encrypted2));
}
const op_def zchar2_op_defs[] =
{
{"4.type2execchar", ztype2execchar},
op_def_end(0)
};