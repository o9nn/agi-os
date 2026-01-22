#include "dos_.h"
#include "ghost.h"
#include "oper.h"
#include "store.h"
private int
zinport(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
make_int(op, inport((int)op->value.intval));
return 0;
}
private int
zinportb(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
make_int(op, inportb((int)op->value.intval));
return 0;
}
private int
zoutport(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
check_type(op[-1], t_integer);
outport((int)op[-1].value.intval, (int)op->value.intval);
pop(1);
return 0;
}
private int
zoutportb(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
check_int_leu(op[-1], 0xff);
outportb((int)op[-1].value.intval, (byte) op->value.intval);
pop(1);
return 0;
}
private int
zpeek(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
make_int(op, *(byte *) (op->value.intval));
return 0;
}
private int
zpoke(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
check_int_leu(op[-1], 0xff);
*(byte *) (op[-1].value.intval) = (byte) op->value.intval;
pop(1);
return 0;
}
const op_def zdosio_op_defs[] =
{
{"1.inport", zinport},
{"1.inportb", zinportb},
{"2.outport", zoutport},
{"2.outportb", zoutportb},
{"1.peek", zpeek},
{"2.poke", zpoke},
op_def_end(0)
};