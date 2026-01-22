#include "math_.h"
#include "ghost.h"
#include "oper.h"
#include "store.h"
#define MIN_INTVAL min_long
#define MAX_INTVAL max_long
#define MAX_HALF_INTVAL ((1L << (size_of(long) * 4 - 1)) - 1)
int
zop_add(register os_ptr op)
{
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_real:
switch (r_type(op - 1)) {
default:
return_op_typecheck(op - 1);
case t_real:
op[-1].value.realval += op->value.realval;
break;
case t_integer:
make_real(op - 1, (double)op[-1].value.intval + op->value.realval);
}
break;
case t_integer:
switch (r_type(op - 1)) {
default:
return_op_typecheck(op - 1);
case t_real:
op[-1].value.realval += (double)op->value.intval;
break;
case t_integer: {
long int2 = op->value.intval;
if (((op[-1].value.intval += int2) ^ int2) < 0 &&
((op[-1].value.intval - int2) ^ int2) >= 0
) {
make_real(op - 1, (double)(op[-1].value.intval - int2) + int2);
}
}
}
}
return 0;
}
int
zadd(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code = zop_add(op);
if (code == 0) {
pop(1);
}
return code;
}
int
zdiv(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
os_ptr op1 = op - 1;
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_real:
if (op->value.realval == 0)
return_error(e_undefinedresult);
switch (r_type(op1)) {
default:
return_op_typecheck(op1);
case t_real:
op1->value.realval /= op->value.realval;
break;
case t_integer:
make_real(op1, (double)op1->value.intval / op->value.realval);
}
break;
case t_integer:
if (op->value.intval == 0)
return_error(e_undefinedresult);
switch (r_type(op1)) {
default:
return_op_typecheck(op1);
case t_real:
op1->value.realval /= (double)op->value.intval;
break;
case t_integer:
make_real(op1, (double)op1->value.intval / (double)op->value.intval);
}
}
pop(1);
return 0;
}
int
zmul(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_real:
switch (r_type(op - 1)) {
default:
return_op_typecheck(op - 1);
case t_real:
op[-1].value.realval *= op->value.realval;
break;
case t_integer:
make_real(op - 1, (double)op[-1].value.intval * op->value.realval);
}
break;
case t_integer:
switch (r_type(op - 1)) {
default:
return_op_typecheck(op - 1);
case t_real:
op[-1].value.realval *= (double)op->value.intval;
break;
case t_integer: {
long int1 = op[-1].value.intval;
long int2 = op->value.intval;
long abs1 = (int1 >= 0 ? int1 : -int1);
long abs2 = (int2 >= 0 ? int2 : -int2);
float fprod;
if ((abs1 > MAX_HALF_INTVAL || abs2 > MAX_HALF_INTVAL) &&
abs1 != 0 &&
abs2 > MAX_INTVAL / abs1 &&
(fprod = (float)int1 * int2,
(int1 * int2 != MIN_INTVAL ||
fprod != (float)MIN_INTVAL))
)
make_real(op - 1, fprod);
else
op[-1].value.intval = int1 * int2;
}
}
}
pop(1);
return 0;
}
int
zop_sub(register os_ptr op)
{
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_real:
switch (r_type(op - 1)) {
default:
return_op_typecheck(op - 1);
case t_real:
op[-1].value.realval -= op->value.realval;
break;
case t_integer:
make_real(op - 1, (double)op[-1].value.intval - op->value.realval);
}
break;
case t_integer:
switch (r_type(op - 1)) {
default:
return_op_typecheck(op - 1);
case t_real:
op[-1].value.realval -= (double)op->value.intval;
break;
case t_integer: {
long int1 = op[-1].value.intval;
if ((int1 ^ (op[-1].value.intval = int1 - op->value.intval)) < 0 &&
(int1 ^ op->value.intval) < 0
) {
make_real(op - 1, (float)int1 - op->value.intval);
}
}
}
}
return 0;
}
int
zsub(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int code = zop_sub(op);
if (code == 0) {
pop(1);
}
return code;
}
int
zidiv(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
check_type(op[-1], t_integer);
if (op->value.intval == 0)
return_error(e_undefinedresult);
if ((op[-1].value.intval /= op->value.intval) ==
MIN_INTVAL && op->value.intval == -1
) {
return_error(e_rangecheck);
}
pop(1);
return 0;
}
int
zmod(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
check_type(op[-1], t_integer);
if (op->value.intval == 0)
return_error(e_undefinedresult);
op[-1].value.intval %= op->value.intval;
pop(1);
return 0;
}
int
zneg(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_real:
op->value.realval = -op->value.realval;
break;
case t_integer:
if (op->value.intval == MIN_INTVAL)
make_real(op, -(float)MIN_INTVAL);
else
op->value.intval = -op->value.intval;
}
return 0;
}
int
zabs(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_real:
if (op->value.realval >= 0)
return 0;
break;
case t_integer:
if (op->value.intval >= 0)
return 0;
break;
}
return zneg(i_ctx_p);
}
int
zceiling(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_real:
op->value.realval = ceil(op->value.realval);
case t_integer:;
}
return 0;
}
int
zfloor(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_real:
op->value.realval = floor(op->value.realval);
case t_integer:;
}
return 0;
}
int
zround(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_real:
op->value.realval = floor(op->value.realval + 0.5);
case t_integer:;
}
return 0;
}
int
ztruncate(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_real:
op->value.realval =
(op->value.realval < 0.0 ?
ceil(op->value.realval) :
floor(op->value.realval));
case t_integer:;
}
return 0;
}
private int
zbitadd(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_integer);
check_type(op[-1], t_integer);
op[-1].value.intval += op->value.intval;
pop(1);
return 0;
}
const op_def zarith_op_defs[] =
{
{"1abs", zabs},
{"2add", zadd},
{"2.bitadd", zbitadd},
{"1ceiling", zceiling},
{"2div", zdiv},
{"2idiv", zidiv},
{"1floor", zfloor},
{"2mod", zmod},
{"2mul", zmul},
{"1neg", zneg},
{"1round", zround},
{"2sub", zsub},
{"1truncate", ztruncate},
op_def_end(0)
};