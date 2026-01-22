#include "math_.h"
#include "ghost.h"
#include "gxfarith.h"
#include "oper.h"
#include "store.h"
#define zrand_state (i_ctx_p->rand_state)
const long rand_state_initial = 1;
int
zsqrt(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
double num;
int code = real_param(op, &num);
if (code < 0)
return code;
if (num < 0.0)
return_error(e_rangecheck);
make_real(op, sqrt(num));
return 0;
}
private int
zarccos(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
double num, result;
int code = real_param(op, &num);
if (code < 0)
return code;
result = acos(num) * radians_to_degrees;
make_real(op, result);
return 0;
}
private int
zarcsin(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
double num, result;
int code = real_param(op, &num);
if (code < 0)
return code;
result = asin(num) * radians_to_degrees;
make_real(op, result);
return 0;
}
int
zatan(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
double args[2];
double result;
int code = num_params(op, 2, args);
if (code < 0)
return code;
code = gs_atan2_degrees(args[0], args[1], &result);
if (code < 0)
return code;
make_real(op - 1, result);
pop(1);
return 0;
}
int
zcos(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
double angle;
int code = real_param(op, &angle);
if (code < 0)
return code;
make_real(op, gs_cos_degrees(angle));
return 0;
}
int
zsin(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
double angle;
int code = real_param(op, &angle);
if (code < 0)
return code;
make_real(op, gs_sin_degrees(angle));
return 0;
}
int
zexp(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
double args[2];
double result;
double ipart;
int code = num_params(op, 2, args);
if (code < 0)
return code;
if (args[0] == 0.0 && args[1] == 0.0)
return_error(e_undefinedresult);
if (args[0] < 0.0 && modf(args[1], &ipart) != 0.0)
return_error(e_undefinedresult);
result = pow(args[0], args[1]);
make_real(op - 1, result);
pop(1);
return 0;
}
int
zln(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
double num;
int code = real_param(op, &num);
if (code < 0)
return code;
if (num <= 0.0)
return_error(e_rangecheck);
make_real(op, log(num));
return 0;
}
int
zlog(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
double num;
int code = real_param(op, &num);
if (code < 0)
return code;
if (num <= 0.0)
return_error(e_rangecheck);
make_real(op, log10(num));
return 0;
}
private int
zrand(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
#define A 16807
#define M 0x7fffffff
#define Q 127773
#define R 2836
zrand_state = A * (zrand_state % Q) - R * (zrand_state / Q);
if (zrand_state <= 0)
zrand_state += M;
#undef A
#undef M
#undef Q
#undef R
push(1);
make_int(op, zrand_state);
return 0;
}
private int
zsrand(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
long state;
check_type(*op, t_integer);
state = op->value.intval;
#if arch_sizeof_long > 4
state = (int)state;
#endif
if (state < 1)
state = -(state % 0x7ffffffe) + 1;
else if (state > 0x7ffffffe)
state = 0x7ffffffe;
zrand_state = state;
pop(1);
return 0;
}
private int
zrrand(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
push(1);
make_int(op, zrand_state);
return 0;
}
const op_def zmath_op_defs[] =
{
{"1arccos", zarccos},
{"1arcsin", zarcsin},
{"2atan", zatan},
{"1cos", zcos},
{"2exp", zexp},
{"1ln", zln},
{"1log", zlog},
{"0rand", zrand},
{"0rrand", zrrand},
{"1sin", zsin},
{"1sqrt", zsqrt},
{"1srand", zsrand},
op_def_end(0)
};