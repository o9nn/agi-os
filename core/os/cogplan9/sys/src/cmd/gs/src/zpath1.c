#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "oparc.h"
#include "estack.h"
#include "ialloc.h"
#include "igstate.h"
#include "gsstruct.h"
#include "gspath.h"
#include "store.h"
private int common_arc(i_ctx_t *,
int (*)(gs_state *, floatp, floatp, floatp, floatp, floatp));
private int common_arct(i_ctx_t *, float *);
int
zarc(i_ctx_t *i_ctx_p)
{
return common_arc(i_ctx_p, gs_arc);
}
int
zarcn(i_ctx_t *i_ctx_p)
{
return common_arc(i_ctx_p, gs_arcn);
}
private int
common_arc(i_ctx_t *i_ctx_p,
int (*aproc)(gs_state *, floatp, floatp, floatp, floatp, floatp))
{
os_ptr op = osp;
double xyra[5];
int code = num_params(op, 5, xyra);
if (code < 0)
return code;
code = (*aproc)(igs, xyra[0], xyra[1], xyra[2], xyra[3], xyra[4]);
if (code >= 0)
pop(5);
return code;
}
int
zarct(i_ctx_t *i_ctx_p)
{
int code = common_arct(i_ctx_p, (float *)0);
if (code < 0)
return code;
pop(5);
return 0;
}
private int
zarcto(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
float tanxy[4];
int code = common_arct(i_ctx_p, tanxy);
if (code < 0)
return code;
make_real(op - 4, tanxy[0]);
make_real(op - 3, tanxy[1]);
make_real(op - 2, tanxy[2]);
make_real(op - 1, tanxy[3]);
pop(1);
return 0;
}
private int
common_arct(i_ctx_t *i_ctx_p, float *tanxy)
{
os_ptr op = osp;
double args[5];
int code = num_params(op, 5, args);
if (code < 0)
return code;
return gs_arcto(igs, args[0], args[1], args[2], args[3], args[4], tanxy);
}
private int
zdashpath(i_ctx_t *i_ctx_p)
{
return gs_dashpath(igs);
}
private int
zflattenpath(i_ctx_t *i_ctx_p)
{
return gs_flattenpath(igs);
}
private int
zreversepath(i_ctx_t *i_ctx_p)
{
return gs_reversepath(igs);
}
private int
zstrokepath(i_ctx_t *i_ctx_p)
{
return gs_strokepath(igs);
}
private int
zclippath(i_ctx_t *i_ctx_p)
{
return gs_clippath(igs);
}
private int
zpathbbox(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_rect box;
int code;
check_type(*op, t_boolean);
code = gs_upathbbox(igs, &box, op->value.boolval);
if (code < 0)
return code;
push(3);
make_real(op - 3, box.p.x);
make_real(op - 2, box.p.y);
make_real(op - 1, box.q.x);
make_real(op, box.q.y);
return 0;
}
private int path_continue(i_ctx_t *);
private int path_cleanup(i_ctx_t *);
private int
zpathforall(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_path_enum *penum;
int code;
check_proc(op[-3]);
check_proc(op[-2]);
check_proc(op[-1]);
check_proc(*op);
check_estack(8);
if ((penum = gs_path_enum_alloc(imemory, "pathforall")) == 0)
return_error(e_VMerror);
code = gs_path_enum_init(penum, igs);
if (code < 0) {
ifree_object(penum, "path_cleanup");
return code;
}
push_mark_estack(es_for, path_cleanup);
memcpy(esp + 1, op - 3, 4 * sizeof(ref));
esp += 5;
make_istruct(esp, 0, penum);
push_op_estack(path_continue);
pop(4);
op -= 4;
return o_push_estack;
}
private void pf_push(i_ctx_t *, gs_point *, int);
private int
path_continue(i_ctx_t *i_ctx_p)
{
gs_path_enum *penum = r_ptr(esp, gs_path_enum);
gs_point ppts[3];
int code;
check_ostack(6);
code = gs_path_enum_next(penum, ppts);
switch (code) {
case 0:
esp -= 6;
path_cleanup(i_ctx_p);
return o_pop_estack;
default:
return code;
case gs_pe_moveto:
esp[2] = esp[-4];
pf_push(i_ctx_p, ppts, 1);
break;
case gs_pe_lineto:
esp[2] = esp[-3];
pf_push(i_ctx_p, ppts, 1);
break;
case gs_pe_curveto:
esp[2] = esp[-2];
pf_push(i_ctx_p, ppts, 3);
break;
case gs_pe_closepath:
esp[2] = esp[-1];
break;
}
push_op_estack(path_continue);
++esp;
return o_push_estack;
}
private void
pf_push(i_ctx_t *i_ctx_p, gs_point * ppts, int n)
{
os_ptr op = osp;
while (n--) {
op += 2;
make_real(op - 1, ppts->x);
make_real(op, ppts->y);
ppts++;
}
osp = op;
}
private int
path_cleanup(i_ctx_t *i_ctx_p)
{
gs_path_enum *penum = r_ptr(esp + 6, gs_path_enum);
gs_path_enum_cleanup(penum);
ifree_object(penum, "path_cleanup");
return 0;
}
const op_def zpath1_op_defs[] =
{
{"5arc", zarc},
{"5arcn", zarcn},
{"5arct", zarct},
{"5arcto", zarcto},
{"0clippath", zclippath},
{"0.dashpath", zdashpath},
{"0flattenpath", zflattenpath},
{"4pathforall", zpathforall},
{"0reversepath", zreversepath},
{"0strokepath", zstrokepath},
{"1.pathbbox", zpathbbox},
{"0%path_continue", path_continue},
op_def_end(0)
};