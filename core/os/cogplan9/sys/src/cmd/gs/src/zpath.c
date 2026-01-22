#include "math_.h"
#include "ghost.h"
#include "oper.h"
#include "igstate.h"
#include "gsmatrix.h"
#include "gspath.h"
#include "store.h"
private int common_to(i_ctx_t *,
int (*)(gs_state *, floatp, floatp));
private int common_curve(i_ctx_t *,
int (*)(gs_state *, floatp, floatp, floatp, floatp, floatp, floatp));
private int
znewpath(i_ctx_t *i_ctx_p)
{
return gs_newpath(igs);
}
private int
zcurrentpoint(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_point pt;
int code = gs_currentpoint(igs, &pt);
if (code < 0)
return code;
push(2);
make_real(op - 1, pt.x);
make_real(op, pt.y);
return 0;
}
int
zmoveto(i_ctx_t *i_ctx_p)
{
return common_to(i_ctx_p, gs_moveto);
}
int
zrmoveto(i_ctx_t *i_ctx_p)
{
return common_to(i_ctx_p, gs_rmoveto);
}
int
zlineto(i_ctx_t *i_ctx_p)
{
return common_to(i_ctx_p, gs_lineto);
}
int
zrlineto(i_ctx_t *i_ctx_p)
{
return common_to(i_ctx_p, gs_rlineto);
}
private int
common_to(i_ctx_t *i_ctx_p,
int (*add_proc)(gs_state *, floatp, floatp))
{
os_ptr op = osp;
double opxy[2];
int code;
if ((code = num_params(op, 2, opxy)) < 0 ||
(code = (*add_proc)(igs, opxy[0], opxy[1])) < 0
)
return code;
pop(2);
return 0;
}
int
zcurveto(i_ctx_t *i_ctx_p)
{
return common_curve(i_ctx_p, gs_curveto);
}
int
zrcurveto(i_ctx_t *i_ctx_p)
{
return common_curve(i_ctx_p, gs_rcurveto);
}
private int
common_curve(i_ctx_t *i_ctx_p,
int (*add_proc)(gs_state *, floatp, floatp, floatp, floatp, floatp, floatp))
{
os_ptr op = osp;
double opxy[6];
int code;
if ((code = num_params(op, 6, opxy)) < 0)
return code;
code = (*add_proc)(igs, opxy[0], opxy[1], opxy[2], opxy[3], opxy[4], opxy[5]);
if (code >= 0)
pop(6);
return code;
}
int
zclosepath(i_ctx_t *i_ctx_p)
{
return gs_closepath(igs);
}
private int
zinitclip(i_ctx_t *i_ctx_p)
{
return gs_initclip(igs);
}
private int
zclip(i_ctx_t *i_ctx_p)
{
return gs_clip(igs);
}
private int
zeoclip(i_ctx_t *i_ctx_p)
{
return gs_eoclip(igs);
}
const op_def zpath_op_defs[] =
{
{"0clip", zclip},
{"0closepath", zclosepath},
{"0currentpoint", zcurrentpoint},
{"6curveto", zcurveto},
{"0eoclip", zeoclip},
{"0initclip", zinitclip},
{"2lineto", zlineto},
{"2moveto", zmoveto},
{"0newpath", znewpath},
{"6rcurveto", zrcurveto},
{"2rlineto", zrlineto},
{"2rmoveto", zrmoveto},
op_def_end(0)
};