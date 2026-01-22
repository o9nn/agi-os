#include "ghost.h"
#include "oper.h"
#include "gsmatrix.h"
#include "gxfixed.h"
#include "gxfont.h"
#include "gxtext.h"
#include "estack.h"
#include "ichar.h"
#include "ifont.h"
#include "igstate.h"
#include "store.h"
private int cshow_continue(i_ctx_t *);
private int cshow_restore_font(i_ctx_t *);
private int
zcshow(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
os_ptr proc_op = op - 1;
os_ptr str_op = op;
gs_text_enum_t *penum;
int code;
if (r_is_proc(proc_op))
;
else if (r_is_proc(op)) {
proc_op = op;
str_op = op - 1;
} else {
check_op(2);
return_error(e_typecheck);
}
if ((code = op_show_setup(i_ctx_p, str_op)) != 0 ||
(code = gs_cshow_begin(igs, str_op->value.bytes, r_size(str_op),
imemory, &penum)) < 0)
return code;
if ((code = op_show_finish_setup(i_ctx_p, penum, 2, NULL)) < 0) {
ifree_object(penum, "op_show_enum_setup");
return code;
}
sslot = *proc_op;
pop(2);
return cshow_continue(i_ctx_p);
}
private int
cshow_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_text_enum_t *penum = senum;
int code;
check_estack(4);
code = gs_text_process(penum);
if (code != TEXT_PROCESS_INTERVENE) {
code = op_show_continue_dispatch(i_ctx_p, 0, code);
if (code == o_push_estack)
make_op_estack(esp - 1, cshow_continue);
return code;
}
{
ref *pslot = &sslot;
gs_point wpt;
gs_font *font = gs_text_current_font(penum);
gs_font *root_font = gs_rootfont(igs);
gs_font *scaled_font;
uint font_space = r_space(pfont_dict(font));
uint root_font_space = r_space(pfont_dict(root_font));
int fdepth = penum->fstack.depth;
gs_text_current_width(penum, &wpt);
if (font == root_font)
scaled_font = font;
else if (fdepth > 0) {
uint save_space = idmemory->current_space;
gs_font * immediate_parent = penum->fstack.items[fdepth - 1].font;
ialloc_set_space(idmemory, font_space);
code = gs_makefont(font->dir, font,
&immediate_parent->FontMatrix,
&scaled_font);
ialloc_set_space(idmemory, save_space);
if (code < 0)
return code;
}
else {
uint save_space = idmemory->current_space;
ialloc_set_space(idmemory, font_space);
code = gs_makefont(font->dir, font, &root_font->FontMatrix,
&scaled_font);
ialloc_set_space(idmemory, save_space);
if (code < 0)
return code;
}
push(3);
make_int(op - 2, gs_text_current_char(penum) & 0xff);
make_real(op - 1, wpt.x);
make_real(op, wpt.y);
make_struct(&ssfont, font_space, font);
make_struct(&srfont, root_font_space, root_font);
push_op_estack(cshow_restore_font);
gs_set_currentfont(igs, scaled_font);
*++esp = *pslot;
}
return o_push_estack;
}
private int
cshow_restore_font(i_ctx_t *i_ctx_p)
{
gs_setfont(igs, r_ptr(&srfont, gs_font));
gs_set_currentfont(igs, r_ptr(&ssfont, gs_font));
return cshow_continue(i_ctx_p);
}
private int
zrootfont(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
push(1);
*op = *pfont_dict(gs_rootfont(igs));
return 0;
}
const op_def zcfont_op_defs[] =
{
{"2cshow", zcshow},
{"0rootfont", zrootfont},
{"0%cshow_continue", cshow_continue},
{"0%cshow_restore_font", cshow_restore_font},
op_def_end(0)
};