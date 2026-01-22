#include "string_.h"
#include "ghost.h"
#include "oper.h"
#include "dstack.h"
#include "estack.h"
#include "gsstruct.h"
#include "stream.h"
#include "files.h"
#include "store.h"
#include "strimpl.h"
#include "sfilter.h"
#include "idict.h"
#include "iname.h"
#include "iscan.h"
#include "itoken.h"
private int ztoken_continue(i_ctx_t *);
private int token_continue(i_ctx_t *, stream *, scanner_state *, bool);
int
ztoken(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
switch (r_type(op)) {
default:
return_op_typecheck(op);
case t_file: {
stream *s;
scanner_state state;
check_read_file(s, op);
check_ostack(1);
scanner_state_init(&state, false);
return token_continue(i_ctx_p, s, &state, true);
}
case t_string: {
ref token;
int orig_ostack_depth = ref_stack_count(&o_stack);
int code = scan_string_token(i_ctx_p, op, &token);
switch (code) {
case scan_EOF:
make_false(op);
return 0;
default:
if (code < 0) {
if (orig_ostack_depth < ref_stack_count(&o_stack))
pop(ref_stack_count(&o_stack)- orig_ostack_depth);
return code;
}
}
push(2);
op[-1] = token;
make_true(op);
return 0;
}
}
}
private int
ztoken_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
scanner_state *pstate;
check_read_file(s, op - 1);
check_stype(*op, st_scanner_state);
pstate = r_ptr(op, scanner_state);
pop(1);
return token_continue(i_ctx_p, s, pstate, false);
}
private int
token_continue(i_ctx_t *i_ctx_p, stream * s, scanner_state * pstate,
bool save)
{
os_ptr op = osp;
int code;
ref token;
ref fref;
ref_assign(&fref, op);
again:
pop(1);
code = scan_token(i_ctx_p, s, &token, pstate);
op = osp;
switch (code) {
default:
if (code > 0)
code = gs_note_error(e_syntaxerror);
push(1);
ref_assign(op, &fref);
break;
case scan_BOS:
code = 0;
case 0:
push(2);
ref_assign(op - 1, &token);
make_true(op);
break;
case scan_EOF:
push(1);
make_false(op);
code = 0;
break;
case scan_Refill:
push(1);
ref_assign(op, &fref);
code = scan_handle_refill(i_ctx_p, op, pstate, save, false,
ztoken_continue);
switch (code) {
case 0:
goto again;
case o_push_estack:
return code;
}
break;
}
if (code <= 0 && !save) {
ifree_object(pstate, "token_continue");
}
return code;
}
int ztokenexec_continue(i_ctx_t *);
private int tokenexec_continue(i_ctx_t *, stream *, scanner_state *, bool);
int
ztokenexec(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
scanner_state state;
check_read_file(s, op);
check_estack(1);
scanner_state_init(&state, false);
return tokenexec_continue(i_ctx_p, s, &state, true);
}
int
ztokenexec_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream *s;
scanner_state *pstate;
check_read_file(s, op - 1);
check_stype(*op, st_scanner_state);
pstate = r_ptr(op, scanner_state);
pop(1);
return tokenexec_continue(i_ctx_p, s, pstate, false);
}
private int
tokenexec_continue(i_ctx_t *i_ctx_p, stream * s, scanner_state * pstate,
bool save)
{
os_ptr op = osp;
int code;
ref fref;
ref_assign(&fref, op);
pop(1);
again:
check_estack(1);
code = scan_token(i_ctx_p, s, (ref *) (esp + 1), pstate);
op = osp;
switch (code) {
case 0:
if (r_is_proc(esp + 1)) {
push(1);
ref_assign(op, esp + 1);
code = 0;
break;
}
case scan_BOS:
++esp;
code = o_push_estack;
break;
case scan_EOF:
code = 0;
break;
case scan_Refill:
code = scan_handle_refill(i_ctx_p, &fref, pstate, save, true,
ztokenexec_continue);
switch (code) {
case 0:
goto again;
case o_push_estack:
return code;
}
break;
case scan_Comment:
case scan_DSC_Comment:
return ztoken_handle_comment(i_ctx_p, &fref, pstate, esp + 1, code,
save, true, ztokenexec_continue);
default:
break;
}
if (code < 0) {
push(1);
ref_assign(op, &fref);
}
if (!save) {
ifree_object(pstate, "token_continue");
}
return code;
}
int
ztoken_handle_comment(i_ctx_t *i_ctx_p, const ref *fop, scanner_state *sstate,
const ref *ptoken, int scan_code,
bool save, bool push_file, op_proc_t cont)
{
const char *proc_name;
scanner_state *pstate;
os_ptr op;
ref *ppcproc;
int code;
switch (scan_code) {
case scan_Comment:
proc_name = "%ProcessComment";
break;
case scan_DSC_Comment:
proc_name = "%ProcessDSCComment";
break;
default:
return_error(e_Fatal);
}
if (ostop - osp < 2) {
code = ref_stack_extend(&o_stack, 2);
if (code < 0)
return code;
}
check_estack(4);
code = name_enter_string(imemory, proc_name, esp + 4);
if (code < 0)
return code;
if (save) {
pstate = ialloc_struct(scanner_state, &st_scanner_state,
"ztoken_handle_comment");
if (pstate == 0)
return_error(e_VMerror);
*pstate = *sstate;
} else
pstate = sstate;
if (!pstate->s_pstack)
osp[2] = *ptoken;
make_op_estack(esp + 1, cont);
make_istruct(esp + 2, 0, pstate);
esp[3] = *fop;
r_clear_attrs(esp + 3, a_executable);
ppcproc = dict_find_name(esp + 4);
if (ppcproc == 0) {
if (pstate->s_pstack)
--osp;
esp += 3;
} else {
if (pstate->s_pstack) {
op = ++osp;
*op = op[-1];
} else {
op = osp += 2;
}
op[-1] = *fop;
esp[4] = *ppcproc;
esp += 4;
}
return o_push_estack;
}
int
ztoken_scanner_options(const ref *upref, int old_options)
{
typedef struct named_scanner_option_s {
const char *pname;
int option;
} named_scanner_option_t;
static const named_scanner_option_t named_options[4] = {
{"ProcessComment", SCAN_PROCESS_COMMENTS},
{"ProcessDSCComment", SCAN_PROCESS_DSC_COMMENTS},
{"PDFScanRules", SCAN_PDF_RULES},
{"PDFScanInvNum", SCAN_PDF_INV_NUM}
};
int options = old_options;
int i;
for (i = 0; i < countof(named_options); ++i) {
const named_scanner_option_t *pnso = &named_options[i];
ref *ppcproc;
int code = dict_find_string(upref, pnso->pname, &ppcproc);
if (code >= 0) {
if (r_has_type(ppcproc, t_null))
options &= ~pnso->option;
else
options |= pnso->option;
}
}
return options;
}
const op_def ztoken_op_defs[] =
{
{"1token", ztoken},
{"1.tokenexec", ztokenexec},
{"2%ztokenexec_continue", ztokenexec_continue},
op_def_end(0)
};