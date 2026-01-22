#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "estack.h"
#include "gsstruct.h"
#include "ialloc.h"
#include "istruct.h"
#include "stream.h"
#include "strimpl.h"
#include "ifilter.h"
#include "files.h"
#include "store.h"
private
CLEAR_MARKS_PROC(sproc_clear_marks)
{
stream_proc_state *const pptr = vptr;
r_clear_attrs(&pptr->proc, l_mark);
r_clear_attrs(&pptr->data, l_mark);
}
private
ENUM_PTRS_WITH(sproc_enum_ptrs, stream_proc_state *pptr) return 0;
case 0:
ENUM_RETURN_REF(&pptr->proc);
case 1:
ENUM_RETURN_REF(&pptr->data);
ENUM_PTRS_END
private RELOC_PTRS_WITH(sproc_reloc_ptrs, stream_proc_state *pptr);
RELOC_REF_VAR(pptr->proc);
r_clear_attrs(&pptr->proc, l_mark);
RELOC_REF_VAR(pptr->data);
r_clear_attrs(&pptr->data, l_mark);
RELOC_PTRS_END
private_st_stream_proc_state();
private int
s_proc_init(ref * sop, stream ** psstrm, uint mode,
const stream_template * temp, const stream_procs * procs,
gs_ref_memory_t *imem)
{
gs_memory_t *const mem = (gs_memory_t *)imem;
stream *sstrm = file_alloc_stream(mem, "s_proc_init(stream)");
stream_proc_state *state = (stream_proc_state *)
s_alloc_state(mem, &st_sproc_state, "s_proc_init(state)");
if (sstrm == 0 || state == 0) {
gs_free_object(mem, state, "s_proc_init(state)");
return_error(e_VMerror);
}
s_std_init(sstrm, NULL, 0, procs, mode);
sstrm->procs.process = temp->process;
state->template = temp;
state->memory = mem;
state->eof = 0;
state->proc = *sop;
make_empty_string(&state->data, a_all);
state->index = 0;
sstrm->state = (stream_state *) state;
*psstrm = sstrm;
return 0;
}
private int
s_handle_intc(i_ctx_t *i_ctx_p, const ref *pstate, int nstate,
op_proc_t cont)
{
int npush = nstate + 2;
check_estack(npush);
if (nstate)
memcpy(esp + 2, pstate, nstate * sizeof(ref));
#if 0
{
int code = gs_interpret_error(e_interrupt, (ref *) (esp + npush));
if (code < 0)
return code;
}
#else
npush--;
#endif
make_op_estack(esp + 1, cont);
esp += npush;
return o_push_estack;
}
private void
s_proc_set_defaults(stream_state * st)
{
stream_proc_state *const ss = (stream_proc_state *) st;
make_null(&ss->proc);
make_null(&ss->data);
}
private stream_proc_process(s_proc_read_process);
private int s_proc_read_continue(i_ctx_t *);
private const stream_template s_proc_read_template = {
&st_sproc_state, NULL, s_proc_read_process, 1, 1,
NULL, s_proc_set_defaults
};
private const stream_procs s_proc_read_procs = {
s_std_noavailable, s_std_noseek, s_std_read_reset,
s_std_read_flush, s_std_null, NULL
};
int
sread_proc(ref * sop, stream ** psstrm, gs_ref_memory_t *imem)
{
int code =
s_proc_init(sop, psstrm, s_mode_read, &s_proc_read_template,
&s_proc_read_procs, imem);
if (code < 0)
return code;
(*psstrm)->end_status = CALLC;
return code;
}
private int
s_proc_read_process(stream_state * st, stream_cursor_read * ignore_pr,
stream_cursor_write * pw, bool last)
{
stream_proc_state *const ss = (stream_proc_state *) st;
uint count = r_size(&ss->data) - ss->index;
if (count > 0) {
uint wcount = pw->limit - pw->ptr;
if (wcount < count)
count = wcount;
memcpy(pw->ptr + 1, ss->data.value.bytes + ss->index, count);
pw->ptr += count;
ss->index += count;
return 1;
}
return (ss->eof ? EOFC : CALLC);
}
int
s_handle_read_exception(i_ctx_t *i_ctx_p, int status, const ref * fop,
const ref * pstate, int nstate, op_proc_t cont)
{
int npush = nstate + 4;
stream *ps;
stream *psstdin;
switch (status) {
case INTC:
return s_handle_intc(i_ctx_p, pstate, nstate, cont);
case CALLC:
break;
default:
return_error(e_ioerror);
}
for (ps = fptr(fop); ps->strm != 0;)
ps = ps->strm;
check_estack(npush);
if (nstate)
memcpy(esp + 2, pstate, nstate * sizeof(ref));
make_op_estack(esp + 1, cont);
esp += npush;
make_op_estack(esp - 2, s_proc_read_continue);
esp[-1] = *fop;
r_clear_attrs(esp - 1, a_executable);
*esp = ((stream_proc_state *) ps->state)->proc;
zget_stdin(i_ctx_p, &psstdin);
if (ps == psstdin) {
check_estack(1);
esp += 1;
make_op_estack(esp, zneedstdin);
}
return o_push_estack;
}
private int
s_proc_read_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
os_ptr opbuf = op - 1;
stream *ps;
stream_proc_state *ss;
check_file(ps, op);
check_read_type(*opbuf, t_string);
while ((ps->end_status = 0, ps->strm) != 0)
ps = ps->strm;
ss = (stream_proc_state *) ps->state;
ss->data = *opbuf;
ss->index = 0;
if (r_size(opbuf) == 0)
ss->eof = true;
pop(2);
return 0;
}
private stream_proc_flush(s_proc_write_flush);
private stream_proc_process(s_proc_write_process);
private int s_proc_write_continue(i_ctx_t *);
private const stream_template s_proc_write_template = {
&st_sproc_state, NULL, s_proc_write_process, 1, 1,
NULL, s_proc_set_defaults
};
private const stream_procs s_proc_write_procs = {
s_std_noavailable, s_std_noseek, s_std_write_reset,
s_proc_write_flush, s_std_null, NULL
};
int
swrite_proc(ref * sop, stream ** psstrm, gs_ref_memory_t *imem)
{
return s_proc_init(sop, psstrm, s_mode_write, &s_proc_write_template,
&s_proc_write_procs, imem);
}
private int
s_proc_write_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * ignore_pw, bool last)
{
stream_proc_state *const ss = (stream_proc_state *) st;
uint rcount = pr->limit - pr->ptr;
if (rcount > 0) {
uint wcount = r_size(&ss->data) - ss->index;
uint count = min(rcount, wcount);
memcpy(ss->data.value.bytes + ss->index, pr->ptr + 1, count);
pr->ptr += count;
ss->index += count;
if (rcount > wcount)
return CALLC;
else if (last) {
ss->eof = true;
return CALLC;
} else
return 0;
}
return ((ss->eof = last) ? EOFC : 0);
}
private int
s_proc_write_flush(stream *s)
{
int result = s_process_write_buf(s, false);
stream_proc_state *const ss = (stream_proc_state *)s->state;
return (result < 0 || ss->index == 0 ? result : CALLC);
}
int
s_handle_write_exception(i_ctx_t *i_ctx_p, int status, const ref * fop,
const ref * pstate, int nstate, op_proc_t cont)
{
stream *ps;
stream *psstderr;
stream *psstdout;
stream_proc_state *psst;
switch (status) {
case INTC:
return s_handle_intc(i_ctx_p, pstate, nstate, cont);
case CALLC:
break;
default:
return_error(e_ioerror);
}
for (ps = fptr(fop); ps->strm != 0;)
ps = ps->strm;
psst = (stream_proc_state *) ps->state;
{
int npush = nstate + 6;
check_estack(npush);
if (nstate)
memcpy(esp + 2, pstate, nstate * sizeof(ref));
make_op_estack(esp + 1, cont);
esp += npush;
make_op_estack(esp - 4, s_proc_write_continue);
esp[-3] = *fop;
r_clear_attrs(esp - 3, a_executable);
make_bool(esp - 1, !psst->eof);
}
esp[-2] = psst->proc;
*esp = psst->data;
r_set_size(esp, psst->index);
zget_stdout(i_ctx_p, &psstdout);
zget_stderr(i_ctx_p, &psstderr);
if ((ps == psstderr) || (ps == psstdout)) {
check_estack(1);
esp += 1;
make_op_estack(esp, (ps == psstderr) ? zneedstderr : zneedstdout);
}
return o_push_estack;
}
private int
s_proc_write_continue(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
os_ptr opbuf = op - 1;
stream *ps;
stream_proc_state *ss;
check_file(ps, op);
check_write_type(*opbuf, t_string);
while (ps->strm != 0) {
if (ps->end_status == CALLC)
ps->end_status = 0;
ps = ps->strm;
}
ps->end_status = 0;
ss = (stream_proc_state *) ps->state;
ss->data = *opbuf;
ss->index = 0;
pop(2);
return 0;
}
bool
s_is_proc(const stream *s)
{
return (s->procs.process == s_proc_read_process ||
s->procs.process == s_proc_write_process);
}
const op_def zfproc_op_defs[] =
{
{"2%s_proc_read_continue", s_proc_read_continue},
{"2%s_proc_write_continue", s_proc_write_continue},
op_def_end(0)
};