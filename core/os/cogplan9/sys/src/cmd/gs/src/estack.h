#ifndef estack_INCLUDED
#  define estack_INCLUDED
#include "iestack.h"
#include "icstate.h"
#define esfile (iexec_stack.current_file)
#define esfile_clear_cache() estack_clear_cache(&iexec_stack)
#define esfile_set_cache(pref) estack_set_cache(&iexec_stack, pref)
#define esfile_check_cache() estack_check_cache(&iexec_stack)
#define iexec_stack (i_ctx_p->exec_stack)
#define e_stack (iexec_stack.stack)
#define esbot (e_stack.bot)
#define esp (e_stack.p)
#define estop (e_stack.top)
#define make_mark_estack(ep, es_idx, proc)\
make_tasv(ep, t_null, a_executable, es_idx, opproc, proc)
#define push_mark_estack(es_idx, proc)\
(++esp, make_mark_estack(esp, es_idx, proc))
#define r_is_estack_mark(ep)\
r_has_type_attrs(ep, t_null, a_executable)
#define estack_mark_index(ep) r_size(ep)
#define set_estack_mark_index(ep, es_idx) r_set_size(ep, es_idx)
#define make_op_estack(ep, proc)\
make_oper(ep, 0, proc)
#define push_op_estack(proc)\
(++esp, make_op_estack(esp, proc))
#define check_estack(n)\
if ( esp > estop - (n) )\
{ int es_code_ = ref_stack_extend(&e_stack, n);\
if ( es_code_ < 0 ) return es_code_;\
}
#define check_esp(n)\
if ( esp < esbot + ((n) - 1) )\
{ e_stack.requested = (n); return_error(e_ExecStackUnderflow); }
#define es_other 0
#define es_show 1
#define es_for 2
#define es_stopped 3
void pop_estack(i_ctx_t *, uint);
#endif