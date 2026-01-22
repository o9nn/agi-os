#ifndef dstack_INCLUDED
# define dstack_INCLUDED
#include "idstack.h"
#include "icstate.h"
#define idict_stack (i_ctx_p->dict_stack)
#define d_stack (idict_stack.stack)
#define min_dstack_size (idict_stack.min_size)
#define dstack_userdict_index (idict_stack.userdict_index)
#define dsspace (idict_stack.def_space)
#define dtop_can_store(pvalue) ((int)r_space(pvalue) <= dsspace)
#define dtop_keys (idict_stack.top_keys)
#define dtop_npairs (idict_stack.top_npairs)
#define dtop_values (idict_stack.top_values)
#define dict_set_top() dstack_set_top(&idict_stack);
#define dict_is_permanent_on_dstack(pdict)\
dstack_dict_is_permanent(&idict_stack, pdict)
#define dicts_gc_cleanup() dstack_gc_cleanup(&idict_stack)
#define systemdict (&idict_stack.system_dict)
#define dsbot (d_stack.bot)
#define dsp (d_stack.p)
#define dstop (d_stack.top)
#define check_dstack(n)\
if ( dstop - dsp < (n) )\
{ d_stack.requested = (n); return_error(e_dictstackoverflow); }
#define dict_find_name_by_index(nidx)\
dstack_find_name_by_index(&idict_stack, nidx)
#define dict_find_name(pnref) dict_find_name_by_index(name_index(imemory, pnref))
#define dict_find_name_by_index_inline(nidx, htemp)\
dstack_find_name_by_index_inline(&idict_stack, nidx, htemp)
#define if_dict_find_name_by_index_top(nidx, htemp, pvslot)\
if_dstack_find_name_by_index_top(&idict_stack, nidx, htemp, pvslot)
#endif