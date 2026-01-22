#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gsstruct.h"
#include "ialloc.h"
#include "idict.h"
#include "stream.h"
#include "strimpl.h"
#include "ifilter.h"
#include "sarc4.h"
private int
z_arcfour_d(i_ctx_t * i_ctx_p)
{
os_ptr op = osp;
ref *sop = NULL;
stream_arcfour_state state;
check_type(*op, t_dictionary);
check_dict_read(*op);
if (dict_find_string(op, "Key", &sop) <= 0)
return_error(e_rangecheck);
s_arcfour_set_key(&state, sop->value.const_bytes, r_size(sop));
return filter_read(i_ctx_p, 0, &s_arcfour_template,
(stream_state *) & state, 0);
}
private int
z_arcfour_e(i_ctx_t * i_ctx_p)
{
os_ptr op = osp;
ref *sop = NULL;
stream_arcfour_state state;
check_type(*op, t_dictionary);
check_dict_read(*op);
if (dict_find_string(op, "Key", &sop) <= 0)
return_error(e_rangecheck);
s_arcfour_set_key(&state, sop->value.const_bytes, r_size(sop));
return filter_write(i_ctx_p, 0, &s_arcfour_template,
(stream_state *) & state, 0);
}
const op_def zfarc4_op_defs[] = {
op_def_begin_filter(),
{"2ArcfourDecode", z_arcfour_d},
{"2ArcfourEncode", z_arcfour_e},
op_def_end(0)
};