#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gsstruct.h"
#include "gstypes.h"
#include "ialloc.h"
#include "idict.h"
#include "store.h"
#include "stream.h"
#include "strimpl.h"
#include "ifilter.h"
#include "sjpx.h"
private int
z_jpx_decode(i_ctx_t * i_ctx_p)
{
os_ptr op = osp;
ref *sop = NULL;
stream_jpxd_state state;
state.jpx_memory = imemory->non_gc_memory;
if (r_has_type(op, t_dictionary)) {
check_dict_read(*op);
if ( dict_find_string(op, "Colorspace", &sop) > 0) {
dlprintf("found Colorspace parameter (NYI)\n");
}
}
return filter_read(i_ctx_p, 0, &s_jpxd_template,
(stream_state *) & state, 0);
}
const op_def zfjpx_op_defs[] = {
op_def_begin_filter(),
{"2JPXDecode", z_jpx_decode},
op_def_end(0)
};