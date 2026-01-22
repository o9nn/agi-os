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
#include "sjbig2.h"
typedef struct jbig2_global_data_s {
Jbig2GlobalCtx *global_ctx;
} jbig2_global_data_t;
private void jbig2_global_data_finalize(void *vptr);
gs_private_st_simple_final(st_jbig2_global_data_t, jbig2_global_data_t,
"jbig2globalctx", jbig2_global_data_finalize);
private int
z_jbig2decode(i_ctx_t * i_ctx_p)
{
os_ptr op = osp;
ref *sop = NULL;
jbig2_global_data_t *gref;
stream_jbig2decode_state state;
s_jbig2decode_set_global_ctx((stream_state*)&state, NULL);
if (r_has_type(op, t_dictionary)) {
check_dict_read(*op);
if ( dict_find_string(op, ".jbig2globalctx", &sop) > 0) {
gref = r_ptr(sop, jbig2_global_data_t);
s_jbig2decode_set_global_ctx((stream_state*)&state, gref->global_ctx);
}
}
return filter_read(i_ctx_p, 0, &s_jbig2decode_template,
(stream_state *) & state, 0);
}
private int
z_jbig2makeglobalctx(i_ctx_t * i_ctx_p)
{
Jbig2GlobalCtx *global_ctx = NULL;
jbig2_global_data_t *st;
os_ptr op = osp;
byte *data;
int size;
int code = 0;
check_type(*op, t_astruct);
size = gs_object_size(imemory, op->value.pstruct);
data = r_ptr(op, byte);
code = s_jbig2decode_make_global_ctx(data, size,
&global_ctx);
if (size > 0 && global_ctx == NULL) {
dlprintf("failed to create parsed JBIG2GLOBALS object.");
return_error(e_unknownerror);
}
st = ialloc_struct(jbig2_global_data_t,
&st_jbig2_global_data_t,
"jbig2decode parsed global context");
if (st == NULL) return_error(e_VMerror);
st->global_ctx = global_ctx;
make_astruct(op, a_readonly | icurrent_space, (byte*)st);
return code;
}
private void jbig2_global_data_finalize(void *vptr)
{
jbig2_global_data_t *st = vptr;
if (st->global_ctx) jbig2_global_ctx_free(st->global_ctx);
st->global_ctx = NULL;
}
const op_def zfjbig2_op_defs[] = {
{"1.jbig2makeglobalctx", z_jbig2makeglobalctx},
op_def_begin_filter(),
{"2JBIG2Decode", z_jbig2decode},
op_def_end(0)
};