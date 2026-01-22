#include "memory_.h"
#include "stdio_.h"
#include "jpeglib_.h"
#include "ghost.h"
#include "oper.h"
#include "gsmalloc.h"
#include "strimpl.h"
#include "sdct.h"
#include "sjpeg.h"
#include "ialloc.h"
#include "ifilter.h"
#include "iparam.h"
private_st_jpeg_decompress_data();
stream_state_proc_put_params(s_DCTD_put_params, stream_DCT_state);
private int
zDCTD(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_memory_t *mem = (gs_memory_t *)(i_ctx_p->memory.current);
stream_DCT_state state;
dict_param_list list;
jpeg_decompress_data *jddp;
int code;
const ref *dop;
uint dspace;
jddp = gs_alloc_struct_immovable(mem,jpeg_decompress_data,
&st_jpeg_decompress_data, "zDCTD");
if (jddp == 0)
return_error(e_VMerror);
if (s_DCTD_template.set_defaults)
(*s_DCTD_template.set_defaults) ((stream_state *) & state);
state.data.decompress = jddp;
jddp->memory = state.jpeg_memory = mem;
jddp->scanline_buffer = NULL;
state.report_error = filter_report_error;
if ((code = gs_jpeg_create_decompress(&state)) < 0)
goto fail;
if (r_has_type(op, t_dictionary))
dop = op, dspace = r_space(op);
else
dop = 0, dspace = 0;
if ((code = dict_param_list_read(&list, dop, NULL, false, iimemory)) < 0)
goto fail;
if ((code = s_DCTD_put_params((gs_param_list *) & list, &state)) < 0)
goto rel;
jddp->template = s_DCTD_template;
code = filter_read(i_ctx_p, 0, &jddp->template,
(stream_state *) & state, dspace);
if (code >= 0)
return code;
rel:
iparam_list_release(&list);
fail:
gs_jpeg_destroy(&state);
gs_free_object(mem, jddp, "zDCTD fail");
return code;
}
const op_def zfdctd_op_defs[] =
{
op_def_begin_filter(),
{"2DCTDecode", zDCTD},
op_def_end(0)
};