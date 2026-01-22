#include "math_.h"
#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gsstruct.h"
#include "gxcspace.h"
#include "stream.h"
#include "files.h"
#include "gscolor2.h"
#include "gsicc.h"
#include "estack.h"
#include "idict.h"
#include "idparam.h"
#include "igstate.h"
#include "icie.h"
#include "ialloc.h"
private int
zseticcspace(i_ctx_t * i_ctx_p)
{
os_ptr op = osp;
int edepth = ref_stack_count(&e_stack);
int code;
gs_color_space * pcs;
const gs_color_space * palt_cs;
ref * pnval;
ref * pstrmval;
stream * s;
int i, ncomps;
gs_cie_icc * picc_info;
float range_buff[8];
static const float dflt_range[8] = { 0, 1, 0, 1, 0, 1, 0, 1 };
dict_find_string(op, "N", &pnval);
ncomps = pnval->value.intval;
if (dict_find_string(op, "DataSource", &pstrmval) <= 0)
return_error(e_undefined);
check_read_file(s, pstrmval);
palt_cs = gs_currentcolorspace(igs);
if ( !palt_cs->type->can_be_alt_space ||
gs_color_space_get_index(palt_cs) == gs_color_space_index_CIEICC )
return_error(e_rangecheck);
code = dict_floats_param( imemory,
op,
"Range",
2 * ncomps,
range_buff,
dflt_range );
for (i = 0; i < 2 * ncomps && range_buff[i + 1] >= range_buff[i]; i += 2)
;
if (i != 2 * ncomps)
return_error(e_rangecheck);
code = gs_cspace_build_CIEICC(&pcs, NULL, gs_state_memory(igs));
if (code < 0)
return code;
picc_info = pcs->params.icc.picc_info;
picc_info->num_components = ncomps;
picc_info->instrp = s;
picc_info->file_id = (s->read_id | s->write_id);
for (i = 0; i < ncomps; i++) {
picc_info->Range.ranges[i].rmin = range_buff[2 * i];
picc_info->Range.ranges[i].rmax = range_buff[2 * i + 1];
}
memmove( &pcs->params.icc.alt_space,
palt_cs,
sizeof(pcs->params.icc.alt_space) );
gx_increment_cspace_count(palt_cs);
code = gx_load_icc_profile(picc_info);
if (code < 0)
return code;
code = cie_cache_joint(i_ctx_p, &istate->colorrendering.procs,
(gs_cie_common *)picc_info, igs);
if (code < 0)
return code;
return cie_set_finish( i_ctx_p,
pcs,
&istate->colorspace.procs.cie,
edepth,
code );
}
const op_def zicc_ll3_op_defs[] = {
op_def_begin_ll3(),
{ "1.seticcspace", zseticcspace },
op_def_end(0)
};