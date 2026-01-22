#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gsstruct.h"
#include "gscolor.h"
#include "gsmatrix.h"
#include "gxcspace.h"
#include "gxfixed.h"
#include "gxcolor2.h"
#include "estack.h"
#include "ialloc.h"
#include "icsmap.h"
#include "ifunc.h"
#include "igstate.h"
#include "iname.h"
#include "ivmspace.h"
#include "store.h"
#include "gscsepr.h"
#include "gscdevn.h"
#include "gxcdevn.h"
#include "zht2.h"
extern const gs_color_space_type gs_color_space_type_Separation;
extern const gs_color_space_type gs_color_space_type_DeviceN;
private int
zsetseparationspace(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
const ref *pcsa;
gs_color_space cs;
const gs_color_space * pacs;
ref_colorspace cspace_old;
ref sname, name_none, name_all;
gs_device_n_map *pmap = NULL;
gs_function_t *pfn = NULL;
separation_type sep_type;
int code;
const gs_memory_t * mem = imemory;
check_read_type(*op, t_array);
if (r_size(op) != 4)
return_error(e_rangecheck);
pacs = gs_currentcolorspace(igs);
cs = *pacs;
if (!cs.type->can_be_alt_space)
return_error(e_rangecheck);
pcsa = op->value.const_refs + 1;
sname = *pcsa;
switch (r_type(&sname)) {
default:
return_error(e_typecheck);
case t_string:
code = name_from_string(mem, &sname, &sname);
if (code < 0)
return code;
case t_name:
break;
}
if ((code = name_ref(mem, (const byte *)"All", 3, &name_all, 0)) < 0)
return code;
if ((code = name_ref(mem, (const byte *)"None", 4, &name_none, 0)) < 0)
return code;
sep_type = ( name_eq(&sname, &name_all) ? SEP_ALL :
name_eq(&sname, &name_none) ? SEP_NONE : SEP_OTHER);
check_proc(pcsa[2]);
pfn = ref_function(pcsa + 2);
if (pfn == NULL)
return_error(e_rangecheck);
cspace_old = istate->colorspace;
memmove(&cs.params.separation.alt_space, &cs,
sizeof(cs.params.separation.alt_space));
code = gs_build_Separation(&cs, pacs, imemory);
if (code < 0)
return code;
pmap = cs.params.separation.map;
gs_cspace_init(&cs, &gs_color_space_type_Separation, imemory, false);
cs.params.separation.sep_type = sep_type;
cs.params.separation.sep_name = name_index(mem, &sname);
cs.params.separation.get_colorname_string = gs_get_colorname_string;
istate->colorspace.procs.special.separation.layer_name = pcsa[0];
istate->colorspace.procs.special.separation.tint_transform = pcsa[2];
if (code >= 0)
code = gs_cspace_set_sepr_function(&cs, pfn);
if (code >= 0)
code = gs_setcolorspace(igs, &cs);
if (code < 0) {
istate->colorspace = cspace_old;
ifree_object(pmap, ".setseparationspace(pmap)");
return code;
}
rc_decrement(pmap, ".setseparationspace(pmap)");
pop(1);
return 0;
}
private int
zcurrentoverprint(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
push(1);
make_bool(op, gs_currentoverprint(igs));
return 0;
}
private int
zsetoverprint(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
check_type(*op, t_boolean);
gs_setoverprint(igs, op->value.boolval);
pop(1);
return 0;
}
private int
zcurrentoverprintmode(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
push(1);
make_int(op, gs_currentoverprintmode(igs));
return 0;
}
private int
zsetoverprintmode(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
int param;
int code = int_param(op, max_int, &param);
if (code < 0 || (code = gs_setoverprintmode(igs, param)) < 0)
return code;
pop(1);
return 0;
}
const op_def zcssepr_l2_op_defs[] =
{
op_def_begin_level2(),
{"0currentoverprint", zcurrentoverprint},
{"0.currentoverprintmode", zcurrentoverprintmode},
{"1setoverprint", zsetoverprint},
{"1.setoverprintmode", zsetoverprintmode},
{"1.setseparationspace", zsetseparationspace},
op_def_end(0)
};