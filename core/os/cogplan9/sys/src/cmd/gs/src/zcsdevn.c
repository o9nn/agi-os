#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gxcspace.h"
#include "gscolor2.h"
#include "gscdevn.h"
#include "gxcdevn.h"
#include "estack.h"
#include "ialloc.h"
#include "icremap.h"
#include "ifunc.h"
#include "igstate.h"
#include "iname.h"
#include "zht2.h"
extern const gs_color_space_type gs_color_space_type_DeviceN;
private int
zsetdevicenspace(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
const ref *pcsa;
gs_separation_name *names;
gs_device_n_map *pmap;
uint num_components;
gs_color_space cs;
const gs_color_space * pacs;
ref_colorspace cspace_old;
gs_function_t *pfn;
int code;
check_read_type(*op, t_array);
if (r_size(op) != 4)
return_error(e_rangecheck);
pcsa = op->value.const_refs + 1;
if (!r_is_array(pcsa))
return_error(e_typecheck);
num_components = r_size(pcsa);
if (num_components == 0)
return_error(e_rangecheck);
if (num_components > GS_CLIENT_COLOR_MAX_COMPONENTS)
return_error(e_limitcheck);
check_proc(pcsa[2]);
pacs = gs_currentcolorspace(igs);
cs = *pacs;
memmove(&cs.params.device_n.alt_space, &cs,
sizeof(cs.params.device_n.alt_space));
gs_cspace_init(&cs, &gs_color_space_type_DeviceN, imemory, false);
code = gs_build_DeviceN(&cs, num_components, pacs, imemory);
if (code < 0)
return code;
names = cs.params.device_n.names;
pmap = cs.params.device_n.map;
cs.params.device_n.get_colorname_string = gs_get_colorname_string;
{
uint i;
ref sname;
for (i = 0; i < num_components; ++i) {
array_get(imemory, pcsa, (long)i, &sname);
switch (r_type(&sname)) {
case t_string:
code = name_from_string(imemory, &sname, &sname);
if (code < 0) {
ifree_object(names, ".setdevicenspace(names)");
ifree_object(pmap, ".setdevicenspace(map)");
return code;
}
case t_name:
names[i] = name_index(imemory, &sname);
break;
default:
ifree_object(names, ".setdevicenspace(names)");
ifree_object(pmap, ".setdevicenspace(map)");
return_error(e_typecheck);
}
}
}
cspace_old = istate->colorspace;
istate->colorspace.procs.special.device_n.layer_names = pcsa[0];
istate->colorspace.procs.special.device_n.tint_transform = pcsa[2];
pfn = ref_function(pcsa + 2);
if (!pfn)
code = gs_note_error(e_rangecheck);
if (code < 0) {
istate->colorspace = cspace_old;
ifree_object(names, ".setdevicenspace(names)");
ifree_object(pmap, ".setdevicenspace(map)");
return code;
}
gs_cspace_set_devn_function(&cs, pfn);
code = gs_setcolorspace(igs, &cs);
if (code < 0) {
istate->colorspace = cspace_old;
return code;
}
rc_decrement(pmap, ".setdevicenspace(map)");
pop(1);
return 0;
}
const op_def zcsdevn_op_defs[] =
{
op_def_begin_ll3(),
{"1.setdevicenspace", zsetdevicenspace},
op_def_end(0)
};