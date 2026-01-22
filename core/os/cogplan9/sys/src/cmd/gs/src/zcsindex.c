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
#include "igstate.h"
#include "ivmspace.h"
#include "store.h"
extern const gs_color_space_type gs_color_space_type_Indexed;
private int indexed_map1(i_ctx_t *);
private int
zsetindexedspace(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
ref *pproc = &istate->colorspace.procs.special.index_proc;
const ref *pcsa;
gs_color_space cs;
ref_colorspace cspace_old;
uint edepth = ref_stack_count(&e_stack);
int num_entries;
int code;
check_read_type(*op, t_array);
if (r_size(op) != 4)
return_error(e_rangecheck);
pcsa = op->value.const_refs + 1;
check_type_only(pcsa[1], t_integer);
if (pcsa[1].value.intval < 0 || pcsa[1].value.intval > 4095)
return_error(e_rangecheck);
num_entries = (int)pcsa[1].value.intval + 1;
cs = *gs_currentcolorspace(igs);
if (!cs.type->can_be_base_space)
return_error(e_rangecheck);
cspace_old = istate->colorspace;
if (r_has_type(&pcsa[2], t_string)) {
int num_values = num_entries * cs_num_components(&cs);
check_read(pcsa[2]);
if (r_size(&pcsa[2]) < num_values)
return_error(e_rangecheck);
memmove(&cs.params.indexed.base_space, &cs,
sizeof(cs.params.indexed.base_space));
gs_cspace_init(&cs, &gs_color_space_type_Indexed, imemory, false);
cs.params.indexed.lookup.table.data = pcsa[2].value.const_bytes;
cs.params.indexed.lookup.table.size = num_values;
cs.params.indexed.use_proc = 0;
make_null(pproc);
code = 0;
} else {
gs_indexed_map *map;
check_proc(pcsa[2]);
code = zcs_begin_map(i_ctx_p, &map, &pcsa[2], num_entries,
(const gs_direct_color_space *)&cs,
indexed_map1);
if (code < 0)
return code;
memmove(&cs.params.indexed.base_space, &cs,
sizeof(cs.params.indexed.base_space));
gs_cspace_init(&cs, &gs_color_space_type_Indexed, imemory, false);
cs.params.indexed.use_proc = 1;
*pproc = pcsa[2];
map->proc.lookup_index = lookup_indexed_map;
cs.params.indexed.lookup.map = map;
}
cs.params.indexed.hival = num_entries - 1;
code = gs_setcolorspace(igs, &cs);
if (code < 0) {
istate->colorspace = cspace_old;
ref_stack_pop_to(&e_stack, edepth);
return code;
}
pop(1);
return (ref_stack_count(&e_stack) == edepth ? 0 : o_push_estack);
}
private int
indexed_map1(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
es_ptr ep = esp;
int i = (int)ep[csme_index].value.intval;
if (i >= 0) {
int m = (int)ep[csme_num_components].value.intval;
int code = float_params(op, m, &r_ptr(&ep[csme_map], gs_indexed_map)->values[i * m]);
if (code < 0)
return code;
pop(m);
op -= m;
if (i == (int)ep[csme_hival].value.intval) {
esp -= num_csme;
return o_pop_estack;
}
}
push(1);
ep[csme_index].value.intval = ++i;
make_int(op, i);
make_op_estack(ep + 1, indexed_map1);
ep[2] = ep[csme_proc];
esp = ep + 2;
return o_push_estack;
}
const op_def zcsindex_l2_op_defs[] =
{
op_def_begin_level2(),
{"1.setindexedspace", zsetindexedspace},
{"1%indexed_map1", indexed_map1},
op_def_end(0)
};
int
zcs_begin_map(i_ctx_t *i_ctx_p, gs_indexed_map ** pmap, const ref * pproc,
int num_entries, const gs_direct_color_space * base_space,
op_proc_t map1)
{
gs_memory_t *mem = gs_state_memory(igs);
int space = imemory_space((gs_ref_memory_t *)mem);
int num_components =
cs_num_components((const gs_color_space *)base_space);
int num_values = num_entries * num_components;
gs_indexed_map *map;
int code = alloc_indexed_map(&map, num_values, mem,
"setcolorspace(mapped)");
es_ptr ep;
if (code < 0)
return code;
rc_init_free(map, mem, 0, free_indexed_map);
*pmap = map;
check_estack(num_csme + 1);
ep = esp += num_csme;
make_int(ep + csme_num_components, num_components);
make_struct(ep + csme_map, space, map);
ep[csme_proc] = *pproc;
make_int(ep + csme_hival, num_entries - 1);
make_int(ep + csme_index, -1);
push_op_estack(map1);
return o_push_estack;
}