#include "gx.h"
#include "memory_.h"
#include "ierrors.h"
#include "gsstruct.h"
#include "iref.h"
#include "iastate.h"
#include "igc.h"
#include "ipacked.h"
#include "iutil.h"
#include "ivmspace.h"
#include "store.h"
public_st_gs_dual_memory();
int
ialloc_init(gs_dual_memory_t *dmem, gs_memory_t * rmem, uint chunk_size,
bool level2)
{
gs_ref_memory_t *ilmem = ialloc_alloc_state(rmem, chunk_size);
gs_ref_memory_t *ilmem_stable = ialloc_alloc_state(rmem, chunk_size);
gs_ref_memory_t *igmem = 0;
gs_ref_memory_t *igmem_stable = 0;
gs_ref_memory_t *ismem = ialloc_alloc_state(rmem, chunk_size);
int i;
if (ilmem == 0 || ilmem_stable == 0 || ismem == 0)
goto fail;
ilmem->stable_memory = (gs_memory_t *)ilmem_stable;
if (level2) {
igmem = ialloc_alloc_state(rmem, chunk_size);
igmem_stable = ialloc_alloc_state(rmem, chunk_size);
if (igmem == 0 || igmem_stable == 0)
goto fail;
igmem->stable_memory = (gs_memory_t *)igmem_stable;
} else
igmem = ilmem, igmem_stable = ilmem_stable;
for (i = 0; i < countof(dmem->spaces_indexed); i++)
dmem->spaces_indexed[i] = 0;
dmem->space_local = ilmem;
dmem->space_global = igmem;
dmem->space_system = ismem;
dmem->spaces.vm_reclaim = gs_gc_reclaim;
dmem->reclaim = 0;
igmem->space = avm_global;
igmem_stable->space = avm_global;
ilmem->space = avm_local;
ilmem_stable->space = avm_local;
ismem->space = avm_system;
# if IGC_PTR_STABILITY_CHECK
igmem->space_id = (i_vm_global << 1) + 1;
igmem_stable->space_id = i_vm_global << 1;
ilmem->space_id = (i_vm_local << 1) + 1;
ilmem_stable->space_id = i_vm_local << 1;
ismem->space_id = (i_vm_system << 1);
# endif
ialloc_set_space(dmem, avm_global);
return 0;
fail:
gs_free_object(rmem, igmem_stable, "ialloc_init failure");
gs_free_object(rmem, igmem, "ialloc_init failure");
gs_free_object(rmem, ismem, "ialloc_init failure");
gs_free_object(rmem, ilmem_stable, "ialloc_init failure");
gs_free_object(rmem, ilmem, "ialloc_init failure");
return_error(e_VMerror);
}
uint
imemory_space(const gs_ref_memory_t * iimem)
{
return iimem->space;
}
void
ialloc_set_space(gs_dual_memory_t * dmem, uint space)
{
gs_ref_memory_t *mem = dmem->spaces_indexed[space >> r_space_shift];
dmem->current = mem;
dmem->current_space = mem->space;
}
uint
imemory_new_mask(const gs_ref_memory_t *imem)
{
return imem->new_mask;
}
int
imemory_save_level(const gs_ref_memory_t *imem)
{
return imem->save_level;
}
void
ialloc_reset_requested(gs_dual_memory_t * dmem)
{
dmem->space_system->gc_status.requested = 0;
dmem->space_global->gc_status.requested = 0;
dmem->space_local->gc_status.requested = 0;
}
#ifdef DEBUG
private int
ialloc_trace_space(const gs_ref_memory_t *imem)
{
return imem->space + (imem->stable_memory == (const gs_memory_t *)imem);
}
#endif
int
gs_register_ref_root(gs_memory_t *mem, gs_gc_root_t *root,
void **pp, client_name_t cname)
{
return gs_register_root(mem, root, ptr_ref_type, pp, cname);
}
int
gs_alloc_ref_array(gs_ref_memory_t * mem, ref * parr, uint attrs,
uint num_refs, client_name_t cname)
{
ref *obj;
if (mem->cc.rtop == mem->cc.cbot &&
num_refs < (mem->cc.ctop - mem->cc.cbot) / sizeof(ref) &&
mem->cc.rtop - (byte *) mem->cc.rcur + num_refs * sizeof(ref) <
max_size_st_refs
) {
ref *end;
obj = (ref *) mem->cc.rtop - 1;
if_debug4('A', "[a%d:+$ ]%s(%u) = 0x%lx\n",
ialloc_trace_space(mem), client_name_string(cname),
num_refs, (ulong) obj);
mem->cc.rcur[-1].o_size += num_refs * sizeof(ref);
end = (ref *) (mem->cc.rtop = mem->cc.cbot +=
num_refs * sizeof(ref));
make_mark(end - 1);
} else {
chunk_t *pcc = mem->pcc;
ref *end;
obj = gs_alloc_struct_array((gs_memory_t *) mem, num_refs + 1,
ref, &st_refs, cname);
if (obj == 0)
return_error(e_VMerror);
end = (ref *) obj + num_refs;
make_mark(end);
if (mem->pcc != pcc || mem->cc.cbot == (byte *) (end + 1)) {
mem->cc.rcur = (obj_header_t *) obj;
mem->cc.rtop = (byte *) (end + 1);
mem->cc.has_refs = true;
} else {
chunk_locator_t cl;
cl.memory = mem;
cl.cp = mem->clast;
chunk_locate_ptr(obj, &cl);
cl.cp->has_refs = true;
}
}
make_array(parr, attrs | mem->space, num_refs, obj);
return 0;
}
int
gs_resize_ref_array(gs_ref_memory_t * mem, ref * parr,
uint new_num_refs, client_name_t cname)
{
uint old_num_refs = r_size(parr);
uint diff;
ref *obj = parr->value.refs;
if (new_num_refs > old_num_refs || !r_has_type(parr, t_array))
return_error(e_Fatal);
diff = old_num_refs - new_num_refs;
if (mem->cc.rtop == mem->cc.cbot &&
(byte *) (obj + (old_num_refs + 1)) == mem->cc.rtop
) {
ref *end = (ref *) (mem->cc.cbot = mem->cc.rtop -=
diff * sizeof(ref));
if_debug4('A', "[a%d:<$ ]%s(%u) 0x%lx\n",
ialloc_trace_space(mem), client_name_string(cname), diff,
(ulong) obj);
mem->cc.rcur[-1].o_size -= diff * sizeof(ref);
make_mark(end - 1);
} else {
if_debug4('A', "[a%d:<$#]%s(%u) 0x%lx\n",
ialloc_trace_space(mem), client_name_string(cname), diff,
(ulong) obj);
mem->lost.refs += diff * sizeof(ref);
}
r_set_size(parr, new_num_refs);
return 0;
}
void
gs_free_ref_array(gs_ref_memory_t * mem, ref * parr, client_name_t cname)
{
uint num_refs = r_size(parr);
ref *obj = parr->value.refs;
if (!r_has_type(parr, t_array))
DO_NOTHING;
else if (mem->cc.rtop == mem->cc.cbot &&
(byte *) (obj + (num_refs + 1)) == mem->cc.rtop
) {
if ((obj_header_t *) obj == mem->cc.rcur) {
gs_free_object((gs_memory_t *) mem, obj, cname);
mem->cc.rcur = 0;
mem->cc.rtop = 0;
} else {
if_debug4('A', "[a%d:-$ ]%s(%u) 0x%lx\n",
ialloc_trace_space(mem), client_name_string(cname),
num_refs, (ulong) obj);
mem->cc.rcur[-1].o_size -= num_refs * sizeof(ref);
mem->cc.rtop = mem->cc.cbot = (byte *) (obj + 1);
make_mark(obj);
}
return;
} else if (num_refs >= (mem->large_size / arch_sizeof_ref - 1)) {
chunk_locator_t cl;
cl.memory = mem;
cl.cp = mem->clast;
if (chunk_locate_ptr(obj, &cl) &&
obj == (ref *) ((obj_header_t *) (cl.cp->cbase) + 1) &&
(byte *) (obj + (num_refs + 1)) == cl.cp->cend
) {
if_debug4('a', "[a%d:-$L]%s(%u) 0x%lx\n",
ialloc_trace_space(mem), client_name_string(cname),
num_refs, (ulong) obj);
alloc_free_chunk(cl.cp, mem);
return;
}
}
if_debug4('A', "[a%d:-$#]%s(%u) 0x%lx\n",
ialloc_trace_space(mem), client_name_string(cname), num_refs,
(ulong) obj);
{
uint size;
switch (r_type(parr)) {
case t_shortarray:
size = num_refs * sizeof(ref_packed);
break;
case t_mixedarray:{
uint i = 0;
const ref_packed *p = parr->value.packed;
for (; i < num_refs; ++i)
p = packed_next(p);
size = (const byte *)p - (const byte *)parr->value.packed;
break;
}
case t_array:
size = num_refs * sizeof(ref);
break;
default:
lprintf3("Unknown type 0x%x in free_ref_array(%u,0x%lx)!",
r_type(parr), num_refs, (ulong) obj);
return;
}
refset_null_new(obj, size / sizeof(ref), 0);
mem->lost.refs += size;
}
}
int
gs_alloc_string_ref(gs_ref_memory_t * mem, ref * psref,
uint attrs, uint nbytes, client_name_t cname)
{
byte *str = gs_alloc_string((gs_memory_t *) mem, nbytes, cname);
if (str == 0)
return_error(e_VMerror);
make_string(psref, attrs | mem->space, nbytes, str);
return 0;
}