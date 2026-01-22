#include "ghost.h"
#include "gsstruct.h"
#include "oper.h"
#include "estack.h"
#include "ialloc.h"
#include "idict.h"
#include "igstate.h"
#include "isave.h"
#include "dstack.h"
#include "stream.h"
#include "files.h"
#include "store.h"
#include "gsmatrix.h"
#include "gsstate.h"
private const bool I_VALIDATE_BEFORE_SAVE = true;
private const bool I_VALIDATE_AFTER_SAVE = true;
private const bool I_VALIDATE_BEFORE_RESTORE = true;
private const bool I_VALIDATE_AFTER_RESTORE = true;
typedef struct vm_save_s vm_save_t;
struct vm_save_s {
gs_state *gsave;
};
gs_private_st_ptrs1(st_vm_save, vm_save_t, "savetype",
vm_save_enum_ptrs, vm_save_reloc_ptrs, gsave);
private void
ivalidate_clean_spaces(i_ctx_t *i_ctx_p)
{
if (gs_debug_c('?')) {
ref_stack_cleanup(&d_stack);
ref_stack_cleanup(&e_stack);
ref_stack_cleanup(&o_stack);
ivalidate_spaces();
}
}
int
zsave(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
uint space = icurrent_space;
vm_save_t *vmsave;
ulong sid;
int code;
gs_state *prev;
if (I_VALIDATE_BEFORE_SAVE)
ivalidate_clean_spaces(i_ctx_p);
ialloc_set_space(idmemory, avm_local);
vmsave = ialloc_struct(vm_save_t, &st_vm_save, "zsave");
ialloc_set_space(idmemory, space);
if (vmsave == 0)
return_error(e_VMerror);
sid = alloc_save_state(idmemory, vmsave);
if (sid == 0) {
ifree_object(vmsave, "zsave");
return_error(e_VMerror);
}
if_debug2('u', "[u]vmsave 0x%lx, id = %lu\n",
(ulong) vmsave, (ulong) sid);
code = gs_gsave_for_save(igs, &prev);
if (code < 0)
return code;
code = gs_gsave(igs);
if (code < 0)
return code;
vmsave->gsave = prev;
push(1);
make_tav(op, t_save, 0, saveid, sid);
if (I_VALIDATE_AFTER_SAVE)
ivalidate_clean_spaces(i_ctx_p);
return 0;
}
private int restore_check_operand(os_ptr, alloc_save_t **, gs_dual_memory_t *);
private int restore_check_stack(const ref_stack_t *, const alloc_save_t *, bool);
private void restore_fix_stack(ref_stack_t *, const alloc_save_t *, bool);
int
zrestore(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
alloc_save_t *asave;
bool last;
vm_save_t *vmsave;
int code = restore_check_operand(op, &asave, idmemory);
if (code < 0)
return code;
if_debug2('u', "[u]vmrestore 0x%lx, id = %lu\n",
(ulong) alloc_save_client_data(asave),
(ulong) op->value.saveid);
if (I_VALIDATE_BEFORE_RESTORE)
ivalidate_clean_spaces(i_ctx_p);
osp--;
{
int code;
if ((code = restore_check_stack(&o_stack, asave, false)) < 0 ||
(code = restore_check_stack(&e_stack, asave, true)) < 0 ||
(code = restore_check_stack(&d_stack, asave, false)) < 0
) {
osp++;
return code;
}
}
restore_fix_stack(&o_stack, asave, false);
restore_fix_stack(&e_stack, asave, true);
restore_fix_stack(&d_stack, asave, false);
do {
vmsave = alloc_save_client_data(alloc_save_current(idmemory));
gs_grestoreall_for_restore(igs, vmsave->gsave);
vmsave->gsave = 0;
last = alloc_restore_state_step(asave);
}
while (!last);
{
uint space = icurrent_space;
ialloc_set_space(idmemory, avm_local);
ifree_object(vmsave, "zrestore");
ialloc_set_space(idmemory, space);
}
dict_set_top();
if (I_VALIDATE_AFTER_RESTORE)
ivalidate_clean_spaces(i_ctx_p);
i_ctx_p->LockFilePermissions = false;
return 0;
}
private int
restore_check_operand(os_ptr op, alloc_save_t ** pasave,
gs_dual_memory_t *idmem)
{
vm_save_t *vmsave;
ulong sid;
alloc_save_t *asave;
check_type(*op, t_save);
vmsave = r_ptr(op, vm_save_t);
if (vmsave == 0)
return_error(e_invalidrestore);
sid = op->value.saveid;
asave = alloc_find_save(idmem, sid);
if (asave == 0)
return_error(e_invalidrestore);
*pasave = asave;
return 0;
}
private int
restore_check_stack(const ref_stack_t * pstack, const alloc_save_t * asave,
bool is_estack)
{
ref_stack_enum_t rsenum;
ref_stack_enum_begin(&rsenum, pstack);
do {
const ref *stkp = rsenum.ptr;
uint size = rsenum.size;
for (; size; stkp++, size--) {
const void *ptr;
switch (r_type(stkp)) {
case t_array:
ptr = stkp->value.refs;
break;
case t_dictionary:
ptr = stkp->value.pdict;
break;
case t_file:
{
stream *s;
if (is_estack &&
(r_has_attr(stkp, a_executable) ||
file_is_invalid(s, stkp))
)
continue;
}
ptr = stkp->value.pfile;
break;
case t_name:
if (alloc_name_is_since_save((const gs_memory_t *)pstack->memory,
stkp, asave))
return_error(e_invalidrestore);
continue;
case t_string:
if (r_size(stkp) == 0 &&
r_has_attr(stkp, a_executable) && is_estack
)
continue;
ptr = stkp->value.bytes;
break;
case t_mixedarray:
case t_shortarray:
ptr = stkp->value.packed;
break;
case t_device:
ptr = stkp->value.pdevice;
break;
case t_fontID:
case t_struct:
case t_astruct:
ptr = stkp->value.pstruct;
break;
default:
continue;
}
if (alloc_is_since_save(ptr, asave))
return_error(e_invalidrestore);
}
} while (ref_stack_enum_next(&rsenum));
return 0;
}
private void
restore_fix_stack(ref_stack_t * pstack, const alloc_save_t * asave,
bool is_estack)
{
ref_stack_enum_t rsenum;
ref_stack_enum_begin(&rsenum, pstack);
do {
ref *stkp = rsenum.ptr;
uint size = rsenum.size;
for (; size; stkp++, size--) {
r_clear_attrs(stkp, l_new);
if (is_estack) {
ref ofile;
ref_assign(&ofile, stkp);
switch (r_type(stkp)) {
case t_string:
if (r_size(stkp) == 0 &&
alloc_is_since_save(stkp->value.bytes,
asave)
) {
make_empty_const_string(stkp,
avm_foreign);
break;
}
continue;
case t_file:
if (alloc_is_since_save(stkp->value.pfile,
asave)
) {
make_invalid_file(stkp);
break;
}
continue;
default:
continue;
}
r_copy_attrs(stkp, a_all | a_executable,
&ofile);
}
}
} while (ref_stack_enum_next(&rsenum));
}
private int
zvmstatus(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
gs_memory_status_t mstat, dstat;
gs_memory_status(imemory, &mstat);
if (imemory == imemory_global) {
gs_memory_status_t sstat;
gs_memory_status(imemory_system, &sstat);
mstat.allocated += sstat.allocated;
mstat.used += sstat.used;
}
gs_memory_status(imemory->non_gc_memory, &dstat);
push(3);
make_int(op - 2, imemory_save_level(iimemory_local));
make_int(op - 1, mstat.used);
make_int(op, mstat.allocated + dstat.allocated - dstat.used);
return 0;
}
private int
zforgetsave(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
alloc_save_t *asave;
vm_save_t *vmsave;
int code = restore_check_operand(op, &asave, idmemory);
if (code < 0)
return 0;
vmsave = alloc_save_client_data(asave);
restore_fix_stack(&o_stack, asave, false);
restore_fix_stack(&e_stack, asave, false);
restore_fix_stack(&d_stack, asave, false);
{
gs_state *pgs = igs;
gs_state *last;
while (gs_state_saved(last = gs_state_saved(pgs)) != 0)
pgs = last;
gs_state_swap_saved(last, vmsave->gsave);
gs_grestore(last);
gs_grestore(last);
}
alloc_forget_save(asave);
{
uint space = icurrent_space;
ialloc_set_space(idmemory, avm_local);
vmsave->gsave = 0;
ifree_object(vmsave, "zrestore");
ialloc_set_space(idmemory, space);
}
pop(1);
return 0;
}
const op_def zvmem_op_defs[] =
{
{"1.forgetsave", zforgetsave},
{"1restore", zrestore},
{"0save", zsave},
{"0vmstatus", zvmstatus},
op_def_end(0)
};