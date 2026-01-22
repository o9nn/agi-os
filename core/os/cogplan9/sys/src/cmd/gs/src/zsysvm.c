#include "ghost.h"
#include "oper.h"
#include "ialloc.h"
#include "ivmspace.h"
#include "store.h"
private int
specific_vm_op(i_ctx_t *i_ctx_p, op_proc_t opproc, uint space)
{
uint save_space = icurrent_space;
int code;
ialloc_set_space(idmemory, space);
code = opproc(i_ctx_p);
ialloc_set_space(idmemory, save_space);
return code;
}
private int
zglobalvmarray(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zarray, avm_global);
}
private int
zglobalvmdict(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zdict, avm_global);
}
private int
zglobalvmpackedarray(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zpackedarray, avm_global);
}
private int
zglobalvmstring(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zstring, avm_global);
}
private int
zlocalvmarray(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zarray, avm_local);
}
private int
zlocalvmdict(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zdict, avm_local);
}
private int
zlocalvmpackedarray(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zpackedarray, avm_local);
}
private int
zlocalvmstring(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zstring, avm_local);
}
private int
zsystemvmarray(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zarray, avm_system);
}
private int
zsystemvmdict(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zdict, avm_system);
}
private int
zsystemvmpackedarray(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zpackedarray, avm_system);
}
private int
zsystemvmstring(i_ctx_t *i_ctx_p)
{
return specific_vm_op(i_ctx_p, zstring, avm_system);
}
private int
zsystemvmcheck(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
make_bool(op, (r_space(op) == avm_system ? true : false));
return 0;
}
const op_def zsysvm_op_defs[] =
{
{"1.globalvmarray", zglobalvmarray},
{"1.globalvmdict", zglobalvmdict},
{"1.globalvmpackedarray", zglobalvmpackedarray},
{"1.globalvmstring", zglobalvmstring},
{"1.localvmarray", zlocalvmarray},
{"1.localvmdict", zlocalvmdict},
{"1.localvmpackedarray", zlocalvmpackedarray},
{"1.localvmstring", zlocalvmstring},
{"1.systemvmarray", zsystemvmarray},
{"1.systemvmcheck", zsystemvmcheck},
{"1.systemvmdict", zsystemvmdict},
{"1.systemvmpackedarray", zsystemvmpackedarray},
{"1.systemvmstring", zsystemvmstring},
op_def_end(0)
};