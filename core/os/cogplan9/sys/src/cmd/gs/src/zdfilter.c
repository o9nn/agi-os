#include "string_.h"
#include "ghost.h"
#include "oper.h"
#include "ialloc.h"
#include "idict.h"
#include "igstate.h"
#include "iname.h"
#include "interp.h"
#include "iparam.h"
#include "ivmspace.h"
#include "gsmatrix.h"
#include "gsstate.h"
#include "gxdevice.h"
#include "store.h"
#include "gsdfilt.h"
private int
zpopdevicefilter(i_ctx_t *i_ctx_p)
{
gs_memory_t *mem = gs_memory_stable(imemory);
return gs_pop_device_filter(mem, igs);
}
const op_def zdfilter_op_defs[] =
{
{"0.popdevicefilter", zpopdevicefilter},
op_def_end(0)
};