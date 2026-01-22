#include "gx.h"
#include "gp.h"
#include "gpcheck.h"
#include "iapi.h"
#include "iref.h"
#include "iminst.h"
#include "imain.h"
#ifdef CHECK_INTERRUPTS
int
gp_check_interrupts(const gs_memory_t *mem)
{
if(mem == NULL) {
mem = gs_lib_ctx_get_non_gc_memory_t();
}
if (mem && mem->gs_lib_ctx && mem->gs_lib_ctx->poll_fn)
return (*mem->gs_lib_ctx->poll_fn)(mem->gs_lib_ctx->caller_handle);
return 0;
}
#endif