#ifndef __CARBON__
#include <Timer.h>
#else
#include <Carbon.h>
#endif
#include "gx.h"
#include "gp.h"
#include "gsdll.h"
#include "gpcheck.h"
#include "iapi.h"
#include "iref.h"
#include "iminst.h"
#include "imain.h"
#ifdef CHECK_INTERRUPTS
extern HWND hwndtext;
int gp_check_interrupts(const gs_memory_t *mem)
{
static unsigned long	lastYieldTicks = 0;
int iRetVal = 0;
if ((TickCount() - lastYieldTicks) > 2) {
lastYieldTicks = TickCount();
if (pgsdll_callback) {
iRetVal = (*pgsdll_callback)(GSDLL_POLL, 0, (long) hwndtext);
} else {
if (mem == NULL) {
mem = gs_lib_ctx_get_non_gc_memory_t();
}
if (mem && mem->gs_lib_ctx && mem->gs_lib_ctx->poll_fn)
iRetVal = (*mem->gs_lib_ctx->poll_fn)(mem->gs_lib_ctx->caller_handle);
}
}
return iRetVal;
}
#endif