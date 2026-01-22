#include "stdio_.h"
#include "jpeglib_.h"
#include "gsmemory.h"
#include "gsmalloc.h"
#include "strimpl.h"
#include "sdct.h"
public_st_DCT_state();
void
s_DCT_set_defaults(stream_state * st)
{
stream_DCT_state *const ss = (stream_DCT_state *) st;
ss->jpeg_memory = (gs_memory_t *)gs_lib_ctx_get_non_gc_memory_t();
ss->data.common = 0;
ss->ColorTransform = -1;
ss->QFactor = 1.0;
ss->Markers.data = 0;
ss->Markers.size = 0;
}