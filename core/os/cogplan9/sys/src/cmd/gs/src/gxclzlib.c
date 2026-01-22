#include "std.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gxclmem.h"
#include "szlibx.h"
private stream_zlib_state cl_zlibE_state;
private stream_zlib_state cl_zlibD_state;
void
gs_cl_zlib_init(gs_memory_t * mem)
{
s_zlib_set_defaults((stream_state *) & cl_zlibE_state);
cl_zlibE_state.no_wrapper = true;
cl_zlibE_state.template = &s_zlibE_template;
s_zlib_set_defaults((stream_state *) & cl_zlibD_state);
cl_zlibD_state.no_wrapper = true;
cl_zlibD_state.template = &s_zlibD_template;
}
const stream_state *
clist_compressor_state(void *client_data)
{
return (const stream_state *)&cl_zlibE_state;
}
const stream_state *
clist_decompressor_state(void *client_data)
{
return (const stream_state *)&cl_zlibD_state;
}