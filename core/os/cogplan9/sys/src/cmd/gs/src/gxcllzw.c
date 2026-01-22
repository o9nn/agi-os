#include "std.h"
#include "gstypes.h"
#include "gsmemory.h"
#include "gxclmem.h"
#include "slzwx.h"
private stream_LZW_state cl_LZWE_state;
private stream_LZW_state cl_LZWD_state;
void
gs_cl_lzw_init(gs_memory_t * mem)
{
s_LZW_set_defaults((stream_state *) & cl_LZWE_state);
cl_LZWE_state.template = &s_LZWE_template;
s_LZW_set_defaults((stream_state *) & cl_LZWD_state);
cl_LZWD_state.template = &s_LZWD_template;
}
const stream_state *
clist_compressor_state(void *client_data)
{
return (const stream_state *)&cl_LZWE_state;
}
const stream_state *
clist_decompressor_state(void *client_data)
{
return (const stream_state *)&cl_LZWD_state;
}