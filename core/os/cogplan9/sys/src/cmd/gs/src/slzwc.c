#include "std.h"
#include "strimpl.h"
#include "slzwx.h"
public_st_LZW_state();
void
s_LZW_set_defaults(stream_state * st)
{
stream_LZW_state *const ss = (stream_LZW_state *) st;
s_LZW_set_defaults_inline(ss);
}
void
s_LZW_release(stream_state * st)
{
stream_LZW_state *const ss = (stream_LZW_state *) st;
gs_free_object(st->memory, ss->table.decode, "LZW(close)");
}