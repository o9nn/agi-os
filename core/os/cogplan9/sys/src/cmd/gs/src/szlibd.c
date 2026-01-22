#include "memory_.h"
#include "std.h"
#include "gsmemory.h"
#include "gsmalloc.h"
#include "strimpl.h"
#include "szlibxx.h"
private int
s_zlibD_init(stream_state * st)
{
stream_zlib_state *const ss = (stream_zlib_state *)st;
int code = s_zlib_alloc_dynamic_state(ss);
if (code < 0)
return ERRC;
if (inflateInit2(&ss->dynamic->zstate,
(ss->no_wrapper ? -ss->windowBits : ss->windowBits))
!= Z_OK
) {
s_zlib_free_dynamic_state(ss);
return ERRC;
}
st->min_left=1;
return 0;
}
private int
s_zlibD_reset(stream_state * st)
{
stream_zlib_state *const ss = (stream_zlib_state *)st;
if (inflateReset(&ss->dynamic->zstate) != Z_OK)
return ERRC;
return 0;
}
private int
s_zlibD_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool ignore_last)
{
stream_zlib_state *const ss = (stream_zlib_state *)st;
z_stream *zs = &ss->dynamic->zstate;
const byte *p = pr->ptr;
int status;
static const unsigned char jaws_empty[] = {0x58, 0x85, 1, 0, 0, 0, 0, 0, 1, 0x0A};
if (pw->ptr == pw->limit)
return 1;
if (pr->ptr == pr->limit)
return 0;
zs->next_in = (Bytef *)p + 1;
zs->avail_in = pr->limit - p;
zs->next_out = pw->ptr + 1;
zs->avail_out = pw->limit - pw->ptr;
if (zs->total_in == 0 && zs->avail_in >= 10 && !memcmp(zs->next_in, jaws_empty, 10)) {
pr->ptr += 10;
return EOFC;
}
status = inflate(zs, Z_PARTIAL_FLUSH);
pr->ptr = zs->next_in - 1;
pw->ptr = zs->next_out - 1;
switch (status) {
case Z_OK:
return (pw->ptr == pw->limit ? 1 : pr->ptr > p ? 0 : 1);
case Z_STREAM_END:
return EOFC;
default:
return ERRC;
}
}
private void
s_zlibD_release(stream_state * st)
{
stream_zlib_state *const ss = (stream_zlib_state *)st;
inflateEnd(&ss->dynamic->zstate);
s_zlib_free_dynamic_state(ss);
}
const stream_template s_zlibD_template = {
&st_zlib_state, s_zlibD_init, s_zlibD_process, 1, 1, s_zlibD_release,
s_zlib_set_defaults, s_zlibD_reset
};