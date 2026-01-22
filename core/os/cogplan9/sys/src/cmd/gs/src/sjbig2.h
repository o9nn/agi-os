#ifndef sjbig2_INCLUDED
# define sjbig2_INCLUDED
#include "stdint_.h"
#include "scommon.h"
#include <jbig2.h>
typedef struct stream_jbig2decode_state_s
{
stream_state_common;
Jbig2GlobalCtx *global_ctx;
Jbig2Ctx *decode_ctx;
Jbig2Image *image;
long offset;
int error;
}
stream_jbig2decode_state;
public int
s_jbig2decode_make_global_ctx(byte *data, uint length, Jbig2GlobalCtx **global_ctx);
public int
s_jbig2decode_set_global_ctx(stream_state *ss, Jbig2GlobalCtx *global_ctx);
#define private_st_jbig2decode_state() \
gs_private_st_simple(st_jbig2decode_state, stream_jbig2decode_state,\
"jbig2decode filter state")
extern const stream_template s_jbig2decode_template;
#endif