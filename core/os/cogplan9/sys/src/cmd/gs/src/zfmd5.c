#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gsstruct.h"
#include "ialloc.h"
#include "stream.h"
#include "strimpl.h"
#include "smd5.h"
#include "ifilter.h"
private int
zMD5E(i_ctx_t *i_ctx_p)
{
return filter_write_simple(i_ctx_p, &s_MD5E_template);
}
const op_def zfmd5_op_defs[] =
{
op_def_begin_filter(),
{"1MD5Encode", zMD5E},
op_def_end(0)
};