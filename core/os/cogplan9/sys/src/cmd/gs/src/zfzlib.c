#include "ghost.h"
#include "oper.h"
#include "idict.h"
#include "strimpl.h"
#include "spdiffx.h"
#include "spngpx.h"
#include "szlibx.h"
#include "idparam.h"
#include "ifilter.h"
#include "ifrpred.h"
#include "ifwpred.h"
private int
filter_zlib(i_ctx_t *i_ctx_p, stream_zlib_state *pzls)
{
os_ptr op = osp;
int code = 0;
(*s_zlibE_template.set_defaults)((stream_state *)pzls);
if (r_has_type(op, t_dictionary))
code = dict_int_param(op, "Effort", -1, 9, -1, &pzls->level);
return code;
}
private int
zzlibE(i_ctx_t *i_ctx_p)
{
stream_zlib_state zls;
int code = filter_zlib(i_ctx_p, &zls);
if (code < 0)
return code;
return filter_write(i_ctx_p, 0, &s_zlibE_template, (stream_state *)&zls, 0);
}
private int
zzlibD(i_ctx_t *i_ctx_p)
{
stream_zlib_state zls;
(*s_zlibD_template.set_defaults)((stream_state *)&zls);
return filter_read(i_ctx_p, 0, &s_zlibD_template, (stream_state *)&zls, 0);
}
private int
zFlateE(i_ctx_t *i_ctx_p)
{
stream_zlib_state zls;
int code = filter_zlib(i_ctx_p, &zls);
if (code < 0)
return code;
return filter_write_predictor(i_ctx_p, 0, &s_zlibE_template,
(stream_state *)&zls);
}
private int
zFlateD(i_ctx_t *i_ctx_p)
{
stream_zlib_state zls;
(*s_zlibD_template.set_defaults)((stream_state *)&zls);
return filter_read_predictor(i_ctx_p, 0, &s_zlibD_template,
(stream_state *)&zls);
}
const op_def zfzlib_op_defs[] =
{
op_def_begin_filter(),
{"1zlibEncode", zzlibE},
{"1zlibDecode", zzlibD},
{"1FlateEncode", zFlateE},
{"1FlateDecode", zFlateD},
op_def_end(0)
};