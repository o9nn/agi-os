#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gsstruct.h"
#include "ialloc.h"
#include "stream.h"
#include "strimpl.h"
#include "sbcp.h"
#include "ifilter.h"
private int
no_bcp_signal_interrupt(stream_state * st)
{
return 0;
}
private int
no_bcp_request_status(stream_state * st)
{
return 0;
}
private int
zBCPE(i_ctx_t *i_ctx_p)
{
return filter_write_simple(i_ctx_p, &s_BCPE_template);
}
private int
zBCPD(i_ctx_t *i_ctx_p)
{
stream_BCPD_state state;
state.signal_interrupt = no_bcp_signal_interrupt;
state.request_status = no_bcp_request_status;
return filter_read(i_ctx_p, 0, &s_BCPD_template, (stream_state *)&state, 0);
}
private int
zTBCPE(i_ctx_t *i_ctx_p)
{
return filter_write_simple(i_ctx_p, &s_TBCPE_template);
}
private int
zTBCPD(i_ctx_t *i_ctx_p)
{
stream_BCPD_state state;
state.signal_interrupt = no_bcp_signal_interrupt;
state.request_status = no_bcp_request_status;
return filter_read(i_ctx_p, 0, &s_TBCPD_template, (stream_state *)&state, 0);
}
const op_def zfbcp_op_defs[] =
{
op_def_begin_filter(),
{"1BCPEncode", zBCPE},
{"1BCPDecode", zBCPD},
{"1TBCPEncode", zTBCPE},
{"1TBCPDecode", zTBCPD},
op_def_end(0)
};