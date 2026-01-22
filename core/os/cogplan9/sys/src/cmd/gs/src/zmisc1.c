#include "memory_.h"
#include "ghost.h"
#include "oper.h"
#include "gscrypt1.h"
#include "stream.h"
#include "strimpl.h"
#include "sfilter.h"
#include "idict.h"
#include "idparam.h"
#include "ifilter.h"
private int type1crypt(i_ctx_t *,
int (*)(byte *, const byte *, uint, ushort *));
private int
ztype1encrypt(i_ctx_t *i_ctx_p)
{
return type1crypt(i_ctx_p, gs_type1_encrypt);
}
private int
ztype1decrypt(i_ctx_t *i_ctx_p)
{
return type1crypt(i_ctx_p, gs_type1_decrypt);
}
private int
type1crypt(i_ctx_t *i_ctx_p,
int (*proc)(byte *, const byte *, uint, ushort *))
{
os_ptr op = osp;
crypt_state state;
uint ssize;
check_type(op[-2], t_integer);
state = op[-2].value.intval;
if (op[-2].value.intval != state)
return_error(e_rangecheck);
check_read_type(op[-1], t_string);
check_write_type(*op, t_string);
ssize = r_size(op - 1);
if (r_size(op) < ssize)
return_error(e_rangecheck);
discard((*proc)(op->value.bytes, op[-1].value.const_bytes, ssize,
&state));
op[-2].value.intval = state;
op[-1] = *op;
r_set_size(op - 1, ssize);
pop(1);
return 0;
}
private int
eexec_param(os_ptr op, ushort * pcstate)
{
int npop = 1;
if (r_has_type(op, t_dictionary))
++npop, --op;
check_type(*op, t_integer);
*pcstate = op->value.intval;
if (op->value.intval != *pcstate)
return_error(e_rangecheck);
return npop;
}
private int
zexE(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream_exE_state state;
int code = eexec_param(op, &state.cstate);
if (code < 0)
return code;
return filter_write(i_ctx_p, code, &s_exE_template, (stream_state *)&state, 0);
}
private int
zexD(i_ctx_t *i_ctx_p)
{
os_ptr op = osp;
stream_exD_state state;
int code;
(*s_exD_template.set_defaults)((stream_state *)&state);
if (r_has_type(op, t_dictionary)) {
uint cstate;
bool is_eexec;
check_dict_read(*op);
if ((code = dict_uint_param(op, "seed", 0, 0xffff, 0x10000,
&cstate)) < 0 ||
(code = dict_int_param(op, "lenIV", 0, max_int, 4,
&state.lenIV)) < 0 ||
(code = dict_bool_param(op, "eexec", false,
&is_eexec)) < 0
)
return code;
state.cstate = cstate;
state.binary = (is_eexec ? -1 : 1);
code = 1;
} else {
state.binary = 1;
code = eexec_param(op, &state.cstate);
}
if (code < 0)
return code;
if (r_has_type(op - 1, t_file)) {
stream *s = (op - 1)->value.pfile;
if (s->state != 0 && s->state->template == &s_PFBD_template) {
stream_PFBD_state *pss = (stream_PFBD_state *)s->state;
state.pfb_state = pss;
if (pss->record_type == 2) {
if (pss->binary_to_hex && sbufavailable(s) > 0) {
state.binary = 0;
state.hex_left = sbufavailable(s);
} else {
state.binary = 1;
}
pss->binary_to_hex = 0;
}
state.record_left = pss->record_left;
}
}
return filter_read(i_ctx_p, code, &s_exD_template, (stream_state *)&state, 0);
}
const op_def zmisc1_op_defs[] =
{
{"3.type1encrypt", ztype1encrypt},
{"3.type1decrypt", ztype1decrypt},
op_def_begin_filter(),
{"2eexecEncode", zexE},
{"2eexecDecode", zexD},
op_def_end(0)
};