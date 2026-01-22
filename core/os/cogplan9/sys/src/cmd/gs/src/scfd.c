#include "stdio_.h"
#include "memory_.h"
#include "gdebug.h"
#include "strimpl.h"
#include "scf.h"
#include "scfx.h"
private_st_CFD_state();
private void
s_CFD_set_defaults(register stream_state * st)
{
stream_CFD_state *const ss = (stream_CFD_state *) st;
s_CFD_set_defaults_inline(ss);
}
private int
s_CFD_init(stream_state * st)
{
stream_CFD_state *const ss = (stream_CFD_state *) st;
int raster = ss->raster =
ROUND_UP((ss->Columns + 7) >> 3, ss->DecodedByteAlign);
byte white = (ss->BlackIs1 ? 0 : 0xff);
s_hcd_init_inline(ss);
ss->lbuf = gs_alloc_bytes(st->memory, raster + 4, "CFD lbuf");
ss->lprev = 0;
if (ss->lbuf == 0)
return ERRC;
if (ss->K != 0) {
ss->lprev = gs_alloc_bytes(st->memory, raster + 4, "CFD lprev");
if (ss->lprev == 0)
return ERRC;
memset(ss->lbuf, white, raster);
ss->lbuf[raster] = 0xa0;
}
ss->k_left = min(ss->K, 0);
ss->run_color = 0;
ss->damaged_rows = 0;
ss->skipping_damage = false;
ss->cbit = 0;
ss->uncomp_run = 0;
ss->rows_left = (ss->Rows <= 0 || ss->EndOfBlock ? -1 : ss->Rows + 1);
ss->row = 0;
ss->rpos = ss->wpos = raster - 1;
ss->eol_count = 0;
ss->invert = white;
ss->min_left = 1;
return 0;
}
private void
s_CFD_release(stream_state * st)
{
stream_CFD_state *const ss = (stream_CFD_state *) st;
gs_free_object(st->memory, ss->lprev, "CFD lprev(close)");
gs_free_object(st->memory, ss->lbuf, "CFD lbuf(close)");
}
#define cfd_declare_state\
hcd_declare_state;\
register byte *q;\
int qbit
#define cfd_load_state()\
hcd_load_state(),\
q = ss->lbuf + ss->wpos, qbit = ss->cbit
#define cfd_store_state()\
hcd_store_state(),\
ss->wpos = q - ss->lbuf, ss->cbit = qbit
#define avail_bits(n) hcd_bits_available(n)
#define ensure_bits(n, outl) hcd_ensure_bits(n, outl)
#define peek_bits(n) hcd_peek_bits(n)
#define peek_var_bits(n) hcd_peek_var_bits(n)
#define skip_bits(n) hcd_skip_bits(n)
#ifdef DEBUG
#  define IF_DEBUG(expr) expr
#else
#  define IF_DEBUG(expr) DO_NOTHING
#endif
#define get_run(decode, initial_bits, min_bits, runlen, str, locl, outl)\
BEGIN\
const cfd_node *np;\
int clen;\
\
HCD_ENSURE_BITS_ELSE(initial_bits) {\
\
if (bits_left < min_bits) goto outl;\
np = &decode[hcd_peek_bits_left() << (initial_bits - bits_left)];\
if ((clen = np->code_length) > bits_left) goto outl;\
goto locl;\
}\
np = &decode[peek_bits(initial_bits)];\
if ((clen = np->code_length) > initial_bits) {\
IF_DEBUG(uint init_bits = peek_bits(initial_bits));\
if (!avail_bits(clen)) goto outl;\
clen -= initial_bits;\
skip_bits(initial_bits);\
ensure_bits(clen, outl);		\
np = &decode[np->run_length + peek_var_bits(clen)];\
if_debug4('W', "%s xcode=0x%x,%d rlen=%d\n", str,\
(init_bits << np->code_length) +\
peek_var_bits(np->code_length),\
initial_bits + np->code_length,\
np->run_length);\
skip_bits(np->code_length);\
} else {\
locl:	if_debug4('W', "%s code=0x%x,%d rlen=%d\n", str,\
peek_var_bits(clen), clen, np->run_length);\
skip_bits(clen);\
}\
runlen = np->run_length;\
END
#define skip_data(rlen, makeup_label)\
if ( (qbit -= rlen) < 0 )\
{	q -= qbit >> 3, qbit &= 7;\
if ( rlen >= 64 ) goto makeup_label;\
}
#define invert_data(rlen, black_byte, makeup_action, d)\
if ( rlen > qbit )\
{	*q++ ^= (1 << qbit) - 1;\
rlen -= qbit;\
switch ( rlen >> 3 )\
{\
case 7:		\
if ( rlen + qbit >= 64 ) goto d;\
*q++ = black_byte;\
case 6: *q++ = black_byte;\
case 5: *q++ = black_byte;\
case 4: *q++ = black_byte;\
case 3: *q++ = black_byte;\
case 2: *q++ = black_byte;\
case 1: *q = black_byte;\
rlen &= 7;\
if ( !rlen ) { qbit = 0; break; }\
q++;\
case 0:			\
qbit = 8 - rlen;\
*q ^= 0xff << qbit;\
break;\
default:	\
d:			memset(q, black_byte, rlen >> 3);\
q += rlen >> 3;\
rlen &= 7;\
if ( !rlen ) qbit = 0, q--;\
else qbit = 8 - rlen, *q ^= 0xff << qbit;\
makeup_action;\
}\
}\
else\
qbit -= rlen,\
*q ^= ((1 << rlen) - 1) << qbit
private int cf_decode_eol(stream_CFD_state *, stream_cursor_read *);
private int cf_decode_1d(stream_CFD_state *, stream_cursor_read *);
private int cf_decode_2d(stream_CFD_state *, stream_cursor_read *);
private int cf_decode_uncompressed(stream_CFD_state *, stream_cursor_read *);
private int
s_CFD_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_CFD_state *const ss = (stream_CFD_state *) st;
int wstop = ss->raster - 1;
int eol_count = ss->eol_count;
int k_left = ss->k_left;
int rows_left = ss->rows_left;
int status = 0;
#ifdef DEBUG
const byte *rstart = pr->ptr;
const byte *wstart = pw->ptr;
#endif
top:
#ifdef DEBUG
{
hcd_declare_state;
hcd_load_state();
if_debug8('w', "[w]CFD_process top: eol_count=%d, k_left=%d, rows_left=%d\n"
"    bits=0x%lx, bits_left=%d, read %u, wrote %u%s\n",
eol_count, k_left, rows_left,
(ulong) bits, bits_left,
(uint) (p - rstart), (uint) (pw->ptr - wstart),
(ss->skipping_damage ? ", skipping damage" : ""));
}
#endif
if (ss->skipping_damage) {
hcd_declare_state;
int skip;
status = 0;
do {
switch ((skip = cf_decode_eol(ss, pr))) {
default:
hcd_load_state();
skip_bits(-skip);
hcd_store_state();
continue;
case 0:
goto out;
case 1:
{
hcd_load_state();
bits_left += run_eol_code_length;
hcd_store_state();
}
ss->skipping_damage = false;
}
}
while (ss->skipping_damage);
ss->damaged_rows++;
}
if (ss->wpos == wstop && ss->cbit <= (-ss->Columns & 7) &&
(k_left == 0 ? !(ss->run_color & ~1) : ss->run_color == 0)
) {
if (ss->rpos < ss->wpos) {
stream_cursor_read cr;
cr.ptr = ss->lbuf + ss->rpos;
cr.limit = ss->lbuf + ss->wpos;
status = stream_move(&cr, pw);
ss->rpos = cr.ptr - ss->lbuf;
if (status)
goto out;
}
ss->row++;
if (rows_left > 0 && --rows_left == 0)
goto ck_eol;
if (ss->K != 0) {
byte *prev_bits = ss->lprev;
ss->lprev = ss->lbuf;
ss->lbuf = prev_bits;
if (ss->K > 0)
k_left = (k_left == 0 ? ss->K : k_left) - 1;
}
ss->rpos = ss->wpos = -1;
ss->eol_count = eol_count = 0;
ss->cbit = 0;
ss->invert = (ss->BlackIs1 ? 0 : 0xff);
memset(ss->lbuf, ss->invert, wstop + 1);
ss->run_color = 0;
if (ss->EncodedByteAlign & !ss->EndOfLine)
ss->bits_left &= ~7;
}
if (ss->wpos < 0) {
ck_eol:
while ((status = cf_decode_eol(ss, pr)) > 0) {
if_debug0('w', "[w]EOL\n");
if (ss->K > 0) {
hcd_declare_state;
hcd_load_state();
ensure_bits(1, out);
k_left = (peek_bits(1) ? 0 : 1);
skip_bits(1);
hcd_store_state();
}
++eol_count;
if (eol_count == (ss->K < 0 ? 2 : 6)) {
status = EOFC;
goto out;
}
}
if (rows_left == 0) {
status = EOFC;
goto out;
}
if (status == 0)
goto out;
switch (eol_count) {
case 0:
if (ss->EndOfLine) {
status = ERRC;
goto check;
}
case 1:
break;
default:
status = ERRC;
goto check;
}
}
if (k_left < 0) {
if_debug0('w', "[w2]new row\n");
status = cf_decode_2d(ss, pr);
} else if (k_left == 0) {
if_debug0('w', "[w1]new row\n");
status = cf_decode_1d(ss, pr);
} else {
if_debug1('w', "[w1]new 2-D row, %d left\n", k_left);
status = cf_decode_2d(ss, pr);
}
if_debug3('w', "[w]CFD status = %d, wpos = %d, cbit = %d\n",
status, ss->wpos, ss->cbit);
check:switch (status) {
case 1:
goto top;
case ERRC:
if (ss->damaged_rows >= ss->DamagedRowsBeforeError ||
!(ss->EndOfLine && ss->K >= 0)
)
break;
{
ss->wpos = wstop;
ss->cbit = -ss->Columns & 7;
ss->run_color = 0;
}
ss->skipping_damage = true;
goto top;
default:
ss->damaged_rows = 0;
}
out:ss->k_left = k_left;
ss->rows_left = rows_left;
ss->eol_count = eol_count;
return status;
}
private int
cf_decode_eol(stream_CFD_state * ss, stream_cursor_read * pr)
{
hcd_declare_state;
int zeros;
int look_ahead;
hcd_load_state();
for (zeros = 0; zeros < run_eol_code_length - 1; zeros++) {
ensure_bits(1, out);
if (peek_bits(1))
return -(zeros + 1);
skip_bits(1);
}
look_ahead = (ss->K > 0 ? 2 : 1);
for (;;) {
ensure_bits(look_ahead, back);
if (peek_bits(1))
break;
skip_bits(1);
}
skip_bits(1);
hcd_store_state();
return 1;
back:
bits &= (1 << bits_left) - 1;
bits_left += run_eol_code_length - 1;
hcd_store_state();
out:return 0;
}
private int
cf_decode_1d(stream_CFD_state * ss, stream_cursor_read * pr)
{
cfd_declare_state;
byte black_byte = (ss->BlackIs1 ? 0xff : 0);
int end_bit = -ss->Columns & 7;
byte *stop = ss->lbuf - 1 + ss->raster;
int run_color = ss->run_color;
int status;
int bcnt;
cfd_load_state();
if_debug1('w', "[w1]entry run_color = %d\n", ss->run_color);
if (ss->run_color > 0)
goto db;
else
goto dw;
#define q_at_stop() (q >= stop && (qbit <= end_bit || q > stop))
top:run_color = 0;
if (q_at_stop())
goto done;
dw:
get_run(cf_white_decode, cfd_white_initial_bits, cfd_white_min_bits,
bcnt, "[w1]white", dwl, out0);
if (bcnt < 0) {
switch (bcnt) {
case run_uncompressed:
cfd_store_state();
bcnt = cf_decode_uncompressed(ss, pr);
if (bcnt < 0)
return bcnt;
cfd_load_state();
if (bcnt)
goto db;
else
goto dw;
default:
status = ERRC;
goto out;
}
}
skip_data(bcnt, dwx);
if (q_at_stop()) {
run_color = 0;
goto done;
}
run_color = 1;
db:
get_run(cf_black_decode, cfd_black_initial_bits, cfd_black_min_bits,
bcnt, "[w1]black", dbl, out1);
if (bcnt < 0) {
status = ERRC;
goto out;
}
invert_data(bcnt, black_byte, goto dbx, idb);
goto top;
dwx:
run_color = -1;
goto dw;
dbx:
run_color = 2;
goto db;
done:if (q > stop || qbit < end_bit)
status = ERRC;
else
status = 1;
out:cfd_store_state();
ss->run_color = run_color;
if_debug1('w', "[w1]exit run_color = %d\n", run_color);
return status;
out0:
status = 0;
goto out;
out1:
status = 0;
goto out;
}
private int
cf_decode_2d(stream_CFD_state * ss, stream_cursor_read * pr)
{
cfd_declare_state;
byte invert_white = (ss->BlackIs1 ? 0 : 0xff);
byte black_byte = ~invert_white;
byte invert = ss->invert;
int end_count = -ss->Columns & 7;
uint raster = ss->raster;
byte *q0 = ss->lbuf;
byte *prev_q01 = ss->lprev + 1;
byte *endptr = q0 - 1 + raster;
int init_count = raster << 3;
register int count;
int rlen;
int status;
cfd_load_state();
count = ((endptr - q) << 3) + qbit;
endptr[1] = 0xa0;
if_debug1('W', "[w2]raster=%d\n", raster);
switch (ss->run_color) {
case -2:
ss->run_color = 0;
goto hww;
case -1:
ss->run_color = 0;
goto hbw;
case 1:
ss->run_color = 0;
goto hwb;
case 2:
ss->run_color = 0;
goto hbb;
}
top:if (count <= end_count) {
status = (count < end_count ? ERRC : 1);
goto out;
}
if_debug1('W', "[w2]%4d:\n", count);
#ifdef DEBUG
{
int pcount = (endptr - q) * 8 + qbit;
if (pcount != count)
dlprintf2("[w2]Error: count=%d pcount=%d\n",
count, pcount);
}
#endif
ensure_bits(3, out3);
#define vertical_0 (countof(cf2_run_vertical) / 2)
switch (peek_bits(3)) {
default  :
v0:	    skip_bits(1);
rlen = vertical_0;
break;
case 2:
skip_bits(3);
rlen = vertical_0 + 1;
break;
case 3:
skip_bits(3);
rlen = vertical_0 - 1;
break;
case 1:
skip_bits(3);
if (invert == invert_white)
goto hww;
else
goto hbb;
case 0:
get_run(cf_2d_decode, cfd_2d_initial_bits, cfd_2d_min_bits,
rlen, "[w2]", d2l, out0);
if (rlen < 0)
switch (rlen) {
case run2_pass:
break;
case run_uncompressed:
{
int which;
cfd_store_state();
which = cf_decode_uncompressed(ss, pr);
if (which < 0) {
status = which;
goto out;
}
cfd_load_state();
invert = (which ? ~invert_white : invert_white);
}
goto top;
default:
status = ERRC;
goto out;
}
}
{
int prev_count = count;
byte prev_data;
int dlen;
static const byte count_bit[8] =
{0x80, 1, 2, 4, 8, 0x10, 0x20, 0x40};
byte *prev_q = prev_q01 + (q - q0);
int plen;
if (!(count & 7))
prev_q++;
prev_data = prev_q[-1] ^ invert;
if ((prev_data & count_bit[prev_count & 7]) &&
(prev_count < init_count || invert != invert_white)
) {
if_debug1('W', " data=0x%x", prev_data);
skip_black_pixels(prev_data, prev_q,
prev_count, invert, plen);
if (prev_count < end_count)
prev_count = end_count;
if_debug1('W', " b1 other=%d", prev_count);
}
if (prev_count != end_count) {
if_debug1('W', " data=0x%x", prev_data);
skip_white_pixels(prev_data, prev_q,
prev_count, invert, plen);
if (prev_count < end_count)
prev_count = end_count;
if_debug1('W', " b1 same=%d", prev_count);
}
if (rlen == run2_pass) {
if (prev_count != end_count) {
if_debug1('W', " data=0x%x", prev_data);
skip_black_pixels(prev_data, prev_q,
prev_count, invert, plen);
if (prev_count < end_count)
prev_count = end_count;
}
if_debug2('W', " b2=%d, pass %d\n",
prev_count, count - prev_count);
} else {
prev_count += rlen - vertical_0;
if_debug2('W', " vertical %d -> %d\n",
rlen - vertical_0, prev_count);
}
if (invert == invert_white) {
q = endptr - (prev_count >> 3);
qbit = prev_count & 7;
} else {
dlen = count - prev_count;
invert_data(dlen, black_byte, DO_NOTHING, idd);
}
count = prev_count;
if (rlen >= 0)
invert = ~invert;
}
goto top;
out3:
if (bits_left > 0 && peek_bits(1)) {
goto v0;
}
out0:status = 0;
out:cfd_store_state();
ss->invert = invert;
if (status == ERRC && ss->Rows > 0 && ss->row > ss->Rows)
status = EOFC;
return status;
hww:get_run(cf_white_decode, cfd_white_initial_bits, cfd_white_min_bits,
rlen, " white", wwl, outww);
if ((count -= rlen) < end_count) {
status = ERRC;
goto out;
}
skip_data(rlen, hww);
hwb:get_run(cf_black_decode, cfd_black_initial_bits, cfd_black_min_bits,
rlen, " black", wbl, outwb);
if ((count -= rlen) < end_count) {
status = ERRC;
goto out;
}
invert_data(rlen, black_byte, goto hwb, ihwb);
goto top;
outww:ss->run_color = -2;
goto out0;
outwb:ss->run_color = 1;
goto out0;
hbb:get_run(cf_black_decode, cfd_black_initial_bits, cfd_black_min_bits,
rlen, " black", bbl, outbb);
if ((count -= rlen) < end_count) {
status = ERRC;
goto out;
}
invert_data(rlen, black_byte, goto hbb, ihbb);
hbw:get_run(cf_white_decode, cfd_white_initial_bits, cfd_white_min_bits,
rlen, " white", bwl, outbw);
if ((count -= rlen) < end_count) {
status = ERRC;
goto out;
}
skip_data(rlen, hbw);
goto top;
outbb:ss->run_color = 2;
goto out0;
outbw:ss->run_color = -1;
goto out0;
}
#if 1
private int
cf_decode_uncompressed(stream_CFD_state * ss, stream_cursor_read * pr)
{
return ERRC;
}
#else
private int
cf_decode_uncompressed(stream * s)
{
cfd_declare_state;
const cfd_node *np;
int clen, rlen;
cfd_load_state();
while (1) {
ensure_bits(cfd_uncompressed_initial_bits, NOOUT);
np = &cf_uncompressed_decode[peek_bits(cfd_uncompressed_initial_bits)];
clen = np->code_length;
rlen = np->run_length;
if (clen > cfd_uncompressed_initial_bits) {
break;
}
if (rlen == cfd_uncompressed_initial_bits) {
if_debug1('W', "[wu]%d\n", rlen);
if ((qbit -= cfd_uncompressed_initial_bits) < 0)
qbit += 8, q++;
} else {
if_debug1('W', "[wu]%d+1\n", rlen);
if (qbit -= rlen < 0)
qbit += 8, q++;
*q ^= 1 << qbit;
}
skip_bits(clen);
}
clen -= cfd_uncompressed_initial_bits;
skip_bits(cfd_uncompressed_initial_bits);
ensure_bits(clen, NOOUT);
np = &cf_uncompressed_decode[rlen + peek_var_bits(clen)];
rlen = np->run_length;
skip_bits(np->code_length);
if_debug1('w', "[wu]exit %d\n", rlen);
if (rlen >= 0) {
if ((qbit -= rlen >> 1) < 0)
qbit += 8, q++;
rlen &= 1;
}
out:
cfd_store_state();
return rlen;
}
#endif
const stream_template s_CFD_template =
{&st_CFD_state, s_CFD_init, s_CFD_process, 1, 1, s_CFD_release,
s_CFD_set_defaults
};