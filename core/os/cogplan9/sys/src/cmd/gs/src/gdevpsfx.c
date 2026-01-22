#include "math_.h"
#include "memory_.h"
#include "gx.h"
#include "gserrors.h"
#include "gxfixed.h"
#include "gxmatrix.h"
#include "gxfont.h"
#include "gxfont1.h"
#include "gxtype1.h"
#include "stream.h"
#include "gdevpsf.h"
#define CE_OFFSET 32
typedef struct {
fixed v0, v1;
ushort index;
} cv_stem_hint;
typedef struct {
int count;
int current;
int replaced_count;
cv_stem_hint data[max_total_stem_hints];
} cv_stem_hint_table;
private void
skip_iv(gs_type1_state *pcis)
{
int skip = pcis->pfont->data.lenIV;
ip_state_t *ipsp = &pcis->ipstack[pcis->ips_count - 1];
const byte *cip = ipsp->cs_data.bits.data;
crypt_state state = crypt_charstring_seed;
for (; skip > 0; ++cip, --skip)
decrypt_skip_next(*cip, state);
ipsp->ip = cip;
ipsp->dstate = state;
}
private void
type1_next_init(gs_type1_state *pcis, const gs_glyph_data_t *pgd,
gs_font_type1 *pfont)
{
gs_type1_interp_init(pcis, NULL, NULL, NULL, NULL, false, 0, pfont);
pcis->flex_count = flex_max;
pcis->ipstack[0].cs_data = *pgd;
skip_iv(pcis);
}
inline private void
type1_clear(gs_type1_state *pcis)
{
pcis->os_count = 0;
}
private int
type1_callsubr(gs_type1_state *pcis, int index)
{
gs_font_type1 *pfont = pcis->pfont;
ip_state_t *ipsp1 = &pcis->ipstack[pcis->ips_count];
int code = pfont->data.procs.subr_data(pfont, index, false,
&ipsp1->cs_data);
if (code < 0)
return_error(code);
pcis->ips_count++;
skip_iv(pcis);
return code;
}
private int
type1_stem1(gs_type1_state *pcis, cv_stem_hint_table *psht, const fixed *pv,
fixed lsb, byte *active_hints)
{
fixed v0 = pv[0] + lsb, v1 = v0 + pv[1];
cv_stem_hint *bot = &psht->data[0];
cv_stem_hint *orig_top = bot + psht->count;
cv_stem_hint *top = orig_top;
if (psht->count >= max_total_stem_hints)
return_error(gs_error_limitcheck);
while (top > bot &&
(v0 < top[-1].v0 || (v0 == top[-1].v0 && v1 < top[-1].v1))
) {
*top = top[-1];
top--;
}
if (top > bot && v0 == top[-1].v0 && v1 == top[-1].v1) {
memmove(top, top + 1, (char *)orig_top - (char *)top);
if (active_hints) {
uint index = top[-1].index;
active_hints[index >> 3] |= 0x80 >> (index & 7);
}
return 0;
}
top->v0 = v0;
top->v1 = v1;
psht->count++;
return 0;
}
private void
type1_stem3(gs_type1_state *pcis, cv_stem_hint_table *psht, const fixed *pv3,
fixed lsb, byte *active_hints)
{
type1_stem1(pcis, psht, pv3, lsb, active_hints);
type1_stem1(pcis, psht, pv3 + 2, lsb, active_hints);
type1_stem1(pcis, psht, pv3 + 4, lsb, active_hints);
}
private int
type1_next(gs_type1_state *pcis)
{
ip_state_t *ipsp = &pcis->ipstack[pcis->ips_count - 1];
const byte *cip, *cipe;
crypt_state state;
#define CLEAR (csp = pcis->ostack - 1)
fixed *csp = &pcis->ostack[pcis->os_count - 1];
const bool encrypted = pcis->pfont->data.lenIV >= 0;
int c, code, num_results, c0;
load:
cip = ipsp->ip;
cipe = ipsp->cs_data.bits.data + ipsp->cs_data.bits.size;
state = ipsp->dstate;
for (;;) {
if (cip >= cipe)
return_error(gs_error_invalidfont);
c0 = *cip++;
charstring_next(c0, state, c, encrypted);
if (c >= c_num1) {
if (c < c_pos2_0) {
decode_push_num1(csp, pcis->ostack, c);
} else if (c < cx_num4) {
decode_push_num2(csp, pcis->ostack, c, cip, state, encrypted);
} else if (c == cx_num4) {
long lw;
decode_num4(lw, cip, state, encrypted);
CS_CHECK_PUSH(csp, pcis->ostack);
*++csp = int2fixed(lw);
} else
return_error(gs_error_invalidfont);
continue;
}
#ifdef DEBUG
if (gs_debug_c('1')) {
const fixed *p;
for (p = pcis->ostack; p <= csp; ++p)
dprintf1(" %g", fixed2float(*p));
if (c == cx_escape) {
crypt_state cstate = state;
int cn;
charstring_next(*cip, cstate, cn, encrypted);
dprintf1(" [*%d]\n", cn);
} else
dprintf1(" [%d]\n", c);
}
#endif
switch ((char_command) c) {
default:
break;
case c_undef0:
case c_undef2:
case c_undef17:
return_error(gs_error_invalidfont);
case c_callsubr:
code = type1_callsubr(pcis, fixed2int_var(*csp) +
pcis->pfont->data.subroutineNumberBias);
if (code < 0)
return_error(code);
ipsp->ip = cip, ipsp->dstate = state;
--csp;
++ipsp;
goto load;
case c_return:
gs_glyph_data_free(&ipsp->cs_data, "type1_next");
pcis->ips_count--;
--ipsp;
goto load;
case c_undoc15:
CLEAR;
continue;
case cx_escape:
charstring_next(*cip, state, c, encrypted);
++cip;
switch ((char1_extended_command) c) {
default:
c += CE_OFFSET;
break;
case ce1_div:
csp[-1] = float2fixed((double)csp[-1] / (double)*csp);
--csp;
continue;
case ce1_undoc15:
CLEAR;
continue;
case ce1_callothersubr:
switch (fixed2int_var(*csp)) {
case 0:
pcis->ignore_pops = 2;
break;
case 3:
pcis->ignore_pops = 1;
break;
case 14:
num_results = 1; goto blend;
case 15:
num_results = 2; goto blend;
case 16:
num_results = 3; goto blend;
case 17:
num_results = 4; goto blend;
case 18:
num_results = 6;
blend:
code = gs_type1_blend(pcis, csp, num_results);
if (code < 0)
return code;
csp -= code;
continue;
default:
break;
}
break;
case ce1_pop:
if (pcis->ignore_pops != 0) {
pcis->ignore_pops--;
continue;
}
return_error(gs_error_rangecheck);
}
break;
}
break;
}
ipsp->ip = cip, ipsp->dstate = state;
pcis->ips_count = ipsp + 1 - &pcis->ipstack[0];
pcis->os_count = csp + 1 - &pcis->ostack[0];
return c;
}
private void
sputc2(stream *s, int i)
{
sputc(s, (byte)(i >> 8));
sputc(s, (byte)i);
}
private void
sputc4(stream *s, int i)
{
sputc2(s, i >> 16);
sputc2(s, i);
}
private void
type2_put_op(stream *s, int op)
{
if (op >= CE_OFFSET) {
spputc(s, cx_escape);
spputc(s, (byte)(op - CE_OFFSET));
} else
sputc(s, (byte)op);
}
private void
type2_put_int(stream *s, int i)
{
if (i >= -107 && i <= 107)
sputc(s, (byte)(i + 139));
else if (i <= 1131 && i >= 0)
sputc2(s, (c_pos2_0 << 8) + i - 108);
else if (i >= -1131 && i < 0)
sputc2(s, (c_neg2_0 << 8) - i - 108);
else if (i >= -32768 && i <= 32767) {
spputc(s, c2_shortint);
sputc2(s, i);
} else {
type2_put_int(s, i >> 10);
type2_put_int(s, 1024);
type2_put_op(s, CE_OFFSET + ce2_mul);
type2_put_int(s, i & 1023);
type2_put_op(s, CE_OFFSET + ce2_add);
}
}
private void
type2_put_fixed(stream *s, fixed v)
{
if (fixed_is_int(v))
type2_put_int(s, fixed2int_var(v));
else if (v >= int2fixed(-32768) && v < int2fixed(32768)) {
spputc(s, cx_num4);
sputc4(s, v << (16 - _fixed_shift));
} else {
type2_put_int(s, fixed2int_var(v));
type2_put_fixed(s, fixed_fraction(v));
type2_put_op(s, CE_OFFSET + ce2_add);
}
}
private void
type2_put_stems(stream *s, int os_count, const cv_stem_hint_table *psht, int op)
{
fixed prev = 0;
int pushed = os_count;
int i;
for (i = 0; i < psht->count; ++i, pushed += 2) {
fixed v0 = psht->data[i].v0;
fixed v1 = psht->data[i].v1;
if (pushed > ostack_size - 2) {
type2_put_op(s, op);
pushed = 0;
}
type2_put_fixed(s, v0 - prev);
type2_put_fixed(s, v1 - v0);
prev = v1;
}
type2_put_op(s, op);
}
private void
type2_put_hintmask(stream *s, const byte *mask, uint size)
{
uint ignore;
type2_put_op(s, c2_hintmask);
sputs(s, mask, size, &ignore);
}
#define MAX_STACK ostack_size
int
psf_convert_type1_to_type2(stream *s, const gs_glyph_data_t *pgd,
gs_font_type1 *pfont)
{
gs_type1_state cis;
cv_stem_hint_table hstem_hints;
cv_stem_hint_table vstem_hints;
bool first = true;
bool replace_hints = false;
bool hints_changed = false;
enum {
dotsection_in = 0,
dotsection_out = -1
} dotsection_flag = dotsection_out;
byte active_hints[(max_total_stem_hints + 7) / 8];
byte dot_save_hints[(max_total_stem_hints + 7) / 8];
uint hintmask_size;
#define HINTS_CHANGED()\
BEGIN\
hints_changed = replace_hints;\
if (hints_changed)\
CHECK_OP(); \
END
#define CHECK_HINTS_CHANGED()\
BEGIN\
if (hints_changed) {\
type2_put_hintmask(s, active_hints, hintmask_size);\
hints_changed = false;\
}\
END
int depth;
int prev_op;
#define CLEAR_OP()\
(depth = 0, prev_op = -1)
#define CHECK_OP()\
BEGIN\
if (prev_op >= 0) {\
type2_put_op(s, prev_op);\
CLEAR_OP();\
}\
END
fixed mx0 = 0, my0 = 0;
hstem_hints.count = hstem_hints.replaced_count = hstem_hints.current = 0;
vstem_hints.count = vstem_hints.replaced_count = vstem_hints.current = 0;
type1_next_init(&cis, pgd, pfont);
for (;;) {
int c = type1_next(&cis);
fixed *csp = &cis.ostack[cis.os_count - 1];
switch (c) {
default:
if (c < 0)
return c;
type1_clear(&cis);
continue;
case c1_hsbw:
gs_type1_sbw(&cis, cis.ostack[0], fixed_0, cis.ostack[1], fixed_0);
goto clear;
case cx_hstem:
type1_stem1(&cis, &hstem_hints, csp - 1, cis.lsb.y, NULL);
goto clear;
case cx_vstem:
type1_stem1(&cis, &vstem_hints, csp - 1, cis.lsb.x, NULL);
goto clear;
case CE_OFFSET + ce1_sbw:
gs_type1_sbw(&cis, cis.ostack[0], cis.ostack[1],
cis.ostack[2], cis.ostack[3]);
goto clear;
case CE_OFFSET + ce1_vstem3:
type1_stem3(&cis, &vstem_hints, csp - 5, cis.lsb.x, NULL);
goto clear;
case CE_OFFSET + ce1_hstem3:
type1_stem3(&cis, &hstem_hints, csp - 5, cis.lsb.y, NULL);
clear:
type1_clear(&cis);
continue;
case ce1_callothersubr:
if (*csp == int2fixed(3))
replace_hints = true;
cis.os_count -= 2;
continue;
case CE_OFFSET + ce1_dotsection:
replace_hints = true;
continue;
case CE_OFFSET + ce1_seac:
case cx_endchar:
break;
}
break;
}
{
int i;
for (i = 0; i < hstem_hints.count; ++i)
hstem_hints.data[i].index = i;
for (i = 0; i < vstem_hints.count; ++i)
vstem_hints.data[i].index = i + hstem_hints.count;
}
if (replace_hints) {
hintmask_size =
(hstem_hints.count + vstem_hints.count + 7) / 8;
memset(active_hints, 0, hintmask_size);
} else
hintmask_size = 0;
type1_next_init(&cis, pgd, pfont);
CLEAR_OP();
for (;;) {
int c = type1_next(&cis);
fixed *csp = &cis.ostack[cis.os_count - 1];
#define POP(n)\
(csp -= (n), cis.os_count -= (n))
int i;
fixed mx, my;
switch (c) {
default:
if (c < 0)
return c;
if (c >= CE_OFFSET)
return_error(gs_error_rangecheck);
copy:
CHECK_OP();
CHECK_HINTS_CHANGED();
put:
for (i = 0; i < cis.os_count; ++i)
type2_put_fixed(s, cis.ostack[i]);
depth += cis.os_count;
prev_op = c;
type1_clear(&cis);
continue;
case cx_hstem:
type1_stem1(&cis, &hstem_hints, csp - 1, cis.lsb.y, active_hints);
hint:
HINTS_CHANGED();
type1_clear(&cis);
continue;
case cx_vstem:
type1_stem1(&cis, &vstem_hints, csp - 1, cis.lsb.x, active_hints);
goto hint;
case CE_OFFSET + ce1_vstem3:
type1_stem3(&cis, &vstem_hints, csp - 5, cis.lsb.x, active_hints);
goto hint;
case CE_OFFSET + ce1_hstem3:
type1_stem3(&cis, &hstem_hints, csp - 5, cis.lsb.y, active_hints);
goto hint;
case CE_OFFSET + ce1_dotsection:
if (dotsection_flag == dotsection_out) {
memcpy(dot_save_hints, active_hints, hintmask_size);
memset(active_hints, 0, hintmask_size);
dotsection_flag = dotsection_in;
} else {
memcpy(active_hints, dot_save_hints, hintmask_size);
dotsection_flag = dotsection_out;
}
HINTS_CHANGED();
continue;
case c1_closepath:
continue;
case CE_OFFSET + ce1_setcurrentpoint:
if (first) {
mx0 = csp[-1], my0 = *csp;
}
continue;
case cx_vmoveto:
mx = 0, my = *csp;
POP(1); goto move;
case cx_hmoveto:
mx = *csp, my = 0;
POP(1); goto move;
case cx_rmoveto:
mx = csp[-1], my = *csp;
POP(2);
move:
CHECK_OP();
if (first) {
if (cis.os_count)
type2_put_fixed(s, *csp);
mx += cis.lsb.x + mx0, my += cis.lsb.y + my0;
first = false;
}
if (cis.flex_count != flex_max) {
if (type1_next(&cis) != ce1_callothersubr)
return_error(gs_error_rangecheck);
csp = &cis.ostack[cis.os_count - 1];
if (*csp != int2fixed(2) || csp[-1] != fixed_0)
return_error(gs_error_rangecheck);
cis.flex_count++;
csp[-1] = mx, *csp = my;
continue;
}
CHECK_HINTS_CHANGED();
if (mx == 0) {
type2_put_fixed(s, my);
depth = 1, prev_op = cx_vmoveto;
} else if (my == 0) {
type2_put_fixed(s, mx);
depth = 1, prev_op = cx_hmoveto;
} else {
type2_put_fixed(s, mx);
type2_put_fixed(s, my);
depth = 2, prev_op = cx_rmoveto;
}
type1_clear(&cis);
continue;
case c1_hsbw:
gs_type1_sbw(&cis, cis.ostack[0], fixed_0, cis.ostack[1], fixed_0);
cis.ostack[0] = cis.ostack[1];
sbw:
if (cis.ostack[0] == pfont->data.defaultWidthX)
cis.os_count = 0;
else {
cis.ostack[0] -= pfont->data.nominalWidthX;
cis.os_count = 1;
}
if (hstem_hints.count) {
if (cis.os_count)
type2_put_fixed(s, cis.ostack[0]);
type2_put_stems(s, cis.os_count, &hstem_hints,
(replace_hints ? c2_hstemhm : cx_hstem));
cis.os_count = 0;
}
if (vstem_hints.count) {
if (cis.os_count)
type2_put_fixed(s, cis.ostack[0]);
type2_put_stems(s, cis.os_count, &vstem_hints,
(replace_hints ? c2_vstemhm : cx_vstem));
cis.os_count = 0;
}
continue;
case CE_OFFSET + ce1_seac:
csp[-3] += cis.lsb.x - csp[-4];
memmove(csp - 4, csp - 3, sizeof(*csp) * 4);
POP(1);
case cx_endchar:
CHECK_OP();
for (i = 0; i < cis.os_count; ++i)
type2_put_fixed(s, cis.ostack[i]);
type2_put_op(s, cx_endchar);
return 0;
case CE_OFFSET + ce1_sbw:
gs_type1_sbw(&cis, cis.ostack[0], cis.ostack[1],
cis.ostack[2], cis.ostack[3]);
cis.ostack[0] = cis.ostack[2];
goto sbw;
case ce1_callothersubr:
CHECK_OP();
switch (fixed2int_var(*csp)) {
default:
return_error(gs_error_rangecheck);
case 0:
csp[-18] += csp[-16], csp[-17] += csp[-15];
memmove(csp - 16, csp - 14, sizeof(*csp) * 11);
cis.os_count -= 6, csp -= 6;
c = CE_OFFSET + ce2_flex;
cis.flex_count = flex_max;
cis.ignore_pops = 2;
goto copy;
case 1:
cis.flex_count = 0;
cis.os_count -= 2;
continue;
case 3:
memset(active_hints, 0, hintmask_size);
HINTS_CHANGED();
cis.ignore_pops = 1;
cis.os_count -= 2;
continue;
case 12:
case 13:
cis.os_count -= 2 + fixed2int(csp[-1]);
continue;
}
case cx_rlineto:
if (depth > MAX_STACK - 2)
goto copy;
switch (prev_op) {
case cx_rlineto:
goto put;
case cx_rrcurveto:
c = c2_rcurveline;
goto put;
default:
goto copy;
}
case cx_hlineto:
if (depth > MAX_STACK - 1 ||
prev_op != (depth & 1 ? cx_vlineto : cx_hlineto))
goto copy;
c = prev_op;
goto put;
case cx_vlineto:
if (depth > MAX_STACK - 1 ||
prev_op != (depth & 1 ? cx_hlineto : cx_vlineto))
goto copy;
c = prev_op;
goto put;
case cx_hvcurveto:
if ((depth & 1) || depth > MAX_STACK - 4 ||
prev_op != (depth & 4 ? cx_vhcurveto : cx_hvcurveto))
goto copy;
c = prev_op;
goto put;
case cx_vhcurveto:
if ((depth & 1) || depth > MAX_STACK - 4 ||
prev_op != (depth & 4 ? cx_hvcurveto : cx_vhcurveto))
goto copy;
c = prev_op;
goto put;
case cx_rrcurveto:
if (depth == 0) {
if (csp[-1] == 0) {
c = c2_vvcurveto;
csp[-1] = csp[0];
if (csp[-5] == 0) {
memmove(csp - 5, csp - 4, sizeof(*csp) * 4);
POP(2);
} else
POP(1);
} else if (*csp == 0) {
c = c2_hhcurveto;
if (csp[-4] == 0) {
memmove(csp - 4, csp - 3, sizeof(*csp) * 3);
POP(2);
} else {
*csp = csp[-5], csp[-5] = csp[-4], csp[-4] = *csp;
POP(1);
}
}
goto copy;
}
if (depth > MAX_STACK - 6)
goto copy;
switch (prev_op) {
case c2_hhcurveto:
if (csp[-4] == 0 && *csp == 0) {
memmove(csp - 4, csp - 3, sizeof(*csp) * 3);
c = prev_op;
POP(2);
goto put;
}
goto copy;
case c2_vvcurveto:
if (csp[-5] == 0 && csp[-1] == 0) {
memmove(csp - 5, csp - 4, sizeof(*csp) * 3);
csp[-2] = *csp;
c = prev_op;
POP(2);
goto put;
}
goto copy;
case cx_hvcurveto:
if (depth & 1)
goto copy;
if (!(depth & 4))
goto hrc;
vrc:
if (csp[-5] != 0)
goto copy;
memmove(csp - 5, csp - 4, sizeof(*csp) * 5);
c = prev_op;
POP(1);
goto put;
case cx_vhcurveto:
if (depth & 1)
goto copy;
if (!(depth & 4))
goto vrc;
hrc:
if (csp[-4] != 0)
goto copy;
memmove(csp - 4, csp - 3, sizeof(*csp) * 2);
csp[-2] = *csp;
c = prev_op;
POP(1);
goto put;
case cx_rlineto:
c = c2_rlinecurve;
goto put;
case cx_rrcurveto:
goto put;
default:
goto copy;
}
}
}
}