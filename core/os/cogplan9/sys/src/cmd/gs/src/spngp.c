#include "memory_.h"
#include "strimpl.h"
#include "spngpx.h"
private_st_PNGP_state();
#define cNone 10
#define cSub 11
#define cUp 12
#define cAverage 13
#define cPaeth 14
#define cOptimum 15
#define cEncode -10
#define cDecode -4
private const byte pngp_case_needs_prev[] = {
0, 0, 1, 1, 1, 1
};
private void
s_PNGP_set_defaults(stream_state * st)
{
stream_PNGP_state *const ss = (stream_PNGP_state *) st;
s_PNGP_set_defaults_inline(ss);
}
private int
s_PNGP_reinit(stream_state * st)
{
stream_PNGP_state *const ss = (stream_PNGP_state *) st;
if (ss->prev_row != 0)
memset(ss->prev_row + ss->bpp, 0, ss->row_count);
ss->row_left = 0;
return 0;
}
private int
s_pngp_init(stream_state * st, bool need_prev)
{
stream_PNGP_state *const ss = (stream_PNGP_state *) st;
int bits_per_pixel = ss->Colors * ss->BitsPerComponent;
long bits_per_row = (long)bits_per_pixel * ss->Columns;
byte *prev_row = 0;
#if arch_sizeof_long > arch_sizeof_int
if (bits_per_row > max_uint * 7L)
return ERRC;
#endif
ss->row_count = (uint) ((bits_per_row + 7) >> 3);
ss->end_mask = (1 << (-bits_per_row & 7)) - 1;
ss->bpp = (bits_per_pixel + 7) >> 3;
if (need_prev) {
prev_row = gs_alloc_bytes(st->memory, ss->bpp + ss->row_count,
"PNGPredictor prev row");
if (prev_row == 0)
return ERRC;
memset(prev_row, 0, ss->bpp);
}
ss->prev_row = prev_row;
return s_PNGP_reinit(st);
}
private int
s_PNGPE_init(stream_state * st)
{
stream_PNGP_state *const ss = (stream_PNGP_state *) st;
return s_pngp_init(st, pngp_case_needs_prev[ss->Predictor - cNone]);
}
private int
s_PNGPD_init(stream_state * st)
{
return s_pngp_init(st, true);
}
private void
s_PNGP_release(stream_state *st)
{
stream_PNGP_state *const ss = (stream_PNGP_state *) st;
if (ss->prev_row)
gs_free_object(st->memory, ss->prev_row, "PNGPredictor prev row");
}
private int
paeth_predictor(int a, int b, int c)
{
int ac = b - c, bc = a - c, abcc = ac + bc;
int pa = (ac < 0 ? -ac : ac), pb = (bc < 0 ? -bc : bc),
pc = (abcc < 0 ? -abcc : abcc);
return (pa <= pb && pa <= pc ? a : pb <= pc ? b : c);
}
private void
s_pngp_process(stream_state * st, stream_cursor_write * pw,
const byte * dprev, stream_cursor_read * pr,
const byte * upprev, const byte * up, uint count)
{
stream_PNGP_state *const ss = (stream_PNGP_state *) st;
byte *q = pw->ptr + 1;
const byte *p = pr->ptr + 1;
pr->ptr += count;
pw->ptr += count;
ss->row_left -= count;
switch (ss->case_index) {
case cEncode + cNone:
case cDecode + cNone:
memcpy(q, p, count);
break;
case cEncode + cSub:
for (; count; ++q, ++dprev, ++p, --count)
*q = (byte) (*p - *dprev);
break;
case cDecode + cSub:
for (; count; ++q, ++dprev, ++p, --count)
*q = (byte) (*p + *dprev);
break;
case cEncode + cUp:
for (; count; ++q, ++up, ++p, --count)
*q = (byte) (*p - *up);
break;
case cDecode + cUp:
for (; count; ++q, ++up, ++p, --count)
*q = (byte) (*p + *up);
break;
case cEncode + cAverage:
for (; count; ++q, ++dprev, ++up, ++p, --count)
*q = (byte) (*p - arith_rshift_1((int)*dprev + (int)*up));
break;
case cDecode + cAverage:
for (; count; ++q, ++dprev, ++up, ++p, --count)
*q = (byte) (*p + arith_rshift_1((int)*dprev + (int)*up));
break;
case cEncode + cPaeth:
for (; count; ++q, ++dprev, ++up, ++upprev, ++p, --count)
*q = (byte) (*p - paeth_predictor(*dprev, *up, *upprev));
break;
case cDecode + cPaeth:
for (; count; ++q, ++dprev, ++up, ++upprev, ++p, --count)
*q = (byte) (*p + paeth_predictor(*dprev, *up, *upprev));
break;
}
}
private uint
s_pngp_count(const stream_state * st_const, const stream_cursor_read * pr,
const stream_cursor_write * pw)
{
const stream_PNGP_state *const ss_const =
(const stream_PNGP_state *)st_const;
uint rcount = pr->limit - pr->ptr;
uint wcount = pw->limit - pw->ptr;
uint row_left = ss_const->row_left;
if (rcount < row_left)
row_left = rcount;
if (wcount < row_left)
row_left = wcount;
return row_left;
}
private int
optimum_predictor(const stream_state * st, const stream_cursor_read * pr)
{
return cSub;
}
private int
s_PNGPE_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_PNGP_state *const ss = (stream_PNGP_state *) st;
int bpp = ss->bpp;
int status = 0;
while (pr->ptr < pr->limit) {
uint count;
if (ss->row_left == 0) {
int predictor;
if (pw->ptr >= pw->limit) {
status = 1;
break;
}
predictor =
(ss->Predictor == cOptimum ?
optimum_predictor(st, pr) :
ss->Predictor);
*++(pw->ptr) = (byte) predictor - cNone;
ss->case_index = predictor + cEncode;
ss->row_left = ss->row_count;
memset(ss->prev, 0, bpp);
continue;
}
count = s_pngp_count(st, pr, pw);
if (count == 0) {
status = 1;
break;
} {
byte *up = ss->prev_row + bpp + ss->row_count - ss->row_left;
uint n = min(count, bpp);
s_pngp_process(st, pw, ss->prev, pr, up - bpp, up, n);
if (ss->prev_row)
memcpy(up - bpp, ss->prev, n);
if (ss->row_left == 0)
continue;
if (n < bpp) {
int prev_left = bpp - n;
memmove(ss->prev, ss->prev + n, prev_left);
memcpy(ss->prev + prev_left, pr->ptr - (n - 1), n);
if (pw->ptr >= pw->limit && pr->ptr < pr->limit)
status = 1;
break;
}
count -= bpp;
s_pngp_process(st, pw, pr->ptr - (bpp - 1), pr,
up, up + bpp, count);
memcpy(ss->prev, pr->ptr - (bpp - 1), bpp);
if (ss->prev_row) {
memcpy(up, pr->ptr - (bpp + count - 1), count);
if (ss->row_left == 0)
memcpy(up + count, ss->prev, bpp);
}
}
}
return status;
}
private int
s_PNGPD_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_PNGP_state *const ss = (stream_PNGP_state *) st;
int bpp = ss->bpp;
int status = 0;
while (pr->ptr < pr->limit) {
uint count;
if (ss->row_left == 0) {
int predictor = pr->ptr[1];
if (predictor >= cOptimum - cNone) {
status = ERRC;
break;
}
pr->ptr++;
ss->case_index = predictor + cNone + cDecode;
ss->row_left = ss->row_count;
memset(ss->prev, 0, bpp);
continue;
}
count = s_pngp_count(st, pr, pw);
if (count == 0) {
status = 1;
break;
} {
byte *up = ss->prev_row + bpp + ss->row_count - ss->row_left;
uint n = min(count, bpp);
s_pngp_process(st, pw, ss->prev, pr, up - bpp, up, n);
if (ss->prev_row)
memcpy(up - bpp, ss->prev, n);
if (ss->row_left == 0)
continue;
if (n < bpp) {
int prev_left = bpp - n;
memmove(ss->prev, ss->prev + n, prev_left);
memcpy(ss->prev + prev_left, pw->ptr - (n - 1), n);
if (pw->ptr >= pw->limit && pr->ptr < pr->limit)
status = 1;
break;
}
count -= bpp;
s_pngp_process(st, pw, pw->ptr - (bpp - 1), pr,
up, up + bpp, count);
memcpy(ss->prev, pw->ptr - (bpp - 1), bpp);
if (ss->prev_row) {
memcpy(up, pw->ptr - (bpp + count - 1), count);
if (ss->row_left == 0)
memcpy(up + count, ss->prev, bpp);
}
}
}
return status;
}
const stream_template s_PNGPE_template = {
&st_PNGP_state, s_PNGPE_init, s_PNGPE_process, 1, 1, s_PNGP_release,
s_PNGP_set_defaults, s_PNGP_reinit
};
const stream_template s_PNGPD_template = {
&st_PNGP_state, s_PNGPD_init, s_PNGPD_process, 1, 1, s_PNGP_release,
s_PNGP_set_defaults, s_PNGP_reinit
};