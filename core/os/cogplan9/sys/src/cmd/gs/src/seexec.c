#include "stdio_.h"
#include "strimpl.h"
#include "sfilter.h"
#include "gscrypt1.h"
#include "scanchar.h"
private_st_exE_state();
private int
s_exE_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_exE_state *const ss = (stream_exE_state *) st;
const byte *p = pr->ptr;
byte *q = pw->ptr;
uint rcount = pr->limit - p;
uint wcount = pw->limit - q;
uint count;
int status;
if (rcount <= wcount)
count = rcount, status = 0;
else
count = wcount, status = 1;
gs_type1_encrypt(q + 1, p + 1, count, (crypt_state *)&ss->cstate);
pr->ptr += count;
pw->ptr += count;
return status;
}
const stream_template s_exE_template = {
&st_exE_state, NULL, s_exE_process, 1, 2
};
private_st_exD_state();
private void
s_exD_set_defaults(stream_state * st)
{
stream_exD_state *const ss = (stream_exD_state *) st;
ss->binary = -1;
ss->lenIV = 4;
ss->record_left = max_long;
ss->hex_left = max_long;
ss->pfb_state = 0;
}
private int
s_exD_init(stream_state * st)
{
stream_exD_state *const ss = (stream_exD_state *) st;
ss->odd = -1;
ss->skip = ss->lenIV;
return 0;
}
private int
s_exD_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_exD_state *const ss = (stream_exD_state *) st;
const byte *p = pr->ptr;
byte *q = pw->ptr;
int skip = ss->skip;
int rcount = pr->limit - p;
int wcount = pw->limit - q;
int status = 0;
int count = (wcount < rcount ? (status = 1, wcount) : rcount);
if (ss->binary < 0) {
const byte *const decoder = scan_char_decoder;
int i;
if (ss->pfb_state == 0) {
for (; rcount; rcount--, p++) {
byte c = p[1];
if(c != '\t' && c != char_CR && c != char_EOL && c != ' ')
break;
}
pr->ptr = p;
count = min(wcount, rcount);
}
if (rcount < 8 && !last)
return 0;
ss->binary = 0;
for (i = min(8, rcount); i > 0; i--)
if (!(decoder[p[i]] <= 0xf ||
decoder[p[i]] == ctype_space)
) {
ss->binary = 1;
if (ss->pfb_state != 0) {
ss->record_left = ss->pfb_state->record_left;
}
break;
}
}
if (ss->binary) {
if (count > ss->record_left) {
count = ss->record_left;
status = 0;
}
if ((ss->record_left -= count) == 0)
ss->record_left = max_long;
pr->ptr = p + count;
} else {
stream_cursor_read r;
const byte *start;
hp:	r = *pr;
start = r.ptr;
if (r.limit - r.ptr > ss->hex_left)
r.limit = r.ptr + ss->hex_left;
status = s_hex_process(&r, pw, &ss->odd,
hex_ignore_leading_whitespace);
pr->ptr = r.ptr;
ss->hex_left -= r.ptr - start;
if (ss->hex_left == 0)
ss->binary = 1;
count = pw->ptr - q;
if (status < 0 && ss->odd < 0) {
if (count) {
--p;
status = 0;
} else if (*p == '%')
goto hp;
}
p = q;
}
if (skip >= count && skip != 0) {
gs_type1_decrypt(q + 1, p + 1, count,
(crypt_state *) & ss->cstate);
ss->skip -= count;
count = 0;
status = 0;
} else {
gs_type1_decrypt(q + 1, p + 1, skip,
(crypt_state *) & ss->cstate);
count -= skip;
gs_type1_decrypt(q + 1, p + 1 + skip, count,
(crypt_state *) & ss->cstate);
ss->skip = 0;
}
pw->ptr = q + count;
return status;
}
const stream_template s_exD_template = {
&st_exD_state, s_exD_init, s_exD_process, 8, 200,
NULL, s_exD_set_defaults
};