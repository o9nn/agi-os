#include "stdio_.h"
#include "memory_.h"
#include "string_.h"
#include "strimpl.h"
#include "sstring.h"
#include "scanchar.h"
private_st_AXE_state();
private int
s_AXE_init(stream_state * st)
{
stream_AXE_state *const ss = (stream_AXE_state *) st;
return s_AXE_init_inline(ss);
}
private int
s_AXE_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_AXE_state *const ss = (stream_AXE_state *) st;
const byte *p = pr->ptr;
byte *q = pw->ptr;
int rcount = pr->limit - p;
int wcount = pw->limit - q;
int count;
int pos = ss->count;
const char *hex_digits = "0123456789ABCDEF";
int status = 0;
if (last && ss->EndOfData)
wcount--;
wcount -= (wcount + 64) / 65;
wcount >>= 1;
count = (wcount < rcount ? (status = 1, wcount) : rcount);
while (--count >= 0) {
*++q = hex_digits[*++p >> 4];
*++q = hex_digits[*p & 0xf];
if (!(++pos & 31) && (count != 0 || !last))
*++q = '\n';
}
if (last && status == 0 && ss->EndOfData)
*++q = '>';
pr->ptr = p;
pw->ptr = q;
ss->count = pos & 31;
return status;
}
const stream_template s_AXE_template =
{&st_AXE_state, s_AXE_init, s_AXE_process, 1, 3
};
private_st_AXD_state();
private int
s_AXD_init(stream_state * st)
{
stream_AXD_state *const ss = (stream_AXD_state *) st;
return s_AXD_init_inline(ss);
}
private int
s_AXD_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_AXD_state *const ss = (stream_AXD_state *) st;
int code = s_hex_process(pr, pw, &ss->odd, hex_ignore_whitespace);
switch (code) {
case 0:
if (ss->odd >= 0 && last) {
if (pw->ptr == pw->limit)
return 1;
*++(pw->ptr) = ss->odd << 4;
}
case 1:
for (; pr->ptr < pr->limit; pr->ptr++)
if (scan_char_decoder[pr->ptr[1]] != ctype_space) {
if (pr->ptr[1] == '>') {
pr->ptr++;
goto eod;
}
return 1;
}
return 0;
default:
return code;
case ERRC:
;
}
if (*pr->ptr != '>') {
--(pr->ptr);
return ERRC;
}
eod:if (ss->odd >= 0) {
if (pw->ptr == pw->limit)
return 1;
*++(pw->ptr) = ss->odd << 4;
}
return EOFC;
}
const stream_template s_AXD_template =
{&st_AXD_state, s_AXD_init, s_AXD_process, 2, 1
};
private int
s_PSSE_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
const byte *p = pr->ptr;
const byte *rlimit = pr->limit;
byte *q = pw->ptr;
byte *wlimit = pw->limit;
int status = 0;
while (p < rlimit) {
int c = *++p;
if (c < 32 || c >= 127) {
const char *pesc;
const char *const esc = "\n\r\t\b\f";
if (c < 32 && c != 0 && (pesc = strchr(esc, c)) != 0) {
if (wlimit - q < 2) {
--p;
status = 1;
break;
}
*++q = '\\';
*++q = "nrtbf"[pesc - esc];
continue;
}
if (wlimit - q < 4) {
--p;
status = 1;
break;
}
q[1] = '\\';
q[2] = (c >> 6) + '0';
q[3] = ((c >> 3) & 7) + '0';
q[4] = (c & 7) + '0';
q += 4;
continue;
} else if (c == '(' || c == ')' || c == '\\') {
if (wlimit - q < 2) {
--p;
status = 1;
break;
}
*++q = '\\';
} else {
if (q == wlimit) {
--p;
status = 1;
break;
}
}
*++q = c;
}
if (last && status == 0) {
if (q == wlimit)
status = 1;
else
*++q = ')';
}
pr->ptr = p;
pw->ptr = q;
return status;
}
const stream_template s_PSSE_template =
{&st_stream_state, NULL, s_PSSE_process, 1, 4
};
private_st_PSSD_state();
int
s_PSSD_init(stream_state * st)
{
stream_PSSD_state *const ss = (stream_PSSD_state *) st;
ss->from_string = false;
return s_PSSD_partially_init_inline(ss);
}
private int
s_PSSD_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_PSSD_state *const ss = (stream_PSSD_state *) st;
const byte *p = pr->ptr;
const byte *rlimit = pr->limit;
byte *q = pw->ptr;
byte *wlimit = pw->limit;
int status = 0;
int c;
#define check_p(n)\
if ( p == rlimit ) { p -= n; goto out; }
#define check_q(n)\
if ( q == wlimit ) { p -= n; status = 1; goto out; }
while (p < rlimit) {
c = *++p;
if (c == '\\' && !ss->from_string) {
check_p(1);
switch ((c = *++p)) {
case 'n':
c = '\n';
goto put;
case 'r':
c = '\r';
goto put;
case 't':
c = '\t';
goto put;
case 'b':
c = '\b';
goto put;
case 'f':
c = '\f';
goto put;
default:
put:check_q(2);
*++q = c;
continue;
case char_CR:
check_p(2);
if (p[1] == char_EOL)
p++;
continue;
case char_EOL:
continue;
case '0':
case '1':
case '2':
case '3':
case '4':
case '5':
case '6':
case '7':
{
int d;
check_p(2);
d = p[1];
c -= '0';
if (d >= '0' && d <= '7') {
if (p + 1 == rlimit) {
p -= 2;
goto out;
}
check_q(2);
c = (c << 3) + d - '0';
d = p[2];
if (d >= '0' && d <= '7') {
c = (c << 3) + d - '0';
p += 2;
} else
p++;
} else
check_q(2);
*++q = c;
continue;
}
}
} else
switch (c) {
case '(':
check_q(1);
ss->depth++;
break;
case ')':
if (ss->depth == 0) {
status = EOFC;
goto out;
}
check_q(1);
ss->depth--;
break;
case char_CR:
check_p(1);
check_q(1);
if (p[1] == char_EOL)
p++;
*++q = '\n';
continue;
case char_EOL:
c = '\n';
default:
check_q(1);
break;
}
*++q = c;
}
#undef check_p
#undef check_q
out:pr->ptr = p;
pw->ptr = q;
if (last && status == 0 && p != rlimit)
status = ERRC;
return status;
}
const stream_template s_PSSD_template =
{&st_PSSD_state, s_PSSD_init, s_PSSD_process, 4, 1
};
int
s_hex_process(stream_cursor_read * pr, stream_cursor_write * pw,
int *odd_digit, hex_syntax syntax)
{
const byte *p = pr->ptr;
const byte *rlimit = pr->limit;
byte *q = pw->ptr;
byte *wlimit = pw->limit;
byte *q0 = q;
byte val1 = (byte) * odd_digit;
byte val2;
uint rcount;
byte *flimit;
const byte *const decoder = scan_char_decoder;
int code = 0;
if (q >= wlimit)
return 1;
if (val1 <= 0xf)
goto d2;
d1:if ((rcount = (rlimit - p) >> 1) == 0)
goto x1;
flimit = (rcount < wlimit - q ? q + rcount : wlimit);
f1:if ((val1 = decoder[p[1]]) <= 0xf &&
(val2 = decoder[p[2]]) <= 0xf
) {
p += 2;
*++q = (val1 << 4) + val2;
if (q < flimit)
goto f1;
if (q >= wlimit)
goto px;
}
x1:if (p >= rlimit)
goto end1;
if ((val1 = decoder[*++p]) > 0xf) {
if (val1 == ctype_space) {
switch (syntax) {
case hex_ignore_whitespace:
goto x1;
case hex_ignore_leading_whitespace:
if (q == q0 && *odd_digit < 0)
goto x1;
--p;
code = 1;
goto end1;
case hex_ignore_garbage:
goto x1;
}
} else if (syntax == hex_ignore_garbage)
goto x1;
code = ERRC;
goto end1;
}
d2:if (p >= rlimit) {
*odd_digit = val1;
goto ended;
}
if ((val2 = decoder[*++p]) > 0xf) {
if (val2 == ctype_space)
switch (syntax) {
case hex_ignore_whitespace:
goto d2;
case hex_ignore_leading_whitespace:
if (q == q0)
goto d2;
--p;
*odd_digit = val1;
code = 1;
goto ended;
case hex_ignore_garbage:
;
}
if (syntax == hex_ignore_garbage)
goto d2;
*odd_digit = val1;
code = ERRC;
goto ended;
}
*++q = (val1 << 4) + val2;
if (q < wlimit)
goto d1;
px:code = 1;
end1:*odd_digit = -1;
ended:pr->ptr = p;
pw->ptr = q;
return code;
}