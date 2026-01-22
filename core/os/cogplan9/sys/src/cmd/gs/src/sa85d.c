#include "std.h"
#include "strimpl.h"
#include "sa85d.h"
#include "scanchar.h"
private_st_A85D_state();
private int
s_A85D_init(stream_state * st)
{
stream_A85D_state *const ss = (stream_A85D_state *) st;
return s_A85D_init_inline(ss);
}
private int a85d_finish(int, ulong, stream_cursor_write *);
private int
s_A85D_process(stream_state * st, stream_cursor_read * pr,
stream_cursor_write * pw, bool last)
{
stream_A85D_state *const ss = (stream_A85D_state *) st;
register const byte *p = pr->ptr;
register byte *q = pw->ptr;
const byte *rlimit = pr->limit;
byte *wlimit = pw->limit;
int ccount = ss->odd;
ulong word = ss->word;
int status = 0;
while (p < rlimit) {
int ch = *++p;
uint ccode = ch - '!';
if (ccode < 85) {
if (ccount == 4) {
if (wlimit - q < 4) {
p--;
status = 1;
break;
}
if (word >= 0x03030303 && ccode > 0) {
status = ERRC;
break;
}
word = word * 85 + ccode;
q[1] = (byte) (word >> 24);
q[2] = (byte) (word >> 16);
q[3] = (byte) ((uint) word >> 8);
q[4] = (byte) word;
q += 4;
word = 0;
ccount = 0;
} else {
word = word * 85 + ccode;
++ccount;
}
} else if (ch == 'z' && ccount == 0) {
if (wlimit - q < 4) {
p--;
status = 1;
break;
}
q[1] = q[2] = q[3] = q[4] = 0,
q += 4;
} else if (scan_char_decoder[ch] == ctype_space)
DO_NOTHING;
else if (ch == '~') {
int i = 1;
if (p == rlimit) {
if (last)
status = ERRC;
else
p--;
break;
}
if ((int)(wlimit - q) < ccount - 1) {
status = 1;
p--;
break;
}
while ((p[i] == 13 || p[i] == 10) && (p+i <= rlimit))
i++;
if (p[i] != '>') {
if (p+i == rlimit) {
if (last)
status = ERRC;
else
p--;
}
break;
}
p += i;
pw->ptr = q;
status = a85d_finish(ccount, word, pw);
q = pw->ptr;
break;
} else {
status = ERRC;
break;
}
}
pw->ptr = q;
if (status == 0 && last) {
if ((int)(wlimit - q) < ccount - 1)
status = 1;
else
status = a85d_finish(ccount, word, pw);
}
pr->ptr = p;
ss->odd = ccount;
ss->word = word;
return status;
}
private int
a85d_finish(int ccount, ulong word, stream_cursor_write * pw)
{
byte *q = pw->ptr;
int status = EOFC;
switch (ccount) {
case 0:
break;
case 1:
status = ERRC;
break;
case 2:
word = word * (85L * 85 * 85) + 85L * 85 * 85 - 1L;
goto o1;
case 3:
word = word * (85L * 85) + 85L * 85L - 1L;
goto o2;
case 4:
word = word * 85L + 84L;
q[3] = (byte) (word >> 8);
o2: q[2] = (byte) (word >> 16);
o1: q[1] = (byte) (word >> 24);
q += ccount - 1;
pw->ptr = q;
}
return status;
}
const stream_template s_A85D_template = {
&st_A85D_state, s_A85D_init, s_A85D_process, 2, 4
};