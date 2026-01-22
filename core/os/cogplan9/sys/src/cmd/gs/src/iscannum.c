#include "math_.h"
#include "ghost.h"
#include "ierrors.h"
#include "scommon.h"
#include "iscannum.h"
#include "scanchar.h"
#include "store.h"
int
scan_number(const byte * str, const byte * end, int sign,
ref * pref, const byte ** psp, const bool PDFScanInvNum)
{
const byte *sp = str;
#define GET_NEXT(cvar, sp, end_action)\
if (sp >= end) { end_action; } else cvar = *sp++
#define NUM_POWERS_10 6
static const float powers_10[NUM_POWERS_10 + 1] = {
1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6
};
static const double neg_powers_10[NUM_POWERS_10 + 1] = {
1e0, 1e-1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6
};
int ival;
long lval;
double dval;
int exp10;
int code = 0;
int c, d;
const byte *const decoder = scan_char_decoder;
#define IS_DIGIT(d, c)\
((d = decoder[c]) < 10)
#define WOULD_OVERFLOW(val, d, maxv)\
(val >= maxv / 10 && (val > maxv / 10 || d > (int)(maxv % 10)))
GET_NEXT(c, sp, return_error(e_syntaxerror));
if (!IS_DIGIT(d, c)) {
if (c != '.')
return_error(e_syntaxerror);
GET_NEXT(c, sp, return_error(e_syntaxerror));
if (!IS_DIGIT(d, c))
return_error(e_syntaxerror);
ival = 0;
goto i2r;
}
ival = d;
if (end - sp >= 3) {
if (!IS_DIGIT(d, (c = *sp))) {
sp++;
goto ind;
}
ival = ival * 10 + d;
if (!IS_DIGIT(d, (c = sp[1]))) {
sp += 2;
goto ind;
}
ival = ival * 10 + d;
sp += 3;
if (!IS_DIGIT(d, (c = sp[-1])))
goto ind;
ival = ival * 10 + d;
}
for (;; ival = ival * 10 + d) {
GET_NEXT(c, sp, goto iret);
if (!IS_DIGIT(d, c))
break;
if (WOULD_OVERFLOW(ival, d, max_int))
goto i2l;
}
ind:
switch (c) {
case '.':
GET_NEXT(c, sp, c = EOFC);
goto i2r;
default:
*psp = sp;
code = 1;
break;
case 'e':
case 'E':
if (sign < 0)
ival = -ival;
dval = ival;
exp10 = 0;
goto fe;
case '#':
{
const uint radix = (uint)ival;
ulong uval = 0, lmax;
if (sign || radix < min_radix || radix > max_radix)
return_error(e_syntaxerror);
if (!(radix & (radix - 1))) {
int shift;
switch (radix) {
case 2:
shift = 1, lmax = max_ulong >> 1;
break;
case 4:
shift = 2, lmax = max_ulong >> 2;
break;
case 8:
shift = 3, lmax = max_ulong >> 3;
break;
case 16:
shift = 4, lmax = max_ulong >> 4;
break;
case 32:
shift = 5, lmax = max_ulong >> 5;
break;
default:
return_error(e_rangecheck);
}
for (;; uval = (uval << shift) + d) {
GET_NEXT(c, sp, break);
d = decoder[c];
if (d >= radix) {
*psp = sp;
code = 1;
break;
}
if (uval > lmax)
return_error(e_limitcheck);
}
} else {
int lrem = max_ulong % radix;
lmax = max_ulong / radix;
for (;; uval = uval * radix + d) {
GET_NEXT(c, sp, break);
d = decoder[c];
if (d >= radix) {
*psp = sp;
code = 1;
break;
}
if (uval >= lmax &&
(uval > lmax || d > lrem)
)
return_error(e_limitcheck);
}
}
make_int(pref, uval);
return code;
}
}
iret:
make_int(pref, (sign < 0 ? -ival : ival));
return code;
i2l:
for (lval = ival;;) {
if (WOULD_OVERFLOW(lval, d, max_long)) {
if (lval == max_long / 10 &&
d == (int)(max_long % 10) + 1 && sign < 0
) {
GET_NEXT(c, sp, c = EOFC);
dval = -(double)min_long;
if (c == 'e' || c == 'E') {
exp10 = 0;
goto fs;
} else if (c == '.') {
GET_NEXT(c, sp, c = EOFC);
exp10 = 0;
goto fd;
} else if (!IS_DIGIT(d, c)) {
lval = min_long;
break;
}
} else
dval = lval;
goto l2d;
}
lval = lval * 10 + d;
GET_NEXT(c, sp, goto lret);
if (!IS_DIGIT(d, c))
break;
}
switch (c) {
case '.':
GET_NEXT(c, sp, c = EOFC);
exp10 = 0;
goto l2r;
case EOFC:
break;
default:
*psp = sp;
code = 1;
break;
case 'e':
case 'E':
exp10 = 0;
goto le;
case '#':
return_error(e_syntaxerror);
}
lret:
make_int(pref, (sign < 0 ? -lval : lval));
return code;
l2d:
exp10 = 0;
for (;;) {
dval = dval * 10 + d;
GET_NEXT(c, sp, c = EOFC);
if (!IS_DIGIT(d, c))
break;
}
switch (c) {
case '.':
GET_NEXT(c, sp, c = EOFC);
exp10 = 0;
goto fd;
default:
*psp = sp;
code = 1;
case EOFC:
if (sign < 0)
dval = -dval;
goto rret;
case 'e':
case 'E':
exp10 = 0;
goto fs;
case '#':
return_error(e_syntaxerror);
}
i2r:
exp10 = 0;
while (IS_DIGIT(d, c) || c == '-') {
if (c == '-') {
if (!PDFScanInvNum)
break;
do {
GET_NEXT(c, sp, c = EOFC);
} while (IS_DIGIT(d, c));
break;
}
if (WOULD_OVERFLOW(ival, d, max_int)) {
lval = ival;
goto l2r;
}
ival = ival * 10 + d;
exp10--;
GET_NEXT(c, sp, c = EOFC);
}
if (sign < 0)
ival = -ival;
if (!(c == 'e' || c == 'E' || exp10 < -NUM_POWERS_10)) {
if (c != EOFC)
*psp = sp, code = 1;
make_real(pref, ival * neg_powers_10[-exp10]);
return code;
}
dval = ival;
goto fe;
l2r:
while (IS_DIGIT(d, c) || c == '-') {
if (c == '-') {
if (!PDFScanInvNum)
break;
do {
GET_NEXT(c, sp, c = EOFC);
} while (IS_DIGIT(d, c));
break;
}
if (WOULD_OVERFLOW(lval, d, max_long)) {
dval = lval;
goto fd;
}
lval = lval * 10 + d;
exp10--;
GET_NEXT(c, sp, c = EOFC);
}
le:
if (sign < 0)
lval = -lval;
dval = lval;
goto fe;
fd:
while (IS_DIGIT(d, c)) {
dval = dval * 10 + d;
exp10--;
GET_NEXT(c, sp, c = EOFC);
}
fs:
if (sign < 0)
dval = -dval;
fe:
switch (c) {
case 'e':
case 'E':
{
int esign = 0;
int iexp;
GET_NEXT(c, sp, return_error(e_syntaxerror));
switch (c) {
case '-':
esign = 1;
case '+':
GET_NEXT(c, sp, return_error(e_syntaxerror));
}
if (!IS_DIGIT(d, c))
return_error(e_syntaxerror);
iexp = d;
for (;; iexp = iexp * 10 + d) {
GET_NEXT(c, sp, break);
if (!IS_DIGIT(d, c)) {
*psp = sp;
code = 1;
break;
}
if (iexp > 99)
return_error(e_limitcheck);
}
if (esign)
exp10 -= iexp;
else
exp10 += iexp;
break;
}
default:
*psp = sp;
code = 1;
case EOFC:
;
}
if (exp10 > 0) {
while (exp10 > NUM_POWERS_10)
dval *= powers_10[NUM_POWERS_10],
exp10 -= NUM_POWERS_10;
if (exp10 > 0)
dval *= powers_10[exp10];
} else if (exp10 < 0) {
while (exp10 < -NUM_POWERS_10)
dval /= powers_10[NUM_POWERS_10],
exp10 += NUM_POWERS_10;
if (exp10 < 0)
dval /= powers_10[-exp10];
}
if (dval >= 0) {
if (dval > MAX_FLOAT)
return_error(e_limitcheck);
} else {
if (dval < -MAX_FLOAT)
return_error(e_limitcheck);
}
rret:
make_real(pref, dval);
return code;
}