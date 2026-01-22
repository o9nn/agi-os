#include "math_.h"
#include "memory_.h"
#include "ghost.h"
#include "ierrors.h"
#include "stream.h"
#include "ibnum.h"
#include "imemory.h"
#include "iutil.h"
const byte enc_num_bytes[] = {
enc_num_bytes_values
};
int
num_array_format(const ref * op)
{
switch (r_type(op)) {
case t_string:
{
const byte *bp = op->value.bytes;
int format;
if (r_size(op) < 4 || bp[0] != bt_num_array_value)
return_error(e_rangecheck);
format = bp[1];
if (!num_is_valid(format) ||
sdecodeshort(bp + 2, format) !=
(r_size(op) - 4) / encoded_number_bytes(format)
)
return_error(e_rangecheck);
return format;
}
case t_array:
case t_mixedarray:
case t_shortarray:
return num_array;
default:
return_error(e_typecheck);
}
}
uint
num_array_size(const ref * op, int format)
{
return (format == num_array ? r_size(op) :
(r_size(op) - 4) / encoded_number_bytes(format));
}
int
num_array_get(const gs_memory_t *mem, const ref * op, int format, uint index, ref * np)
{
if (format == num_array) {
int code = array_get(mem, op, (long)index, np);
if (code < 0)
return t_null;
switch (r_type(np)) {
case t_integer:
return t_integer;
case t_real:
return t_real;
default:
return_error(e_rangecheck);
}
} else {
uint nbytes = encoded_number_bytes(format);
if (index >= (r_size(op) - 4) / nbytes)
return t_null;
return sdecode_number(op->value.bytes + 4 + index * nbytes,
format, np);
}
}
static const double binary_scale[32] = {
#define EXPN2(n) (0.5 / (1L << (n-1)))
1.0, EXPN2(1), EXPN2(2), EXPN2(3),
EXPN2(4), EXPN2(5), EXPN2(6), EXPN2(7),
EXPN2(8), EXPN2(9), EXPN2(10), EXPN2(11),
EXPN2(12), EXPN2(13), EXPN2(14), EXPN2(15),
EXPN2(16), EXPN2(17), EXPN2(18), EXPN2(19),
EXPN2(20), EXPN2(21), EXPN2(22), EXPN2(23),
EXPN2(24), EXPN2(25), EXPN2(26), EXPN2(27),
EXPN2(28), EXPN2(29), EXPN2(30), EXPN2(31)
#undef EXPN2
};
int
sdecode_number(const byte * str, int format, ref * np)
{
switch (format & 0x170) {
case num_int32:
case num_int32 + 16:
if ((format & 31) == 0) {
np->value.intval = sdecodelong(str, format);
return t_integer;
} else {
np->value.realval =
(double)sdecodelong(str, format) *
binary_scale[format & 31];
return t_real;
}
case num_int16:
if ((format & 15) == 0) {
np->value.intval = sdecodeshort(str, format);
return t_integer;
} else {
np->value.realval =
sdecodeshort(str, format) *
binary_scale[format & 15];
return t_real;
}
case num_float:
np->value.realval = sdecodefloat(str, format);
return t_real;
default:
return_error(e_syntaxerror);
}
}
uint
sdecodeushort(const byte * p, int format)
{
int a = p[0], b = p[1];
return (num_is_lsb(format) ? (b << 8) + a : (a << 8) + b);
}
int
sdecodeshort(const byte * p, int format)
{
int v = (int)sdecodeushort(p, format);
return (v & 0x7fff) - (v & 0x8000);
}
long
sdecodelong(const byte * p, int format)
{
int a = p[0], b = p[1], c = p[2], d = p[3];
long v = (num_is_lsb(format) ?
((long)d << 24) + ((long)c << 16) + (b << 8) + a :
((long)a << 24) + ((long)b << 16) + (c << 8) + d);
#if arch_sizeof_long > 4
v = (v ^ 0x80000000L) - 0x80000000L;
#endif
return v;
}
float
sdecodefloat(const byte * p, int format)
{
bits32 lnum;
float fnum;
if ((format & ~(num_msb | num_lsb)) == num_float_native) {
memcpy(&lnum, p, 4);
fnum = *(float *)&lnum;
} else {
lnum = (bits32) sdecodelong(p, format);
#if !arch_floats_are_IEEE
{
int sign_expt = lnum >> 23;
int expt = sign_expt & 0xff;
long mant = lnum & 0x7fffff;
if (expt == 0 && mant == 0)
fnum = 0;
else {
mant += 0x800000;
fnum = (float)ldexp((float)mant, expt - 127 - 23);
}
if (sign_expt & 0x100)
fnum = -fnum;
}
#else
fnum = *(float *)&lnum;
#endif
}
return fnum;
}