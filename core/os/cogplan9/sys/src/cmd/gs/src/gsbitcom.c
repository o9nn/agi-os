#include "std.h"
#include "gstypes.h"
#include "gdebug.h"
#include "gsbitops.h"
#ifndef ALPHA_LSB_FIRST
#  define ALPHA_LSB_FIRST 0
#endif
static const byte half_byte_1s[16] = {
0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4
};
static const byte bits5_trailing_1s[32] = {
0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 3,
0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 4
};
static const byte bits5_leading_1s[32] = {
0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 4
};
static const byte compress_1_1[3] = {
0, 1, 1
};
static const byte compress_2_1[5] = {
0, 0, 1, 1, 1
};
static const byte compress_2_2[5] = {
0, 1, 2, 2, 3
};
static const byte compress_3_1[9] = {
0, 0, 0, 0, 1, 1, 1, 1, 1
};
static const byte compress_3_2[9] = {
0, 0, 1, 1, 2, 2, 2, 3, 3
};
static const byte compress_4_1[17] = {
0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1
};
static const byte compress_4_2[17] = {
0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3
};
static const byte compress_4_4[17] = {
0, 1, 2, 3, 4, 5, 6, 7, 8, 8, 9, 10, 11, 12, 13, 14, 15
};
static const byte *const compress_tables[4][4] = {
{compress_1_1, compress_2_1, compress_3_1, compress_4_1},
{0, compress_2_2, compress_3_2, compress_4_2},
{0, 0, 0, compress_4_4}
};
void
bits_compress_scaled(const byte * src, int srcx, uint width, uint height,
uint sraster, byte * dest, uint draster,
const gs_log2_scale_point *plog2_scale, int log2_out_bits)
{
int log2_x = plog2_scale->x, log2_y = plog2_scale->y;
int xscale = 1 << log2_x;
int yscale = 1 << log2_y;
int out_bits = 1 << log2_out_bits;
int input_byte_out_bits = out_bits << (3 - log2_x);
byte input_byte_out_mask = (1 << input_byte_out_bits) - 1;
const byte *table =
compress_tables[log2_out_bits][log2_x + log2_y - 1];
uint sskip = sraster << log2_y;
uint dwidth = (width >> log2_x) << log2_out_bits;
uint dskip = draster - ((dwidth + 7) >> 3);
uint mask = (1 << xscale) - 1;
uint count_max = 1 << (log2_x + log2_y);
const byte *srow = src + (srcx >> 3);
int in_shift_initial = 8 - xscale - (srcx & 7);
int in_shift_check = (out_bits <= xscale ? 8 - xscale : -1);
byte *d = dest;
uint h;
for (h = height; h; srow += sskip, h -= yscale) {
const byte *s = srow;
#if ALPHA_LSB_FIRST
#  define out_shift_initial 0
#  define out_shift_update(out_shift, nbits) ((out_shift += (nbits)) >= 8)
#else
#  define out_shift_initial (8 - out_bits)
#  define out_shift_update(out_shift, nbits) ((out_shift -= (nbits)) < 0)
#endif
int out_shift = out_shift_initial;
byte out = 0;
int in_shift = in_shift_initial;
int dw = 8 - (srcx & 7);
int w;
for (w = width; w > 0; w -= dw, dw = 8) {
int index;
int in_shift_final = (w >= dw ? 0 : dw - w);
if (in_shift == in_shift_check && in_shift_final == 0)
switch (*s) {
case 0:
for (index = sraster; index != sskip; index += sraster)
if (s[index] != 0)
goto p;
if (out_shift_update(out_shift, input_byte_out_bits))
*d++ = out, out_shift &= 7, out = 0;
s++;
continue;
#if !ALPHA_LSB_FIRST
case 0xff:
for (index = sraster; index != sskip; index += sraster)
if (s[index] != 0xff)
goto p;
{
int shift =
(out_shift -= input_byte_out_bits) + out_bits;
if (shift > 0)
out |= input_byte_out_mask << shift;
else {
out |= input_byte_out_mask >> -shift;
*d++ = out;
out_shift += 8;
out = input_byte_out_mask << (8 + shift);
}
}
s++;
continue;
#endif
default:
;
}
p:
do {
uint count;
for (index = 0, count = 0; index != sskip;
index += sraster
)
count += half_byte_1s[(s[index] >> in_shift) & mask];
if (count != 0 && table[count] == 0) {
uint orig_count = count;
uint shifted_mask = mask << in_shift;
byte in;
if_debug3('B', "[B]count(%d,%d)=%d\n",
(width - w) / xscale,
(height - h) / yscale, count);
if (yscale > 1) {
if (h < height && (in = s[0] & shifted_mask) != 0) {
uint lower;
for (index = 0, lower = 0;
-(index -= sraster) <= sskip &&
(in &= s[index]) != 0;
)
lower += half_byte_1s[in >> in_shift];
if_debug1('B', "[B]  lower adds %d\n",
lower);
if (lower <= orig_count)
count += lower;
}
if (h > yscale && (in = s[sskip - sraster] & shifted_mask) != 0) {
uint upper;
for (index = sskip, upper = 0;
index < sskip << 1 &&
(in &= s[index]) != 0;
index += sraster
)
upper += half_byte_1s[in >> in_shift];
if_debug1('B', "[B]  upper adds %d\n",
upper);
if (upper < orig_count)
count += upper;
}
}
if (xscale > 1) {
uint mask1 = (mask << 1) + 1;
if (w < width) {
int lshift = in_shift + xscale - 1;
uint left;
for (index = 0, left = 0;
index < sskip; index += sraster
) {
uint bits =
((s[index - 1] << 8) +
s[index]) >> lshift;
left += bits5_trailing_1s[bits & mask1];
}
if_debug1('B', "[B]  left adds %d\n",
left);
if (left < orig_count)
count += left;
}
if (w > xscale) {
int rshift = in_shift - xscale + 8;
uint right;
for (index = 0, right = 0;
index < sskip; index += sraster
) {
uint bits =
((s[index] << 8) +
s[index + 1]) >> rshift;
right += bits5_leading_1s[(bits & mask1) << (4 - xscale)];
}
if_debug1('B', "[B]  right adds %d\n",
right);
if (right <= orig_count)
count += right;
}
}
if (count > count_max)
count = count_max;
}
out += table[count] << out_shift;
if (out_shift_update(out_shift, out_bits))
*d++ = out, out_shift &= 7, out = 0;
}
while ((in_shift -= xscale) >= in_shift_final);
s++, in_shift += 8;
}
if (out_shift != out_shift_initial)
*d++ = out;
for (w = dskip; w != 0; w--)
*d++ = 0;
#undef out_shift_initial
#undef out_shift_update
}
}